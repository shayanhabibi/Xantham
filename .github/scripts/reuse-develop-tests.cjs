function eligiblePullRequest(context) {
  const pr = context.payload.pull_request;
  return context.eventName === 'pull_request' && pr?.base.ref === 'master' &&
    pr.head.ref === 'develop' && pr.head.repo?.full_name === context.payload.repository.full_name;
}

function successfulRun(runs, sha, repository) {
  return runs.find(run => ['push', 'workflow_dispatch'].includes(run.event) && run.head_branch === 'develop' &&
    run.head_sha === sha && run.head_repository?.full_name === repository && run.repository?.full_name === repository &&
    run.status === 'completed' && run.conclusion === 'success');
}

function eligibleMaster(context) {
  return ['push', 'workflow_dispatch'].includes(context.eventName) && context.ref === 'refs/heads/master';
}

async function testedCommit(github, context, core) {
  const { owner, repo } = context.repo;
  if (eligiblePullRequest(context)) {
    const pr = context.payload.pull_request;
    const comparison = await github.rest.repos.compareCommits({ owner, repo, base: pr.base.sha, head: pr.head.sha });
    if (comparison.data.merge_base_commit.sha === pr.base.sha) return { sha: pr.head.sha, master: false };
    core.info('Master is not an ancestor of develop; run the full suite against the merge.');
  } else if (eligibleMaster(context)) {
    const { data: merge } = await github.rest.repos.getCommit({ owner, repo, ref: context.sha });
    if (merge.sha !== context.sha || merge.parents?.length !== 2 || !merge.commit?.tree?.sha) return;
    const { data: parent } = await github.rest.repos.getCommit({ owner, repo, ref: merge.parents[1].sha });
    if (parent.sha === merge.parents[1].sha && parent.commit?.tree?.sha === merge.commit.tree.sha)
      return { sha: parent.sha, master: true };
    core.info('Master differs from its merge parent; run the full suite against master.');
  }
}

function fullTestJob(jobs) {
  return jobs.some(job => job.name === 'test' && job.conclusion === 'success' &&
    job.steps?.some(step => step.name === 'Test' && step.conclusion === 'success'));
}

module.exports = async function reuseDevelopTests({ github, context, core, sleep = ms => new Promise(resolve => setTimeout(resolve, ms)) }) {
  core.setOutput('reused', 'false');
  core.setOutput('master-reused', 'false');
  const { owner, repo } = context.repo;
  const proof = await testedCommit(github, context, core).catch(() => {
    core.info('Unable to verify commit trees; run the full suite.');
  });
  if (!proof) return;
  // Release PRs may await develop for thirty minutes; publishing only waits two minutes.
  const attempts = proof.master ? 3 : 7;
  const wait = proof.master ? 60000 : 300000;
  for (let attempt = 0; attempt < attempts; attempt++) {
    const response = await github.rest.actions.listWorkflowRuns({ owner, repo, workflow_id: 'test.yml',
      branch: 'develop', head_sha: proof.sha, per_page: 100 }).catch(() => ({ data: { workflow_runs: [] } }));
    const runs = response.data.workflow_runs;
    for (const passed of runs.filter(run => successfulRun([run], proof.sha, `${owner}/${repo}`))) {
      const jobs = await github.paginate(github.rest.actions.listJobsForWorkflowRun, {
        owner, repo, run_id: passed.id, per_page: 100
      }).catch(() => []);
      if (!fullTestJob(jobs)) continue;
      core.setOutput('reused', 'true');
      core.setOutput('master-reused', String(proof.master));
      core.setOutput('source-sha', proof.sha);
      core.setOutput('source-run', String(passed.id));
      await core.summary.addHeading('Reused develop tests').addLink(`Exact commit ${proof.sha}`, passed.html_url)
        .addRaw(proof.master ? '\nMaster has the identical Git tree as this fully tested merge parent.\n'
          : '\nMaster is an ancestor of this tested commit; the merge has the same source tree.\n').write();
      return;
    }
    const pending = runs.some(run => run.head_sha === proof.sha && run.status !== 'completed');
    if (!pending || attempt === attempts - 1) {
      core.info('No successful exact-commit develop run is available; run the full suite.');
      return;
    }
    core.info(`Waiting ${wait / 1000} seconds for the exact-commit develop test run.`);
    await sleep(wait);
  }
};
module.exports.eligiblePullRequest = eligiblePullRequest;
module.exports.successfulRun = successfulRun;
module.exports.eligibleMaster = eligibleMaster;
module.exports.fullTestJob = fullTestJob;
