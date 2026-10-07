function eligiblePullRequest(context) {
  const pr = context.payload.pull_request;
  return context.eventName === 'pull_request' && pr?.base.ref === 'master' &&
    pr.head.ref === 'develop' && pr.head.repo?.full_name === context.payload.repository.full_name;
}

function successfulRun(runs, sha, repository) {
  return runs.find(run => ['push', 'workflow_dispatch'].includes(run.event) && run.head_branch === 'develop' &&
    run.head_sha === sha && run.head_repository?.full_name === repository &&
    run.status === 'completed' && run.conclusion === 'success');
}

module.exports = async function reuseDevelopTests({ github, context, core, sleep = ms => new Promise(resolve => setTimeout(resolve, ms)) }) {
  core.setOutput('reused', 'false');
  if (!eligiblePullRequest(context)) return;
  const { owner, repo } = context.repo;
  const pr = context.payload.pull_request;
  const comparison = await github.rest.repos.compareCommits({ owner, repo, base: pr.base.sha, head: pr.head.sha });
  if (comparison.data.merge_base_commit.sha !== pr.base.sha) {
    core.info('Master is not an ancestor of develop; run the full suite against the merge.');
    return;
  }
  for (let attempt = 0; attempt < 7; attempt++) {
    const response = await github.rest.actions.listWorkflowRuns({ owner, repo, workflow_id: 'test.yml',
      branch: 'develop', head_sha: pr.head.sha, per_page: 100 });
    const runs = response.data.workflow_runs;
    const passed = successfulRun(runs, pr.head.sha, `${owner}/${repo}`);
    if (passed) {
      const jobs = await github.paginate(github.rest.actions.listJobsForWorkflowRun, {
        owner, repo, run_id: passed.id, per_page: 100
      });
      if (!jobs.some(job => job.name === 'test' && job.steps.some(step => step.name === 'Test' && step.conclusion === 'success'))) {
        core.info('The develop run only performed quick checks; run the full suite.');
        return;
      }
      core.setOutput('reused', 'true');
      await core.summary.addHeading('Reused develop tests').addLink(`Exact commit ${pr.head.sha}`, passed.html_url)
        .addRaw('\nMaster is an ancestor of this tested commit; the merge has the same source tree.\n').write();
      return;
    }
    const pending = runs.some(run => run.head_sha === pr.head.sha && run.status !== 'completed');
    if (!pending || attempt === 6) {
      core.info('No successful exact-commit develop run is available; run the full suite.');
      return;
    }
    core.info('Waiting five minutes for the exact-commit develop test run.');
    await sleep(300000);
  }
};
module.exports.eligiblePullRequest = eligiblePullRequest;
module.exports.successfulRun = successfulRun;
