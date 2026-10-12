const assert = require('node:assert/strict');
const { fullTestJob } = require('./reuse-develop-tests.cjs');

module.exports = async function requireMasterTests({ github, context, core,
  sleep = ms => new Promise(resolve => setTimeout(resolve, ms)), attempts = 7 }) {
  core.setOutput('verified', 'false');
  assert.ok(['push', 'workflow_dispatch'].includes(context.eventName) && context.ref === 'refs/heads/master',
    'Publication evidence is restricted to master push or manual runs');
  const { owner, repo } = context.repo;
  const repository = `${owner}/${repo}`;
  for (let attempt = 0; attempt < attempts; attempt++) {
    const { data } = await github.rest.actions.listWorkflowRuns({ owner, repo, workflow_id: 'test.yml',
      branch: 'master', head_sha: context.sha, per_page: 100 });
    const matching = data.workflow_runs.filter(run => ['push', 'workflow_dispatch'].includes(run.event) &&
      run.head_branch === 'master' && run.head_sha === context.sha &&
      run.repository?.full_name === repository && run.head_repository?.full_name === repository);
    for (const run of matching.filter(run => run.status === 'completed' && run.conclusion === 'success')) {
      const jobs = await github.paginate(github.rest.actions.listJobsForWorkflowRun, {
        owner, repo, run_id: run.id, per_page: 100
      });
      // Test's full marker covers either fresh shards or authenticated develop-tree reuse.
      if (!fullTestJob(jobs)) continue;
      await core.summary.addHeading('Verified master tests')
        .addLink(`Exact commit ${context.sha}`, run.html_url).write();
      core.setOutput('source-run', String(run.id));
      core.setOutput('verified', 'true');
      return;
    }
    if (matching.length && matching.every(run => run.status === 'completed')) break;
    if (attempt === attempts - 1) break;
    core.info('Waiting ten minutes for full Test verification of the exact master commit.');
    await sleep(600000);
  }
  throw new Error(`No successful full Test verification for master commit ${context.sha}; rerun Test, then Publish.`);
};
