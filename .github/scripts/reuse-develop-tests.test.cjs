const test = require('node:test');
const assert = require('node:assert/strict');
const reuse = require('./reuse-develop-tests.cjs');

function fixture() {
  const context = { eventName: 'pull_request', repo: { owner: 'owner', repo: 'repo' }, payload: {
    repository: { full_name: 'owner/repo' }, pull_request: { base: { ref: 'master', sha: 'base' },
      head: { ref: 'develop', sha: 'head', repo: { full_name: 'owner/repo' } } } } };
  const run = { event: 'push', head_branch: 'develop', head_sha: 'head',
    head_repository: { full_name: 'owner/repo' }, status: 'completed', conclusion: 'success', html_url: 'https://example.com/run' };
  const output = {};
  const summary = { addHeading() { return this; }, addLink() { return this; }, addRaw() { return this; }, async write() {} };
  const core = { setOutput: (key, value) => output[key] = value, info() {}, summary };
  return { context, run, core, output };
}

test('only same-repository develop to master PRs qualify', () => {
  const { context } = fixture();
  assert.equal(reuse.eligiblePullRequest(context), true);
  context.payload.pull_request.head.repo.full_name = 'fork/repo';
  assert.equal(reuse.eligiblePullRequest(context), false);
  context.payload.pull_request.head.repo.full_name = 'owner/repo';
  context.eventName = 'push';
  assert.equal(reuse.eligiblePullRequest(context), false);
});

test('evidence must be successful push tests on the exact trusted develop commit', () => {
  const { run } = fixture();
  for (const change of [{ head_sha: 'older' }, { event: 'pull_request' }, { head_branch: 'feature' },
    { conclusion: 'failure' }, { status: 'in_progress' }, { head_repository: { full_name: 'fork/repo' } }]) {
    assert.equal(reuse.successfulRun([{ ...run, ...change }], 'head', 'owner/repo'), undefined);
  }
  assert.equal(reuse.successfulRun([run], 'head', 'owner/repo'), run);
});

test('waits five minutes for exact-commit evidence, then reuses it', async () => {
  const f = fixture();
  let calls = 0;
  const waits = [];
  const github = { paginate: async () => [{ name: 'test', steps: [{ name: 'Test', conclusion: 'success' }] }], rest: {
    repos: { compareCommits: async () => ({ data: { merge_base_commit: { sha: 'base' } } }) },
    actions: { listJobsForWorkflowRun() {}, listWorkflowRuns: async () => ({ data: { workflow_runs: [calls++ ? f.run : { ...f.run, status: 'in_progress', conclusion: null }] } }) }
  } };
  await reuse({ ...f, github, sleep: async ms => waits.push(ms) });
  assert.deepEqual(waits, [300000]);
  assert.equal(f.output.reused, 'true');
});

test('diverged base runs full tests without consulting develop runs', async () => {
  const f = fixture();
  const github = { rest: { repos: { compareCommits: async () => ({ data: { merge_base_commit: { sha: 'older-base' } } }) } } };
  await reuse({ ...f, github });
  assert.equal(f.output.reused, 'false');
});

test('missing or failed evidence runs full tests', async () => {
  for (const runs of [[], [{ ...fixture().run, conclusion: 'failure' }]]) {
    const f = fixture();
    const github = { rest: {
      repos: { compareCommits: async () => ({ data: { merge_base_commit: { sha: 'base' } } }) },
      actions: { listWorkflowRuns: async () => ({ data: { workflow_runs: runs } }) }
    } };
    await reuse({ ...f, github });
    assert.equal(f.output.reused, 'false');
  }
});

test('a successful quick-check run is not full-test evidence', async () => {
  const f = fixture();
  const github = { paginate: async () => [{ name: 'test', steps: [{ name: 'Test', conclusion: 'skipped' }] }], rest: {
    repos: { compareCommits: async () => ({ data: { merge_base_commit: { sha: 'base' } } }) },
    actions: { listJobsForWorkflowRun() {}, listWorkflowRuns: async () => ({ data: { workflow_runs: [f.run] } }) }
  } };
  await reuse({ ...f, github });
  assert.equal(f.output.reused, 'false');
});
