const test = require('node:test');
const assert = require('node:assert/strict');
const reuse = require('./reuse-develop-tests.cjs');

function fixture() {
  const context = { eventName: 'pull_request', repo: { owner: 'owner', repo: 'repo' }, payload: {
    repository: { full_name: 'owner/repo' }, pull_request: { base: { ref: 'master', sha: 'base' },
      head: { ref: 'develop', sha: 'head', repo: { full_name: 'owner/repo' } } } } };
  const run = { event: 'push', head_branch: 'develop', head_sha: 'head',
    id: 100, repository: { full_name: 'owner/repo' }, head_repository: { full_name: 'owner/repo' }, status: 'completed', conclusion: 'success', html_url: 'https://example.com/run' };
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

test('evidence must be successful push or manual tests on the exact trusted develop commit', () => {
  const { run } = fixture();
  for (const change of [{ head_sha: 'older' }, { event: 'pull_request' }, { head_branch: 'feature' },
    { conclusion: 'failure' }, { status: 'in_progress' }, { head_repository: { full_name: 'fork/repo' } },
    { repository: { full_name: 'fork/repo' } }]) {
    assert.equal(reuse.successfulRun([{ ...run, ...change }], 'head', 'owner/repo'), undefined);
  }
  assert.equal(reuse.successfulRun([run], 'head', 'owner/repo'), run);
  const manual = { ...run, event: 'workflow_dispatch' };
  assert.equal(reuse.successfulRun([manual], 'head', 'owner/repo'), manual);
});

test('waits five minutes for exact-commit evidence, then reuses it', async () => {
  const f = fixture();
  let calls = 0;
  const waits = [];
  const github = { paginate: async () => [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'success' }] }], rest: {
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
  const github = { paginate: async () => [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'skipped' }] }], rest: {
    repos: { compareCommits: async () => ({ data: { merge_base_commit: { sha: 'base' } } }) },
    actions: { listJobsForWorkflowRun() {}, listWorkflowRuns: async () => ({ data: { workflow_runs: [f.run] } }) }
  } };
  await reuse({ ...f, github });
  assert.equal(f.output.reused, 'false');
});

function masterFixture(eventName = 'push') {
  const f = fixture();
  f.context = { ...f.context, eventName, ref: 'refs/heads/master', sha: 'merge', payload: {} };
  const calls = [];
  const merge = { sha: 'merge', parents: [{ sha: 'old-master' }, { sha: 'head' }], commit: { tree: { sha: 'identical-tree' } } };
  const parent = { sha: 'head', commit: { tree: { sha: 'identical-tree' } } };
  const jobs = [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'success' }] }];
  const github = { paginate: async () => jobs, rest: {
    repos: { getCommit: async ({ ref }) => { calls.push(ref); return { data: ref === 'merge' ? merge : parent }; } },
    actions: { listJobsForWorkflowRun() {}, listWorkflowRuns: async args => {
      assert.equal(args.branch, 'develop'); assert.equal(args.head_sha, 'head');
      return { data: { workflow_runs: [f.run] } };
    } }
  } };
  return { ...f, github, calls, merge, parent, jobs };
}

test('master push and manual master runs reuse only the exact identical-tree merge parent', async () => {
  for (const eventName of ['push', 'workflow_dispatch']) {
    const f = masterFixture(eventName);
    await reuse(f);
    assert.deepEqual(f.calls, ['merge', 'head']);
    assert.equal(f.output.reused, 'true');
    assert.equal(f.output['master-reused'], 'true');
    assert.equal(f.output['source-sha'], 'head');
    assert.equal(f.output['source-run'], '100');
  }
});

test('non-master, non-merge, changed-tree and mismatched commits cannot reuse parent evidence', async () => {
  for (const mutate of [f => f.context.ref = 'refs/heads/develop', f => f.context.eventName = 'pull_request',
    f => f.merge.parents = [{ sha: 'head' }], f => f.merge.parents.push({ sha: 'third' }),
    f => f.parent.commit.tree.sha = 'changed', f => f.merge.sha = 'other', f => f.parent.sha = 'other',
    f => delete f.merge.commit.tree, f => delete f.parent.commit.tree]) {
    const f = masterFixture(); mutate(f);
    await reuse(f);
    assert.equal(f.output.reused, 'false');
    assert.equal(f.output['master-reused'], 'false');
  }
});

test('master reuse rejects quick, missing, failed and cancelled full-test jobs', async () => {
  for (const jobs of [[], [{ name: 'publish-tests', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'success' }] }],
    ...['skipped', 'failure', 'cancelled', undefined].map(conclusion => [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion }] }]),
    ...['skipped', 'failure', 'cancelled', undefined].map(conclusion => [{ name: 'test', conclusion, steps: [{ name: 'Test', conclusion: 'success' }] }])]) {
    const f = masterFixture(); f.github.paginate = async () => jobs;
    await reuse(f);
    assert.equal(f.output['master-reused'], 'false');
  }
});

test('pending master parent evidence waits at most two minutes and falls back to full tests', async () => {
  const f = masterFixture(); f.run.status = 'in_progress'; f.run.conclusion = null;
  const waits = [];
  await reuse({ ...f, sleep: async ms => waits.push(ms) });
  assert.deepEqual(waits, [60000, 60000]);
  assert.equal(f.output.reused, 'false');
  assert.equal(f.output['master-reused'], 'false');
});

test('an older full run for the same exact commit remains usable after a successful quick rerun', async () => {
  const f = masterFixture();
  f.github.rest.actions.listWorkflowRuns = async () => ({ data: { workflow_runs: [{ ...f.run, id: 101 }, f.run] } });
  f.github.paginate = async (_, { run_id }) => run_id === 101 ? [] : f.jobs;
  await reuse(f);
  assert.equal(f.output['source-run'], '100');
});

test('unavailable commit, run or job evidence falls back to full verification', async () => {
  for (const stage of ['commit', 'runs', 'jobs']) {
    const f = masterFixture();
    const unavailable = async () => { throw new Error('API unavailable'); };
    if (stage === 'commit') f.github.rest.repos.getCommit = unavailable;
    if (stage === 'runs') f.github.rest.actions.listWorkflowRuns = unavailable;
    if (stage === 'jobs') f.github.paginate = unavailable;
    await reuse(f);
    assert.equal(f.output.reused, 'false');
    assert.equal(f.output['master-reused'], 'false');
  }
});
