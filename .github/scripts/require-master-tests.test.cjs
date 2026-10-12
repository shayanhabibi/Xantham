const test = require('node:test');
const assert = require('node:assert/strict');
const verify = require('./require-master-tests.cjs');

function fixture(runs, jobs = [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'success' }] }]) {
  const outputs = {};
  const context = { eventName: 'push', ref: 'refs/heads/master', sha: 'abc', repo: { owner: 'owner', repo: 'repo' } };
  const github = { rest: { actions: {
    listWorkflowRuns: async args => {
      assert.equal(args.workflow_id, 'test.yml');
      assert.equal(args.head_sha, 'abc');
      assert.equal(args.branch, 'master');
      return { data: { workflow_runs: typeof runs === 'function' ? runs() : runs } };
    }, listJobsForWorkflowRun() {}
  } }, paginate: async () => jobs };
  const core = { setOutput: (name, value) => { outputs[name] = value; }, info() {},
    summary: { addHeading() { return this; }, addLink() { return this; }, async write() {} } };
  return { github, context, core, outputs, sleep: async () => {}, attempts: 2 };
}
const run = { id: 1, event: 'push', head_branch: 'master', head_sha: 'abc',
  repository: { full_name: 'owner/repo' }, head_repository: { full_name: 'owner/repo' },
  status: 'completed', conclusion: 'success', html_url: 'https://example.test/run/1' };

test('accepts exact master verification, including manual Test dispatch', async () => {
  for (const event of ['push', 'workflow_dispatch']) {
    const f = fixture([{ ...run, event }]);
    await verify(f);
    assert.equal(f.outputs.verified, 'true');
    assert.equal(f.outputs['source-run'], '1');
  }
});

test('rejects PR, branch, SHA, repository, pending and failed evidence', async () => {
  for (const patch of [{ event: 'pull_request' }, { head_branch: 'develop' }, { head_sha: 'other' },
    { repository: { full_name: 'fork/repo' } }, { head_repository: { full_name: 'fork/repo' } },
    { status: 'in_progress' }, { conclusion: 'failure' }, { conclusion: 'cancelled' }]) {
    const f = fixture([{ ...run, ...patch }]);
    await assert.rejects(verify(f));
    assert.notEqual(f.outputs.verified, 'true');
  }
});

test('requires the successful full marker, never just a green quick check', async () => {
  for (const jobs of [[], [{ name: 'test', conclusion: 'success', steps: [] }],
    [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'skipped' }] }],
    [{ name: 'test', conclusion: 'failure', steps: [{ name: 'Test', conclusion: 'success' }] }]])
    await assert.rejects(verify(fixture([run], jobs)));
});

test('waits for absent or pending Test runs and accepts completed evidence', async () => {
  for (const initial of [[], [{ ...run, status: 'in_progress', conclusion: null }]]) {
    let calls = 0;
    const f = fixture(() => ++calls === 1 ? initial : [run]);
    let sleeps = 0;
    f.sleep = async ms => { assert.equal(ms, 600000); sleeps++; };
    await verify(f);
    assert.equal(sleeps, 1);
    assert.equal(f.outputs.verified, 'true');
  }
});

test('a newer quick run does not hide an older full run for the exact commit', async () => {
  const f = fixture([{ ...run, id: 2 }, run]);
  f.github.paginate = async (_, args) => args.run_id === 2 ? [] :
    [{ name: 'test', conclusion: 'success', steps: [{ name: 'Test', conclusion: 'success' }] }];
  await verify(f);
  assert.equal(f.outputs['source-run'], '1');
});

test('default wait is bounded to one hour at ten-minute intervals', async () => {
  const f = fixture([]);
  delete f.attempts;
  let waited = 0;
  f.sleep = async ms => { waited += ms; };
  await assert.rejects(verify(f));
  assert.equal(waited, 3600000);
  assert.notEqual(f.outputs.verified, 'true');
});

test('fails closed on timeout, API errors, and non-master invocation', async () => {
  await assert.rejects(verify(fixture([])), /No successful full/);
  const f = fixture([run]);
  f.github.paginate = async () => { throw new Error('API unavailable'); };
  await assert.rejects(verify(f), /API unavailable/);
  assert.notEqual(f.outputs.verified, 'true');
  for (const patch of [{ eventName: 'pull_request' }, { ref: 'refs/heads/develop' }]) {
    const invalid = fixture([run]);
    Object.assign(invalid.context, patch);
    await assert.rejects(verify(invalid), /master/);
  }
});
