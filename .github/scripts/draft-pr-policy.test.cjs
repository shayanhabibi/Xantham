const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const gates = [
  ['test.yml', 'scope', 'pull_request'],
  ['test.yml', 'test', 'pull_request'],
  ['catalog-portability.yml', 'produce', 'pull_request'],
  ['push_master.yml', 'package', 'pull_request'],
  ['conventional-pr-title.yml', 'conventional-title', 'pull_request_target'],
];

for (const [file, job, event] of gates) {
  test(`${file}/${job} skips drafts and validates ready PRs without disabling branch runs`, () => {
    const workflow = fs.readFileSync(`.github/workflows/${file}`, 'utf8');
    const trigger = workflow.split(`\n  ${event}:\n`)[1].split(/\n  \w/)[0];
    for (const action of ['opened', 'synchronize', 'reopened', 'ready_for_review', 'converted_to_draft'])
      assert.ok(trigger.includes(action), `${event} must handle ${action}`);
    const block = workflow.split(`\n  ${job}:\n`)[1].split(/\n  [\w-]+:/)[0];
    const expression = block.match(/^    if: (.+)$/m)[1];
    const evaluate = (eventName, draft, ref) => vm.runInNewContext(expression, {
      github: { event_name: eventName, event: { pull_request: { draft } }, ref },
      always: () => true,
    });
    assert.equal(evaluate(event, true, 'refs/pull/1/merge'), false);
    assert.equal(evaluate(event, false, 'refs/pull/1/merge'), true);
    for (const branch of ['master', 'develop']) {
      assert.equal(evaluate('workflow_dispatch', undefined, `refs/heads/${branch}`), true);
      if (file !== 'push_master.yml' || branch === 'master')
        assert.equal(evaluate('push', undefined, `refs/heads/${branch}`), true);
    }
    if (file === 'push_master.yml') {
      assert.equal(evaluate('push', undefined, 'refs/heads/feature'), false);
      assert.equal(evaluate('workflow_dispatch', undefined, 'refs/heads/feature'), false);
    }
  });
}
