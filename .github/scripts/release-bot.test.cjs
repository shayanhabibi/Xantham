const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { execFileSync } = require('node:child_process');
const bot = require('./release-bot.cjs');

function pr() {
  return { state: 'open', head: { ref: 'develop', sha: 'head', repo: { full_name: 'owner/repo' } },
    base: { ref: 'master', sha: 'base', repo: { full_name: 'owner/repo' } } };
}
function comment(body = '/release', id = 57953499) {
  return { eventName: 'issue_comment', payload: { comment: { body, user: { id } }, issue: { number: 1, pull_request: {} } } };
}
test('commands are exact, maintainer-only, and PR-only', () => {
  assert.deepEqual(bot.releaseRequest(comment()), { number: 1, preview: false });
  assert.deepEqual(bot.releaseRequest(comment('/release preview', 8174976)), { number: 1, preview: true });
  assert.equal(bot.releaseRequest(comment('/release; echo bad')), null);
  assert.throws(() => bot.releaseRequest(comment('/release', 12)), /maintainers/);
  const issue = comment();
  delete issue.payload.issue.pull_request;
  assert.equal(bot.releaseRequest(issue), null);
  assert.throws(() => bot.releaseRequest({ eventName: 'workflow_dispatch', payload: { sender: { id: 12 } } }), /maintainers/);
});
test('closed, fork and feature PRs cannot publish release updates', () => {
  bot.verifyPullRequest(pr(), 'owner/repo');
  for (const change of [{ state: 'closed' }, { head: { ...pr().head, ref: 'feature' } },
    { base: { ...pr().base, ref: 'develop' } }, { head: { ...pr().head, repo: { full_name: 'fork/repo' } } }]) {
    assert.throws(() => bot.verifyPullRequest({ ...pr(), ...change }, 'owner/repo'), /same-repository/);
  }
  assert.throws(() => bot.verifyPullRequest(pr(), 'owner/repo', { head: 'old', base: 'base' }), /moved/);
  assert.throws(() => bot.verifyPullRequest(pr(), 'owner/repo', { head: 'head', base: 'old' }), /moved/);
});
test('release commits contain only known project versions and changelogs', () => {
  bot.verifyChangedPaths(['src/Xantham.Cli/Xantham.Cli.fsproj', 'src/Xantham.Fable.Node/CHANGELOG.md']);
  for (const file of ['.github/workflows/test.yml', 'src/Xantham.Cli/Program.fs', 'src/Unknown/CHANGELOG.md']) {
    assert.throws(() => bot.verifyChangedPaths([file]), /outside/);
  }
});

function fixture(t) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'xantham-release-bot-'));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const git = (...args) => execFileSync('git', args, { cwd: root, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
  git('init', '-b', 'develop');
  git('config', 'core.autocrlf', 'false');
  git('config', 'user.name', 'Test');
  git('config', 'user.email', 'test@example.test');
  const names = ['Xantham.Cli', 'Xantham.TypeScript.Wire', 'Xantham.Generator', 'Xantham.Fable.Core', 'Xantham.Fable.Core.TS', 'Xantham.Fable.Node'];
  for (const name of names) {
    fs.mkdirSync(path.join(root, 'src', name), { recursive: true });
    fs.writeFileSync(path.join(root, 'src', name, `${name}.fsproj`), '<Project><PropertyGroup><Version>1.0.0</Version></PropertyGroup></Project>\n');
  }
  git('add', '.'); git('commit', '-m', 'chore: released baseline');
  const base = git('rev-parse', 'HEAD');
  fs.writeFileSync(path.join(root, 'src/Xantham.Cli/change.fs'), 'module Change\n');
  git('add', '.'); git('commit', '-m', 'fix(cli): update shipped code');
  const head = git('rev-parse', 'HEAD');
  const remote = path.join(root, '.git', 'remote.git');
  git('init', '--bare', remote);
  git('remote', 'add', 'origin', remote);
  git('push', 'origin', 'develop');
  const xml = path.join(root, 'src/Xantham.Cli/Xantham.Cli.fsproj');
  fs.writeFileSync(xml, fs.readFileSync(xml, 'utf8').replace('1.0.0', '1.0.1'));
  const dispatched = [];
  const summary = { addHeading() { return this; }, addRaw() { return this; }, async write() {} };
  const input = { root, context: { repo: { owner: 'owner', repo: 'repo' } },
    push: async () => { git('push', 'origin', 'HEAD:refs/heads/develop'); },
    request: { number: 1, preview: false, head, base }, core: { summary }, github: { rest: {
      pulls: { get: async () => ({ data: { ...pr(), head: { ...pr().head, sha: head }, base: { ...pr().base, sha: base } } }) },
      actions: { createWorkflowDispatch: async args => dispatched.push(args) }
    } } };
  return { input, git, xml, dispatched, remote, head };
}
test('preview validates versions but does not commit, push, or dispatch', async t => {
  const f = fixture(t);
  f.input.request.preview = true;
  await bot.finish(f.input);
  assert.equal(f.git('rev-parse', 'HEAD'), f.head);
  assert.equal(f.git('ls-remote', 'origin', 'refs/heads/develop').split('\t')[0], f.head);
  assert.deepEqual(f.dispatched, []);
});
test('release pushes once with bot identity; retries dispatch checks without another version commit', async t => {
  const f = fixture(t);
  await bot.finish(f.input);
  const published = f.git('rev-parse', 'HEAD');
  assert.notEqual(published, f.head);
  assert.equal(f.git('ls-remote', 'origin', 'refs/heads/develop').split('\t')[0], published);
  assert.equal(f.git('log', '-1', '--format=%an'), 'github-actions[bot]');
  assert.deepEqual(f.dispatched, []);
  f.input.request.head = published;
  f.input.github.rest.pulls.get = async () => ({ data: { ...pr(), head: { ...pr().head, sha: published }, base: { ...pr().base, sha: f.input.request.base } } });
  await bot.finish(f.input);
  assert.equal(f.git('rev-parse', 'HEAD'), published);
  assert.deepEqual(f.dispatched.map(x => x.workflow_id), ['test.yml', 'push_master.yml', 'conventional-pr-title.yml']);
});
test('stale versions and moved PRs fail before pushing or dispatching', async t => {
  const f = fixture(t);
  fs.writeFileSync(f.xml, fs.readFileSync(f.xml, 'utf8').replace('1.0.1', '1.0.0'));
  await assert.rejects(bot.finish(f.input), /newer versions/);
  fs.writeFileSync(f.xml, fs.readFileSync(f.xml, 'utf8').replace('1.0.0', '1.0.1'));
  f.input.github.rest.pulls.get = async () => ({ data: pr() });
  await assert.rejects(bot.finish(f.input), /moved/);
  assert.equal(f.git('rev-parse', 'HEAD'), f.head);
  assert.deepEqual(f.dispatched, []);
});
