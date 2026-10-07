const { execFileSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { run: validateVersions } = require('./release-versions.cjs');

const maintainers = new Set([57953499, 8174976]);
function releaseRequest(context) {
  if (context.eventName === 'issue_comment') {
    const command = /^\/release(?: (preview))?\s*$/.exec(context.payload.comment?.body || '');
    if (!command || !context.payload.issue?.pull_request) return null;
    if (!maintainers.has(context.payload.comment.user.id)) throw new Error('Only the release maintainers can request a release.');
    return { number: context.payload.issue.number, preview: !!command[1] };
  }
  if (context.eventName !== 'workflow_dispatch' || !maintainers.has(context.payload.sender?.id)) throw new Error('Only the release maintainers can request a release.');
  const number = Number(context.payload.inputs?.pr);
  if (!Number.isSafeInteger(number) || number <= 0) throw new Error('A release PR number is required.');
  return { number, preview: context.payload.inputs?.preview !== 'false' };
}

function verifyPullRequest(pr, repository, expected) {
  if (pr.state !== 'open' || pr.head.ref !== 'develop' || pr.base.ref !== 'master' ||
      pr.head.repo?.full_name !== repository || pr.base.repo?.full_name !== repository) {
    throw new Error('Release preparation requires an open, same-repository develop → master PR.');
  }
  if (expected && (pr.head.sha !== expected.head || pr.base.sha !== expected.base)) {
    throw new Error('Develop or master moved during preparation. Run /release again.');
  }
}

function verifyChangedPaths(paths) {
  const names = ['Xantham.Cli', 'Xantham.TypeScript.Wire', 'Xantham.Generator',
    'Xantham.Fable.Core', 'Xantham.Fable.Core.TS', 'Xantham.Fable.Node'];
  const allowed = new Set(names.flatMap(name => [`src/${name}/${name}.fsproj`, `src/${name}/CHANGELOG.md`]));
  if (paths.some(file => !allowed.has(file))) throw new Error('ShipIt changed files outside package versions and changelogs.');
}

async function prepare({ github, context, core }) {
  const request = releaseRequest(context);
  core.setOutput('requested', 'false');
  if (!request) return;
  const { data: pr } = await github.rest.pulls.get({ ...context.repo, pull_number: request.number });
  verifyPullRequest(pr, `${context.repo.owner}/${context.repo.repo}`);
  core.setOutput('requested', 'true');
  core.setOutput('pr', String(request.number));
  core.setOutput('preview', String(request.preview));
  core.setOutput('head', pr.head.sha);
  core.setOutput('base', pr.base.sha);
}

async function pushWithDeployKey({ github, root, repository, sshKey }) {
  if (!sshKey) throw new Error('The RELEASE_DEPLOY_KEY repository secret is required.');
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'xantham-release-ssh-'));
  try {
    const key = path.join(directory, 'key');
    const hosts = path.join(directory, 'known_hosts');
    fs.writeFileSync(key, sshKey.trim() + '\n', { mode: 0o600 });
    const { data: metadata } = await github.rest.meta.get();
    fs.writeFileSync(hosts, metadata.ssh_keys.map(value => `github.com ${value}\n`).join(''), { mode: 0o600 });
    // Host keys come from GitHub's authenticated HTTPS API, never ssh-keyscan.
    const ssh = `ssh -i "${key}" -o IdentitiesOnly=yes -o StrictHostKeyChecking=yes -o UserKnownHostsFile="${hosts}"`;
    execFileSync('git', ['push', `git@github.com:${repository}.git`, 'HEAD:refs/heads/develop'], { cwd: root,
      env: { ...process.env, GIT_SSH_COMMAND: ssh }, stdio: ['ignore', 'pipe', 'pipe'] });
  } finally {
    fs.rmSync(directory, { recursive: true, force: true });
  }
}

async function finish({ github, context, core, root, request, sshKey, push = pushWithDeployKey }) {
  const git = (...args) => execFileSync('git', args, { cwd: root, encoding: 'utf8' }).trim();
  if (git('rev-parse', 'HEAD') !== request.head) throw new Error('Checkout does not match the authorized PR commit.');
  const paths = git('diff', '--name-only', 'HEAD').split('\n').filter(Boolean);
  verifyChangedPaths(paths);
  if (git('ls-files', '--others', '--exclude-standard').trim()) throw new Error('Release preparation created unexpected untracked files.');
  validateVersions(request.base, root);
  const diff = git('diff', '--stat');
  await core.summary.addHeading(request.preview ? 'Release preview' : 'Release preparation')
    .addRaw(diff ? `\n\`\`\`text\n${diff}\n\`\`\`\n` : '\nNo package version changes are needed.\n').write();
  if (request.preview) return;
  const { data: pr } = await github.rest.pulls.get({ ...context.repo, pull_number: request.number });
  verifyPullRequest(pr, `${context.repo.owner}/${context.repo.repo}`, request);
  if (paths.length) {
    git('config', 'user.name', 'github-actions[bot]');
    git('config', 'user.email', '41898282+github-actions[bot]@users.noreply.github.com');
    git('add', '--', ...paths);
    git('commit', '-m', 'chore: prepare package release');
    await push({ github, root, repository: `${context.repo.owner}/${context.repo.repo}`, sshKey });
    await core.summary.addRaw('\nRelease updates were pushed to develop. Normal push and PR CI will verify this commit.\n').write();
    return;
  }
  // A retry after a successful push, or a release with no bumps, can restart checks.
  for (const workflow_id of ['test.yml', 'push_master.yml', 'conventional-pr-title.yml']) {
    await github.rest.actions.createWorkflowDispatch({ ...context.repo, workflow_id, ref: 'develop',
      ...(workflow_id === 'conventional-pr-title.yml' ? { inputs: { pr: String(request.number) } } : {}) });
  }
  await core.summary.addRaw('\nDevelop verification and package checks were dispatched. Review the version diff and merge this PR with a merge commit once checks pass.\n').write();
}

module.exports = { releaseRequest, verifyPullRequest, verifyChangedPaths, prepare, finish, pushWithDeployKey };
