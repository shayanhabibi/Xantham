function requiresFullVerification(path) {
  if (/\.(fs|fsproj|fsx|fsi|sln|slnx|props|targets)$/i.test(path)) return true;
  if (/^(src|tests|tools)\//.test(path)) return true;
  if (/^(\.github\/|docs\/|site\/content\/|static\/)/.test(path)) return false;
  if (/\.(md|rst|txt|png|jpg|jpeg|svg|gif|ico)$/i.test(path)) return false;
  // Unrecognized files, dependency pins, and compiler/build configuration require verification.
  return true;
}

module.exports = async function verificationScope({ github, context, core }) {
  core.setOutput('full', 'true');
  if (context.eventName === 'workflow_dispatch') return;
  let files;
  const { owner, repo } = context.repo;
  if (context.eventName === 'pull_request') {
    if (context.payload.pull_request.changed_files >= 3000) return;
    files = await github.paginate(github.rest.pulls.listFiles, { owner, repo,
      pull_number: context.payload.pull_request.number, per_page: 100 });
  } else if (context.eventName === 'push') {
    const base = context.payload.before;
    if (!base || /^0+$/.test(base)) return;
    const response = await github.rest.repos.compareCommits({ owner, repo, base, head: context.sha });
    files = response.data.files;
    // Compare responses cap their file list at 300. Missing/truncated evidence runs full checks.
    if (!files || files.length >= 300) return;
  } else return;
  const full = files.some(file => requiresFullVerification(file.filename) ||
    (file.previous_filename && requiresFullVerification(file.previous_filename)));
  core.setOutput('full', String(full));
  core.info(full ? 'Source or build inputs changed: full verification.' : 'Documentation/workflow-only change: quick checks.');
};
module.exports.requiresFullVerification = requiresFullVerification;
