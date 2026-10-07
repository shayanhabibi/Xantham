module.exports = async function tagGoldens({ github, context, core }) {
  const run = context.payload.workflow_run;
  const repository = `${context.repo.owner}/${context.repo.repo}`;
  if (run?.event !== "push" || run.conclusion !== "success" || run.head_branch !== "master"
    || run.repository?.full_name !== repository || run.head_repository?.full_name !== repository
    || !/^[0-9a-f]{40}$/.test(run.head_sha)) {
    throw new Error("Golden tags require a successful master push in this repository");
  }
  const { data } = await github.rest.actions.listWorkflowRunArtifacts({
    ...context.repo, run_id: run.id, per_page: 100,
  });
  const artifact = data.artifacts.find(value => value.name === `goldens-${run.head_sha}` && !value.expired);
  if (!artifact?.digest) throw new Error("The successful run has no authenticated golden artifact");

  const tag = `goldens/${run.head_sha}`;
  let existing;
  try {
    existing = (await github.rest.git.getRef({ ...context.repo, ref: `tags/${tag}` })).data;
  } catch (error) {
    if (error.status !== 404) throw error;
  }
  if (existing) {
    const target = existing.object.type === "tag"
      ? (await github.rest.git.getTag({ ...context.repo, tag_sha: existing.object.sha })).data.object
      : existing.object;
    if (target.type !== "commit" || target.sha !== run.head_sha) {
      throw new Error(`${tag} points at a different commit`);
    }
    core.info(`${tag} already exists`);
    return;
  }
  const annotation = await github.rest.git.createTag({
    ...context.repo, tag, object: run.head_sha, type: "commit",
    message: `Verified golden corpus\nSource commit: ${run.head_sha}\nCI: ${run.html_url}\nArtifact: ${artifact.name} (${artifact.id})\nDigest: ${artifact.digest}\n`,
  });
  await github.rest.git.createRef({ ...context.repo, ref: `refs/tags/${tag}`, sha: annotation.data.sha });
  core.info(`Created ${tag}`);
};
