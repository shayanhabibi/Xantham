import assert from "node:assert/strict";
import test from "node:test";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const sha = "0123456789abcdef0123456789abcdef01234567";
function fixture(overrides = {}, existing = null) {
  const calls = [];
  const context = {
    repo: { owner: "owner", repo: "repo" },
    payload: { workflow_run: {
      id: 42, event: "push", conclusion: "success", head_branch: "master", head_sha: sha,
      repository: { full_name: "owner/repo" }, head_repository: { full_name: "owner/repo" },
      html_url: "https://github.com/owner/repo/actions/runs/42", ...overrides,
    } },
  };
  const github = { rest: {
    actions: { listWorkflowRunArtifacts: async () => {
      calls.push("artifacts");
      return { data: { artifacts: [{ id: 7, name: `goldens-${sha}`, expired: false, digest: "sha256:artifact" }] } };
    } },
    git: {
      getRef: async () => {
        calls.push("getRef");
        if (!existing) throw Object.assign(new Error("missing"), { status: 404 });
        return { data: { object: { type: "tag", sha: "tag-object" } } };
      },
      getTag: async () => ({ data: { object: { type: "commit", sha: existing } } }),
      createTag: async options => { calls.push(options); return { data: { sha: "new-tag-object" } }; },
      createRef: async options => { calls.push(options); },
    },
  } };
  return { github, context, core: { info() {} }, calls };
}

test("successful master goldens receive an annotated tag on their tested commit", async () => {
  const tag = require("../.github/scripts/tag-goldens.cjs");
  const f = fixture();
  await tag(f);
  const annotation = f.calls.find(value => value?.tag);
  assert.equal(annotation.tag, `goldens/${sha}`);
  assert.equal(annotation.object, sha);
  assert.equal(annotation.type, "commit");
  assert.match(annotation.message, /sha256:artifact/);
  assert.match(annotation.message, /actions\/runs\/42/);
  assert.deepEqual(f.calls.at(-1), { owner: "owner", repo: "repo", ref: `refs/tags/goldens/${sha}`, sha: "new-tag-object" });
});

test("pull requests, failures and foreign repositories cannot create golden tags", async () => {
  const tag = require("../.github/scripts/tag-goldens.cjs");
  for (const overrides of [
    { event: "pull_request" }, { conclusion: "failure" }, { head_branch: "develop" },
    { head_repository: { full_name: "fork/repo" } }, { head_sha: "bad-sha" },
  ]) {
    const f = fixture(overrides);
    await assert.rejects(tag(f));
    assert.deepEqual(f.calls, []);
  }
});

test("missing golden artifacts prevent tagging; reruns preserve existing tags", async () => {
  const tag = require("../.github/scripts/tag-goldens.cjs");
  const missing = fixture();
  missing.github.rest.actions.listWorkflowRunArtifacts = async () => ({ data: { artifacts: [] } });
  await assert.rejects(tag(missing), /artifact/);
  assert.deepEqual(missing.calls, []);
  const rerun = fixture({}, sha);
  await tag(rerun);
  assert.deepEqual(rerun.calls, ["artifacts", "getRef"]);
  const conflict = fixture({}, "abcdef0123456789abcdef0123456789abcdef01");
  await assert.rejects(tag(conflict), /different commit/);
  assert.deepEqual(conflict.calls, ["artifacts", "getRef"]);
});
