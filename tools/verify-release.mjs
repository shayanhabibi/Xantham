// Exercise the installed tool against a real disposable monorepo, including consumers.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const repository = fileURLToPath(new URL('../', import.meta.url));
const scratch = path.join(repository, 'tests/.scratch');
fs.mkdirSync(scratch, { recursive: true });
const root = fs.mkdtempSync(path.join(scratch, 'xantham-shipit-'));
function run(command, ...args) {
  const result = spawnSync(command, args, { cwd: root, encoding: 'utf8', timeout: 120_000 });
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout.trim();
}
try {
  run('git', 'init', '-b', 'develop');
  run('git', 'config', 'user.name', 'ShipIt contract');
  run('git', 'config', 'user.email', 'shipit@example.test');
  run('git', 'remote', 'add', 'origin', 'https://github.com/example/xantham-contract.git');
  fs.mkdirSync(path.join(root, '.config'));
  fs.copyFileSync(path.join(repository, '.config/dotnet-tools.json'), path.join(root, '.config/dotnet-tools.json'));
  fs.mkdirSync(path.join(root, '.github/scripts'), { recursive: true });
  const validator = path.join(root, '.github/scripts/release-versions.cjs');
  fs.copyFileSync(path.join(repository, '.github/scripts/release-versions.cjs'), validator);
  const names = fs.readdirSync(path.join(repository, 'src')).filter(name => fs.existsSync(path.join(repository, 'src', name, 'CHANGELOG.md')));
  assert.equal(names.length, 7);
  const expected = {};
  for (const name of names) {
    const directory = path.join(root, 'src', name);
    fs.mkdirSync(directory, { recursive: true });
    fs.copyFileSync(path.join(repository, 'src', name, `${name}.fsproj`), path.join(directory, `${name}.fsproj`));
    fs.copyFileSync(path.join(repository, 'src', name, 'CHANGELOG.md'), path.join(directory, 'CHANGELOG.md'));
    const version = /<Version>([^<]+)<\/Version>/.exec(fs.readFileSync(path.join(directory, `${name}.fsproj`), 'utf8'))[1];
    const changelogVersion = /^## (\S+)/m.exec(fs.readFileSync(path.join(directory, 'CHANGELOG.md'), 'utf8'))[1];
    assert.equal(changelogVersion, version, `${name}: changelog must match the project version`);
    if (['Xantham.TypeScript.Wire', 'Xantham.Generator', 'Xantham.Generator.Myriad', 'Xantham.Cli'].includes(name)) {
      const [major, minor, patch] = version.split(/[.-]/);
      expected[name] = version.includes('-') ? `${major}.${minor}.${patch}` : `${major}.${minor}.${BigInt(patch) + 1n}`;
    } else expected[name] = version;
  }
  run('git', 'add', '.');
  run('git', 'commit', '-m', 'chore: baseline');
  const baseline = run('git', 'rev-parse', 'HEAD');
  for (const name of names) {
    const changelog = path.join(root, 'src', name, 'CHANGELOG.md');
    fs.writeFileSync(changelog, fs.readFileSync(changelog, 'utf8').replace(/last_commit_released: \w+/, `last_commit_released: ${baseline}`));
  }
  run('git', 'add', '.');
  run('git', 'commit', '-m', 'chore: seed released commit');
  fs.writeFileSync(path.join(root, 'src/Xantham.TypeScript.Wire/contract.fs'), 'module Contract\n');
  run('git', 'add', '.');
  run('git', 'commit', '-m', 'fix(wire): exercise dependency releases');
  const stale = spawnSync(process.execPath, [validator, baseline], { cwd: root, encoding: 'utf8' });
  assert.equal(stale.status, 1, stale.stdout + stale.stderr);
  for (const name of ['Xantham.TypeScript.Wire', 'Xantham.Generator', 'Xantham.Generator.Myriad', 'Xantham.Cli']) assert.ok(stale.stderr.includes(name));
  run('dotnet', 'tool', 'restore');
  run('dotnet', 'shipit', '--allow-branch', 'develop', '--mode', 'local', '--skip-merge-commit');
  for (const [name, version] of Object.entries(expected)) {
    const xml = fs.readFileSync(path.join(root, 'src', name, `${name}.fsproj`), 'utf8');
    assert.ok(xml.includes(`<Version>${version}</Version>`), `${name}: expected ${version}`);
    assert.ok(xml.includes('<AssemblyVersion>0.0.0.0</AssemblyVersion>'));
    assert.ok(fs.readFileSync(path.join(root, 'src', name, 'CHANGELOG.md'), 'utf8').includes(`## ${version}`));
  }
  assert.equal(run('git', 'log', '-1', '--format=%s'), 'fix(wire): exercise dependency releases');
  run(process.execPath, validator, baseline);
  console.log('ShipIt contract passed: independent versions, transitive consumer bumps, matching XML/changelogs, stable assembly versions, no commits.');
} finally {
  fs.rmSync(root, { recursive: true, force: true });
}
