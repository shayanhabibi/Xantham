import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

export function partition(names, count) {
  assert.ok(Number.isInteger(count) && count > 0, 'Shard count must be positive');
  assert.equal(new Set(names).size, names.length, 'Duplicate test names cannot be sharded');
  assert.ok(names.length >= count, 'Every shard must contain tests');
  const shards = Array.from({ length: count }, () => []);
  [...names].sort().forEach((name, index) => shards[index % count].push(name));
  return shards;
}

export function testNames(output) {
  const names = JSON.parse(output);
  assert.ok(Array.isArray(names) && names.length && names.every(name => typeof name === 'string' && name.trim()),
    'Empty or invalid test inventory');
  return names;
}

export function batches(names) {
  // Leave room for the executable path and Windows quoting within its command-line limit.
  const result = [[]];
  let size = 0;
  for (const name of names) {
    assert.ok(name.length < 12000, 'Test name exceeds command line budget');
    if (size + name.length + 3 > 12000) { result.push([]); size = 0; }
    result.at(-1).push(name);
    size += name.length + 3;
  }
  return result;
}

export function verifyExecution(output, expected) {
  const plain = output.replace(/\x1b\[[0-9;]*m/g, '');
  const summary = /EXPECTO!\s+([\d,]+)\s+tests run[\s\S]*?\b([\d,]+)\s+ignored,/.exec(plain);
  assert.ok(summary, 'Expecto did not report how many tests ran and were ignored');
  assert.equal(Number(summary[1].replaceAll(',', '')) + Number(summary[2].replaceAll(',', '')), expected,
    'Expecto selection differs from planned shard');
}

function run(config, shard, listOnly) {
  const match = /^(\d+)\/(\d+)$/.exec(shard);
  assert.ok(match, 'Shard must be INDEX/COUNT, starting at 1');
  const [, indexText, countText] = match;
  const index = Number(indexText), count = Number(countText);
  assert.ok(index >= 1 && index <= count, 'Shard index is out of range');
  const root = fileURLToPath(new URL('../', import.meta.url));
  const projects = [...fs.readFileSync(path.join(root, 'Xantham.slnx'), 'utf8').matchAll(/Path="([^"]+\.Tests\.fsproj)"/g)].map(match => match[1]);
  assert.ok(projects.length, 'No Expecto projects discovered in solution');
  for (const project of projects) {
    const name = path.basename(project, '.fsproj');
    const cwd = path.join(root, path.dirname(project), 'bin', config, 'net10.0');
    const assembly = path.join(cwd, `${name}.dll`);
    const invoke = args => {
      const result = spawnSync('dotnet', [assembly, ...args], { cwd, encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 });
      assert.ifError(result.error);
      assert.equal(result.status, 0, result.stdout + result.stderr);
      return result.stdout;
    };
    const names = testNames(invoke(['--xantham-list-tests']));
    const selected = partition(names, count)[index - 1];
    console.log(`${name}: shard ${shard}, ${selected.length}/${names.length} tests`);
    if (listOnly) { console.log(JSON.stringify(selected)); continue; }
    for (const batch of batches(selected)) {
      const output = invoke(['--run', ...batch, '--debug', '--no-spinner', '--fail-on-focused-tests']);
      process.stdout.write(output);
      verifyExecution(output, batch.length);
    }
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  run(process.argv[2] ?? 'Release', process.argv[3] ?? '1/1', process.argv[4] === '--list');
}
