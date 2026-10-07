const { test } = require('node:test');
const assert = require('node:assert/strict');
const { affectedPackages, compareVersions, validateVersions } = require('./release-versions.cjs');

const packages = [
  { name: 'wire', directory: 'src/wire', dependencies: [], version: '1.0.1', previous: '1.0.0' },
  { name: 'generator', directory: 'src/generator', dependencies: ['wire'], version: '1.0.0', previous: '1.0.0' },
  { name: 'cli', directory: 'src/cli', dependencies: ['generator'], version: '2.0.0', previous: '1.0.0' },
];
test('an upstream source change requires every transitive consumer to advance', () => {
  assert.deepEqual([...affectedPackages(packages, ['src/wire/Client.fs'])].sort(), ['cli', 'generator', 'wire']);
  assert.throws(() => validateVersions(packages, ['src/wire/Client.fs']), /generator.*1.0.0/);
});
test('documentation and changelog changes do not require artificial package releases', () => {
  assert.equal(affectedPackages(packages, ['README.md', 'CONTRIBUTING.md', 'src/wire/CHANGELOG.md', '.github/workflows/test.yml']).size, 0);
  validateVersions(packages, ['src/wire/CHANGELOG.md']);
});
test('shared build properties affect every package', () => {
  assert.equal(affectedPackages(packages, ['Directory.Build.targets']).size, 3);
});
test('a new package needs no previous version; a changed existing package cannot downgrade', () => {
  validateVersions([{ ...packages[0], previous: null }], ['src/wire/New.fs']);
  assert.throws(() => validateVersions([{ ...packages[0], version: '0.9.0' }], ['src/wire/New.fs']), /wire/);
});
test('SemVer ordering handles numeric prereleases, stable releases and metadata', () => {
  assert.ok(compareVersions('1.0.0-beta.10', '1.0.0-beta.2') > 0);
  assert.ok(compareVersions('1.0.0', '1.0.0-rc.1') > 0);
  assert.equal(compareVersions('1.0.0+second', '1.0.0+first'), 0);
  assert.throws(() => compareVersions('1.0.0-beta.01', '1.0.0'), /SemVer/);
});
