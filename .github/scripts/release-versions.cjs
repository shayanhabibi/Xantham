// Compare with the protected release base, not ShipIt's pre-bump source commit.
const fs = require('node:fs');
const path = require('node:path');
const { execFileSync } = require('node:child_process');

function compareVersions(a, b) {
  const parse = value => {
    const match = /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-([\da-zA-Z-]+(?:\.[\da-zA-Z-]+)*))?(?:\+[\da-zA-Z-]+(?:\.[\da-zA-Z-]+)*)?$/.exec(value);
    if (!match) throw new Error(`Invalid SemVer: ${value}`);
    const pre = match[4]?.split('.');
    if (pre?.some(part => /^\d+$/.test(part) && part.length > 1 && part[0] === '0')) throw new Error(`Invalid SemVer: ${value}`);
    return { numbers: match.slice(1, 4).map(BigInt), pre };
  };
  const left = parse(a), right = parse(b);
  const compare = (x, y) => x < y ? -1 : x > y ? 1 : 0;
  for (let i = 0; i < 3; i++) {
    const result = compare(left.numbers[i], right.numbers[i]);
    if (result) return result;
  }
  if (!left.pre || !right.pre) return compare(!left.pre, !right.pre);
  for (let i = 0; i < Math.max(left.pre.length, right.pre.length); i++) {
    if (left.pre[i] === undefined) return -1;
    if (right.pre[i] === undefined) return 1;
    const x = left.pre[i], y = right.pre[i];
    const xn = /^\d+$/.test(x), yn = /^\d+$/.test(y);
    const result = xn && yn ? compare(BigInt(x), BigInt(y)) : xn !== yn ? (xn ? -1 : 1) : compare(x, y);
    if (result) return result;
  }
  return 0;
}

function affectedPackages(packages, files) {
  const shared = files.some(file => /^(Directory\.Build\.(props|targets)|global\.json)$/.test(file));
  const affected = new Set(packages.filter(pkg => shared || files.some(file =>
    file.startsWith(`${pkg.directory}/`) && !file.endsWith('/CHANGELOG.md'))).map(pkg => pkg.name));
  let count;
  do {
    count = affected.size;
    for (const pkg of packages) if (pkg.dependencies.some(name => affected.has(name))) affected.add(pkg.name);
  } while (affected.size !== count);
  return affected;
}

function validateVersions(packages, files) {
  const affected = affectedPackages(packages, files);
  const stale = packages.filter(pkg => affected.has(pkg.name) && pkg.previous !== null && compareVersions(pkg.version, pkg.previous) <= 0);
  if (stale.length) throw new Error(`Changed packages need newer versions (including dependents):\n${stale.map(pkg => `${pkg.name}: ${pkg.previous} -> ${pkg.version}`).join('\n')}\nRun build.fsx -- bump and review the changelogs; use force_version for changes without a release-producing commit type.`);
  return affected;
}

function run(base = process.env.XANTHAM_RELEASE_BASE || 'origin/master', root = path.resolve(__dirname, '../..')) {
  const git = (...args) => execFileSync('git', args, { cwd: root, encoding: 'utf8' }).trim();
  if (!/^[A-Za-z0-9][A-Za-z0-9_./-]*$/.test(base) || /^0+$/.test(base)) throw new Error('A valid, existing release base is required. Fetch full Git history.');
  git('merge-base', '--is-ancestor', base, 'HEAD');
  const files = [...git('diff', '--name-only', '--no-renames', base).split('\n'),
    ...git('ls-files', '--others', '--exclude-standard', '--', 'src') .split('\n')].filter(Boolean);
  const version = xml => {
    const matches = [...xml.matchAll(/<Version>\s*([^<]+)\s*<\/Version>/g)];
    if (matches.length !== 1) throw new Error('Each published project must have exactly one literal Version.');
    return matches[0][1].trim();
  };
  const packages = [];
  for (const directory of fs.readdirSync(path.join(root, 'src'))) {
    const project = `src/${directory}/${directory}.fsproj`;
    if (!fs.existsSync(path.join(root, project))) continue;
    const xml = fs.readFileSync(path.join(root, project), 'utf8');
    if (!xml.includes('<Version>')) continue;
    let previous = null;
    // Distinguish a new project from missing history or a failed git command.
    if (git('ls-tree', '--name-only', base, '--', project)) previous = version(git('show', `${base}:${project}`));
    packages.push({
      name: directory, directory: `src/${directory}`, version: version(xml), previous,
      dependencies: [...xml.matchAll(/<ProjectReference\s+Include="([^"]+)"/g)].map(match => path.posix.basename(match[1].replaceAll('\\', '/'), '.fsproj')),
    });
  }
  const required = ['Xantham.Cli', 'Xantham.TypeScript.Wire', 'Xantham.Generator',
    'Xantham.Fable.Core', 'Xantham.Fable.Core.TS', 'Xantham.Fable.Node'];
  const names = new Set(packages.map(pkg => pkg.name));
  if (required.some(name => !names.has(name)) ||
      packages.some(pkg => !required.includes(pkg.name) && pkg.name !== 'Xantham.Generator.Myriad')) {
    throw new Error('Expected the six established packages and optional Xantham.Generator.Myriad. Update the release policy when adopting a package.');
  }
  const affected = validateVersions(packages, files);
  console.log(`Release versions verified against ${base}: ${[...affected].join(', ') || 'no package payload changes'}.`);
}

module.exports = { affectedPackages, compareVersions, validateVersions, run };
if (require.main === module) {
  try { run(process.argv[2]); } catch (error) { console.error(error.message); process.exitCode = 1; }
}
