import assert from 'node:assert/strict';
import test from 'node:test';
import { aliasConfiguration, aliasExtensions, descriptor, projectSource } from './package-fickl-core-ts.mjs';

test('alias configuration preserves compiler ownership settings and canonical input', () => {
  const original = { declarationCatalog: true, compilerLib: { module: 'Fable.Core.TS', autoOpenEs: true }, groups: { 'typescript/lib': 'ship' } };
  const alias = aliasConfiguration(original);
  assert.equal(alias.compilerLib.module, 'Fickl.Core.Ts');
  assert.equal(alias.compilerLib.autoOpenEs, true);
  assert.equal(original.compilerLib.module, 'Fable.Core.TS');
  assert.deepEqual(alias.groups, original.groups);
});

test('extension rename preserves real Fable imports', () => {
  assert.equal(aliasExtensions('module Fable.Core.TSExtensions\nopen Fable.Core.TS\nopen Fable.Core.JsInterop'),
    'module Fickl.Core.TsExtensions\nopen Fickl.Core.Ts\nopen Fable.Core\nopen Fable.Core.JsInterop');
});

test('standalone project carries support sources without private dependencies', () => {
  const source = projectSource({ version: '0.4.0-beta.2', fable: '5.6.0', fickl: '0.7.2', sdk: '1.4.1' });
  assert.ok(source.indexOf('Library.fs') < source.indexOf('Fickl.Core.Ts.fs'));
  assert.ok(!source.includes('ProjectReference'));
  assert.ok(!source.includes('Xantham.Fable.Core'));
});

test('catalogue packaging rejects canonical names and authenticates actual alias DLL', () => {
  const args = { version: '0.4.0-beta.2', catalogue: { schemaVersion: 2, owner: 'Input', owners: [], declarations: [{ fSharpName: 'Fickl.Core.Ts.Es.Promise' }], compatibility: {} }, compressed: Buffer.from('catalogue'), assembly: Buffer.from('alias DLL'), configuration: '{}', bindings: [] };
  const result = descriptor(args);
  assert.equal(result.packageId, 'Fickl.Core.Ts');
  assert.equal(result.assembly, 'lib/net8.0/Fickl.Core.Ts.dll');
  assert.notEqual(result.assemblySha256, descriptor({ ...args, assembly: Buffer.from('other DLL') }).assemblySha256);
  assert.throws(() => descriptor({ ...args, catalogue: { ...args.catalogue, declarations: [{ fSharpName: 'Fable.Core.TS.Es.Promise' }] } }), /alias namespace/);
});
