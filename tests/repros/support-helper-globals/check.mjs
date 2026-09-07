import assert from 'node:assert/strict';
import test from 'node:test';
import {
  erasedPropertyName,
  propertyName,
  propertyValue,
  optionalValue,
  brandRoundTrip,
} from './fable-out/Helpers.js';

test('erased key Value member works as a control', () => {
  assert.equal(erasedPropertyName(), 'Count');
});

test('TypeKeyOf.create/value returns the selected property name', () => {
  assert.equal(propertyName(), 'Count');
});

test('TypeKeyOf.item reads the selected property', () => {
  assert.equal(propertyValue(), 42);
});

test('KeyOf.item reads the selected property independently of TypeKeyOf', () => {
  assert.equal(optionalValue(), 42);
});

test('Brand string helpers round-trip a string', () => {
  assert.equal(brandRoundTrip('identifier'), 'identifier');
});
