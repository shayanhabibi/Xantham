---
title: Litmus Tests
category: Overview
categoryindex: 0
---

# Litmus Tests

*Xantham* is tested to be able to consume several convoluted TypeScript libraries
and produce an output that is reference complete for the decoder.

Whether the reference generator produces acceptable output is not tested - the 
litmus test is to verify that we can complete the first two stages correctly,
without stack overflows or missing references/types.

The following litmus test type hierarchies have been completed:

- `@types/three`
- `solid-js`
- `typescript`