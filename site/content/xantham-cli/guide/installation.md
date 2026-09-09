---
title: Setting up
order: 0
---

Install the tool from, install the pinned compiler version, write `xantham.json`, 
run `xantham generate`{bash}.

For the compiler client `xantham` is built on, see 
[the compiler client documentation](/content/wire/index.md).

:::warning
The generator is in **alpha**.
:::

## Install the tool

<div class="mockup-code w-full leading-none">
<pre data-prefix="$" class=""><code><span class="text-blue-400">dotnet</span> tool install <span class="text-gray-400">--global</span> xantham <span class="text-gray-400">--prerelease</span></code></pre>
<pre data-prefix=">" class="text-success leading-1"><code>You can invoke the tool using the following command: xantham</code></pre>
<pre data-prefix=">" class="text-success leading-1"><code>Tool 'xantham' (version '0.1.0-alpha.4') was successfully installed.</code></pre>
</div>
<br/>

The package install the command `xantham` and targets `net10.0`.

`xantham --version`{bash} prints the installed version, and 
`xantham --help`{bash} the whole command line.

## Pin the compiler for the tool

:::warning
You must have `node`/`npm` installed on PATH.
:::

<div class="mockup-code w-full leading-none">
<pre data-prefix="$" class=""><code><span class="text-blue-400">xantham</span> tsc version</code></pre>
<pre data-prefix=">" class="text-warning leading-1"><code>^7.1.0-dev.20260902.1 not found in cache. Run `xantham tsc init`</code></pre>
<pre data-prefix=">" ></pre>
<pre data-prefix="$" class=""><code><span class="text-blue-400">xantham</span> tsc init</code></pre>
<pre data-prefix=">" class="text-accent leading-1"><code>added 2 packages, and audited 3 packages in 7s</code></pre>
<pre data-prefix=">" class="text-accent leading-1"><code>found 0 vulnerabilities</code></pre>
<pre data-prefix=">" ></pre>
<pre data-prefix="$" class=""><code><span class="text-blue-400">xantham</span> tsc version</code></pre>
<pre data-prefix=">" class="text-success leading-1"><code>^7.1.0-dev.20260902.1 cached at: ~\.cache\xantham\7.1.0-d<span class="text-accent/40">...</span></code></pre>
</div>
<br/>

The native Go `tsc` binary shipped inside the `typescript` npm package
must match the client protocol that `xantham` shipped with, especially
as there is no version handshake protocol in `tsc --api` at the moment.

