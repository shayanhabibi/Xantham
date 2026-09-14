---
title: Xantham
description: Documentation for Xantham
layout: splash
---

[//]: # (<span role="image" class="absolute -mt-17 ml-18 scale-50 xantham-mascot-tile xantham-expressions-peek"> </span>)

<div class="xantham-banner drop-shadow dark:bg-bottom bg-top border-1 border-gray-300 dark:border-gray-500 rounded-[24px]"></div>
<section class="xantham-hero">
    <div class="dark:bg-inherit grid max-w-screen-xl px-4 py-8 mx-auto lg:gap-8 xl:gap-0 lg:py-16 lg:grid-cols-12">
<div class="hidden lg:col-span-5 lg:flex place-self-center">
<span role="image" class="xantham-mascot-tile xantham-actions-wave"> </span>
</div> 
        <div class="ml-auto place-self-center lg:col-span-7">
            <h1 class="max-w-2xl mb-4 text-4xl font-extrabold tracking-tight leading-none md:text-5xl xl:text-6xl dark:text-white">Superpowered Fable Generator</h1>
            <p class="max-w-2xl mb-6 font-light text-gray-500 lg:mb-8 md:text-lg lg:text-xl dark:text-gray-400">From single file type declaration files, to multiple dependency typescript packages, <span class="underline decoration-2 font-bold">xantham</span> can handle it all.</p>
            <a href="https://github.com/shayanhabibi/xantham" class="inline-flex items-center justify-center px-5 py-3 mr-3 text-base font-medium text-center text-white rounded-lg bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 dark:focus:ring-primary-900">
                Leave a Star
                <svg class="w-5 h-5 ml-2 -mr-1" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd" d="M10.293 3.293a1 1 0 011.414 0l6 6a1 1 0 010 1.414l-6 6a1 1 0 01-1.414-1.414L14.586 11H3a1 1 0 110-2h11.586l-4.293-4.293a1 1 0 010-1.414z" clip-rule="evenodd"></path></svg>
            </a>
            <div class="aura aura-xs">
                <a href="xantham-cli/" class="inline-flex bg-(--root-bg) items-center justify-center px-5 py-3 text-base font-medium text-center text-gray-900 border border-gray-300 rounded-lg hover:bg-gray-100 focus:ring-4 focus:ring-gray-100 dark:text-white dark:border-gray-700 dark:hover:bg-gray-700 dark:focus:ring-gray-800">
                    Get Started!
                </a> 
            </div>
        </div>
</div>

<div class="flex flex-col px-4 py-8 lg:py-16 gap-8 mx-auto place-self-center">
<div>
<h1>TSC Packages</h1>
    <div class="card lg:card-side ">
        <div class="card-body">
            <h2 class="card-title">Xantham.TypeScript.Wire</h2>

```bash
dotnet add package Xantham.TypeScript.Wire
```

Xantham parses TypeScript using the native TypeScript compiler API.<br/>
The protocol is generated directly from the source, and is packaged for independent use.
<div class="py-4 card-actions justify-start">
                <a href="xantham-cli/" class="btn">
                    See the Docs
                </a> 
            </div>
        </div>
        <figure class="lg:flex hidden lg:min-w-36">
            <span role="image" class="xantham-mascot-tile xantham-poses-floating"> </span>
        </figure>
    </div>
</div>
<h1>Support Packages</h1>
    <div class="card lg:card-side ">
        <figure class="lg:flex hidden lg:min-w-36">
            <span role="image" class="xantham-mascot-tile xantham-actions-generate"> </span>
        </figure>
        <div class="card-body">
            <h2 class="card-title">Xantham.Fable.Core</h2>

```bash
dotnet add package Xantham.Fable.Core
```

Bindings use erased utility types which are pre-packaged and known to the generator.<br/>
They help to bridge the gap for seamless interop.<br/>

<div class="card-actions justify-start">


<a href="xantham-cli/" class="btn">
                    See the Docs
                </a> 
            </div>
        </div>
    </div>
    <div class="card lg:card-side ">
        <div class="card-body">
<h2 class="card-title">Xantham.Fable.Core.TS</h2>

```bash
dotnet add package Xantham.Fable.Core.TS
```

Xantham generated `lib/dom` and `lib/es*` bindings!<br/>
The generator assumes availability of all TypeScript compiler library sources.<br/>
Either add this package reference, or generate your own!
</div>
        <figure class="lg:flex hidden lg:min-w-36">
            <span role="image" class="xantham-mascot-tile xantham-poses-with-coffee"> </span>
        </figure>
    </div>
    <div class="card lg:card-side">
        <figure class="lg:flex hidden lg:min-w-36">
            <span role="image" class="xantham-mascot-tile xantham-actions-explore"> </span>
        </figure>
        <div class="card-body">
<h2 class="card-title">Xantham.Fable.Node</h2>

```bash
dotnet add package Xantham.Fable.Node
```

Xantham generate bindings for `@types/node` measuring up to 270k loc with comments!<br/>
        </div>
    </div>
</div>
</section>
