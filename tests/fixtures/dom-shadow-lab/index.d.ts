// No import and no export anywhere in file: global script, harvested through `harvest-globals`
// like `globals-lab`. Every interface below reuses a name the default DOM lib already declares
// globally (`Response`, `Headers`, ...), and adds one member neither TypeScript nor the DOM lib
// objects to - ordinary, legal declaration merging. Under the compiler's default lib set, each
// merge sends the symbol's first declaration handle to the DOM lib, so `Grouping.classify`
// reads it as `CompilerLib` even though this package's own extra member sits in the same
// symbol's declaration list.

declare interface Response {
    xanthamDomShadowLabMarker?: string;
}

declare interface Request {
    xanthamDomShadowLabMarker?: string;
}

declare interface Headers {
    xanthamDomShadowLabMarker?: string;
}

declare interface Event {
    xanthamDomShadowLabMarker?: string;
}

declare interface EventTarget {
    xanthamDomShadowLabMarker?: string;
}

declare interface Blob {
    xanthamDomShadowLabMarker?: string;
}

declare interface File {
    xanthamDomShadowLabMarker?: string;
}

declare interface FormData {
    xanthamDomShadowLabMarker?: string;
}

declare interface URL {
    xanthamDomShadowLabMarker?: string;
}

declare interface URLSearchParams {
    xanthamDomShadowLabMarker?: string;
}

declare interface AbortController {
    xanthamDomShadowLabMarker?: string;
}

declare interface AbortSignal {
    xanthamDomShadowLabMarker?: string;
}

declare interface ReadableStream {
    xanthamDomShadowLabMarker?: string;
}

declare interface WritableStream {
    xanthamDomShadowLabMarker?: string;
}

declare interface TransformStream {
    xanthamDomShadowLabMarker?: string;
}

declare interface MessageChannel {
    xanthamDomShadowLabMarker?: string;
}

declare interface MessagePort {
    xanthamDomShadowLabMarker?: string;
}

declare interface TextEncoder {
    xanthamDomShadowLabMarker?: string;
}

declare interface TextDecoder {
    xanthamDomShadowLabMarker?: string;
}

declare interface Crypto {
    xanthamDomShadowLabMarker?: string;
}

declare interface Performance {
    xanthamDomShadowLabMarker?: string;
}
