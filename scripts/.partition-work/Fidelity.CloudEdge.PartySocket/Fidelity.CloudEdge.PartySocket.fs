namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

module Partysocket =
    [<Import("partysocket", "PartySocketOptions")>]
    type PartySocketOptions =
        abstract disableNameValidation: option<bool> with get, set
        abstract query: option<U2<Params, unit -> U2<Params, Promise<Params>>>> with get, set
        abstract path: option<string> with get, set
        abstract protocols: option<Erased.Empty> with get, set
        abstract protocol: option<LiteralUnions.WsWss> with get, set
        abstract prefix: option<string> with get, set
        abstract basePath: option<string> with get, set
        abstract party: option<string> with get, set
        abstract room: option<string> with get, set
        abstract host: string with get, set
        abstract id: option<string> with get, set
        abstract debugLogger: (option<ResizeArray<option<obj>> -> unit>) with get, set
        abstract shouldReconnectOnClose: (option<Erased.Empty -> bool>) with get, set
        abstract startClosed: option<bool> with get, set
        abstract maxEnqueuedMessages: option<float> with get, set
        abstract maxRetries: option<float> with get, set
        abstract connectionTimeout: option<float> with get, set
        abstract minUptime: option<float> with get, set
        abstract reconnectionDelayGrowFactor: option<float> with get, set
        abstract minReconnectionDelay: option<float> with get, set
        abstract maxReconnectionDelay: option<float> with get, set

        [<EmitProperty("WebSocket")>]
        abstract webSocket: option<obj> with get, set

        abstract debug: option<bool> with get, set

    [<Import("partysocket", "Params")>]
    type Params = interface end

    [<Import("partysocket", "Maybe")>]
    type Maybe<'T> = interface end

    [<Import("partysocket", "PartyFetchOptions")>]
    type PartyFetchOptions =
        abstract fetch: (option<U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> -> option<CloudflareWorkersTypes.RequestInit<CloudflareWorkersTypes.RequestInitCfProperties>> -> Promise<CloudflareWorkersTypes.Response>>) with get, set
        abstract query: option<U2<Params, unit -> U2<Params, Promise<Params>>>> with get, set
        abstract protocol: option<LiteralUnions.HttpHttps> with get, set
        abstract path: option<string> with get, set
        abstract prefix: option<string> with get, set
        abstract basePath: option<string> with get, set
        abstract party: option<string> with get, set
        abstract room: string with get, set
        abstract host: string with get, set

    [<Import("partysocket", "default")>]
    type PartySocket =
        [<EmitConstructor>]
        abstract Create: partySocketOptions: PartySocketOptions -> PartySocket

        abstract roomUrl: string with get
        abstract id: string with get
        abstract setWSProperties: option<obj> with get, set
        abstract basePath: option<string> with get, set
        abstract path: string with get, set
        abstract host: string with get, set
        abstract room: option<string> with get, set
        abstract name: string with get, set
        abstract _pkurl: string with get, set
        abstract _pk: string with get, set
        abstract partySocketOptions: PartySocketOptions with get
        abstract updateProperties: partySocketOptions: PartySocket.UpdateProperties.PartySocketOptions -> unit
        abstract reconnect: ?code: float * ?reason: string -> unit
        abstract fetch: options: PartyFetchOptions * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    module PartySocket =
        type PartySocketOptions =
            abstract disableNameValidation: option<bool> with get, set
            abstract query: option<U2<Params, unit -> U2<Params, Promise<Params>>>> with get, set
            abstract path: option<string> with get, set
            abstract protocols: option<Erased.Empty> with get, set
            abstract protocol: option<LiteralUnions.WsWss> with get, set
            abstract prefix: option<string> with get, set
            abstract basePath: option<string> with get, set
            abstract party: option<string> with get, set
            abstract room: option<string> with get, set
            abstract host: string with get, set
            abstract id: option<string> with get, set
            abstract debugLogger: (option<ResizeArray<option<obj>> -> unit>) with get, set
            abstract shouldReconnectOnClose: (option<Erased.Empty -> bool>) with get, set
            abstract startClosed: option<bool> with get, set
            abstract maxEnqueuedMessages: option<float> with get, set
            abstract maxRetries: option<float> with get, set
            abstract connectionTimeout: option<float> with get, set
            abstract minUptime: option<float> with get, set
            abstract reconnectionDelayGrowFactor: option<float> with get, set
            abstract minReconnectionDelay: option<float> with get, set
            abstract maxReconnectionDelay: option<float> with get, set

            [<EmitProperty("WebSocket")>]
            abstract webSocket: option<obj> with get, set

            abstract debug: option<bool> with get, set

        module UpdateProperties =
            type PartySocketOptions =
                abstract disableNameValidation: option<bool> with get, set
                abstract query: option<U2<Params, unit -> U2<Params, Promise<Params>>>> with get, set
                abstract path: option<string> with get, set
                abstract protocols: option<U4<ResizeArray<string>, unit -> option<U2<ResizeArray<string>, string>>, unit -> Promise<option<U2<ResizeArray<string>, string>>>, string>> with get, set
                abstract protocol: option<LiteralUnions.WsWss> with get, set
                abstract prefix: option<string> with get, set
                abstract basePath: option<string> with get, set
                abstract party: option<string> with get, set
                abstract room: option<string> with get, set
                abstract host: option<string> with get, set
                abstract id: option<string> with get, set
                abstract debugLogger: (option<ResizeArray<option<obj>> -> unit>) with get, set
                abstract shouldReconnectOnClose: (option<Erased.Empty -> bool>) with get, set
                abstract startClosed: option<bool> with get, set
                abstract maxEnqueuedMessages: option<float> with get, set
                abstract maxRetries: option<float> with get, set
                abstract connectionTimeout: option<float> with get, set
                abstract minUptime: option<float> with get, set
                abstract reconnectionDelayGrowFactor: option<float> with get, set
                abstract minReconnectionDelay: option<float> with get, set
                abstract maxReconnectionDelay: option<float> with get, set

                [<EmitProperty("WebSocket")>]
                abstract webSocket: option<obj> with get, set

                abstract debug: option<bool> with get, set

    module SharedLiterals =
        type WebSocketConnectionTimeout_ca64dcf1 =
            abstract debugLogger: (option<ResizeArray<option<obj>> -> unit>) with get, set
            abstract shouldReconnectOnClose: (option<Erased.Empty -> bool>) with get, set
            abstract startClosed: option<bool> with get, set
            abstract maxEnqueuedMessages: option<float> with get, set
            abstract maxRetries: option<float> with get, set
            abstract connectionTimeout: option<float> with get, set
            abstract minUptime: option<float> with get, set
            abstract reconnectionDelayGrowFactor: option<float> with get, set
            abstract minReconnectionDelay: option<float> with get, set
            abstract maxReconnectionDelay: option<float> with get, set

            [<EmitProperty("WebSocket")>]
            abstract webSocket: option<obj> with get, set

            abstract debug: option<bool> with get, set

        type BasePathFetchHost6e773867 =
            abstract fetch: (option<U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> -> option<CloudflareWorkersTypes.RequestInit<CloudflareWorkersTypes.RequestInitCfProperties>> -> Promise<CloudflareWorkersTypes.Response>>) with get, set
            abstract query: option<U2<Params, unit -> U2<Params, Promise<Params>>>> with get, set
            abstract protocol: option<LiteralUnions.HttpHttps> with get, set
            abstract path: option<string> with get, set
            abstract prefix: option<string> with get, set
            abstract basePath: option<string> with get, set
            abstract party: option<string> with get, set
            abstract room: string with get, set
            abstract host: string with get, set

        type BasePathDisableNameValidati975e004f =
            abstract disableNameValidation: option<bool> with get, set
            abstract query: option<U2<Params, unit -> U2<Params, Promise<Params>>>> with get, set
            abstract path: option<string> with get, set
            abstract protocols: option<Erased.Empty> with get, set
            abstract protocol: option<LiteralUnions.WsWss> with get, set
            abstract prefix: option<string> with get, set
            abstract basePath: option<string> with get, set
            abstract party: option<string> with get, set
            abstract room: option<string> with get, set
            abstract host: string with get, set
            abstract id: option<string> with get, set
