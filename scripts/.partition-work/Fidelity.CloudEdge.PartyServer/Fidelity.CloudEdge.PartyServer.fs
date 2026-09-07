namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

type IPartyserver =
    [<Import("partyserver", "getServerByName")>]
    static member getServerByName<'Env, 'T, 'Props>(serverNamespace: obj, name: string, ?options: obj) : Promise<Partyserver.GetServerByName.Case2> = JS.undefined

    [<Import("partyserver", "routePartykitRequest")>]
    static member routePartykitRequest<'Env, 'T, 'Props>(req: obj, ?env: obj, ?options: obj) : Promise<option<CloudflareWorkersTypes.Response>> = JS.undefined

module Partyserver =
    [<Import("partyserver", "RoutingRetryOptions")>]
    type RoutingRetryOptions =
        abstract maxDelayMs: option<float> with get, set
        abstract baseDelayMs: option<float> with get, set
        abstract maxAttempts: option<float> with get, set
        abstract onRetry: event: RoutingRetryEvent -> option<Promise<unit>>

    [<Import("partyserver", "RoutingRetryEvent")>]
    type RoutingRetryEvent =
        abstract className: option<string> with get, set
        abstract name: string with get, set
        abstract delayMs: float with get, set
        abstract maxAttempts: float with get, set
        abstract attempt: float with get, set
        abstract error: option<obj> with get, set

    [<Import("partyserver", "PartyServerOptions")>]
    type PartyServerOptions<'Env, 'Props> =
        abstract routingRetry: option<U2<RoutingRetryOptions, bool>> with get, set
        abstract cors: option<U4<obj, CloudflareWorkersTypes.Headers, seq<seq<string>>, bool>> with get, set
        abstract props: option<'Props> with get, set
        abstract locationHint: option<CloudflareWorkersTypes.DurableObjectLocationHint> with get, set
        abstract jurisdiction: option<CloudflareWorkersTypes.DurableObjectJurisdiction> with get, set
        abstract prefix: option<string> with get, set
        abstract onBeforeConnect: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * lobby: Lobby<'Env> -> option<U3<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, Promise<option<U2<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>>>>>>
        abstract onBeforeRequest: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * lobby: Lobby<'Env> -> option<U3<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, Promise<option<U2<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>>>>>>

    [<Import("partyserver", "Lobby")>]
    type Lobby<'Env> =
        abstract name: string with get, set
        abstract className: option<obj> with get, set
        /// <deprecated>
        /// Use `className` instead, which returns the Durable Object class name.<br/>
        /// In the next major version, `party` will return the class name instead of the kebab-case namespace.
        /// </deprecated>
        abstract party: string with get, set

    type GetServerByName =
        abstract routingRetry: option<U2<RoutingRetryOptions, bool>> with get, set
        abstract props: option<obj> with get, set
        abstract locationHint: option<CloudflareWorkersTypes.DurableObjectLocationHint> with get, set
        abstract jurisdiction: option<CloudflareWorkersTypes.DurableObjectJurisdiction> with get, set

    [<Import("partyserver", "Server")>]
    type Server<'Env, 'Props> =
        [<EmitConstructor>]
        abstract Create: ctx: Erased.Empty * env: 'Env -> Server<'Env, 'Props>

        abstract name: string with get
        abstract options: Server.Options with get, set

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract sql<'T> : strings: obj * [<ParamArray>] values: ResizeArray<option<U3<string, float, bool>>> -> ResizeArray<'T>
        abstract fetch: request: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>
        abstract webSocketMessage: ws: CloudflareWorkersTypes.WebSocket * message: WSMessage -> Promise<unit>
        abstract webSocketClose: ws: CloudflareWorkersTypes.WebSocket * code: float * reason: string * wasClean: bool -> Promise<unit>
        abstract webSocketError: ws: CloudflareWorkersTypes.WebSocket * ?error: obj -> Promise<unit>
        abstract _unsafeEnsureInitialized: unit -> Promise<unit>
        abstract setName: name: string * ?props: 'Props -> Promise<unit>
        abstract _initAndFetch: name: string * ?props: obj * request: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>
        abstract broadcast: msg: WSMessage * ?without: ResizeArray<string> -> unit
        abstract getConnection: id: string -> option<obj>
        abstract getConnections: ?tag: string -> seq<obj>
        abstract getConnectionTags: connection: Server.GetConnectionTags.Connection * context: ConnectionContext -> U2<ResizeArray<string>, Promise<ResizeArray<string>>>
        abstract onStart: ?props: 'Props -> option<Promise<unit>>
        abstract onConnect: connection: obj * ctx: ConnectionContext -> option<Promise<unit>>
        abstract onMessage: connection: obj * message: WSMessage -> option<Promise<unit>>
        abstract onClose: connection: obj * code: float * reason: string * wasClean: bool -> option<Promise<unit>>
        abstract onError: connection: obj * ?error: obj -> option<Promise<unit>>
        abstract onRequest: request: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> U2<CloudflareWorkersTypes.Response, Promise<CloudflareWorkersTypes.Response>>
        abstract onException: ?error: obj -> option<Promise<unit>>
        abstract onAlarm: unit -> option<Promise<unit>>
        abstract alarm: unit -> Promise<unit>

    [<Import("partyserver", "ConnectionState")>]
    type ConnectionState<'T> = interface end

    type ConnectionSetStateFn = ConnectionState<obj> -> obj

    [<Import("partyserver", "ConnectionContext")>]
    type ConnectionContext =
        abstract request: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> with get, set

    [<Import("partyserver", "ImmutableObject")>]
    type ImmutableObject =
        abstract Item: key: string -> option<obj>

    type WSMessage = U3<ArrayBuffer, obj, string>

    [<Import("partyserver", "Connection")>]
    type Connection =
        /// <deprecated>
        /// Use `this.name` on the Server instead.<br/>
        /// The server name. Populated from `Server.name` after initialization.
        /// </deprecated>
        abstract server: string with get, set
        abstract tags: System.Collections.Generic.IReadOnlyList<string> with get, set
        abstract state: ConnectionState<obj> with get, set
        abstract uri: option<string> with get, set
        abstract id: string with get, set
        abstract binaryType: LiteralUnions.ArraybufferBlob with get, set
        abstract extensions: option<string> with get, set
        abstract protocol: option<string> with get, set
        abstract url: option<string> with get, set
        abstract readyState: float with get, set
        abstract accept: ?options: CloudflareWorkersTypes.WebSocketAcceptOptions -> unit
        abstract send: message: U3<ArrayBuffer, obj, string> -> unit
        abstract close: ?code: float * ?reason: string -> unit
        abstract serializeAttachment: ?attachment: obj -> unit
        abstract serializeAttachment: attachment: obj -> unit
        abstract deserializeAttachment: unit -> option<obj>
        abstract addEventListener: ``type``: obj * handler: U2<CloudflareWorkersTypes.EventListener, CloudflareWorkersTypes.EventListenerObject<proptypekey<obj, obj>>> * ?options: U2<CloudflareWorkersTypes.EventTargetAddEventListenerOptions, bool> -> unit
        abstract removeEventListener<'Type> : ``type``: 'Type * handler: U2<CloudflareWorkersTypes.EventListener, CloudflareWorkersTypes.EventListenerObject<proptypekey<obj, 'Type>>> * ?options: U2<CloudflareWorkersTypes.EventTargetEventListenerOptions, bool> -> unit
        abstract dispatchEvent: event: proptypekey<obj, keyof<obj>> -> bool
        abstract setState: ?state: U2<obj, ConnectionState<obj> -> obj> -> ConnectionState<obj>

    module GetServerByName =
        type Case2 =
            abstract name: option<string> with get
            abstract id: CloudflareWorkersTypes.DurableObjectId with get
            abstract Invoke: [<ParamArray>] args: CloudflareWorkersTypes.Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>
            abstract connect: address: U2<CloudflareWorkersTypes.SocketAddress, string> * ?options: CloudflareWorkersTypes.SocketOptions -> CloudflareWorkersTypes.Socket

    module Server =
        type Options =
            abstract hibernate: option<bool> with get, set

        module GetConnectionTags =
            type Connection =
                abstract server: string with get, set
                abstract tags: System.Collections.Generic.IReadOnlyList<string> with get, set
                abstract state: option<ConnectionState<obj>> with get, set
                abstract uri: option<string> with get, set
                abstract id: string with get, set
                abstract binaryType: LiteralUnions.ArraybufferBlob with get, set
                abstract extensions: option<string> with get, set
                abstract protocol: option<string> with get, set
                abstract url: option<string> with get, set
                abstract readyState: float with get, set
                abstract accept: ?options: CloudflareWorkersTypes.WebSocketAcceptOptions -> unit
                abstract send: message: U3<ArrayBuffer, obj, string> -> unit
                abstract close: ?code: float * ?reason: string -> unit
                abstract serializeAttachment: ?attachment: obj -> unit
                abstract serializeAttachment: attachment: obj -> unit
                abstract deserializeAttachment: unit -> option<obj>
                abstract addEventListener: ``type``: obj * handler: U2<CloudflareWorkersTypes.EventListener, CloudflareWorkersTypes.EventListenerObject<proptypekey<obj, obj>>> * ?options: U2<CloudflareWorkersTypes.EventTargetAddEventListenerOptions, bool> -> unit
                abstract removeEventListener<'Type> : ``type``: 'Type * handler: U2<CloudflareWorkersTypes.EventListener, CloudflareWorkersTypes.EventListenerObject<proptypekey<obj, 'Type>>> * ?options: U2<CloudflareWorkersTypes.EventTargetEventListenerOptions, bool> -> unit
                abstract dispatchEvent: event: proptypekey<obj, keyof<obj>> -> bool
                abstract setState: ?state: U2<obj, ConnectionState<obj> -> obj> -> ConnectionState<obj>
