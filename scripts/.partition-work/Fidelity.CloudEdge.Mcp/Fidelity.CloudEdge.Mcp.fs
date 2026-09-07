namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

type IModelcontextprotocolSdk =
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "discoverOAuthProtectedResourceMetadata")>]
    static member discoverOAuthProtectedResourceMetadata(serverUrl: U2<obj, string>, ?opts: obj, ?fetchFn: obj) : Promise<ModelcontextprotocolSdk.Shared.AuthJs.OAuthProtectedResourceMetadata> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "isHttpsUrl")>]
    static member isHttpsUrl(?value: string) : bool = JS.undefined

    /// <param name="input" >
    /// - A Response object or string containing the error response
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "parseErrorResponse")>]
    static member parseErrorResponse(input: U2<obj, string>) : Promise<ModelcontextprotocolSdk.OAuthError> = JS.undefined

    /// <deprecated>
    /// This function is deprecated in favor of `discoverAuthorizationServerMetadata`.
    /// </deprecated>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "discoverOAuthMetadata")>]
    static member discoverOAuthMetadata(issuer: U2<obj, string>, ?authorizationServerUrlprotocolVersion: obj, ?fetchFn: obj) : Promise<option<ModelcontextprotocolSdk.Shared.AuthJs.OAuthMetadata>> = JS.undefined

    /// <param name="authorizationCode" >
    /// - The authorization code received from the authorization endpoint
    /// </param>
    /// <param name="codeVerifier" >
    /// - The PKCE code verifier
    /// </param>
    /// <param name="redirectUri" >
    /// - The redirect URI used in the authorization request
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "prepareAuthorizationCodeRequest")>]
    static member prepareAuthorizationCodeRequest(authorizationCode: string, codeVerifier: string, redirectUri: U2<obj, string>) : CloudflareWorkersTypes.URLSearchParams = JS.undefined

    /// <param name="authorizationServerUrl" >
    /// - The authorization server URL obtained from the MCP Server's<br/>
    ///   protected resource metadata, or the MCP server's URL if the<br/>
    ///   metadata was not found.
    /// </param>
    /// <param name="options" >
    /// - Configuration options
    /// </param>
    /// <param name="options.fetchFn" >
    /// - Optional fetch function for making HTTP requests, defaults to global fetch
    /// </param>
    /// <param name="options.protocolVersion" >
    /// - MCP protocol version to use, defaults to LATEST_PROTOCOL_VERSION
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "discoverAuthorizationServerMetadata")>]
    static member discoverAuthorizationServerMetadata(authorizationServerUrl: U2<obj, string>, ?fetchFnprotocolVersion: obj) : Promise<option<U2<ModelcontextprotocolSdk.Shared.AuthJs.OAuthMetadata, ModelcontextprotocolSdk.Shared.AuthJs.OpenIdProviderDiscoveryMetadata>>> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "registerClient")>]
    static member registerClient(authorizationServerUrl: U2<obj, string>, metadataclientMetadatascopefetchFn: obj) : Promise<ModelcontextprotocolSdk.Shared.AuthJs.OAuthClientInformationFull> = JS.undefined

    /// <param name="serverUrl" >
    /// - The MCP resource server URL
    /// </param>
    /// <param name="opts" >
    /// - Optional configuration
    /// </param>
    /// <param name="opts.resourceMetadataUrl" >
    /// - Override URL for the protected resource metadata endpoint
    /// </param>
    /// <param name="opts.fetchFn" >
    /// - Custom fetch function for HTTP requests
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "discoverOAuthServerInfo")>]
    static member discoverOAuthServerInfo(serverUrl: U2<obj, string>, ?opts: obj) : Promise<ModelcontextprotocolSdk.OAuthServerInfo> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "mergeCapabilities")>]
    static member mergeCapabilities(``base``: obj, additional: obj) : ModelcontextprotocolSdk.TypesJs.ServerCapabilities = JS.undefined

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "mergeCapabilities")>]
    static member mergeCapabilities(``base``: obj, additional: SharedLiterals.ElicitationExperimentalExtFf9b7be32) : ModelcontextprotocolSdk.TypesJs.ClientCapabilities = JS.undefined

    /// <deprecated>
    /// Use `extractWWWAuthenticateParams` instead.
    /// </deprecated>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "extractResourceMetadataUrl")>]
    static member extractResourceMetadataUrl(res: obj) : option<CloudflareWorkersTypes.URL> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "auth")>]
    static member auth(provider: obj, options: obj) : Promise<ModelcontextprotocolSdk.AuthResult> = JS.undefined

    /// <example>
    /// // Provider for client_credentials:<br/>
    /// class MyProvider implements OAuthClientProvider {<br/>
    ///   prepareTokenRequest(scope) {<br/>
    ///     const params = new URLSearchParams({ grant_type: 'client_credentials' });<br/>
    ///     if (scope) params.set('scope', scope);<br/>
    ///     return params;<br/>
    ///   }<br/>
    ///   // ... other methods<br/>
    /// }<br/>
    /// <br/>
    /// const tokens = await fetchToken(provider, authServerUrl, { metadata });
    /// </example>
    /// <param name="provider" >
    /// - OAuth client provider that implements prepareTokenRequest()
    /// </param>
    /// <param name="authorizationServerUrl" >
    /// - The authorization server's base URL
    /// </param>
    /// <param name="options" >
    /// - Configuration for the token request
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "fetchToken")>]
    static member fetchToken(provider: obj, authorizationServerUrl: U2<obj, string>, ?metadataresourceauthorizationCodefetchFn: obj) : Promise<ModelcontextprotocolSdk.Shared.AuthJs.OAuthTokens> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "extractWWWAuthenticateParams")>]
    static member extractWWWAuthenticateParams(res: obj) : ModelcontextprotocolSdk.ExtractWWWAuthenticateParams = JS.undefined

    [<Import("@modelcontextprotocol/sdk/types.js", "assertCompleteRequestResourceTemplate")>]
    static member assertCompleteRequestResourceTemplate(request: obj) : bool = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "selectResourceURL")>]
    static member selectResourceURL(serverUrl: U2<obj, string>, provider: obj, ?resourceMetadata: obj) : Promise<option<CloudflareWorkersTypes.URL>> = JS.undefined

    /// <param name="authorizationServerUrl" >
    /// - The authorization server's base URL
    /// </param>
    /// <param name="options" >
    /// - Configuration object containing client info, auth code, etc.
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "exchangeAuthorization")>]
    static member exchangeAuthorization(authorizationServerUrl: U2<obj, string>, metadataclientInformationauthorizationCodecodeVerifierredirectUriresourceaddClientAuthenticationfetchFn: obj) : Promise<ModelcontextprotocolSdk.Shared.AuthJs.OAuthTokens> = JS.undefined

    /// <param name="authorizationServerUrl" >
    /// - The authorization server's base URL
    /// </param>
    /// <param name="options" >
    /// - Configuration object containing client info, refresh token, etc.
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "refreshAuthorization")>]
    static member refreshAuthorization(authorizationServerUrl: U2<obj, string>, metadataclientInformationrefreshTokenresourceaddClientAuthenticationfetchFn: obj) : Promise<ModelcontextprotocolSdk.Shared.AuthJs.OAuthTokens> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "buildDiscoveryUrls")>]
    static member buildDiscoveryUrls(authorizationServerUrl: U2<obj, string>) : ResizeArray<ModelcontextprotocolSdk.BuildDiscoveryUrls> = JS.undefined

    [<Import("@modelcontextprotocol/sdk/shared/transport.js", "normalizeHeaders")>]
    static member normalizeHeaders(headers: option<U3<obj, obj, seq<seq<string>>>>) : obj = JS.undefined

    /// <param name="capabilities" >
    /// - The client's elicitation capabilities
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/index.js", "getSupportedElicitationModes")>]
    static member getSupportedElicitationModes(capabilities: option<SharedLiterals.FormUrl>) : ModelcontextprotocolSdk.GetSupportedElicitationModes = JS.undefined

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "startAuthorization")>]
    static member startAuthorization(authorizationServerUrl: U2<obj, string>, metadataclientInformationredirectUrlscopestateresource: obj) : Promise<ModelcontextprotocolSdk.StartAuthorization.Case2> = JS.undefined

    /// <param name="baseFetch" >
    /// - The base fetch function to wrap (defaults to global fetch)
    /// </param>
    /// <param name="baseInit" >
    /// - The base RequestInit to merge with each request
    /// </param>
    [<Import("@modelcontextprotocol/sdk/shared/transport.js", "createFetchWithInit")>]
    static member createFetchWithInit(?baseFetch: obj, ?baseInit: obj) : ModelcontextprotocolSdk.FetchLike = JS.undefined

    [<Import("@modelcontextprotocol/sdk/types.js", "assertCompleteRequestPrompt")>]
    static member assertCompleteRequestPrompt(request: obj) : bool = JS.undefined

    /// <param name="clientInformation" >
    /// - OAuth client information containing credentials
    /// </param>
    /// <param name="supportedMethods" >
    /// - Authentication methods supported by the authorization server
    /// </param>
    [<Import("@modelcontextprotocol/sdk/client/auth.js", "selectClientAuthMethod")>]
    static member selectClientAuthMethod(clientInformation: obj, supportedMethods: ResizeArray<string>) : ModelcontextprotocolSdk.Client.AuthJs.ClientAuthMethod = JS.undefined

module ModelcontextprotocolSdk =
    type RegisterClient =
        abstract scope: option<string> with get, set
        abstract clientMetadata: Shared.AuthJs.OAuthClientMetadata with get, set
        abstract metadata: option<AuthorizationServerMetadata> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "RequestOptions")>]
    type RequestOptions =
        abstract resumptionToken: option<string> with get, set
        abstract relatedRequestId: option<ProgressToken> with get, set
        abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
        abstract task: option<TypesJs.TaskCreationParams> with get, set
        abstract maxTotalTimeout: option<float> with get, set
        abstract resetTimeoutOnProgress: option<bool> with get, set
        abstract timeout: option<float> with get, set
        abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
        abstract onprogress: progress: TypesJs.Progress -> unit
        abstract onresumptiontoken: token: string -> unit

    [<Import("@modelcontextprotocol/sdk/types.js", "RequestInfo")>]
    type RequestInfo =
        abstract url: option<CloudflareWorkersTypes.URL> with get, set
        abstract headers: IsomorphicHeaders with get, set

    type AnyObjectSchema =
        abstract Item: key: string -> U2<option<obj>, proptypekey<obj, string>>

    type DiscoverAuthorizationServerMetadata =
        abstract protocolVersion: option<string> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/types.js", "SingleSelectEnumSchema")>]
    type SingleSelectEnumSchema = interface end

    type BaseToolCallback<'SendResultT, 'ExtraT, 'Args> = U3<SharedLiterals._def_e995c0ec2<'Args, obj, obj, obj> -> 'ExtraT -> U2<TypesJs.Result, Promise<'SendResultT>>, option<obj> -> 'ExtraT -> U2<TypesJs.Result, Promise<'SendResultT>>, 'ExtraT -> U2<TypesJs.Result, Promise<'SendResultT>>>

    [<Import("../shared/uriTemplate.js", "UriTemplate")>]
    type UriTemplate =
        [<EmitConstructor>]
        abstract Create: template: string -> UriTemplate

        abstract partToRegExp: option<obj> with get, set
        abstract escapeRegExp: option<obj> with get, set
        abstract expandPart: option<obj> with get, set
        abstract encodeValue: option<obj> with get, set
        abstract getNames: option<obj> with get, set
        abstract getOperator: option<obj> with get, set
        abstract parse: option<obj> with get, set
        abstract variableNames: ResizeArray<string> with get
        abstract parts: option<obj> with get
        abstract template: option<obj> with get
        abstract validateLength: option<obj> with get, set
        abstract isTemplate: str: string -> bool
        abstract toString: unit -> string
        abstract expand: variables: Variables -> string
        abstract ``match``: uri: string -> option<Variables>

    [<Import("./zod-compat.js", "ShapeOutput")>]
    type ShapeOutput =
        abstract Item: key: string -> option<obj>

    type JsonSchemaValidatorResult<'T> = U2<SharedLiterals.DataErrorMessageValid<'T>, SharedLiterals.DataErrorMessageValid2>

    [<Import("@modelcontextprotocol/sdk/types.js", "ListChangedOptions")>]
    type ListChangedOptions =
        abstract onChanged: ListChangedCallback with get, set
        abstract debounceMs: option<float> with get, set
        abstract autoRefresh: option<bool> with get, set

    type PrimitiveSchemaDefinition =
        [<EmitProperty("default")>]
        abstract ``default``: option<bool> with get, set

        abstract description: option<string> with get, set
        abstract title: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AddClientAuthentication = CloudflareWorkersTypes.Headers -> CloudflareWorkersTypes.URLSearchParams -> U2<CloudflareWorkersTypes.URL, string> -> option<AuthorizationServerMetadata> -> option<Promise<unit>>

    [<Import("../experimental/tasks/interfaces.js", "TaskMessageQueue")>]
    type TaskMessageQueue =
        abstract enqueue: taskId: string * message: QueuedMessage * ?sessionId: string * ?maxSize: float -> Promise<unit>
        abstract dequeue: taskId: string * ?sessionId: string -> Promise<option<U4<QueuedRequest, QueuedNotification, QueuedResponse, QueuedError>>>
        abstract dequeueAll: taskId: string * ?sessionId: string -> Promise<ResizeArray<QueuedMessage>>

    [<Import("@modelcontextprotocol/sdk/types.js", "IsomorphicHeaders")>]
    type IsomorphicHeaders = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "JSONRPCMessage")>]
    type JSONRPCMessage = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "LoggingLevel")>]
    type LoggingLevel = interface end

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "Protocol")>]
    type Protocol<'SendRequestT, 'SendNotificationT, 'SendResultT> =
        [<EmitConstructor>]
        abstract Create: ?_options: ProtocolOptions -> Protocol<'SendRequestT, 'SendNotificationT, 'SendResultT>

        abstract requestTaskStore: option<obj> with get, set
        /// <param name="taskId" >
        /// The task ID to wait for
        /// </param>
        /// <param name="signal" >
        /// Abort signal to cancel the wait
        /// </param>
        abstract _waitForTaskUpdate: option<obj> with get, set
        /// <param name="taskId" >
        /// The task ID whose queue should be cleared
        /// </param>
        /// <param name="sessionId" >
        /// Optional session ID for binding the operation to a specific session
        /// </param>
        abstract _clearTaskQueue: option<obj> with get, set
        /// <param name="taskId" >
        /// The task ID to associate the message with
        /// </param>
        /// <param name="message" >
        /// The message to enqueue
        /// </param>
        /// <param name="sessionId" >
        /// Optional session ID for binding the operation to a specific session
        /// </param>
        abstract _enqueueTaskMessage: option<obj> with get, set
        abstract _cleanupTaskProgressHandler: option<obj> with get, set
        abstract transport: option<Transport> with get
        abstract _onresponse: option<obj> with get, set
        abstract _onprogress: option<obj> with get, set
        abstract _onrequest: option<obj> with get, set
        abstract _onnotification: option<obj> with get, set
        abstract _onerror: option<obj> with get, set
        abstract _onclose: option<obj> with get, set
        abstract _cleanupTimeout: option<obj> with get, set
        abstract _resetTimeout: option<obj> with get, set
        abstract _setupTimeout: option<obj> with get, set
        abstract _oncancel: option<obj> with get, set
        abstract _requestResolvers: option<obj> with get, set
        abstract _taskMessageQueue: option<obj> with get, set
        abstract _taskStore: option<obj> with get, set
        abstract _taskProgressTokens: option<obj> with get, set
        abstract _pendingDebouncedNotifications: option<obj> with get, set
        abstract _timeoutInfo: option<obj> with get, set
        abstract _progressHandlers: option<obj> with get, set
        abstract _responseHandlers: option<obj> with get, set
        abstract _notificationHandlers: option<obj> with get, set
        abstract _requestHandlerAbortControllers: option<obj> with get, set
        abstract _requestHandlers: option<obj> with get, set
        abstract _requestMessageId: option<obj> with get, set
        abstract _transport: option<obj> with get, set
        abstract _options: option<obj> with get, set
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract fallbackRequestHandler: request: TypesJs.JSONRPCRequest * extra: SharedLiterals._metaAuthInfo0d6b92d73 -> Promise<'SendResultT>
        abstract fallbackNotificationHandler: notification: TypesJs.Notification -> Promise<unit>
        abstract connect: transport: Transport -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract assertCapabilityForMethod: method: proptypekey<'SendRequestT, string> -> unit
        abstract assertNotificationCapability: method: proptypekey<'SendNotificationT, string> -> unit
        abstract assertRequestHandlerCapability: method: string -> unit
        abstract assertTaskCapability: method: string -> unit
        abstract assertTaskHandlerCapability: method: string -> unit
        abstract requestStream: request: 'SendRequestT * resultSchema: obj * ?options: RequestOptions -> seq<U4<TaskStatusMessage, TaskCreatedMessage, ErrorMessage, ResultMessage<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>>>
        abstract request: request: 'SendRequestT * resultSchema: obj * ?options: RequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
        abstract getTask: params: SharedLiterals._metaTaskId * ?options: RequestOptions -> Promise<TypesJs.GetTaskResult>
        abstract getTaskResult: params: SharedLiterals._metaTaskId * resultSchema: obj * ?options: RequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
        abstract listTasks: ?params: Protocol.ListTasks.Params * ?options: RequestOptions -> Promise<SharedLiterals._metaNextCursorTasks2>
        abstract cancelTask: params: SharedLiterals.TaskId4 * ?options: RequestOptions -> Promise<SharedLiterals._metaCreatedAtF1242d44>
        abstract notification: notification: 'SendNotificationT * ?options: NotificationOptions -> Promise<unit>
        abstract setRequestHandler: requestSchema: obj * handler: (option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.Result, Promise<'SendResultT>>) -> unit
        abstract removeRequestHandler: method: string -> unit
        abstract assertCanSetRequestHandler: method: string -> unit
        abstract setNotificationHandler: notificationSchema: obj * handler: (option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> option<Promise<unit>>) -> unit
        abstract removeNotificationHandler: method: string -> unit

    [<Import("@modelcontextprotocol/sdk/client/index.js", "Client")>]
    type Client<'RequestT, 'NotificationT, 'ResultT> =
        [<EmitConstructor>]
        abstract Create: _clientInfo: TypesJs.Implementation * ?options: ClientOptions -> Client<'RequestT, 'NotificationT, 'ResultT>

        abstract _setupListChangedHandler: option<obj> with get, set
        abstract getToolOutputValidator: option<obj> with get, set
        abstract cacheToolMetadata: option<obj> with get, set
        abstract isToolTaskRequired: option<obj> with get, set
        abstract isToolTask: option<obj> with get, set
        abstract experimental: Client.Experimental with get
        abstract _setupListChangedHandlers: option<obj> with get, set
        abstract _pendingListChangedConfig: option<obj> with get, set
        abstract _listChangedDebounceTimers: option<obj> with get, set
        abstract _experimental: option<obj> with get, set
        abstract _cachedRequiredTaskTools: option<obj> with get, set
        abstract _cachedKnownTaskTools: option<obj> with get, set
        abstract _cachedToolOutputValidators: option<obj> with get, set
        abstract _jsonSchemaValidator: option<obj> with get, set
        abstract _instructions: option<obj> with get, set
        abstract _capabilities: option<obj> with get, set
        abstract _serverVersion: option<obj> with get, set
        abstract _serverCapabilities: option<obj> with get, set
        abstract _clientInfo: option<obj> with get, set
        abstract registerCapabilities: capabilities: TypesJs.ClientCapabilities -> unit
        abstract setRequestHandler: requestSchema: obj * handler: (option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> SharedLiterals._metaAuthInfo0d6b92d73 -> U10<SharedLiterals._meta2, SharedLiterals._metaContentModelRoleStopReason3, SharedLiterals._metaContentModelRoleStopReason4, SharedLiterals._metaActionContent3, SharedLiterals._metaRoots2, SharedLiterals._metaCreatedAtF1242d44, SharedLiterals._metaNextCursorTasks2, SharedLiterals._metaTask3, TypesJs.Result, Promise<U9<TypesJs.Result, SharedLiterals._meta2, SharedLiterals._metaContentModelRoleStopReason3, SharedLiterals._metaContentModelRoleStopReason4, SharedLiterals._metaActionContent3, SharedLiterals._metaRoots2, SharedLiterals._metaCreatedAtF1242d44, SharedLiterals._metaNextCursorTasks2, SharedLiterals._metaTask3>>>) -> unit
        abstract assertCapability: capability: LiteralUnions.CompletionsExperimentalExtA1abd3ff * method: string -> unit
        abstract connect: transport: Transport * ?options: RequestOptions -> Promise<unit>
        abstract getServerCapabilities: unit -> option<TypesJs.ServerCapabilities>
        abstract getServerVersion: unit -> option<TypesJs.Implementation>
        abstract getInstructions: unit -> option<string>
        abstract assertCapabilityForMethod: method: proptypekey<'RequestT, string> -> unit
        abstract assertNotificationCapability: method: proptypekey<'NotificationT, string> -> unit
        abstract assertRequestHandlerCapability: method: string -> unit
        abstract assertTaskCapability: method: string -> unit
        abstract assertTaskHandlerCapability: method: string -> unit
        abstract ping: ?options: RequestOptions -> Promise<SharedLiterals._meta4>
        abstract complete: params: SharedLiterals._metaArgumentContextRef2 * ?options: RequestOptions -> Promise<Client.Complete>
        abstract setLoggingLevel: level: LoggingLevel * ?options: RequestOptions -> Promise<SharedLiterals._meta4>
        abstract getPrompt: params: SharedLiterals._metaArgumentsName2 * ?options: RequestOptions -> Promise<SharedLiterals._metaDescriptionMessages>
        abstract listPrompts: ?params: SharedLiterals._metaCursor * ?options: RequestOptions -> Promise<Client.ListPrompts>
        abstract listResources: ?params: SharedLiterals._metaCursor * ?options: RequestOptions -> Promise<Client.ListResources>
        abstract listResourceTemplates: ?params: SharedLiterals._metaCursor * ?options: RequestOptions -> Promise<Client.ListResourceTemplates>
        abstract readResource: params: SharedLiterals._metaUri * ?options: RequestOptions -> Promise<SharedLiterals._metaContents>
        abstract subscribeResource: params: SharedLiterals._metaUri * ?options: RequestOptions -> Promise<SharedLiterals._meta4>
        abstract unsubscribeResource: params: SharedLiterals._metaUri * ?options: RequestOptions -> Promise<SharedLiterals._meta4>
        abstract callTool: params: SharedLiterals._metaArgumentsNameTask * ?resultSchema: U2<Zod.ZodType, Zod.ZodType> * ?options: RequestOptions -> Promise<U2<SharedLiterals._metaContentIsErrorStructuredContent, SharedLiterals._metaToolResult>>
        abstract listTools: ?params: SharedLiterals._metaCursor * ?options: RequestOptions -> Promise<Client.ListTools>
        abstract sendRootsListChanged: unit -> Promise<unit>

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "RegisteredResource")>]
    type RegisteredResource =
        abstract enabled: bool with get, set
        abstract metadata: option<ResourceMetadata> with get, set
        abstract title: option<string> with get, set
        abstract name: string with get, set
        abstract readCallback: uri: CloudflareWorkersTypes.URL * extra: SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>
        abstract enable: unit -> unit
        abstract disable: unit -> unit
        abstract update: updates: SharedLiterals.CallbackEnabledMetadataNameTitleUri -> unit
        abstract remove: unit -> unit

    [<Import("../experimental/tasks/interfaces.js", "TaskRequestHandlerExtra")>]
    type TaskRequestHandlerExtra =
        inherit SharedLiterals._metaAuthInfo0d6b92d73
        abstract taskStore: RequestTaskStore with get, set
        abstract taskId: string with get, set

    type ReadResourceTemplateCallback = CloudflareWorkersTypes.URL -> Variables -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>

    [<Import("@modelcontextprotocol/sdk/types.js", "CreateMessageRequestParamsBase")>]
    type CreateMessageRequestParamsBase = interface end

    [<Import("../shared/uriTemplate.js", "Variables")>]
    type Variables = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "ContentBlock")>]
    type ContentBlock = interface end

    type ExtractWWWAuthenticateParams =
        abstract error: option<string> with get, set
        abstract scope: option<string> with get, set
        abstract resourceMetadataUrl: option<CloudflareWorkersTypes.URL> with get, set

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "ResourceMetadata")>]
    type ResourceMetadata = interface end

    [<Import("../validation/types.js", "JsonSchemaType")>]
    type JsonSchemaType =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<Erased.JsonSchemaTyped> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<SharedLiterals.AnchorCommentDefs0800bb6e, ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract multipleOf: option<float> with get, set
        abstract minProperties: option<float> with get, set
        abstract minLength: option<float> with get, set
        abstract minItems: option<float> with get, set
        abstract minimum: option<float> with get, set
        abstract minContains: option<float> with get, set
        abstract maxProperties: option<float> with get, set
        abstract maxLength: option<float> with get, set
        abstract maxItems: option<float> with get, set
        abstract maximum: option<float> with get, set
        abstract maxContains: option<float> with get, set
        abstract items: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<option<obj>>, System.Collections.Generic.IReadOnlyList<option<obj>>>> with get, set
        abstract enum: option<U2<ResizeArray<option<obj>>, System.Collections.Generic.IReadOnlyList<option<obj>>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<obj> with get, set

        abstract contentSchema: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<obj> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>>> with get, set
        abstract additionalProperties: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("$vocabulary")>]
        abstract ``$vocabulary``: option<obj> with get, set

        [<EmitProperty("$schema")>]
        abstract ``$schema``: option<string> with get, set

        [<EmitProperty("$ref")>]
        abstract ``$ref``: option<string> with get, set

        [<EmitProperty("$id")>]
        abstract ``$id``: option<string> with get, set

        [<EmitProperty("$dynamicRef")>]
        abstract ``$dynamicRef``: option<string> with get, set

        [<EmitProperty("$dynamicAnchor")>]
        abstract ``$dynamicAnchor``: option<string> with get, set

        [<EmitProperty("$defs")>]
        abstract ``$defs``: option<obj> with get, set

        [<EmitProperty("$comment")>]
        abstract ``$comment``: option<string> with get, set

        [<EmitProperty("$anchor")>]
        abstract ``$anchor``: option<string> with get, set

    type FetchToken =
        abstract authorizationCode: option<string> with get, set
        abstract resource: option<CloudflareWorkersTypes.URL> with get, set
        abstract metadata: option<AuthorizationServerMetadata> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("../experimental/tasks/interfaces.js", "QueuedError")>]
    type QueuedError =
        inherit BaseQueuedMessage
        abstract message: TypesJs.JSONRPCError with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ListChangedCallback = option<exn> -> option<ResizeArray<obj>> -> unit

    [<Import("../experimental/tasks/interfaces.js", "TaskStore")>]
    type TaskStore =
        abstract createTask: taskParams: CreateTaskOptions * requestId: ProgressToken * request: TypesJs.Request * ?sessionId: string -> Promise<TypesJs.Task>
        abstract getTask: taskId: string * ?sessionId: string -> Promise<option<TypesJs.Task>>
        abstract storeTaskResult: taskId: string * status: LiteralUnions.CompletedFailed * result: TypesJs.Result * ?sessionId: string -> Promise<unit>
        abstract getTaskResult: taskId: string * ?sessionId: string -> Promise<TypesJs.Result>
        abstract updateTaskStatus: taskId: string * status: TaskStatus * ?statusMessage: string * ?sessionId: string -> Promise<unit>
        abstract listTasks: ?cursor: string * ?sessionId: string -> Promise<SharedLiterals.NextCursorTasks>

    [<Import("../experimental/tasks/interfaces.js", "CreateTaskOptions")>]
    type CreateTaskOptions =
        abstract context: option<obj> with get, set
        abstract pollInterval: option<float> with get, set
        abstract ttl: option<float> with get, set

    [<Import("@modelcontextprotocol/sdk/types.js", "MultiSelectEnumSchema")>]
    type MultiSelectEnumSchema = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "JSONRPCResponse")>]
    type JSONRPCResponse = interface end

    [<Import("@modelcontextprotocol/sdk/client/streamableHttp.js", "StreamableHTTPError")>]
    type StreamableHTTPError =
        [<EmitConstructor>]
        abstract Create: ?code: float * ?message: string -> StreamableHTTPError

        abstract code: option<float> with get

    [<Import("@modelcontextprotocol/sdk/types.js", "ClientNotification")>]
    type ClientNotification = interface end

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "RegisteredResourceTemplate")>]
    type RegisteredResourceTemplate =
        abstract enabled: bool with get, set
        abstract metadata: option<ResourceMetadata> with get, set
        abstract title: option<string> with get, set
        abstract resourceTemplate: ResourceTemplate with get, set
        abstract readCallback: uri: CloudflareWorkersTypes.URL * variables: Variables * extra: SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>
        abstract enable: unit -> unit
        abstract disable: unit -> unit
        abstract update: updates: SharedLiterals.CallbackEnabledMetadata8b2e58c4 -> unit
        abstract remove: unit -> unit

    [<Import("@modelcontextprotocol/sdk/server/auth/types.js", "AuthInfo")>]
    type AuthInfo =
        abstract extra: option<obj> with get, set
        abstract resource: option<CloudflareWorkersTypes.URL> with get, set
        abstract expiresAt: option<float> with get, set
        abstract scopes: ResizeArray<string> with get, set
        abstract clientId: string with get, set
        abstract token: string with get, set

    [<Import("@modelcontextprotocol/sdk/types.js", "ClientResult")>]
    type ClientResult = interface end

    type Auth =
        abstract resourceMetadataUrl: option<CloudflareWorkersTypes.URL> with get, set
        abstract scope: option<string> with get, set
        abstract authorizationCode: option<string> with get, set
        abstract serverUrl: U2<CloudflareWorkersTypes.URL, string> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/client/sse.js", "SseError")>]
    type SseError =
        [<EmitConstructor>]
        abstract Create: ?code: float * ?message: string * event: Erased.Eventsource -> SseError

        abstract event: Erased.Eventsource with get
        abstract code: option<float> with get

    [<Import("@modelcontextprotocol/sdk/server/webStandardStreamableHttp.js", "HandleRequestOptions")>]
    type HandleRequestOptions =
        abstract authInfo: option<AuthInfo> with get, set
        abstract parsedBody: option<obj> with get, set

    type ExchangeAuthorization =
        abstract addClientAuthentication: option<AddClientAuthentication> with get, set
        abstract resource: option<CloudflareWorkersTypes.URL> with get, set
        abstract redirectUri: U2<CloudflareWorkersTypes.URL, string> with get, set
        abstract codeVerifier: string with get, set
        abstract authorizationCode: string with get, set
        abstract clientInformation: OAuthClientInformationMixed with get, set
        abstract metadata: option<AuthorizationServerMetadata> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    type GetSupportedElicitationModes =
        abstract supportsUrlMode: bool with get, set
        abstract supportsFormMode: bool with get, set

    [<Import("../experimental/tasks/interfaces.js", "CreateTaskRequestHandlerExtra")>]
    type CreateTaskRequestHandlerExtra =
        inherit SharedLiterals._metaAuthInfo0d6b92d73
        abstract taskStore: RequestTaskStore with get, set

    [<Import("@modelcontextprotocol/sdk/client/sse.js", "SSEClientTransport")>]
    type SSEClientTransport =
        [<EmitConstructor>]
        abstract Create: url: CloudflareWorkersTypes.URL * ?opts: SSEClientTransportOptions -> SSEClientTransport

        abstract _startOrAuth: option<obj> with get, set
        abstract _commonHeaders: option<obj> with get, set
        abstract _authThenStart: option<obj> with get, set
        abstract _protocolVersion: option<obj> with get, set
        abstract _fetchWithInit: option<obj> with get, set
        abstract _fetch: option<obj> with get, set
        abstract _authProvider: option<obj> with get, set
        abstract _requestInit: option<obj> with get, set
        abstract _eventSourceInit: option<obj> with get, set
        abstract _scope: option<obj> with get, set
        abstract _resourceMetadataUrl: option<obj> with get, set
        abstract _url: option<obj> with get, set
        abstract _abortController: option<obj> with get, set
        abstract _endpoint: option<obj> with get, set
        abstract _eventSource: option<obj> with get, set
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract onmessage: message: JSONRPCMessage -> unit
        abstract start: unit -> Promise<unit>
        abstract finishAuth: authorizationCode: string -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract send: message: JSONRPCMessage -> Promise<unit>
        abstract setProtocolVersion: version: string -> unit

    [<Import("@modelcontextprotocol/sdk/client/streamableHttp.js", "StreamableHTTPClientTransport")>]
    type StreamableHTTPClientTransport =
        [<EmitConstructor>]
        abstract Create: url: CloudflareWorkersTypes.URL * ?opts: StreamableHTTPClientTransportOptions -> StreamableHTTPClientTransport

        abstract protocolVersion: option<string> with get
        abstract sessionId: option<string> with get
        abstract _handleSseStream: option<obj> with get, set
        /// <param name="lastEventId" >
        /// The ID of the last received event for resumability
        /// </param>
        /// <param name="attemptCount" >
        /// Current reconnection attempt count for this specific stream
        /// </param>
        abstract _scheduleReconnection: option<obj> with get, set
        /// <param name="attempt" >
        /// Current reconnection attempt count for the specific stream
        /// </param>
        abstract _getNextReconnectionDelay: option<obj> with get, set
        abstract _startOrAuthSse: option<obj> with get, set
        abstract _commonHeaders: option<obj> with get, set
        abstract _authThenStart: option<obj> with get, set
        abstract _reconnectionTimeout: option<obj> with get, set
        abstract _serverRetryMs: option<obj> with get, set
        abstract _lastUpscopingHeader: option<obj> with get, set
        abstract _hasCompletedAuthFlow: option<obj> with get, set
        abstract _protocolVersion: option<obj> with get, set
        abstract _reconnectionOptions: option<obj> with get, set
        abstract _sessionId: option<obj> with get, set
        abstract _fetchWithInit: option<obj> with get, set
        abstract _fetch: option<obj> with get, set
        abstract _authProvider: option<obj> with get, set
        abstract _requestInit: option<obj> with get, set
        abstract _scope: option<obj> with get, set
        abstract _resourceMetadataUrl: option<obj> with get, set
        abstract _url: option<obj> with get, set
        abstract _abortController: option<obj> with get, set
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract onmessage: message: JSONRPCMessage -> unit
        abstract start: unit -> Promise<unit>
        abstract finishAuth: authorizationCode: string -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract send: message: U5<SharedLiterals.IdJsonrpcMethodParams, SharedLiterals.JsonrpcMethodParams, SharedLiterals.IdJsonrpcResult, TypesJs.JSONRPCError, ResizeArray<JSONRPCMessage>> * ?options: StreamableHTTPClientTransport.Send.Options -> Promise<unit>
        abstract terminateSession: unit -> Promise<unit>
        abstract setProtocolVersion: version: string -> unit
        abstract resumeStream: lastEventId: string * ?options: StreamableHTTPClientTransport.ResumeStream.Options -> Promise<unit>

    [<Import("@modelcontextprotocol/sdk/client/streamableHttp.js", "StreamableHTTPClientTransportOptions")>]
    type StreamableHTTPClientTransportOptions =
        abstract sessionId: option<string> with get, set
        abstract reconnectionOptions: option<StreamableHTTPReconnectionOptions> with get, set
        abstract requestInit: option<CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>> with get, set
        abstract authProvider: option<OAuthClientProvider> with get, set
        abstract fetch: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/types.js", "ServerResult")>]
    type ServerResult = interface end

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "OAuthServerInfo")>]
    type OAuthServerInfo =
        abstract resourceMetadata: option<Shared.AuthJs.OAuthProtectedResourceMetadata> with get, set
        abstract authorizationServerMetadata: option<AuthorizationServerMetadata> with get, set
        abstract authorizationServerUrl: string with get, set

    [<Import("../experimental/tasks/interfaces.js", "CreateTaskRequestHandler")>]
    type CreateTaskRequestHandler<'SendResultT, 'Args> = interface end

    type AnyToolHandler<'Args> = U4<SharedLiterals._def_e995c0ec<'Args, obj, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>, ToolTaskHandler<'Args>>

    [<Import("@modelcontextprotocol/sdk/types.js", "ClientRequest")>]
    type ClientRequest = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "ProgressToken")>]
    type ProgressToken = interface end

    type ProgressCallback = TypesJs.Progress -> unit

    [<Import("@modelcontextprotocol/sdk/client/streamableHttp.js", "StartSSEOptions")>]
    type StartSSEOptions =
        abstract replayMessageId: option<Zod.ZodType> with get, set
        abstract resumptionToken: option<string> with get, set
        abstract onresumptiontoken: token: string -> unit

    [<Import("@modelcontextprotocol/sdk/types.js", "ServerRequest")>]
    type ServerRequest = interface end

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "RegisteredPrompt")>]
    type RegisteredPrompt =
        abstract enabled: bool with get, set
        abstract callback: U2<SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>, obj -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>> with get, set
        abstract argsSchema: option<AnyObjectSchema> with get, set
        abstract description: option<string> with get, set
        abstract title: option<string> with get, set
        abstract enable: unit -> unit
        abstract disable: unit -> unit
        abstract update: updates: SharedLiterals.ArgsSchemaCallbackDescript5bdc466b<obj> -> unit
        abstract remove: unit -> unit

    [<Import("../../shared/responseMessage.js", "BaseResponseMessage")>]
    type BaseResponseMessage =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("../experimental/tasks/interfaces.js", "TaskToolExecution")>]
    type TaskToolExecution =
        abstract taskSupport: option<obj> with get, set
        abstract Item: key: string -> option<obj>

    type ResponseMessage<'T> = U4<TaskStatusMessage, TaskCreatedMessage, ResultMessage<'T>, ErrorMessage>

    [<Import("../experimental/tasks/interfaces.js", "BaseQueuedMessage")>]
    type BaseQueuedMessage =
        abstract timestamp: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type QueuedMessage = U4<QueuedRequest, QueuedNotification, QueuedResponse, QueuedError>

    [<Import("../experimental/tasks/interfaces.js", "TaskRequestHandler")>]
    type TaskRequestHandler<'SendResultT, 'Args> = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "CreateMessageRequestParamsWithTools")>]
    type CreateMessageRequestParamsWithTools =
        inherit TypesJs.CreateMessageRequestParams
        abstract tools: ResizeArray<TypesJs.Tool> with get, set

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "RequestHandlerExtra")>]
    type RequestHandlerExtra =
        abstract requestInfo: option<RequestInfo> with get, set
        abstract taskRequestedTtl: option<float> with get, set
        abstract taskStore: option<RequestTaskStore> with get, set
        abstract taskId: option<string> with get, set
        abstract requestId: ProgressToken with get, set
        abstract _meta: option<TypesJs.RequestMeta> with get, set
        abstract sessionId: option<string> with get, set
        abstract authInfo: option<AuthInfo> with get, set
        abstract signal: CloudflareWorkersTypes.AbortSignal with get, set
        abstract sendNotification: notification: obj -> Promise<unit>
        abstract sendRequest: request: obj * resultSchema: obj * ?options: TaskRequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
        abstract closeSSEStream: unit -> unit
        abstract closeStandaloneSSEStream: unit -> unit

    type DiscoverOAuthProtectedResourceMetadata =
        abstract resourceMetadataUrl: option<U2<CloudflareWorkersTypes.URL, string>> with get, set
        abstract protocolVersion: option<string> with get, set

    type DiscoverOAuthServerInfo =
        abstract resourceMetadataUrl: option<CloudflareWorkersTypes.URL> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/types.js", "TaskStatus")>]
    type TaskStatus = interface end

    [<Import("@modelcontextprotocol/sdk/shared/transport.js", "Transport")>]
    type Transport =
        abstract sessionId: option<string> with get, set
        abstract start: unit -> Promise<unit>
        abstract send: message: JSONRPCMessage * ?options: TransportSendOptions -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract onmessage: message: obj * ?extra: MessageExtraInfo -> unit
        abstract setProtocolVersion: version: string -> unit

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "PromptCallback")>]
    type PromptCallback<'Args> = interface end

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "RequestTaskStore")>]
    type RequestTaskStore =
        abstract createTask: taskParams: CreateTaskOptions -> Promise<TypesJs.Task>
        abstract getTask: taskId: string -> Promise<TypesJs.Task>
        abstract storeTaskResult: taskId: string * status: LiteralUnions.CompletedFailed * result: TypesJs.Result -> Promise<unit>
        abstract getTaskResult: taskId: string -> Promise<TypesJs.Result>
        abstract updateTaskStatus: taskId: string * status: TaskStatus * ?statusMessage: string -> Promise<unit>
        abstract listTasks: ?cursor: string -> Promise<SharedLiterals.NextCursorTasks>

    [<Import("../experimental/tasks/mcp-server.js", "ExperimentalMcpServerTasks")>]
    type ExperimentalMcpServerTasks =
        [<EmitConstructor>]
        abstract Create: _mcpServer: McpServer -> ExperimentalMcpServerTasks

        abstract _mcpServer: option<obj> with get
        abstract registerToolTask: name: string * config: ExperimentalMcpServerTasks.RegisterToolTask.Config * handler: ToolTaskHandler<unit> -> RegisteredTool
        abstract registerToolTask<'InputArgs, 'OutputArgs> : name: string * config: ExperimentalMcpServerTasks.RegisterToolTask.Config.Case2 * handler: ToolTaskHandler<'InputArgs> -> RegisteredTool

    type ErrorCode =
        | ConnectionClosed = -32000
        | RequestTimeout = -32001
        | ParseError = -32700
        | InvalidRequest = -32600
        | MethodNotFound = -32601
        | InvalidParams = -32602
        | InternalError = -32603
        | UrlElicitationRequired = -32042

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "ProtocolOptions")>]
    type ProtocolOptions =
        abstract maxTaskQueueSize: option<float> with get, set
        abstract defaultTaskPollInterval: option<float> with get, set
        abstract taskMessageQueue: option<TaskMessageQueue> with get, set
        abstract taskStore: option<TaskStore> with get, set
        abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
        abstract enforceStrictCapabilities: option<bool> with get, set

    [<Import("@modelcontextprotocol/sdk/server/streamableHttp.js", "StreamableHTTPServerTransport")>]
    type StreamableHTTPServerTransport =
        [<EmitConstructor>]
        abstract Create: ?options: WebStandardStreamableHTTPServerTransportOptions -> StreamableHTTPServerTransport

        abstract onmessage: (option<JSONRPCMessage -> option<MessageExtraInfo> -> unit>) with set
        abstract onerror: (option<exn -> unit>) with set
        abstract onclose: (option<unit -> unit>) with set
        abstract sessionId: option<string> with get
        abstract _requestContext: option<obj> with get, set
        abstract _requestListener: option<obj> with get, set
        abstract _webStandardTransport: option<obj> with get, set
        abstract start: unit -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract send: message: JSONRPCMessage * ?options: SharedLiterals.RelatedRequestId -> Promise<unit>
        abstract handleRequest: req: obj * ?res: obj * ?parsedBody: obj -> Promise<unit>
        abstract closeSSEStream: requestId: ProgressToken -> unit
        abstract closeStandaloneSSEStream: unit -> unit

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "UnauthorizedError")>]
    type UnauthorizedError =
        interface
            [<EmitConstructor>]
            abstract Create: ?message: string -> UnauthorizedError
        end

    type ListResourcesCallback = SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ListResourcesResult, Promise<TypesJs.ListResourcesResult>>

    [<Import("@modelcontextprotocol/sdk/client/index.js", "ClientOptions")>]
    type ClientOptions =
        /// <example>
        /// ```typescript<br/>
        /// const client = new Client(<br/>
        ///   { name: 'my-client', version: '1.0.0' },<br/>
        ///   {<br/>
        ///     listChanged: {<br/>
        ///       tools: {<br/>
        ///         onChanged: (error, tools) => {<br/>
        ///           if (error) {<br/>
        ///             console.error('Failed to refresh tools:', error);<br/>
        ///             return;<br/>
        ///           }<br/>
        ///           console.log('Tools updated:', tools);<br/>
        ///         }<br/>
        ///       },<br/>
        ///       prompts: {<br/>
        ///         onChanged: (error, prompts) => console.log('Prompts updated:', prompts)<br/>
        ///       }<br/>
        ///     }<br/>
        ///   }<br/>
        /// );<br/>
        /// ```
        /// </example>
        abstract listChanged: option<ListChangedHandlers> with get, set
        /// <example>
        /// ```typescript<br/>
        /// // ajv<br/>
        /// const client = new Client(<br/>
        ///   { name: 'my-client', version: '1.0.0' },<br/>
        ///   {<br/>
        ///     capabilities: {},<br/>
        ///     jsonSchemaValidator: new AjvJsonSchemaValidator()<br/>
        ///   }<br/>
        /// );<br/>
        /// <br/>
        /// //
        /// </example>
        abstract jsonSchemaValidator: option<JsonSchemaValidator> with get, set
        abstract capabilities: option<TypesJs.ClientCapabilities> with get, set
        abstract maxTaskQueueSize: option<float> with get, set
        abstract defaultTaskPollInterval: option<float> with get, set
        abstract taskMessageQueue: option<TaskMessageQueue> with get, set
        abstract taskStore: option<TaskStore> with get, set
        abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
        abstract enforceStrictCapabilities: option<bool> with get, set

    [<Import("@modelcontextprotocol/sdk/server/index.js", "Server")>]
    type Server<'RequestT, 'NotificationT, 'ResultT> =
        [<EmitConstructor>]
        abstract Create: _serverInfo: TypesJs.Implementation * ?options: ServerOptions -> Server<'RequestT, 'NotificationT, 'ResultT>

        abstract getCapabilities: option<obj> with get, set
        abstract _oninitialize: option<obj> with get, set
        abstract isMessageIgnored: option<obj> with get, set
        abstract LOG_LEVEL_SEVERITY: option<obj> with get
        abstract _loggingLevels: option<obj> with get, set
        abstract experimental: Server.Experimental with get
        abstract _experimental: option<obj> with get, set
        abstract _jsonSchemaValidator: option<obj> with get, set
        abstract _instructions: option<obj> with get, set
        abstract _capabilities: option<obj> with get, set
        abstract _clientVersion: option<obj> with get, set
        abstract _clientCapabilities: option<obj> with get, set
        abstract _serverInfo: option<obj> with get, set
        abstract oninitialized: unit -> unit
        abstract registerCapabilities: capabilities: TypesJs.ServerCapabilities -> unit
        abstract setRequestHandler: requestSchema: obj * handler: (option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> SharedLiterals._metaAuthInfo0d6b92d73 -> U15<SharedLiterals._meta2, SharedLiterals._metaCapabilitiesA112f1992, SharedLiterals._metaCompletion2, SharedLiterals._metaDescriptionMessages4, SharedLiterals._metaNextCursorPrompts2, SharedLiterals._metaNextCursorResources3, SharedLiterals._metaNextCursorResourceTemplates2, SharedLiterals._metaContents4, SharedLiterals._metaContentIsErrorStructuredContent4, SharedLiterals._metaNextCursorTools2, SharedLiterals._metaCreatedAtF1242d44, SharedLiterals._metaNextCursorTasks2, SharedLiterals._metaTask3, TypesJs.Result, Promise<U14<TypesJs.Result, SharedLiterals._meta2, SharedLiterals._metaCapabilitiesA112f1992, SharedLiterals._metaCompletion2, SharedLiterals._metaDescriptionMessages4, SharedLiterals._metaNextCursorPrompts2, SharedLiterals._metaNextCursorResources3, SharedLiterals._metaNextCursorResourceTemplates2, SharedLiterals._metaContents4, SharedLiterals._metaContentIsErrorStructuredContent4, SharedLiterals._metaNextCursorTools2, SharedLiterals._metaCreatedAtF1242d44, SharedLiterals._metaNextCursorTasks2, SharedLiterals._metaTask3>>>) -> unit
        abstract assertCapabilityForMethod: method: proptypekey<'RequestT, string> -> unit
        abstract assertNotificationCapability: method: proptypekey<U10<SharedLiterals.MethodParams13, SharedLiterals.MethodParams14, SharedLiterals.MethodParams15, SharedLiterals.MethodParams16, SharedLiterals.MethodParams17, SharedLiterals.MethodParams18, SharedLiterals.MethodParams19, SharedLiterals.MethodParams20, SharedLiterals.MethodParams21, TypesJs.Notification>, string> -> unit
        abstract assertRequestHandlerCapability: method: string -> unit
        abstract assertTaskCapability: method: string -> unit
        abstract assertTaskHandlerCapability: method: string -> unit
        abstract getClientCapabilities: unit -> option<TypesJs.ClientCapabilities>
        abstract getClientVersion: unit -> option<TypesJs.Implementation>
        abstract ping: unit -> Promise<SharedLiterals._meta4>
        abstract createMessage: params: CreateMessageRequestParamsBase * ?options: RequestOptions -> Promise<TypesJs.CreateMessageResult>
        abstract createMessage: params: CreateMessageRequestParamsWithTools * ?options: RequestOptions -> Promise<TypesJs.CreateMessageResultWithTools>
        abstract createMessage: params: SharedLiterals._metaIncludeContextE2cd0689 * ?options: RequestOptions -> Promise<U2<TypesJs.CreateMessageResult, TypesJs.CreateMessageResultWithTools>>
        abstract elicitInput: params: U2<TypesJs.ElicitRequestFormParams, TypesJs.ElicitRequestURLParams> * ?options: RequestOptions -> Promise<obj>
        abstract createElicitationCompletionNotifier: elicitationId: string * ?options: NotificationOptions -> (unit -> Promise<unit>)
        abstract listRoots: ?params: SharedLiterals._meta2 * ?options: RequestOptions -> Promise<Server.ListRoots>
        abstract sendLoggingMessage: params: SharedLiterals._metaDataLevelLogger * ?sessionId: string -> Promise<unit>
        abstract sendResourceUpdated: params: SharedLiterals._metaUri -> Promise<unit>
        abstract sendResourceListChanged: unit -> Promise<unit>
        abstract sendToolListChanged: unit -> Promise<unit>
        abstract sendPromptListChanged: unit -> Promise<unit>

    [<Import("../../shared/responseMessage.js", "TaskCreatedMessage")>]
    type TaskCreatedMessage =
        inherit BaseResponseMessage
        abstract task: TypesJs.Task with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "OAuthDiscoveryState")>]
    type OAuthDiscoveryState =
        inherit OAuthServerInfo
        abstract resourceMetadataUrl: option<string> with get, set

    type JsonSchemaValidator = option<obj> -> JsonSchemaValidatorResult<obj>

    [<Import("@modelcontextprotocol/sdk/client/auth.js", "OAuthClientProvider")>]
    type OAuthClientProvider =
        abstract clientMetadata: Shared.AuthJs.OAuthClientMetadata with get
        abstract clientMetadataUrl: option<string> with get, set
        abstract redirectUrl: option<U2<CloudflareWorkersTypes.URL, string>> with get
        abstract state: unit -> U2<Promise<string>, string>
        abstract clientInformation: unit -> option<U3<Shared.AuthJs.OAuthClientInformationFull, Shared.AuthJs.OAuthClientInformation, Promise<option<U2<Shared.AuthJs.OAuthClientInformationFull, Shared.AuthJs.OAuthClientInformation>>>>>
        abstract saveClientInformation: clientInformation: OAuthClientInformationMixed -> option<Promise<unit>>
        abstract tokens: unit -> option<U2<Shared.AuthJs.OAuthTokens, Promise<option<Shared.AuthJs.OAuthTokens>>>>
        abstract saveTokens: tokens: Shared.AuthJs.OAuthTokens -> option<Promise<unit>>
        abstract redirectToAuthorization: authorizationUrl: CloudflareWorkersTypes.URL -> option<Promise<unit>>
        abstract saveCodeVerifier: codeVerifier: string -> option<Promise<unit>>
        abstract codeVerifier: unit -> U2<Promise<string>, string>
        abstract addClientAuthentication: headers: CloudflareWorkersTypes.Headers * params: CloudflareWorkersTypes.URLSearchParams * url: U2<CloudflareWorkersTypes.URL, string> * ?metadata: AuthorizationServerMetadata -> option<Promise<unit>>
        abstract validateResourceURL: serverUrl: U2<CloudflareWorkersTypes.URL, string> * ?resource: string -> Promise<option<CloudflareWorkersTypes.URL>>
        abstract invalidateCredentials: scope: LiteralUnions.AllClientDiscoveryTokensVerifier -> option<Promise<unit>>
        abstract prepareTokenRequest: ?scope: string -> option<U2<CloudflareWorkersTypes.URLSearchParams, Promise<option<CloudflareWorkersTypes.URLSearchParams>>>>
        abstract saveDiscoveryState: state: OAuthDiscoveryState -> option<Promise<unit>>
        abstract discoveryState: unit -> option<U2<OAuthDiscoveryState, Promise<option<OAuthDiscoveryState>>>>

    [<Import("@modelcontextprotocol/sdk/types.js", "Role")>]
    type Role = interface end

    [<Import("../server/auth/errors.js", "OAuthError")>]
    type OAuthError =
        [<EmitConstructor>]
        abstract Create: message: string * ?errorUri: string -> OAuthError

        abstract errorCode: string with get, set
        abstract errorUri: option<string> with get
        abstract toResponseObject: unit -> Shared.AuthJs.OAuthErrorResponse

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "McpServer")>]
    type McpServer =
        [<EmitConstructor>]
        abstract Create: serverInfo: TypesJs.Implementation * ?options: ServerOptions -> McpServer

        abstract _createRegisteredTool: option<obj> with get, set
        abstract _createRegisteredPrompt: option<obj> with get, set
        abstract _createRegisteredResourceTemplate: option<obj> with get, set
        abstract _createRegisteredResource: option<obj> with get, set
        abstract setPromptRequestHandlers: option<obj> with get, set
        abstract _promptHandlersInitialized: option<obj> with get, set
        abstract setResourceRequestHandlers: option<obj> with get, set
        abstract _resourceHandlersInitialized: option<obj> with get, set
        abstract handleResourceCompletion: option<obj> with get, set
        abstract handlePromptCompletion: option<obj> with get, set
        abstract setCompletionRequestHandler: option<obj> with get, set
        abstract _completionHandlerInitialized: option<obj> with get, set
        abstract handleAutomaticTaskPolling: option<obj> with get, set
        abstract executeToolHandler: option<obj> with get, set
        abstract validateToolOutput: option<obj> with get, set
        abstract validateToolInput: option<obj> with get, set
        /// <param name="errorMessage" >
        /// - The error message.
        /// </param>
        abstract createToolError: option<obj> with get, set
        abstract setToolRequestHandlers: option<obj> with get, set
        abstract _toolHandlersInitialized: option<obj> with get, set
        abstract experimental: McpServer.Experimental with get
        abstract _experimental: option<obj> with get, set
        abstract _registeredPrompts: option<obj> with get, set
        abstract _registeredTools: option<obj> with get, set
        abstract _registeredResourceTemplates: option<obj> with get, set
        abstract _registeredResources: option<obj> with get, set
        abstract server: Server<TypesJs.Request, TypesJs.Notification, TypesJs.Result> with get
        abstract connect: transport: Transport -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract resource: name: string * uri: string * readCallback: ReadResourceCallback -> RegisteredResource
        abstract resource: name: string * uri: string * metadata: ResourceMetadata * readCallback: ReadResourceCallback -> RegisteredResource
        abstract resource: name: string * template: ResourceTemplate * readCallback: ReadResourceTemplateCallback -> RegisteredResourceTemplate
        abstract resource: name: string * template: ResourceTemplate * metadata: ResourceMetadata * readCallback: ReadResourceTemplateCallback -> RegisteredResourceTemplate
        abstract registerResource: name: string * uriOrTemplate: string * config: ResourceMetadata * readCallback: ReadResourceCallback -> RegisteredResource
        abstract registerResource: name: string * uriOrTemplate: ResourceTemplate * config: ResourceMetadata * readCallback: ReadResourceTemplateCallback -> RegisteredResourceTemplate
        abstract tool: name: string * cb: (obj -> U2<TypesJs.CallToolResult, Promise<TypesJs.CallToolResult>>) -> RegisteredTool
        abstract tool: name: string * description: string * cb: (obj -> U2<TypesJs.CallToolResult, Promise<TypesJs.CallToolResult>>) -> RegisteredTool
        abstract tool: name: string * paramsSchemaOrAnnotations: U2<ZodRawShapeCompat, TypesJs.ToolAnnotations> * cb: U3<SharedLiterals._def_e995c0ec<obj, obj, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>> -> RegisteredTool
        abstract tool: name: string * description: string * paramsSchemaOrAnnotations: U2<ZodRawShapeCompat, TypesJs.ToolAnnotations> * cb: U3<SharedLiterals._def_e995c0ec<obj, obj, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>> -> RegisteredTool
        abstract tool: name: string * paramsSchema: obj * annotations: TypesJs.ToolAnnotations * cb: U3<SharedLiterals._def_e995c0ec<obj, obj, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>> -> RegisteredTool
        abstract tool: name: string * description: string * paramsSchema: obj * annotations: TypesJs.ToolAnnotations * cb: U3<SharedLiterals._def_e995c0ec<obj, obj, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>> -> RegisteredTool
        abstract registerTool<'OutputArgs, 'InputArgs> : name: string * config: McpServer.RegisterTool.Config * cb: U3<SharedLiterals._def_e995c0ec<obj, obj, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>> -> RegisteredTool
        abstract prompt: name: string * cb: (SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>) -> RegisteredPrompt
        abstract prompt: name: string * description: string * cb: (SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>) -> RegisteredPrompt
        abstract prompt: name: string * argsSchema: obj * cb: U2<obj -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>, SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>> -> RegisteredPrompt
        abstract prompt: name: string * description: string * argsSchema: obj * cb: U2<obj -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>, SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>> -> RegisteredPrompt
        abstract registerPrompt: name: string * config: McpServer.RegisterPrompt.Config * cb: U2<obj -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>, SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>> -> RegisteredPrompt
        abstract isConnected: unit -> bool
        abstract sendLoggingMessage: params: SharedLiterals._metaDataLevelLogger * ?sessionId: string -> Promise<unit>
        abstract sendResourceListChanged: unit -> unit
        abstract sendToolListChanged: unit -> unit
        abstract sendPromptListChanged: unit -> unit

    [<Import("@modelcontextprotocol/sdk/shared/transport.js", "FetchLike")>]
    type FetchLike =
        abstract Invoke: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("../experimental/tasks/client.js", "ExperimentalClientTasks")>]
    type ExperimentalClientTasks<'RequestT, 'NotificationT, 'ResultT> =
        [<EmitConstructor>]
        abstract Create: _client: Client<'RequestT, 'NotificationT, 'ResultT> -> ExperimentalClientTasks<'RequestT, 'NotificationT, 'ResultT>

        abstract _client: option<obj> with get
        abstract callToolStream<'T> : params: SharedLiterals._metaArgumentsNameTask * ?resultSchema: 'T * ?options: RequestOptions -> seq<U4<TaskStatusMessage, TaskCreatedMessage, ErrorMessage, ResultMessage<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>>>
        abstract getTask: taskId: string * ?options: RequestOptions -> Promise<TypesJs.GetTaskResult>
        abstract getTaskResult: taskId: string * ?resultSchema: obj * ?options: RequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
        abstract listTasks: ?cursor: string * ?options: RequestOptions -> Promise<TypesJs.ListTasksResult>
        abstract cancelTask: taskId: string * ?options: RequestOptions -> Promise<TypesJs.CancelTaskResult>
        abstract requestStream: request: U18<SharedLiterals.MethodParams5, SharedLiterals.MethodParams59, SharedLiterals.MethodParams62, SharedLiterals.MethodParams63, SharedLiterals.MethodParams64, SharedLiterals.MethodParams65, SharedLiterals.MethodParams66, SharedLiterals.MethodParams67, SharedLiterals.MethodParams68, SharedLiterals.MethodParams55, SharedLiterals.MethodParams56, SharedLiterals.MethodParams57, SharedLiterals.MethodParams58, SharedLiterals.MethodParams6, SharedLiterals.MethodParams7, SharedLiterals.MethodParams8, SharedLiterals.MethodParams9, TypesJs.Request> * resultSchema: obj * ?options: RequestOptions -> seq<U4<TaskStatusMessage, TaskCreatedMessage, ErrorMessage, ResultMessage<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>>>

    [<Import("@modelcontextprotocol/sdk/shared/transport.js", "TransportSendOptions")>]
    type TransportSendOptions =
        abstract resumptionToken: option<string> with get, set
        abstract relatedRequestId: option<ProgressToken> with get, set
        abstract onresumptiontoken: token: string -> unit

    [<Import("@modelcontextprotocol/sdk/server/webStandardStreamableHttp.js", "EventStore")>]
    type EventStore =
        abstract storeEvent: streamId: string * message: JSONRPCMessage -> Promise<string>
        abstract getStreamIdForEventId: eventId: string -> Promise<option<string>>
        abstract replayEventsAfter: lastEventId: string * send: SharedLiterals.Send -> Promise<string>

    [<Import("@modelcontextprotocol/sdk/types.js", "EnumSchema")>]
    type EnumSchema = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "CompatibilityCallToolResult")>]
    type CompatibilityCallToolResult = interface end

    type StartAuthorization =
        abstract resource: option<CloudflareWorkersTypes.URL> with get, set
        abstract state: option<string> with get, set
        abstract scope: option<string> with get, set
        abstract redirectUrl: U2<CloudflareWorkersTypes.URL, string> with get, set
        abstract clientInformation: OAuthClientInformationMixed with get, set
        abstract metadata: option<AuthorizationServerMetadata> with get, set

    [<Import("../experimental/tasks/interfaces.js", "QueuedResponse")>]
    type QueuedResponse =
        inherit BaseQueuedMessage
        abstract message: TypesJs.JSONRPCResultResponse with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@modelcontextprotocol/sdk/server/webStandardStreamableHttp.js", "WebStandardStreamableHTTPServerTransport")>]
    type WebStandardStreamableHTTPServerTransport =
        [<EmitConstructor>]
        abstract Create: ?options: WebStandardStreamableHTTPServerTransportOptions -> WebStandardStreamableHTTPServerTransport

        abstract validateProtocolVersion: option<obj> with get, set
        abstract validateSession: option<obj> with get, set
        abstract handleDeleteRequest: option<obj> with get, set
        abstract handlePostRequest: option<obj> with get, set
        abstract handleUnsupportedRequest: option<obj> with get, set
        abstract writeSSEEvent: option<obj> with get, set
        abstract replayEvents: option<obj> with get, set
        abstract handleGetRequest: option<obj> with get, set
        abstract writePrimingEvent: option<obj> with get, set
        abstract validateRequestHeaders: option<obj> with get, set
        abstract createJsonErrorResponse: option<obj> with get, set
        abstract sessionId: option<string> with get, set
        abstract _retryInterval: option<obj> with get, set
        abstract _enableDnsRebindingProtection: option<obj> with get, set
        abstract _allowedOrigins: option<obj> with get, set
        abstract _allowedHosts: option<obj> with get, set
        abstract _onsessionclosed: option<obj> with get, set
        abstract _onsessioninitialized: option<obj> with get, set
        abstract _eventStore: option<obj> with get, set
        abstract _standaloneSseStreamId: option<obj> with get, set
        abstract _enableJsonResponse: option<obj> with get, set
        abstract _initialized: option<obj> with get, set
        abstract _requestResponseMap: option<obj> with get, set
        abstract _requestToStreamMapping: option<obj> with get, set
        abstract _streamMapping: option<obj> with get, set
        abstract _hasHandledRequest: option<obj> with get, set
        abstract _started: option<obj> with get, set
        abstract sessionIdGenerator: option<obj> with get, set
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract onmessage: message: JSONRPCMessage * ?extra: MessageExtraInfo -> unit
        abstract start: unit -> Promise<unit>
        abstract handleRequest: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * ?options: HandleRequestOptions -> Promise<CloudflareWorkersTypes.Response>
        abstract close: unit -> Promise<unit>
        abstract closeSSEStream: requestId: ProgressToken -> unit
        abstract closeStandaloneSSEStream: unit -> unit
        abstract send: message: JSONRPCMessage * ?options: SharedLiterals.RelatedRequestId -> Promise<unit>

    type RefreshAuthorization =
        abstract addClientAuthentication: option<AddClientAuthentication> with get, set
        abstract resource: option<CloudflareWorkersTypes.URL> with get, set
        abstract refreshToken: string with get, set
        abstract clientInformation: OAuthClientInformationMixed with get, set
        abstract metadata: option<AuthorizationServerMetadata> with get, set
        abstract fetchFn: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/client/sse.js", "SSEClientTransportOptions")>]
    type SSEClientTransportOptions =
        abstract requestInit: option<CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>> with get, set
        abstract eventSourceInit: option<Erased.Eventsource> with get, set
        abstract authProvider: option<OAuthClientProvider> with get, set
        abstract fetch: url: U2<CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>

    [<Import("@modelcontextprotocol/sdk/server/webStandardStreamableHttp.js", "WebStandardStreamableHTTPServerTransportOptions")>]
    type WebStandardStreamableHTTPServerTransportOptions =
        abstract retryInterval: option<float> with get, set
        /// <deprecated>
        /// Use external middleware for DNS rebinding protection instead.
        /// </deprecated>
        abstract enableDnsRebindingProtection: option<bool> with get, set
        /// <deprecated>
        /// Use external middleware for origin validation instead.
        /// </deprecated>
        abstract allowedOrigins: option<ResizeArray<string>> with get, set
        /// <deprecated>
        /// Use external middleware for host validation instead.
        /// </deprecated>
        abstract allowedHosts: option<ResizeArray<string>> with get, set
        abstract eventStore: option<EventStore> with get, set
        abstract enableJsonResponse: option<bool> with get, set
        abstract sessionIdGenerator: unit -> string
        abstract onsessioninitialized: sessionId: string -> option<Promise<unit>>
        abstract onsessionclosed: sessionId: string -> option<Promise<unit>>

    type AnySchema = U2<Zod.ZodType, Zod.ZodType>
    type RequestId = ProgressToken

    [<Import("@modelcontextprotocol/sdk/client/streamableHttp.js", "StreamableHTTPReconnectionOptions")>]
    type StreamableHTTPReconnectionOptions =
        abstract maxRetries: float with get, set
        abstract reconnectionDelayGrowFactor: float with get, set
        abstract initialReconnectionDelay: float with get, set
        abstract maxReconnectionDelay: float with get, set

    [<Import("./zod-compat.js", "ZodRawShapeCompat")>]
    type ZodRawShapeCompat = interface end

    type SchemaOutput<'S> = option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>>

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "ToolCallback")>]
    type ToolCallback<'Args> = interface end

    [<Import("@modelcontextprotocol/sdk/server/index.js", "ServerOptions")>]
    type ServerOptions =
        /// <example>
        /// ```typescript<br/>
        /// // ajv (default)<br/>
        /// const server = new Server(<br/>
        ///   { name: 'my-server', version: '1.0.0' },<br/>
        ///   {<br/>
        ///     capabilities: {}<br/>
        ///     jsonSchemaValidator: new AjvJsonSchemaValidator()<br/>
        ///   }<br/>
        /// );<br/>
        /// <br/>
        /// //
        /// </example>
        abstract jsonSchemaValidator: option<JsonSchemaValidator> with get, set
        abstract instructions: option<string> with get, set
        abstract capabilities: option<TypesJs.ServerCapabilities> with get, set
        abstract maxTaskQueueSize: option<float> with get, set
        abstract defaultTaskPollInterval: option<float> with get, set
        abstract taskMessageQueue: option<TaskMessageQueue> with get, set
        abstract taskStore: option<TaskStore> with get, set
        abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
        abstract enforceStrictCapabilities: option<bool> with get, set

    [<Import("../experimental/tasks/interfaces.js", "ToolTaskHandler")>]
    type ToolTaskHandler<'Args> =
        abstract getTaskResult: BaseToolCallback<TypesJs.CallToolResult, 'Args, unit> with get, set
        abstract getTask: BaseToolCallback<TypesJs.GetTaskResult, 'Args, unit> with get, set
        abstract createTask: BaseToolCallback<TypesJs.CreateTaskResult, 'Args, unit> with get, set

    type ReadResourceCallback = CloudflareWorkersTypes.URL -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>

    [<Import("../experimental/tasks/interfaces.js", "QueuedRequest")>]
    type QueuedRequest =
        inherit BaseQueuedMessage
        abstract message: TypesJs.JSONRPCRequest with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@modelcontextprotocol/sdk/types.js", "MessageExtraInfo")>]
    type MessageExtraInfo =
        abstract authInfo: option<AuthInfo> with get, set
        abstract requestInfo: option<RequestInfo> with get, set
        abstract closeSSEStream: unit -> unit
        abstract closeStandaloneSSEStream: unit -> unit

    [<Import("@modelcontextprotocol/sdk/types.js", "SamplingContent")>]
    type SamplingContent = interface end

    [<Import("../experimental/tasks/server.js", "ExperimentalServerTasks")>]
    type ExperimentalServerTasks<'RequestT, 'NotificationT, 'ResultT> =
        [<EmitConstructor>]
        abstract Create: _server: Server<'RequestT, 'NotificationT, 'ResultT> -> ExperimentalServerTasks<'RequestT, 'NotificationT, 'ResultT>

        abstract _server: option<obj> with get
        abstract requestStream: request: U9<SharedLiterals.MethodParams5, SharedLiterals.MethodParams10, SharedLiterals.MethodParams11, SharedLiterals.MethodParams12, SharedLiterals.MethodParams6, SharedLiterals.MethodParams7, SharedLiterals.MethodParams8, SharedLiterals.MethodParams9, TypesJs.Request> * resultSchema: obj * ?options: RequestOptions -> seq<U4<TaskStatusMessage, TaskCreatedMessage, ErrorMessage, ResultMessage<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>>>
        abstract createMessageStream: params: TypesJs.CreateMessageRequestParams * ?options: RequestOptions -> seq<U4<TaskStatusMessage, TaskCreatedMessage, ErrorMessage, ResultMessage<TypesJs.CreateMessageResult>>>
        abstract elicitInputStream: params: U2<TypesJs.ElicitRequestFormParams, TypesJs.ElicitRequestURLParams> * ?options: RequestOptions -> seq<U4<TaskStatusMessage, TaskCreatedMessage, ErrorMessage, ResultMessage<obj>>>
        abstract getTask: taskId: string * ?options: RequestOptions -> Promise<TypesJs.GetTaskResult>
        abstract getTaskResult: taskId: string * ?resultSchema: obj * ?options: RequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
        abstract listTasks: ?cursor: string * ?options: RequestOptions -> Promise<TypesJs.ListTasksResult>
        abstract cancelTask: taskId: string * ?options: RequestOptions -> Promise<TypesJs.CancelTaskResult>

    type MergeCapabilities =
        abstract Invoke: ``base``: TypesJs.ServerCapabilities * additional: SharedLiterals.CompletionsExperimentalExtA1abd3ff3 -> TypesJs.ServerCapabilities
        abstract Invoke: ``base``: TypesJs.ClientCapabilities * additional: SharedLiterals.ElicitationExperimentalExtFf9b7be32 -> TypesJs.ClientCapabilities

    [<Import("@modelcontextprotocol/sdk/types.js", "SamplingMessageContentBlock")>]
    type SamplingMessageContentBlock = interface end

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "RegisteredTool")>]
    type RegisteredTool =
        abstract enabled: bool with get, set
        abstract handler: U3<obj -> U2<TypesJs.CallToolResult, Promise<TypesJs.CallToolResult>>, SharedLiterals._def_e995c0ec<obj, obj, obj, obj> -> obj -> U2<TypesJs.CallToolResult, Promise<TypesJs.CallToolResult>>, ToolTaskHandler<option<ZodRawShapeCompat>>> with get, set
        abstract _meta: option<obj> with get, set
        abstract execution: option<TypesJs.ToolExecution> with get, set
        abstract annotations: option<TypesJs.ToolAnnotations> with get, set
        abstract outputSchema: option<AnySchema> with get, set
        abstract inputSchema: option<AnySchema> with get, set
        abstract description: option<string> with get, set
        abstract title: option<string> with get, set
        abstract enable: unit -> unit
        abstract disable: unit -> unit
        abstract update: updates: SharedLiterals._metaAnnotations83faa3cf<obj, obj, obj, obj> -> unit
        abstract remove: unit -> unit

    type OAuthClientInformationMixed = U2<Shared.AuthJs.OAuthClientInformationFull, Shared.AuthJs.OAuthClientInformation>

    [<Import("../../shared/responseMessage.js", "ResultMessage")>]
    type ResultMessage<'T> =
        inherit BaseResponseMessage
        abstract result: 'T with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AuthResult =
        | AUTHORIZED
        | REDIRECT

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "TaskRequestOptions")>]
    type TaskRequestOptions = interface end

    type CompleteResourceTemplateCallback = string -> option<SharedLiterals.Arguments3> -> U2<ResizeArray<string>, Promise<ResizeArray<string>>>

    [<Import("../experimental/tasks/interfaces.js", "QueuedNotification")>]
    type QueuedNotification =
        inherit BaseQueuedMessage
        abstract message: TypesJs.JSONRPCNotification with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@modelcontextprotocol/sdk/shared/protocol.js", "NotificationOptions")>]
    type NotificationOptions =
        abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
        abstract relatedRequestId: option<ProgressToken> with get, set

    type DiscoverOAuthMetadata =
        abstract protocolVersion: option<string> with get, set
        abstract authorizationServerUrl: option<U2<CloudflareWorkersTypes.URL, string>> with get, set

    [<Import("../../shared/responseMessage.js", "ErrorMessage")>]
    type ErrorMessage =
        inherit BaseResponseMessage
        abstract error: McpError with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@modelcontextprotocol/sdk/types.js", "ElicitRequestParams")>]
    type ElicitRequestParams = interface end

    [<Import("@modelcontextprotocol/sdk/types.js", "UrlElicitationRequiredError")>]
    type UrlElicitationRequiredError =
        [<EmitConstructor>]
        abstract Create: elicitations: ResizeArray<TypesJs.ElicitRequestURLParams> * ?message: string -> UrlElicitationRequiredError

        inherit McpError
        abstract elicitations: ResizeArray<TypesJs.ElicitRequestURLParams> with get

    [<Import("@modelcontextprotocol/sdk/types.js", "ListChangedHandlers")>]
    type ListChangedHandlers =
        abstract resources: option<SharedLiterals.AutoRefreshDebounceMsOnChanged> with get, set
        abstract prompts: option<SharedLiterals.AutoRefreshDebounceMsOnChanged> with get, set
        abstract tools: option<SharedLiterals.AutoRefreshDebounceMsOnChanged> with get, set

    [<Import("../../shared/responseMessage.js", "TaskStatusMessage")>]
    type TaskStatusMessage =
        inherit BaseResponseMessage
        abstract task: TypesJs.Task with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type BuildDiscoveryUrls =
        [<EmitProperty("type")>]
        abstract ``type``: LiteralUnions.OauthOidc with get, set

        abstract url: CloudflareWorkersTypes.URL with get, set

    type AuthorizationServerMetadata = U2<Shared.AuthJs.OAuthMetadata, Shared.AuthJs.OpenIdProviderDiscoveryMetadata>

    [<Import("@modelcontextprotocol/sdk/server/mcp.js", "ResourceTemplate")>]
    type ResourceTemplate =
        [<EmitConstructor>]
        abstract Create: uriTemplate: U2<UriTemplate, string> * _callbacks: ResourceTemplate._callbacks -> ResourceTemplate

        abstract listCallback: option<ListResourcesCallback> with get
        abstract uriTemplate: UriTemplate with get
        abstract _uriTemplate: option<obj> with get, set
        abstract _callbacks: option<obj> with get, set
        abstract completeCallback: variable: string -> option<CompleteResourceTemplateCallback>

    [<Import("@modelcontextprotocol/sdk/types.js", "McpError")>]
    type McpError =
        [<EmitConstructor>]
        abstract Create: code: float * message: string * ?data: obj -> McpError

        abstract data: option<obj> with get
        abstract code: float with get
        abstract fromError: code: float * message: string * ?data: obj -> McpError

    [<Import("@modelcontextprotocol/sdk/types.js", "ServerNotification")>]
    type ServerNotification = interface end

    type ITypesJs =
        [<CompiledName("SamplingMessageSchema")>]
        member _.samplingMessageSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CreateMessageRequestSchema")>]
        member _.createMessageRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ClientTasksCapabilitySchema")>]
        member _.clientTasksCapabilitySchema: Zod.ZodType = JS.undefined

        [<CompiledName("ClientNotificationSchema")>]
        member _.clientNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("LoggingMessageNotificationSchema")>]
        member _.loggingMessageNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CompatibilityCallToolResultSchema")>]
        member _.compatibilityCallToolResultSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.isJSONRPCNotification: option<obj> -> bool = JS.undefined

        [<CompiledName("InitializedNotificationSchema")>]
        member _.initializedNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("BaseMetadataSchema")>]
        member _.baseMetadataSchema: Zod.ZodType = JS.undefined

        [<CompiledName("SamplingContentSchema")>]
        member _.samplingContentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskStatusNotificationSchema")>]
        member _.taskStatusNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("UnsubscribeRequestParamsSchema")>]
        member _.unsubscribeRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PaginatedRequestParamsSchema")>]
        member _.paginatedRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ModelPreferencesSchema")>]
        member _.modelPreferencesSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceTemplateSchema")>]
        member _.resourceTemplateSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListToolsRequestSchema")>]
        member _.listToolsRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetTaskRequestSchema")>]
        member _.getTaskRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("AudioContentSchema")>]
        member _.audioContentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceUpdatedNotificationParamsSchema")>]
        member _.resourceUpdatedNotificationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceLinkSchema")>]
        member _.resourceLinkSchema: Zod.ZodType = JS.undefined

        [<CompiledName("IconSchema")>]
        member _.iconSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.SUPPORTED_PROTOCOL_VERSIONS: ResizeArray<string> = JS.undefined

        [<CompiledName("ReadResourceRequestSchema")>]
        member _.readResourceRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("UntitledSingleSelectEnumSchemaSchema")>]
        member _.untitledSingleSelectEnumSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ElicitationCompleteNotificationParamsSchema")>]
        member _.elicitationCompleteNotificationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("RootsListChangedNotificationSchema")>]
        member _.rootsListChangedNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResultSchema")>]
        member _.resultSchema: Zod.ZodType = JS.undefined

        /// <deprecated>
        /// Use {@link isJSONRPCResultResponse} instead.<br/>
        /// <br/>
        /// Please note that {@link JSONRPCResponse} is a union of {@link JSONRPCResultResponse} and {@link JSONRPCErrorResponse} as per the updated JSON-RPC specification. (was previously just {@link JSONRPCResultResponse})
        /// </deprecated>
        [<Erase>]
        member _.isJSONRPCResponse: option<obj> -> bool = JS.undefined

        [<CompiledName("ModelHintSchema")>]
        member _.modelHintSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceListChangedNotificationSchema")>]
        member _.resourceListChangedNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CompleteResultSchema")>]
        member _.completeResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("IconsSchema")>]
        member _.iconsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ToolExecutionSchema")>]
        member _.toolExecutionSchema: Zod.ZodType = JS.undefined

        [<CompiledName("NumberSchemaSchema")>]
        member _.numberSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ReadResourceResultSchema")>]
        member _.readResourceResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceContentsSchema")>]
        member _.resourceContentsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskSchema")>]
        member _.taskSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TitledSingleSelectEnumSchemaSchema")>]
        member _.titledSingleSelectEnumSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("JSONRPCNotificationSchema")>]
        member _.jSONRPCNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ServerResultSchema")>]
        member _.serverResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("InitializeRequestSchema")>]
        member _.initializeRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ServerNotificationSchema")>]
        member _.serverNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskStatusNotificationParamsSchema")>]
        member _.taskStatusNotificationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListTasksResultSchema")>]
        member _.listTasksResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskCreationParamsSchema")>]
        member _.taskCreationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CancelledNotificationSchema")>]
        member _.cancelledNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("LoggingLevelSchema")>]
        member _.loggingLevelSchema: Zod.ZodType = JS.undefined

        [<CompiledName("BlobResourceContentsSchema")>]
        member _.blobResourceContentsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CursorSchema")>]
        member _.cursorSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TextResourceContentsSchema")>]
        member _.textResourceContentsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CreateMessageResultWithToolsSchema")>]
        member _.createMessageResultWithToolsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("RequestIdSchema")>]
        member _.requestIdSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ClientRequestSchema")>]
        member _.clientRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ImplementationSchema")>]
        member _.implementationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetPromptRequestSchema")>]
        member _.getPromptRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PromptSchema")>]
        member _.promptSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListChangedOptionsBaseSchema")>]
        member _.listChangedOptionsBaseSchema: Zod.ZodType = JS.undefined

        [<CompiledName("JSONRPCMessageSchema")>]
        member _.jSONRPCMessageSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CallToolResultSchema")>]
        member _.callToolResultSchema: Zod.ZodType = JS.undefined

        /// <deprecated>
        /// Use {@link JSONRPCErrorResponseSchema} instead.
        /// </deprecated>
        [<CompiledName("JSONRPCErrorSchema")>]
        member _.jSONRPCErrorSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListRootsRequestSchema")>]
        member _.listRootsRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PingRequestSchema")>]
        member _.pingRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CreateTaskResultSchema")>]
        member _.createTaskResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CompleteRequestSchema")>]
        member _.completeRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ContentBlockSchema")>]
        member _.contentBlockSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CancelTaskResultSchema")>]
        member _.cancelTaskResultSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.isInitializeRequest: option<obj> -> bool = JS.undefined

        [<CompiledName("ServerRequestSchema")>]
        member _.serverRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("JSONRPCResultResponseSchema")>]
        member _.jSONRPCResultResponseSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListResourceTemplatesResultSchema")>]
        member _.listResourceTemplatesResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskStatusSchema")>]
        member _.taskStatusSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PaginatedRequestSchema")>]
        member _.paginatedRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ElicitRequestParamsSchema")>]
        member _.elicitRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TitledMultiSelectEnumSchemaSchema")>]
        member _.titledMultiSelectEnumSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ServerCapabilitiesSchema")>]
        member _.serverCapabilitiesSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ReadResourceRequestParamsSchema")>]
        member _.readResourceRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("NotificationSchema")>]
        member _.notificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceSchema")>]
        member _.resourceSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ElicitRequestURLParamsSchema")>]
        member _.elicitRequestURLParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("LoggingMessageNotificationParamsSchema")>]
        member _.loggingMessageNotificationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("BooleanSchemaSchema")>]
        member _.booleanSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListTasksRequestSchema")>]
        member _.listTasksRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskMetadataSchema")>]
        member _.taskMetadataSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListToolsResultSchema")>]
        member _.listToolsResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetTaskPayloadResultSchema")>]
        member _.getTaskPayloadResultSchema: Zod.ZodType = JS.undefined

        /// <deprecated>
        /// Use ResourceTemplateReferenceSchema instead
        /// </deprecated>
        [<CompiledName("ResourceReferenceSchema")>]
        member _.resourceReferenceSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TaskAugmentedRequestParamsSchema")>]
        member _.taskAugmentedRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CreateMessageRequestParamsSchema")>]
        member _.createMessageRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListPromptsResultSchema")>]
        member _.listPromptsResultSchema: Zod.ZodType = JS.undefined

        /// <param name="value" >
        /// - The value to check.
        /// </param>
        [<Erase>]
        member _.isJSONRPCErrorResponse: option<obj> -> bool = JS.undefined

        [<CompiledName("SetLevelRequestParamsSchema")>]
        member _.setLevelRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListPromptsRequestSchema")>]
        member _.listPromptsRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("InitializeResultSchema")>]
        member _.initializeResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("StringSchemaSchema")>]
        member _.stringSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("RoleSchema")>]
        member _.roleSchema: Zod.ZodType = JS.undefined

        [<CompiledName("EmptyResultSchema")>]
        member _.emptyResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("EnumSchemaSchema")>]
        member _.enumSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ServerTasksCapabilitySchema")>]
        member _.serverTasksCapabilitySchema: Zod.ZodType = JS.undefined

        [<CompiledName("ClientCapabilitiesSchema")>]
        member _.clientCapabilitiesSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PromptArgumentSchema")>]
        member _.promptArgumentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("SamplingMessageContentBlockSchema")>]
        member _.samplingMessageContentBlockSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CancelledNotificationParamsSchema")>]
        member _.cancelledNotificationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PaginatedResultSchema")>]
        member _.paginatedResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ToolUseContentSchema")>]
        member _.toolUseContentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ElicitRequestFormParamsSchema")>]
        member _.elicitRequestFormParamsSchema: Zod.ZodType = JS.undefined

        /// <param name="value" >
        /// - The value to check.
        /// </param>
        [<Erase>]
        member _.isJSONRPCResultResponse: option<obj> -> bool = JS.undefined

        [<CompiledName("RelatedTaskMetadataSchema")>]
        member _.relatedTaskMetadataSchema: Zod.ZodType = JS.undefined

        [<CompiledName("InitializeRequestParamsSchema")>]
        member _.initializeRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PromptMessageSchema")>]
        member _.promptMessageSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CancelTaskRequestSchema")>]
        member _.cancelTaskRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ProgressNotificationParamsSchema")>]
        member _.progressNotificationParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ToolResultContentSchema")>]
        member _.toolResultContentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("UntitledMultiSelectEnumSchemaSchema")>]
        member _.untitledMultiSelectEnumSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ElicitResultSchema")>]
        member _.elicitResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListResourcesResultSchema")>]
        member _.listResourcesResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ProgressTokenSchema")>]
        member _.progressTokenSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ClientResultSchema")>]
        member _.clientResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("SubscribeRequestParamsSchema")>]
        member _.subscribeRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("AnnotationsSchema")>]
        member _.annotationsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ToolAnnotationsSchema")>]
        member _.toolAnnotationsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ProgressNotificationSchema")>]
        member _.progressNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListResourcesRequestSchema")>]
        member _.listResourcesRequestSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.LATEST_PROTOCOL_VERSION: string = JS.undefined

        [<CompiledName("RequestSchema")>]
        member _.requestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CallToolRequestParamsSchema")>]
        member _.callToolRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("SubscribeRequestSchema")>]
        member _.subscribeRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PrimitiveSchemaDefinitionSchema")>]
        member _.primitiveSchemaDefinitionSchema: Zod.ZodType = JS.undefined

        [<CompiledName("CompleteRequestParamsSchema")>]
        member _.completeRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("JSONRPCResponseSchema")>]
        member _.jSONRPCResponseSchema: Zod.ZodType = JS.undefined

        [<CompiledName("JSONRPCRequestSchema")>]
        member _.jSONRPCRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ProgressSchema")>]
        member _.progressSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ElicitationCompleteNotificationSchema")>]
        member _.elicitationCompleteNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("SetLevelRequestSchema")>]
        member _.setLevelRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PromptReferenceSchema")>]
        member _.promptReferenceSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetPromptRequestParamsSchema")>]
        member _.getPromptRequestParamsSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListResourceTemplatesRequestSchema")>]
        member _.listResourceTemplatesRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("UnsubscribeRequestSchema")>]
        member _.unsubscribeRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("PromptListChangedNotificationSchema")>]
        member _.promptListChangedNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("EmbeddedResourceSchema")>]
        member _.embeddedResourceSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceUpdatedNotificationSchema")>]
        member _.resourceUpdatedNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetTaskPayloadRequestSchema")>]
        member _.getTaskPayloadRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ToolListChangedNotificationSchema")>]
        member _.toolListChangedNotificationSchema: Zod.ZodType = JS.undefined

        [<CompiledName("JSONRPCErrorResponseSchema")>]
        member _.jSONRPCErrorResponseSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ImageContentSchema")>]
        member _.imageContentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("SingleSelectEnumSchemaSchema")>]
        member _.singleSelectEnumSchemaSchema: Zod.ZodType = JS.undefined

        /// <deprecated>
        /// Use {@link isJSONRPCErrorResponse} instead.
        /// </deprecated>
        [<Erase>]
        member _.isJSONRPCError: option<obj> -> bool = JS.undefined

        [<CompiledName("ResourceTemplateReferenceSchema")>]
        member _.resourceTemplateReferenceSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ListRootsResultSchema")>]
        member _.listRootsResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetTaskResultSchema")>]
        member _.getTaskResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("GetPromptResultSchema")>]
        member _.getPromptResultSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ResourceRequestParamsSchema")>]
        member _.resourceRequestParamsSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.isJSONRPCRequest: option<obj> -> bool = JS.undefined

        [<Erase>]
        member _.JSONRPC_VERSION: string = JS.undefined

        [<Erase>]
        member _.isInitializedNotification: option<obj> -> bool = JS.undefined

        [<CompiledName("CallToolRequestSchema")>]
        member _.callToolRequestSchema: Zod.ZodType = JS.undefined

        [<CompiledName("MultiSelectEnumSchemaSchema")>]
        member _.multiSelectEnumSchemaSchema: Zod.ZodType = JS.undefined

        [<CompiledName("TextContentSchema")>]
        member _.textContentSchema: Zod.ZodType = JS.undefined

        [<CompiledName("RootSchema")>]
        member _.rootSchema: Zod.ZodType = JS.undefined

        [<CompiledName("LegacyTitledEnumSchemaSchema")>]
        member _.legacyTitledEnumSchemaSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.RELATED_TASK_META_KEY: string = JS.undefined

        [<CompiledName("CreateMessageResultSchema")>]
        member _.createMessageResultSchema: Zod.ZodType = JS.undefined

        /// <param name="value" >
        /// - The value to check.
        /// </param>
        [<Erase>]
        member _.isTaskAugmentedRequestParams: option<obj> -> bool = JS.undefined

        [<CompiledName("ToolChoiceSchema")>]
        member _.toolChoiceSchema: Zod.ZodType = JS.undefined

        [<CompiledName("ToolSchema")>]
        member _.toolSchema: Zod.ZodType = JS.undefined

        [<Erase>]
        member _.DEFAULT_NEGOTIATED_PROTOCOL_VERSION: string = JS.undefined

    module AnyObjectSchema =
        type Case2 =
            abstract Item: key: string -> U2<option<obj>, proptypekey<obj, string>>

    module Client =
        type Experimental =
            abstract tasks: ExperimentalClientTasks<obj, obj, obj> with get, set

        type ListResourceTemplates =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set
            abstract resourceTemplates: ResizeArray<SharedLiterals._metaAnnotationsE7edce492> with get, set
            abstract Item: x: string -> option<obj>

        type Options =
            /// <example>
            /// ```typescript<br/>
            /// const client = new Client(<br/>
            ///   { name: 'my-client', version: '1.0.0' },<br/>
            ///   {<br/>
            ///     listChanged: {<br/>
            ///       tools: {<br/>
            ///         onChanged: (error, tools) => {<br/>
            ///           if (error) {<br/>
            ///             console.error('Failed to refresh tools:', error);<br/>
            ///             return;<br/>
            ///           }<br/>
            ///           console.log('Tools updated:', tools);<br/>
            ///         }<br/>
            ///       },<br/>
            ///       prompts: {<br/>
            ///         onChanged: (error, prompts) => console.log('Prompts updated:', prompts)<br/>
            ///       }<br/>
            ///     }<br/>
            ///   }<br/>
            /// );<br/>
            /// ```
            /// </example>
            abstract listChanged: option<ListChangedHandlers> with get, set
            /// <example>
            /// ```typescript<br/>
            /// // ajv<br/>
            /// const client = new Client(<br/>
            ///   { name: 'my-client', version: '1.0.0' },<br/>
            ///   {<br/>
            ///     capabilities: {},<br/>
            ///     jsonSchemaValidator: new AjvJsonSchemaValidator()<br/>
            ///   }<br/>
            /// );<br/>
            /// <br/>
            /// //
            /// </example>
            abstract jsonSchemaValidator: option<JsonSchemaValidator> with get, set
            abstract capabilities: option<TypesJs.ClientCapabilities> with get, set
            abstract maxTaskQueueSize: option<float> with get, set
            abstract defaultTaskPollInterval: option<float> with get, set
            abstract taskMessageQueue: option<TaskMessageQueue> with get, set
            abstract taskStore: option<TaskStore> with get, set
            abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
            abstract enforceStrictCapabilities: option<bool> with get, set

        type ListPrompts =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set
            abstract prompts: ResizeArray<SharedLiterals._metaArgumentsE5be7b5f2> with get, set
            abstract Item: x: string -> option<obj>

        type ListResources =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set
            abstract resources: ResizeArray<SharedLiterals._metaAnnotationsE54c02692> with get, set
            abstract Item: x: string -> option<obj>

        type Complete =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set
            abstract completion: Complete.Completion with get, set
            abstract Item: x: string -> option<obj>

        type ListTools =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set
            abstract tools: ResizeArray<ListTools.Tools> with get, set
            abstract Item: x: string -> option<obj>

        module AuthJs =
            [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
            type ClientAuthMethod =
                | [<CompiledName("none")>] None
                | [<CompiledName("client_secret_basic")>] ClientSecretBasic
                | [<CompiledName("client_secret_post")>] ClientSecretPost

        module Complete =
            type Completion =
                abstract hasMore: option<bool> with get, set
                abstract total: option<float> with get, set
                abstract values: ResizeArray<string> with get, set
                abstract Item: x: string -> option<obj>

        module Connect =
            type Options =
                abstract resumptionToken: option<string> with get, set
                abstract relatedRequestId: option<ProgressToken> with get, set
                abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
                abstract task: option<TypesJs.TaskCreationParams> with get, set
                abstract maxTotalTimeout: option<float> with get, set
                abstract resetTimeoutOnProgress: option<bool> with get, set
                abstract timeout: option<float> with get, set
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract onprogress: progress: TypesJs.Progress -> unit
                abstract onresumptiontoken: token: string -> unit

        module ListTools =
            type Tools =
                abstract title: option<string> with get, set
                abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme2>> with get, set
                abstract _meta: option<obj> with get, set
                abstract execution: option<SharedLiterals.TaskSupport2> with get, set
                abstract annotations: option<SharedLiterals.DestructiveHintIdempotentHi8e2ddc362> with get, set
                abstract outputSchema: option<obj> with get, set
                abstract description: option<string> with get, set
                abstract name: string with get, set
                abstract inputSchema: Tools.InputSchema with get, set

            module Tools =
                type InputSchema =
                    abstract required: option<ResizeArray<string>> with get, set
                    abstract properties: option<obj> with get, set

                    [<EmitProperty("type")>]
                    abstract ``type``: string with get, set

                    abstract Item: x: string -> option<obj>

    module ExperimentalClientTasks =
        module CallToolStream =
            type Options =
                abstract resumptionToken: option<string> with get, set
                abstract relatedRequestId: option<ProgressToken> with get, set
                abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
                abstract task: option<TypesJs.TaskCreationParams> with get, set
                abstract maxTotalTimeout: option<float> with get, set
                abstract resetTimeoutOnProgress: option<bool> with get, set
                abstract timeout: option<float> with get, set
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract onprogress: progress: TypesJs.Progress -> unit
                abstract onresumptiontoken: token: string -> unit

    module ExperimentalMcpServerTasks =
        module RegisterToolTask =
            type Config =
                abstract _meta: option<obj> with get, set
                abstract execution: option<Config.Execution> with get, set
                abstract annotations: option<TypesJs.ToolAnnotations> with get, set
                abstract outputSchema: option<obj> with get, set
                abstract description: option<string> with get, set
                abstract title: option<string> with get, set

            module Config =
                type Case2 =
                    abstract _meta: option<obj> with get, set
                    abstract execution: option<obj> with get, set
                    abstract annotations: option<TypesJs.ToolAnnotations> with get, set
                    abstract outputSchema: option<obj> with get, set
                    abstract inputSchema: obj with get, set
                    abstract description: option<string> with get, set
                    abstract title: option<string> with get, set

                type Execution =
                    abstract taskSupport: LiteralUnions.OptionalRequired with get, set
                    abstract Item: key: string -> option<obj>

    module ExperimentalServerTasks =
        module RequestStream =
            type Options =
                abstract resumptionToken: option<string> with get, set
                abstract relatedRequestId: option<ProgressToken> with get, set
                abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
                abstract task: option<TypesJs.TaskCreationParams> with get, set
                abstract maxTotalTimeout: option<float> with get, set
                abstract resetTimeoutOnProgress: option<bool> with get, set
                abstract timeout: option<float> with get, set
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract onprogress: progress: TypesJs.Progress -> unit
                abstract onresumptiontoken: token: string -> unit

    module JsonSchemaValidator =
        module GetValidator =
            type Schema =
                abstract writeOnly: option<bool> with get, set
                abstract uniqueItems: option<bool> with get, set
                abstract unevaluatedProperties: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
                abstract unevaluatedItems: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                [<EmitProperty("type")>]
                abstract ``type``: option<Erased.JsonSchemaTyped> with get, set

                abstract title: option<string> with get, set

                [<EmitProperty("then")>]
                abstract ``then``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
                abstract readOnly: option<bool> with get, set
                abstract propertyNames: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
                abstract properties: option<obj> with get, set
                abstract prefixItems: option<U4<SharedLiterals.AnchorCommentDefs0800bb6e, ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
                abstract patternProperties: option<obj> with get, set
                abstract pattern: option<string> with get, set
                abstract oneOf: option<U2<ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>>> with get, set

                [<EmitProperty("not")>]
                abstract ``not``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                abstract multipleOf: option<float> with get, set
                abstract minProperties: option<float> with get, set
                abstract minLength: option<float> with get, set
                abstract minItems: option<float> with get, set
                abstract minimum: option<float> with get, set
                abstract minContains: option<float> with get, set
                abstract maxProperties: option<float> with get, set
                abstract maxLength: option<float> with get, set
                abstract maxItems: option<float> with get, set
                abstract maximum: option<float> with get, set
                abstract maxContains: option<float> with get, set
                abstract items: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                [<EmitProperty("if")>]
                abstract ``if``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                abstract format: option<string> with get, set
                abstract exclusiveMinimum: option<float> with get, set
                abstract exclusiveMaximum: option<float> with get, set
                abstract examples: option<U2<ResizeArray<option<obj>>, System.Collections.Generic.IReadOnlyList<option<obj>>>> with get, set
                abstract enum: option<U2<ResizeArray<option<obj>>, System.Collections.Generic.IReadOnlyList<option<obj>>>> with get, set

                [<EmitProperty("else")>]
                abstract ``else``: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                abstract description: option<string> with get, set
                abstract deprecated: option<bool> with get, set
                abstract dependentSchemas: option<obj> with get, set
                abstract dependentRequired: option<obj> with get, set
                abstract dependencies: option<obj> with get, set
                abstract definitions: option<obj> with get, set

                [<EmitProperty("default")>]
                abstract ``default``: option<obj> with get, set

                abstract contentSchema: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
                abstract contentMediaType: option<string> with get, set
                abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
                abstract contains: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                [<EmitProperty("const")>]
                abstract ``const``: option<obj> with get, set

                abstract anyOf: option<U2<ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>>> with get, set
                abstract allOf: option<U2<ResizeArray<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>>>> with get, set
                abstract additionalProperties: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set
                abstract additionalItems: option<U2<SharedLiterals.AnchorCommentDefs0800bb6e, bool>> with get, set

                [<EmitProperty("$vocabulary")>]
                abstract ``$vocabulary``: option<obj> with get, set

                [<EmitProperty("$schema")>]
                abstract ``$schema``: option<string> with get, set

                [<EmitProperty("$ref")>]
                abstract ``$ref``: option<string> with get, set

                [<EmitProperty("$id")>]
                abstract ``$id``: option<string> with get, set

                [<EmitProperty("$dynamicRef")>]
                abstract ``$dynamicRef``: option<string> with get, set

                [<EmitProperty("$dynamicAnchor")>]
                abstract ``$dynamicAnchor``: option<string> with get, set

                [<EmitProperty("$defs")>]
                abstract ``$defs``: option<obj> with get, set

                [<EmitProperty("$comment")>]
                abstract ``$comment``: option<string> with get, set

                [<EmitProperty("$anchor")>]
                abstract ``$anchor``: option<string> with get, set

    module McpServer =
        type Resource =
            abstract enabled: bool with get, set
            abstract metadata: option<ResourceMetadata> with get, set
            abstract title: option<string> with get, set
            abstract name: string with get, set
            abstract readCallback: uri: CloudflareWorkersTypes.URL * extra: SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>
            abstract enable: unit -> unit
            abstract disable: unit -> unit
            abstract update: updates: SharedLiterals.CallbackEnabledMetadataNameTitleUri -> unit
            abstract remove: unit -> unit

        type Experimental =
            abstract tasks: ExperimentalMcpServerTasks with get, set

        type Options =
            /// <example>
            /// ```typescript<br/>
            /// // ajv (default)<br/>
            /// const server = new Server(<br/>
            ///   { name: 'my-server', version: '1.0.0' },<br/>
            ///   {<br/>
            ///     capabilities: {}<br/>
            ///     jsonSchemaValidator: new AjvJsonSchemaValidator()<br/>
            ///   }<br/>
            /// );<br/>
            /// <br/>
            /// //
            /// </example>
            abstract jsonSchemaValidator: option<JsonSchemaValidator> with get, set
            abstract instructions: option<string> with get, set
            abstract capabilities: option<TypesJs.ServerCapabilities> with get, set
            abstract maxTaskQueueSize: option<float> with get, set
            abstract defaultTaskPollInterval: option<float> with get, set
            abstract taskMessageQueue: option<TaskMessageQueue> with get, set
            abstract taskStore: option<TaskStore> with get, set
            abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
            abstract enforceStrictCapabilities: option<bool> with get, set

        type Prompt =
            abstract enabled: bool with get, set
            abstract callback: U2<SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>, obj -> SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>> with get, set
            abstract argsSchema: option<AnyObjectSchema> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract enable: unit -> unit
            abstract disable: unit -> unit
            abstract update: updates: SharedLiterals.ArgsSchemaCallbackDescript5bdc466b<obj> -> unit
            abstract remove: unit -> unit

        module Prompt =
            type ArgsSchema =
                abstract Item: key: string -> U2<option<obj>, proptypekey<obj, string>>

            module ArgsSchema =
                type Case2 =
                    abstract Item: key: string -> U2<option<obj>, proptypekey<obj, string>>

        module RegisterPrompt =
            type Config =
                abstract argsSchema: option<obj> with get, set
                abstract description: option<string> with get, set
                abstract title: option<string> with get, set

        module RegisterTool =
            type Config =
                abstract _meta: option<obj> with get, set
                abstract annotations: option<TypesJs.ToolAnnotations> with get, set
                abstract outputSchema: option<obj> with get, set
                abstract inputSchema: option<obj> with get, set
                abstract description: option<string> with get, set
                abstract title: option<string> with get, set

        module Resource =
            type Case2 =
                abstract enabled: bool with get, set
                abstract metadata: option<ResourceMetadata> with get, set
                abstract title: option<string> with get, set
                abstract resourceTemplate: ResourceTemplate with get, set
                abstract readCallback: uri: CloudflareWorkersTypes.URL * variables: Variables * extra: SharedLiterals._metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>
                abstract enable: unit -> unit
                abstract disable: unit -> unit
                abstract update: updates: SharedLiterals.CallbackEnabledMetadata8b2e58c4 -> unit
                abstract remove: unit -> unit

    module PrimitiveSchemaDefinition =
        type Case3 =
            [<EmitProperty("default")>]
            abstract ``default``: option<float> with get, set

            abstract maximum: option<float> with get, set
            abstract minimum: option<float> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: LiteralUnions.IntegerNumber with get, set

        type Case2 =
            [<EmitProperty("default")>]
            abstract ``default``: option<string> with get, set

            abstract format: option<LiteralUnions.DateDateTimeEmailUri> with get, set
            abstract maxLength: option<float> with get, set
            abstract minLength: option<float> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

    module Protocol =
        module ListTasks =
            type Params =
                abstract cursor: option<string> with get, set

        module RequestStream =
            type Options =
                abstract resumptionToken: option<string> with get, set
                abstract relatedRequestId: option<ProgressToken> with get, set
                abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
                abstract task: option<TypesJs.TaskCreationParams> with get, set
                abstract maxTotalTimeout: option<float> with get, set
                abstract resetTimeoutOnProgress: option<bool> with get, set
                abstract timeout: option<float> with get, set
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract onprogress: progress: TypesJs.Progress -> unit
                abstract onresumptiontoken: token: string -> unit

    module QueuedError =
        type Message =
            abstract id: option<U2<string, float>> with get, set
            abstract error: SharedLiterals.CodeDataMessage with get, set
            abstract jsonrpc: string with get, set

    module ResourceTemplate =
        type _callbacks =
            abstract complete: option<System.Collections.Generic.IDictionary<string, CompleteResourceTemplateCallback>> with get, set
            abstract list: option<ListResourcesCallback> with get, set

    module Server =
        type Experimental =
            abstract tasks: ExperimentalServerTasks<obj, obj, obj> with get, set

        type Options =
            /// <example>
            /// ```typescript<br/>
            /// // ajv (default)<br/>
            /// const server = new Server(<br/>
            ///   { name: 'my-server', version: '1.0.0' },<br/>
            ///   {<br/>
            ///     capabilities: {}<br/>
            ///     jsonSchemaValidator: new AjvJsonSchemaValidator()<br/>
            ///   }<br/>
            /// );<br/>
            /// <br/>
            /// //
            /// </example>
            abstract jsonSchemaValidator: option<JsonSchemaValidator> with get, set
            abstract instructions: option<string> with get, set
            abstract capabilities: option<TypesJs.ServerCapabilities> with get, set
            abstract maxTaskQueueSize: option<float> with get, set
            abstract defaultTaskPollInterval: option<float> with get, set
            abstract taskMessageQueue: option<TaskMessageQueue> with get, set
            abstract taskStore: option<TaskStore> with get, set
            abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
            abstract enforceStrictCapabilities: option<bool> with get, set

        type ListRoots =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set
            abstract roots: ResizeArray<ListRoots.Roots> with get, set
            abstract Item: x: string -> option<obj>

        module CreateMessage =
            type Options =
                abstract resumptionToken: option<string> with get, set
                abstract relatedRequestId: option<ProgressToken> with get, set
                abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
                abstract task: option<TypesJs.TaskCreationParams> with get, set
                abstract maxTotalTimeout: option<float> with get, set
                abstract resetTimeoutOnProgress: option<bool> with get, set
                abstract timeout: option<float> with get, set
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract onprogress: progress: TypesJs.Progress -> unit
                abstract onresumptiontoken: token: string -> unit

        module ListRoots =
            type Roots =
                abstract _meta: option<obj> with get, set
                abstract name: option<string> with get, set
                abstract uri: string with get, set

        module WebStandardStreamableHttpJs =
            type EventId = string

    module Shared =
        type IAuthJs =
            [<CompiledName("OAuthClientInformationSchema")>]
            member _.oAuthClientInformationSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthClientInformationFullSchema")>]
            member _.oAuthClientInformationFullSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OpenIdProviderDiscoveryMetadataSchema")>]
            member _.openIdProviderDiscoveryMetadataSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OptionalSafeUrlSchema")>]
            member _.optionalSafeUrlSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthTokenRevocationRequestSchema")>]
            member _.oAuthTokenRevocationRequestSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OpenIdProviderMetadataSchema")>]
            member _.openIdProviderMetadataSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthTokensSchema")>]
            member _.oAuthTokensSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthClientMetadataSchema")>]
            member _.oAuthClientMetadataSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthErrorResponseSchema")>]
            member _.oAuthErrorResponseSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthClientRegistrationErrorSchema")>]
            member _.oAuthClientRegistrationErrorSchema: Zod.ZodType = JS.undefined

            [<CompiledName("SafeUrlSchema")>]
            member _.safeUrlSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthProtectedResourceMetadataSchema")>]
            member _.oAuthProtectedResourceMetadataSchema: Zod.ZodType = JS.undefined

            [<CompiledName("OAuthMetadataSchema")>]
            member _.oAuthMetadataSchema: Zod.ZodType = JS.undefined

        type IProtocolJs =
            [<Erase>]
            member _.DEFAULT_REQUEST_TIMEOUT_MSEC: int = JS.undefined

        module AuthJs =
            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthErrorResponse = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthTokenRevocationRequest = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthProtectedResourceMetadata = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OpenIdProviderDiscoveryMetadata = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthClientInformationFull = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthMetadata = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthClientMetadata = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthClientInformation = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthTokens = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OAuthClientRegistrationError = interface end

            [<Import("@modelcontextprotocol/sdk/shared/auth.js", "__type")>]
            type OpenIdProviderMetadata = interface end

        module ProtocolJs =
            begin end

    module SharedLiterals =
        type DefaultDescriptionItems834741e2 =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract items: Zod.ZodType with get, set
            abstract maxItems: Zod.ZodType with get, set
            abstract minItems: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams16 =
            abstract params: _metaUri with get, set
            abstract method: string with get, set

        type JsonrpcMethodParams2 =
            abstract jsonrpc: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaAnnotations4df6b408 =
            abstract title: option<string> with get, set
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract size: option<float> with get, set
            abstract mimeType: option<string> with get, set
            abstract description: option<string> with get, set

        type Arguments =
            abstract arguments: Zod.ZodType with get, set

        type CallbackEnabledMetadata8b2e58c4 =
            abstract enabled: option<bool> with get, set
            abstract metadata: option<ResourceMetadata> with get, set
            abstract template: option<ResourceTemplate> with get, set
            abstract title: option<string> with get, set
            abstract name: option<string> with get, set
            abstract callback: uri: CloudflareWorkersTypes.URL * variables: Variables * extra: _metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>

        type MethodParams24 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaNextCursorTasks3 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract tasks: ResizeArray<CreatedAtLastUpdatedAtPoll0097a3012> with get, set

        type ListChangedSubscribe2 =
            abstract listChanged: Zod.ZodType with get, set
            abstract subscribe: Zod.ZodType with get, set

        type _metaIncludeContextE2cd06892 =
            abstract toolChoice: Zod.ZodType with get, set
            abstract tools: Zod.ZodType with get, set
            abstract metadata: Zod.ZodType with get, set
            abstract stopSequences: Zod.ZodType with get, set
            abstract maxTokens: Zod.ZodType with get, set
            abstract temperature: Zod.ZodType with get, set
            abstract includeContext: Zod.ZodType with get, set
            abstract systemPrompt: Zod.ZodType with get, set
            abstract modelPreferences: Zod.ZodType with get, set
            abstract messages: Zod.ZodType with get, set
            abstract task: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaContentModelRoleStopReason3 =
            abstract stopReason: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract content: SamplingContent with get, set
            abstract role: Role with get, set
            abstract model: string with get, set

        type _metaTask2 =
            abstract task: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaIdInputNameType2 =
            abstract _meta: Zod.ZodType with get, set
            abstract input: Zod.ZodType with get, set
            abstract id: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type CreatedAtLastUpdatedAtPoll0097a3013 =
            abstract statusMessage: option<string> with get, set
            abstract pollInterval: option<float> with get, set
            abstract lastUpdatedAt: string with get, set
            abstract createdAt: string with get, set
            abstract ttl: option<float> with get, set
            abstract status: TaskStatus with get, set
            abstract taskId: string with get, set

        type AutoNoneRequired =
            abstract none: string with get, set
            abstract auto: string with get, set
            abstract required: string with get, set

        type MethodParams28 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type DataErrorMessageValid2 =
            abstract errorMessage: string with get, set
            abstract data: unit with get
            abstract valid: bool with get, set

        type _metaNextCursorResourceTemplates =
            abstract resourceTemplates: Zod.ZodType with get, set
            abstract nextCursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type AuthorizationDetailsTypesFc177e162 =
            [<EmitProperty("dpop_bound_access_tokens_required")>]
            abstract dpopBoundAccessTokensRequired: option<bool> with get, set

            [<EmitProperty("dpop_signing_alg_values_supported")>]
            abstract dpopSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("authorization_details_types_supported")>]
            abstract authorizationDetailsTypesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("tls_client_certificate_bound_access_tokens")>]
            abstract tlsClientCertificateBoundAccessTokens: option<bool> with get, set

            [<EmitProperty("resource_tos_uri")>]
            abstract resourceTosUri: option<string> with get, set

            [<EmitProperty("resource_policy_uri")>]
            abstract resourcePolicyUri: option<string> with get, set

            [<EmitProperty("resource_documentation")>]
            abstract resourceDocumentation: option<string> with get, set

            [<EmitProperty("resource_name")>]
            abstract resourceName: option<string> with get, set

            [<EmitProperty("resource_signing_alg_values_supported")>]
            abstract resourceSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("bearer_methods_supported")>]
            abstract bearerMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: option<string> with get, set

            [<EmitProperty("authorization_servers")>]
            abstract authorizationServers: option<ResizeArray<string>> with get, set

            abstract resource: string with get, set

        type ElicitationSampling2 =
            abstract elicitation: Zod.ZodType with get, set
            abstract sampling: Zod.ZodType with get, set

        type _metaCreatedAtF1242d444 =
            abstract statusMessage: option<string> with get, set
            abstract pollInterval: option<float> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract lastUpdatedAt: string with get, set
            abstract createdAt: string with get, set
            abstract ttl: option<float> with get, set
            abstract status: TaskStatus with get, set
            abstract taskId: string with get, set

        type Name3 =
            abstract name: Zod.ZodType with get, set

        type _metaArgumentContextRef2 =
            abstract context: option<Arguments2> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract argument: NameValue3 with get, set
            abstract ref: U2<NameType3, TypesJs.ResourceReference> with get, set

        type _metaNextCursorPrompts =
            abstract prompts: Zod.ZodType with get, set
            abstract nextCursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaDescriptionMessages3 =
            abstract description: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract messages: ResizeArray<ContentRole6> with get, set

        type _metaArgumentsName =
            abstract arguments: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type MethodParams22 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaCreatedAtF1242d443 =
            abstract statusMessage: option<string> with get, set
            abstract pollInterval: option<float> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract lastUpdatedAt: string with get, set
            abstract createdAt: string with get, set
            abstract ttl: option<float> with get, set
            abstract status: TaskStatus with get, set
            abstract taskId: string with get, set

        type _def_e995c0ec<'Args, 'Output, 'S, 'T> =
            abstract _zod: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract isNullable: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract isOptional: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract readonly: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract pipe: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract describe: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract catch: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract brand: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("default")>]
            abstract ``default``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract transform: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("and")>]
            abstract ``and``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("or")>]
            abstract ``or``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract promise: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract array: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract nullish: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract nullable: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract optional: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract superRefine: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _refinement: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract refinement: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract refine: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract spa: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract safeParseAsync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract parseAsync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("~validate")>]
            abstract ``~validate``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract safeParse: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract parse: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _parseAsync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _parseSync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _processInputParams: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _getOrReturnCtx: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _getType: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _parse: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("~standard")>]
            abstract ``~standard``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract description: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _def: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _input: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _output: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _type: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

        type _metaContentIsErrorStructuredContent4 =
            abstract isError: option<bool> with get, set
            abstract structuredContent: option<obj> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract content: ResizeArray<ContentBlock> with get, set

        type ContextTools2 =
            abstract tools: Zod.ZodType with get, set
            abstract context: Zod.ZodType with get, set

        type _metaReasonRequestId =
            abstract reason: option<string> with get, set
            abstract requestId: option<U2<string, float>> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        type AcrValuesSupportedA3080752 =
            [<EmitProperty("client_id_metadata_document_supported")>]
            abstract clientIdMetadataDocumentSupported: Zod.ZodType with get, set

            [<EmitProperty("op_tos_uri")>]
            abstract opTosUri: Zod.ZodType with get, set

            [<EmitProperty("op_policy_uri")>]
            abstract opPolicyUri: Zod.ZodType with get, set

            [<EmitProperty("require_request_uri_registration")>]
            abstract requireRequestUriRegistration: Zod.ZodType with get, set

            [<EmitProperty("request_uri_parameter_supported")>]
            abstract requestUriParameterSupported: Zod.ZodType with get, set

            [<EmitProperty("request_parameter_supported")>]
            abstract requestParameterSupported: Zod.ZodType with get, set

            [<EmitProperty("claims_parameter_supported")>]
            abstract claimsParameterSupported: Zod.ZodType with get, set

            [<EmitProperty("ui_locales_supported")>]
            abstract uiLocalesSupported: Zod.ZodType with get, set

            [<EmitProperty("claims_locales_supported")>]
            abstract claimsLocalesSupported: Zod.ZodType with get, set

            [<EmitProperty("service_documentation")>]
            abstract serviceDocumentation: Zod.ZodType with get, set

            [<EmitProperty("claims_supported")>]
            abstract claimsSupported: Zod.ZodType with get, set

            [<EmitProperty("claim_types_supported")>]
            abstract claimTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("display_values_supported")>]
            abstract displayValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_signing_alg_values_supported")>]
            abstract tokenEndpointAuthSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_methods_supported")>]
            abstract tokenEndpointAuthMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("request_object_encryption_enc_values_supported")>]
            abstract requestObjectEncryptionEncValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("request_object_encryption_alg_values_supported")>]
            abstract requestObjectEncryptionAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("request_object_signing_alg_values_supported")>]
            abstract requestObjectSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("userinfo_encryption_enc_values_supported")>]
            abstract userinfoEncryptionEncValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("userinfo_encryption_alg_values_supported")>]
            abstract userinfoEncryptionAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("userinfo_signing_alg_values_supported")>]
            abstract userinfoSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("id_token_encryption_enc_values_supported")>]
            abstract idTokenEncryptionEncValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("id_token_encryption_alg_values_supported")>]
            abstract idTokenEncryptionAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("id_token_signing_alg_values_supported")>]
            abstract idTokenSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("subject_types_supported")>]
            abstract subjectTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("acr_values_supported")>]
            abstract acrValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("grant_types_supported")>]
            abstract grantTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("response_modes_supported")>]
            abstract responseModesSupported: Zod.ZodType with get, set

            [<EmitProperty("response_types_supported")>]
            abstract responseTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: Zod.ZodType with get, set

            [<EmitProperty("registration_endpoint")>]
            abstract registrationEndpoint: Zod.ZodType with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: Zod.ZodType with get, set

            [<EmitProperty("userinfo_endpoint")>]
            abstract userinfoEndpoint: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint")>]
            abstract tokenEndpoint: Zod.ZodType with get, set

            [<EmitProperty("authorization_endpoint")>]
            abstract authorizationEndpoint: Zod.ZodType with get, set

            abstract issuer: Zod.ZodType with get, set

        type MethodParams45 =
            abstract method: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set

        type ClientIdClientDde5cb292 =
            [<EmitProperty("client_secret_expires_at")>]
            abstract clientSecretExpiresAt: Zod.ZodType with get, set

            [<EmitProperty("client_id_issued_at")>]
            abstract clientIdIssuedAt: Zod.ZodType with get, set

            [<EmitProperty("client_secret")>]
            abstract clientSecret: Zod.ZodType with get, set

            [<EmitProperty("client_id")>]
            abstract clientId: Zod.ZodType with get, set

        type DestructiveHintIdempotentHi8e2ddc364 =
            abstract openWorldHint: option<bool> with get, set
            abstract idempotentHint: option<bool> with get, set
            abstract destructiveHint: option<bool> with get, set
            abstract readOnlyHint: option<bool> with get, set
            abstract title: option<string> with get, set

        type _metaMessage0e58bff5 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract message: option<string> with get, set
            abstract total: option<float> with get, set
            abstract progress: float with get, set
            abstract progressToken: Zod.ZodType with get, set

        type DateDateTimeEmailUri =
            [<EmitProperty("date-time")>]
            abstract dateTime: string with get, set

            abstract email: string with get, set
            abstract uri: string with get, set
            abstract date: string with get, set

        type MethodParams50 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaContentModelRoleStopReason6 =
            abstract stopReason: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract content: U6<_metaAnnotationsTextType3, _metaAnnotationsDataMimeTypeType5, _metaAnnotationsDataMimeTypeType6, _metaIdInputNameType, _metaContent8ac9a7d5, ResizeArray<SamplingMessageContentBlock>> with get, set
            abstract role: Role with get, set
            abstract model: string with get, set

        type MethodParams29 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaReasonRequestId2 =
            abstract reason: Zod.ZodType with get, set
            abstract requestId: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaDescriptionMessages2 =
            abstract messages: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type MethodParams52 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type MethodParams17 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type _metaActionContent3 =
            abstract content: option<obj> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract action: LiteralUnions.AcceptCancelDecline with get, set

        type ClientNameClientE372c1cc2 =
            [<EmitProperty("software_statement")>]
            abstract softwareStatement: Zod.ZodType with get, set

            [<EmitProperty("software_version")>]
            abstract softwareVersion: Zod.ZodType with get, set

            [<EmitProperty("software_id")>]
            abstract softwareId: Zod.ZodType with get, set

            abstract jwks: Zod.ZodType with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: Zod.ZodType with get, set

            [<EmitProperty("policy_uri")>]
            abstract policyUri: Zod.ZodType with get, set

            [<EmitProperty("tos_uri")>]
            abstract tosUri: Zod.ZodType with get, set

            abstract contacts: Zod.ZodType with get, set
            abstract scope: Zod.ZodType with get, set

            [<EmitProperty("logo_uri")>]
            abstract logoUri: Zod.ZodType with get, set

            [<EmitProperty("client_uri")>]
            abstract clientUri: Zod.ZodType with get, set

            [<EmitProperty("client_name")>]
            abstract clientName: Zod.ZodType with get, set

            [<EmitProperty("response_types")>]
            abstract responseTypes: Zod.ZodType with get, set

            [<EmitProperty("grant_types")>]
            abstract grantTypes: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_method")>]
            abstract tokenEndpointAuthMethod: Zod.ZodType with get, set

            [<EmitProperty("redirect_uris")>]
            abstract redirectUris: Zod.ZodType with get, set

        type TypeUri =
            abstract uri: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type HasMoreTotalValues =
            abstract hasMore: Zod.ZodType with get, set
            abstract total: Zod.ZodType with get, set
            abstract values: Zod.ZodType with get, set

        type AcrValuesSupported39a3d002 =
            [<EmitProperty("client_id_metadata_document_supported")>]
            abstract clientIdMetadataDocumentSupported: Zod.ZodType with get, set

            [<EmitProperty("op_tos_uri")>]
            abstract opTosUri: Zod.ZodType with get, set

            [<EmitProperty("op_policy_uri")>]
            abstract opPolicyUri: Zod.ZodType with get, set

            [<EmitProperty("require_request_uri_registration")>]
            abstract requireRequestUriRegistration: Zod.ZodType with get, set

            [<EmitProperty("request_uri_parameter_supported")>]
            abstract requestUriParameterSupported: Zod.ZodType with get, set

            [<EmitProperty("request_parameter_supported")>]
            abstract requestParameterSupported: Zod.ZodType with get, set

            [<EmitProperty("claims_parameter_supported")>]
            abstract claimsParameterSupported: Zod.ZodType with get, set

            [<EmitProperty("ui_locales_supported")>]
            abstract uiLocalesSupported: Zod.ZodType with get, set

            [<EmitProperty("claims_locales_supported")>]
            abstract claimsLocalesSupported: Zod.ZodType with get, set

            [<EmitProperty("service_documentation")>]
            abstract serviceDocumentation: Zod.ZodType with get, set

            [<EmitProperty("claims_supported")>]
            abstract claimsSupported: Zod.ZodType with get, set

            [<EmitProperty("claim_types_supported")>]
            abstract claimTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("display_values_supported")>]
            abstract displayValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_signing_alg_values_supported")>]
            abstract tokenEndpointAuthSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_methods_supported")>]
            abstract tokenEndpointAuthMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("request_object_encryption_enc_values_supported")>]
            abstract requestObjectEncryptionEncValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("request_object_encryption_alg_values_supported")>]
            abstract requestObjectEncryptionAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("request_object_signing_alg_values_supported")>]
            abstract requestObjectSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("userinfo_encryption_enc_values_supported")>]
            abstract userinfoEncryptionEncValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("userinfo_encryption_alg_values_supported")>]
            abstract userinfoEncryptionAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("userinfo_signing_alg_values_supported")>]
            abstract userinfoSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("id_token_encryption_enc_values_supported")>]
            abstract idTokenEncryptionEncValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("id_token_encryption_alg_values_supported")>]
            abstract idTokenEncryptionAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("id_token_signing_alg_values_supported")>]
            abstract idTokenSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("subject_types_supported")>]
            abstract subjectTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("acr_values_supported")>]
            abstract acrValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("grant_types_supported")>]
            abstract grantTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("response_modes_supported")>]
            abstract responseModesSupported: Zod.ZodType with get, set

            [<EmitProperty("response_types_supported")>]
            abstract responseTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: Zod.ZodType with get, set

            [<EmitProperty("registration_endpoint")>]
            abstract registrationEndpoint: Zod.ZodType with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: Zod.ZodType with get, set

            [<EmitProperty("userinfo_endpoint")>]
            abstract userinfoEndpoint: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint")>]
            abstract tokenEndpoint: Zod.ZodType with get, set

            [<EmitProperty("authorization_endpoint")>]
            abstract authorizationEndpoint: Zod.ZodType with get, set

            abstract issuer: Zod.ZodType with get, set

            [<EmitProperty("code_challenge_methods_supported")>]
            abstract codeChallengeMethodsSupported: Zod.ZodType with get, set

        type AlertCriticalDebugC0186d25 =
            abstract emergency: string with get, set
            abstract alert: string with get, set
            abstract critical: string with get, set
            abstract warning: string with get, set
            abstract notice: string with get, set
            abstract info: string with get, set
            abstract debug: string with get, set
            abstract error: string with get, set

        type _metaAuthInfo0d6b92d73 =
            abstract requestInfo: option<RequestInfo> with get, set
            abstract taskRequestedTtl: option<float> with get, set
            abstract taskStore: option<RequestTaskStore> with get, set
            abstract taskId: option<string> with get, set
            abstract requestId: ProgressToken with get, set
            abstract _meta: option<TypesJs.RequestMeta> with get, set
            abstract sessionId: option<string> with get, set
            abstract authInfo: option<AuthInfo> with get, set
            abstract signal: CloudflareWorkersTypes.AbortSignal with get, set
            abstract sendNotification: notification: obj -> Promise<unit>
            abstract sendRequest: request: obj * resultSchema: obj * ?options: TaskRequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
            abstract closeSSEStream: unit -> unit
            abstract closeStandaloneSSEStream: unit -> unit

        type _metaContent8ac9a7d52<'T> =
            abstract _meta: Zod.ZodType with get, set
            abstract isError: Zod.ZodType with get, set
            abstract structuredContent: Zod.ZodType with get, set
            abstract content: Zod.ZodType with get, set
            abstract toolUseId: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type _metaContents4 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract contents: ResizeArray<U2<_metaMimeTypeTextUri3, _metaBlobMimeTypeUri3>> with get, set

        type _metaArgumentsNameTask =
            abstract arguments: option<obj> with get, set
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract name: string with get, set

        type Name =
            abstract name: option<string> with get, set

        type ElicitationExperimentalExtFf9b7be33 =
            abstract extensions: Zod.ZodType with get, set
            abstract tasks: Zod.ZodType with get, set
            abstract roots: Zod.ZodType with get, set
            abstract elicitation: Zod.ZodType with get, set
            abstract sampling: Zod.ZodType with get, set
            abstract experimental: Zod.ZodType with get, set

        type DefaultDescriptionOneOfTitleType =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract oneOf: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams27 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type EndTurnMaxTokensStopSequence =
            abstract stopSequence: string with get, set
            abstract endTurn: string with get, set
            abstract maxTokens: string with get, set

        type AcrValuesSupported39a3d0022 =
            [<EmitProperty("client_id_metadata_document_supported")>]
            abstract clientIdMetadataDocumentSupported: option<bool> with get, set

            [<EmitProperty("op_tos_uri")>]
            abstract opTosUri: option<string> with get, set

            [<EmitProperty("op_policy_uri")>]
            abstract opPolicyUri: option<string> with get, set

            [<EmitProperty("require_request_uri_registration")>]
            abstract requireRequestUriRegistration: option<bool> with get, set

            [<EmitProperty("request_uri_parameter_supported")>]
            abstract requestUriParameterSupported: option<bool> with get, set

            [<EmitProperty("request_parameter_supported")>]
            abstract requestParameterSupported: option<bool> with get, set

            [<EmitProperty("claims_parameter_supported")>]
            abstract claimsParameterSupported: option<bool> with get, set

            [<EmitProperty("ui_locales_supported")>]
            abstract uiLocalesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("claims_locales_supported")>]
            abstract claimsLocalesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("service_documentation")>]
            abstract serviceDocumentation: option<string> with get, set

            [<EmitProperty("claims_supported")>]
            abstract claimsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("claim_types_supported")>]
            abstract claimTypesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("display_values_supported")>]
            abstract displayValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("token_endpoint_auth_signing_alg_values_supported")>]
            abstract tokenEndpointAuthSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("token_endpoint_auth_methods_supported")>]
            abstract tokenEndpointAuthMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("request_object_encryption_enc_values_supported")>]
            abstract requestObjectEncryptionEncValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("request_object_encryption_alg_values_supported")>]
            abstract requestObjectEncryptionAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("request_object_signing_alg_values_supported")>]
            abstract requestObjectSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("userinfo_encryption_enc_values_supported")>]
            abstract userinfoEncryptionEncValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("userinfo_encryption_alg_values_supported")>]
            abstract userinfoEncryptionAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("userinfo_signing_alg_values_supported")>]
            abstract userinfoSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("id_token_encryption_enc_values_supported")>]
            abstract idTokenEncryptionEncValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("id_token_encryption_alg_values_supported")>]
            abstract idTokenEncryptionAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("acr_values_supported")>]
            abstract acrValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("grant_types_supported")>]
            abstract grantTypesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("response_modes_supported")>]
            abstract responseModesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("registration_endpoint")>]
            abstract registrationEndpoint: option<string> with get, set

            [<EmitProperty("userinfo_endpoint")>]
            abstract userinfoEndpoint: option<string> with get, set

            [<EmitProperty("code_challenge_methods_supported")>]
            abstract codeChallengeMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("id_token_signing_alg_values_supported")>]
            abstract idTokenSigningAlgValuesSupported: ResizeArray<string> with get, set

            [<EmitProperty("subject_types_supported")>]
            abstract subjectTypesSupported: ResizeArray<string> with get, set

            [<EmitProperty("response_types_supported")>]
            abstract responseTypesSupported: ResizeArray<string> with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: string with get, set

            [<EmitProperty("token_endpoint")>]
            abstract tokenEndpoint: string with get, set

            [<EmitProperty("authorization_endpoint")>]
            abstract authorizationEndpoint: string with get, set

            abstract issuer: string with get, set

        type MethodParams6 =
            abstract params: _metaTaskId with get, set
            abstract method: string with get, set

        type MethodParams66 =
            abstract params: option<_metaCursor> with get, set
            abstract method: string with get, set

        type MethodParams9 =
            abstract params: _metaTaskId with get, set
            abstract method: string with get, set

        type ErrorErrorDescription =
            [<EmitProperty("error_description")>]
            abstract errorDescription: Zod.ZodType with get, set

            abstract error: Zod.ZodType with get, set

        type _metaContents3 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract contents: ResizeArray<U2<_metaMimeTypeTextUri3, _metaBlobMimeTypeUri3>> with get, set

        type DataErrorMessageValid<'T> =
            abstract errorMessage: unit with get
            abstract data: obj with get, set
            abstract valid: bool with get, set

        type _metaNextCursorResources3 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract resources: ResizeArray<_metaAnnotationsE54c02694> with get, set

        type Mode =
            abstract mode: option<LiteralUnions.AutoNoneRequired> with get, set

        type _metaNextCursorTasks2 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract tasks: ResizeArray<CreatedAtLastUpdatedAtPoll0097a3012> with get, set

        type IdJsonrpcResult3 =
            abstract result: SharedLiterals._meta2 with get, set
            abstract id: Zod.ZodType with get, set
            abstract jsonrpc: string with get, set

        type DefaultDescriptionTitleType =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams44 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type ContentRole6 =
            abstract content: ContentBlock with get, set
            abstract role: Role with get, set

        type MethodParams15 =
            abstract params: _metaDataLevelLogger with get, set
            abstract method: string with get, set

        type IdJsonrpcMethodParams3 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract id: Zod.ZodType with get, set
            abstract jsonrpc: string with get, set
            abstract method: string with get, set

        type _metaNextCursorTools2 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract tools: ResizeArray<_metaAnnotations08cb50cb2> with get, set

        type AllServersNoneThisServer =
            abstract allServers: string with get, set
            abstract thisServer: string with get, set
            abstract none: string with get, set

        type _metaNextCursorResources2 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract resources: ResizeArray<_metaAnnotationsE54c02694> with get, set

        type MethodParams33 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type ErrorIdJsonrpc2 =
            abstract error: Zod.ZodType with get, set
            abstract id: Zod.ZodType with get, set
            abstract jsonrpc: Zod.ZodType with get, set

        type CostPriorityHintsIntelligeFaccff052 =
            abstract intelligencePriority: Zod.ZodType with get, set
            abstract speedPriority: Zod.ZodType with get, set
            abstract costPriority: Zod.ZodType with get, set
            abstract hints: Zod.ZodType with get, set

        type ClientNameClientE372c1cc =
            [<EmitProperty("software_statement")>]
            abstract softwareStatement: option<string> with get, set

            [<EmitProperty("software_version")>]
            abstract softwareVersion: option<string> with get, set

            [<EmitProperty("software_id")>]
            abstract softwareId: option<string> with get, set

            abstract jwks: option<obj> with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: option<string> with get, set

            [<EmitProperty("policy_uri")>]
            abstract policyUri: option<string> with get, set

            [<EmitProperty("tos_uri")>]
            abstract tosUri: option<string> with get, set

            abstract contacts: option<ResizeArray<string>> with get, set
            abstract scope: option<string> with get, set

            [<EmitProperty("logo_uri")>]
            abstract logoUri: option<string> with get, set

            [<EmitProperty("client_uri")>]
            abstract clientUri: option<string> with get, set

            [<EmitProperty("client_name")>]
            abstract clientName: option<string> with get, set

            [<EmitProperty("response_types")>]
            abstract responseTypes: option<ResizeArray<string>> with get, set

            [<EmitProperty("grant_types")>]
            abstract grantTypes: option<ResizeArray<string>> with get, set

            [<EmitProperty("token_endpoint_auth_method")>]
            abstract tokenEndpointAuthMethod: option<string> with get, set

            [<EmitProperty("redirect_uris")>]
            abstract redirectUris: ResizeArray<string> with get, set

        type NameType3 =
            abstract name: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type _metaContentRole =
            abstract _meta: option<obj> with get, set
            abstract content: U6<_metaAnnotationsTextType3, _metaAnnotationsDataMimeTypeType5, _metaAnnotationsDataMimeTypeType6, _metaIdInputNameType, _metaContent8ac9a7d5, ResizeArray<SamplingMessageContentBlock>> with get, set
            abstract role: Role with get, set

        type _meta4 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a3> with get, set

        type DefaultDescriptionEnum429987952 =
            [<EmitProperty("default")>]
            abstract ``default``: option<string> with get, set

            abstract enumNames: option<ResizeArray<string>> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract enum: ResizeArray<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type MethodParams7 =
            abstract params: _metaTaskId with get, set
            abstract method: string with get, set

        type _metaContentRole2<'T> =
            abstract _meta: Zod.ZodType with get, set
            abstract content: Zod.ZodType with get, set
            abstract role: Zod.ZodType with get, set

        type MethodParams68 =
            abstract params: _metaUri with get, set
            abstract method: string with get, set

        type MethodParams54 =
            abstract params: _metaArgumentContextRef2 with get, set
            abstract method: string with get, set

        type MethodParams47 =
            abstract method: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set

        type MethodParams42 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaElicitationId =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract elicitationId: string with get, set

        type MethodParams60 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type MethodParams53 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaNextCursorResources =
            abstract resources: Zod.ZodType with get, set
            abstract nextCursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaArgumentContextRef3 =
            abstract context: option<Arguments2> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract argument: NameValue3 with get, set
            abstract ref: U2<NameType3, TypesJs.ResourceReference> with get, set

        type _metaContentModelRoleStopReason4 =
            abstract stopReason: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract content: U6<_metaAnnotationsTextType3, _metaAnnotationsDataMimeTypeType5, _metaAnnotationsDataMimeTypeType6, _metaIdInputNameType, _metaContent8ac9a7d5, ResizeArray<SamplingMessageContentBlock>> with get, set
            abstract role: Role with get, set
            abstract model: string with get, set

        type MethodParams21 =
            abstract params: _metaElicitationId with get, set
            abstract method: string with get, set

        type MethodParams31 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type MethodParams55 =
            abstract params: _metaUri with get, set
            abstract method: string with get, set

        type EnumType2 =
            abstract enum: ResizeArray<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type _metaContentIsErrorStructuredContent5 =
            abstract isError: option<bool> with get, set
            abstract structuredContent: option<obj> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract content: ResizeArray<ContentBlock> with get, set

        type _metaContents2<'T> =
            abstract contents: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaNextCursor =
            abstract nextCursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type InOut3 =
            [<EmitProperty("in")>]
            abstract ``in``: System.Collections.Generic.IDictionary<string, option<obj>> with get, set

            abstract out: System.Collections.Generic.IDictionary<string, option<obj>> with get, set

        type _metaAnnotationsDataMimeTypeType5 =
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract mimeType: string with get, set
            abstract data: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type MethodParams56 =
            abstract params: _metaUri with get, set
            abstract method: string with get, set

        type FormUrl2 =
            abstract url: Zod.ZodType with get, set
            abstract form: Zod.ZodType with get, set

        type Icons =
            abstract icons: Zod.ZodType with get, set

        type MethodParams10 =
            abstract params: _metaIncludeContextE2cd0689 with get, set
            abstract method: string with get, set

        type Create2 =
            abstract create: Zod.ZodType with get, set

        type _metaContentModelRoleStopReason2 =
            abstract content: Zod.ZodType with get, set
            abstract role: Zod.ZodType with get, set
            abstract stopReason: Zod.ZodType with get, set
            abstract model: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type MethodParams25 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type PollIntervalTtl2 =
            abstract pollInterval: Zod.ZodType with get, set
            abstract ttl: Zod.ZodType with get, set

        type MethodParams36 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaLevel2 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract level: LoggingLevel with get, set

        type MethodParams63 =
            abstract params: _metaLevel2 with get, set
            abstract method: string with get, set

        type AuthorizationEndpointClien6e624a62 =
            [<EmitProperty("client_id_metadata_document_supported")>]
            abstract clientIdMetadataDocumentSupported: Zod.ZodType with get, set

            [<EmitProperty("code_challenge_methods_supported")>]
            abstract codeChallengeMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("introspection_endpoint_auth_signing_alg_values_supported")>]
            abstract introspectionEndpointAuthSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("introspection_endpoint_auth_methods_supported")>]
            abstract introspectionEndpointAuthMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("introspection_endpoint")>]
            abstract introspectionEndpoint: Zod.ZodType with get, set

            [<EmitProperty("revocation_endpoint_auth_signing_alg_values_supported")>]
            abstract revocationEndpointAuthSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("revocation_endpoint_auth_methods_supported")>]
            abstract revocationEndpointAuthMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("revocation_endpoint")>]
            abstract revocationEndpoint: Zod.ZodType with get, set

            [<EmitProperty("service_documentation")>]
            abstract serviceDocumentation: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_signing_alg_values_supported")>]
            abstract tokenEndpointAuthSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_methods_supported")>]
            abstract tokenEndpointAuthMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("grant_types_supported")>]
            abstract grantTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("response_modes_supported")>]
            abstract responseModesSupported: Zod.ZodType with get, set

            [<EmitProperty("response_types_supported")>]
            abstract responseTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: Zod.ZodType with get, set

            [<EmitProperty("registration_endpoint")>]
            abstract registrationEndpoint: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint")>]
            abstract tokenEndpoint: Zod.ZodType with get, set

            [<EmitProperty("authorization_endpoint")>]
            abstract authorizationEndpoint: Zod.ZodType with get, set

            abstract issuer: Zod.ZodType with get, set

        type TaskSupport3 =
            abstract taskSupport: Zod.ZodType with get, set

        type _metaTask =
            abstract task: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type RelatedRequestIdRelatedTask =
            abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
            abstract relatedRequestId: option<ProgressToken> with get, set

        type Tools2 =
            abstract tools: Zod.ZodType with get, set

        type _metaCursor2 =
            abstract cursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type IntegerNumber =
            abstract integer: string with get, set
            abstract number: string with get, set

        type NameValue3 =
            abstract value: string with get, set
            abstract name: string with get, set

        type ApplyDefaults2 =
            abstract applyDefaults: Zod.ZodType with get, set

        type NextCursorTasks =
            abstract nextCursor: option<string> with get, set
            abstract tasks: ResizeArray<TypesJs.Task> with get, set

        type _metaCompletion2 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract completion: HasMoreTotalValues2 with get, set

        type _metaContentModelRoleStopReason<'T> =
            abstract content: Zod.ZodType with get, set
            abstract role: Zod.ZodType with get, set
            abstract stopReason: Zod.ZodType with get, set
            abstract model: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaCapabilitiesA112f199 =
            abstract instructions: Zod.ZodType with get, set
            abstract serverInfo: Zod.ZodType with get, set
            abstract capabilities: Zod.ZodType with get, set
            abstract protocolVersion: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type MethodParams46 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaTask3 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract task: CreatedAtLastUpdatedAtPoll0097a3012 with get, set

        type PropertiesRequiredType2<'T> =
            abstract required: Zod.ZodType with get, set
            abstract properties: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams59 =
            abstract params: _metaCapabilities64dfe9033 with get, set
            abstract method: string with get, set

        type MethodParams67 =
            abstract params: option<_metaCursor> with get, set
            abstract method: string with get, set

        type _metaRoots2 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract roots: ResizeArray<_metaNameUri2> with get, set

        type _metaArgumentContextRef<'T> =
            abstract context: Zod.ZodType with get, set
            abstract argument: Zod.ZodType with get, set
            abstract ref: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type CallbackEnabledMetadataNameTitleUri =
            abstract enabled: option<bool> with get, set
            abstract metadata: option<ResourceMetadata> with get, set
            abstract uri: option<string> with get, set
            abstract title: option<string> with get, set
            abstract name: option<string> with get, set
            abstract callback: uri: CloudflareWorkersTypes.URL * extra: _metaAuthInfo0d6b92d73 -> U2<TypesJs.ReadResourceResult, Promise<TypesJs.ReadResourceResult>>

        type MethodParams5 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type MethodParams11 =
            abstract params: ElicitRequestParams with get, set
            abstract method: string with get, set

        type InOut4 =
            [<EmitProperty("in")>]
            abstract ``in``: System.Collections.Generic.IDictionary<string, option<U2<proptypekey<proptypekey<obj, string>, string>, obj>>> with get, set

            abstract out: System.Collections.Generic.IDictionary<string, option<U2<proptypekey<proptypekey<obj, string>, string>, obj>>> with get, set

        type MethodParams62 =
            abstract params: _metaArgumentContextRef2 with get, set
            abstract method: string with get, set

        type DefaultDescriptionItems834741e23 =
            [<EmitProperty("default")>]
            abstract ``default``: option<ResizeArray<string>> with get, set

            abstract maxItems: option<float> with get, set
            abstract minItems: option<float> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract items: EnumType2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type MethodParams38 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type ForbiddenOptionalRequired =
            abstract forbidden: string with get, set
            abstract required: string with get, set
            abstract optional: string with get, set

        type DestructiveHintIdempotentHi8e2ddc363 =
            abstract openWorldHint: Zod.ZodType with get, set
            abstract idempotentHint: Zod.ZodType with get, set
            abstract destructiveHint: Zod.ZodType with get, set
            abstract readOnlyHint: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

        type MethodParams32 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type Ttl2 =
            abstract ttl: Zod.ZodType with get, set

        type _metaMessageModeRequestedSchemaTask3 =
            abstract mode: option<string> with get, set
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract requestedSchema: SharedLiterals.PropertiesRequiredType with get, set
            abstract message: string with get, set

        type _metaCursor =
            abstract cursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        type NameType4 =
            abstract name: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type _meta3 =
            abstract _meta: Zod.ZodType with get, set

        type MethodParams40 =
            abstract method: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set

        type _metaAnnotationsA03c34943 =
            abstract title: option<string> with get, set
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract size: option<float> with get, set
            abstract mimeType: option<string> with get, set
            abstract description: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract name: string with get, set
            abstract uri: string with get, set

        type TokenTokenTypeHint =
            [<EmitProperty("token_type_hint")>]
            abstract tokenTypeHint: Zod.ZodType with get, set

            abstract token: Zod.ZodType with get, set

        type _metaActionContent2<'T> =
            abstract content: Zod.ZodType with get, set
            abstract action: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type Send =
            abstract send: eventId: string * message: JSONRPCMessage -> Promise<unit>

        type DefaultDescriptionEnum42998795 =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract enumNames: Zod.ZodType with get, set
            abstract enum: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type _metaArgumentsName2 =
            abstract arguments: option<obj> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract name: string with get, set

        type ConstTitle2 =
            abstract title: string with get, set

            [<EmitProperty("const")>]
            abstract ``const``: string with get, set

        type _metaCompletion =
            abstract completion: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaTaskId =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract taskId: string with get, set

        type Call2 =
            abstract call: Zod.ZodType with get, set

        type _def_e995c0ec2<'Args, 'Output, 'S, 'T> =
            abstract _zod: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract isNullable: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract isOptional: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract readonly: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract pipe: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract describe: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract catch: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract brand: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("default")>]
            abstract ``default``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract transform: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("and")>]
            abstract ``and``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("or")>]
            abstract ``or``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract promise: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract array: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract nullish: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract nullable: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract optional: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract superRefine: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _refinement: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract refinement: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract refine: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract spa: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract safeParseAsync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract parseAsync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("~validate")>]
            abstract ``~validate``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract safeParse: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract parse: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _parseAsync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _parseSync: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _processInputParams: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _getOrReturnCtx: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _getType: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _parse: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            [<EmitProperty("~standard")>]
            abstract ``~standard``: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

            abstract description: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _def: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _input: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _output: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set
            abstract _type: option<U3<proptypekey<'S, string>, proptypekey<proptypekey<obj, string>, string>, obj>> with get, set

        type CodeDataMessage2 =
            abstract data: Zod.ZodType with get, set
            abstract message: Zod.ZodType with get, set
            abstract code: Zod.ZodType with get, set

        type MethodParams23 =
            abstract method: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set

        type DefaultDescriptionEnumTitleType2 =
            [<EmitProperty("default")>]
            abstract ``default``: option<string> with get, set

            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract enum: ResizeArray<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type _metaAnnotationsE54c02693 =
            abstract title: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract icons: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set
            abstract annotations: Zod.ZodType with get, set
            abstract size: Zod.ZodType with get, set
            abstract mimeType: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract uri: Zod.ZodType with get, set

        type MethodParams43 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type CreatedAtLastUpdatedAtPoll0097a3012 =
            abstract statusMessage: option<string> with get, set
            abstract pollInterval: option<float> with get, set
            abstract lastUpdatedAt: string with get, set
            abstract createdAt: string with get, set
            abstract ttl: option<float> with get, set
            abstract status: TaskStatus with get, set
            abstract taskId: string with get, set

        type _metaCapabilitiesA112f1992 =
            abstract instructions: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract serverInfo: SharedLiterals.DescriptionIconsName444a27cf with get, set
            abstract capabilities: CompletionsExperimentalExtA1abd3ff3 with get, set
            abstract protocolVersion: string with get, set

        type _metaNextCursorTasks =
            abstract tasks: Zod.ZodType with get, set
            abstract nextCursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type CompletionsExperimentalExtA1abd3ff2 =
            abstract extensions: Zod.ZodType with get, set
            abstract tasks: Zod.ZodType with get, set
            abstract tools: Zod.ZodType with get, set
            abstract resources: Zod.ZodType with get, set
            abstract prompts: Zod.ZodType with get, set
            abstract completions: Zod.ZodType with get, set
            abstract logging: Zod.ZodType with get, set
            abstract experimental: Zod.ZodType with get, set

        type NameTitle =
            abstract title: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set

        type DefaultDescriptionFormat568a672d =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract format: Zod.ZodType with get, set
            abstract maxLength: Zod.ZodType with get, set
            abstract minLength: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams30 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type PropertiesRequiredType8 =
            abstract required: Zod.ZodType with get, set
            abstract properties: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams20 =
            abstract params: _metaCreatedAtF1242d44 with get, set
            abstract method: string with get, set

        type _metaAnnotationsResourceType3 =
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract resource: U2<_metaMimeTypeTextUri3, _metaBlobMimeTypeUri3> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type TypeUri2 =
            abstract uri: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type MethodParams12 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type _metaMessage0e58bff52 =
            abstract _meta: Zod.ZodType with get, set
            abstract message: Zod.ZodType with get, set
            abstract total: Zod.ZodType with get, set
            abstract progress: Zod.ZodType with get, set
            abstract progressToken: Zod.ZodType with get, set

        type MethodParams57 =
            abstract params: _metaArgumentsNameTask with get, set
            abstract method: string with get, set

        type _metaDescriptionMessages4 =
            abstract description: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract messages: ResizeArray<ContentRole6> with get, set

        type ErrorErrorDescriptionErrorUri2 =
            [<EmitProperty("error_uri")>]
            abstract errorUri: option<string> with get, set

            [<EmitProperty("error_description")>]
            abstract errorDescription: option<string> with get, set

            abstract error: string with get, set

        type _metaUri =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract uri: string with get, set

        type Mode2 =
            abstract mode: Zod.ZodType with get, set

        type Arguments2 =
            abstract arguments: option<obj> with get, set

        type _metaNextCursorResourceTemplates2 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract resourceTemplates: ResizeArray<_metaAnnotationsE7edce494> with get, set

        type MethodParams4<'T> =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaContentModelRoleStopReason5 =
            abstract stopReason: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract content: SamplingContent with get, set
            abstract role: Role with get, set
            abstract model: string with get, set

        type _metaUri2 =
            abstract uri: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaIncludeContextE2cd06893 =
            abstract toolChoice: option<Mode> with get, set
            abstract tools: option<ResizeArray<_metaAnnotations08cb50cb2>> with get, set
            abstract metadata: option<Erased.Empty> with get, set
            abstract stopSequences: option<ResizeArray<string>> with get, set
            abstract temperature: option<float> with get, set
            abstract includeContext: option<LiteralUnions.AllServersNoneThisServer> with get, set
            abstract systemPrompt: option<string> with get, set
            abstract modelPreferences: option<CostPriorityHintsIntelligeFaccff05> with get, set
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract maxTokens: float with get, set
            abstract messages: ResizeArray<_metaContentRole> with get, set

        type MethodParams26 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaNameUri2 =
            abstract _meta: option<obj> with get, set
            abstract name: option<string> with get, set
            abstract uri: string with get, set

        type MaxTotalTimeoutOnprogressR4c149998 =
            abstract relatedTask: option<TypesJs.RelatedTaskMetadata> with get, set
            abstract task: option<TypesJs.TaskCreationParams> with get, set
            abstract maxTotalTimeout: option<float> with get, set
            abstract resetTimeoutOnProgress: option<bool> with get, set
            abstract timeout: option<float> with get, set
            abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
            abstract onprogress: progress: TypesJs.Progress -> unit

        type _metaNextCursorPrompts2 =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract prompts: ResizeArray<_metaArgumentsE5be7b5f4> with get, set

        type _metaArgumentsE5be7b5f4 =
            abstract title: option<string> with get, set
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set
            abstract _meta: option<obj> with get, set
            abstract arguments: option<ResizeArray<SharedLiterals.DescriptionNameRequired>> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set

        type _metaAnnotations08cb50cb2 =
            abstract title: option<string> with get, set
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set
            abstract _meta: option<obj> with get, set
            abstract execution: option<SharedLiterals.TaskSupport> with get, set
            abstract annotations: option<SharedLiterals.DestructiveHintIdempotentHi8e2ddc36> with get, set
            abstract outputSchema: option<SharedLiterals.PropertiesRequiredType3> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set
            abstract inputSchema: SharedLiterals.PropertiesRequiredType3 with get, set

        type DefaultDescriptionEnumTitleType =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract enum: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type _metaTaskId2 =
            abstract taskId: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaElicitationId2 =
            abstract elicitationId: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type NameValue2 =
            abstract value: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set

        type MethodParams19 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type _metaMessageModeRequestedSchemaTask2 =
            abstract requestedSchema: Zod.ZodType with get, set
            abstract message: Zod.ZodType with get, set
            abstract mode: Zod.ZodType with get, set
            abstract task: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaCapabilities64dfe9033 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract clientInfo: SharedLiterals.DescriptionIconsName444a27cf with get, set
            abstract capabilities: SharedLiterals.ElicitationExperimentalExtFf9b7be32 with get, set
            abstract protocolVersion: string with get, set

        type MethodParams39 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type ErrorErrorDescriptionErrorUri =
            [<EmitProperty("error_uri")>]
            abstract errorUri: Zod.ZodType with get, set

            [<EmitProperty("error_description")>]
            abstract errorDescription: Zod.ZodType with get, set

            abstract error: Zod.ZodType with get, set

        type CreatedAtLastUpdatedAtPoll0097a301<'T> =
            abstract statusMessage: Zod.ZodType with get, set
            abstract pollInterval: Zod.ZodType with get, set
            abstract lastUpdatedAt: Zod.ZodType with get, set
            abstract createdAt: Zod.ZodType with get, set
            abstract ttl: Zod.ZodType with get, set
            abstract status: Zod.ZodType with get, set
            abstract taskId: Zod.ZodType with get, set

        type ClientIdClient68f9a9cd2 =
            [<EmitProperty("client_secret_expires_at")>]
            abstract clientSecretExpiresAt: Zod.ZodType with get, set

            [<EmitProperty("client_id_issued_at")>]
            abstract clientIdIssuedAt: Zod.ZodType with get, set

            [<EmitProperty("client_secret")>]
            abstract clientSecret: Zod.ZodType with get, set

            [<EmitProperty("client_id")>]
            abstract clientId: Zod.ZodType with get, set

            [<EmitProperty("software_statement")>]
            abstract softwareStatement: Zod.ZodType with get, set

            [<EmitProperty("software_version")>]
            abstract softwareVersion: Zod.ZodType with get, set

            [<EmitProperty("software_id")>]
            abstract softwareId: Zod.ZodType with get, set

            abstract jwks: Zod.ZodType with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: Zod.ZodType with get, set

            [<EmitProperty("policy_uri")>]
            abstract policyUri: Zod.ZodType with get, set

            [<EmitProperty("tos_uri")>]
            abstract tosUri: Zod.ZodType with get, set

            abstract contacts: Zod.ZodType with get, set
            abstract scope: Zod.ZodType with get, set

            [<EmitProperty("logo_uri")>]
            abstract logoUri: Zod.ZodType with get, set

            [<EmitProperty("client_uri")>]
            abstract clientUri: Zod.ZodType with get, set

            [<EmitProperty("client_name")>]
            abstract clientName: Zod.ZodType with get, set

            [<EmitProperty("response_types")>]
            abstract responseTypes: Zod.ZodType with get, set

            [<EmitProperty("grant_types")>]
            abstract grantTypes: Zod.ZodType with get, set

            [<EmitProperty("token_endpoint_auth_method")>]
            abstract tokenEndpointAuthMethod: Zod.ZodType with get, set

            [<EmitProperty("redirect_uris")>]
            abstract redirectUris: Zod.ZodType with get, set

        type MethodParams35 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type ConstTitle =
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("const")>]
            abstract ``const``: Zod.ZodType with get, set

        type MethodParams41 =
            abstract method: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set

        type JsonrpcMethodParams3 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract jsonrpc: string with get, set
            abstract method: string with get, set

        type MethodParams64 =
            abstract params: _metaArgumentsName2 with get, set
            abstract method: string with get, set

        type CancelListRequests4 =
            abstract requests: Zod.ZodType with get, set
            abstract cancel: Zod.ZodType with get, set
            abstract list: Zod.ZodType with get, set

        type _metaCreatedAtF1242d442<'T> =
            abstract statusMessage: Zod.ZodType with get, set
            abstract pollInterval: Zod.ZodType with get, set
            abstract lastUpdatedAt: Zod.ZodType with get, set
            abstract createdAt: Zod.ZodType with get, set
            abstract ttl: Zod.ZodType with get, set
            abstract status: Zod.ZodType with get, set
            abstract taskId: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type DefaultDescriptionMaximum2f22e1f5 =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract maximum: Zod.ZodType with get, set
            abstract minimum: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type CancelListRequests3 =
            abstract requests: Zod.ZodType with get, set
            abstract cancel: Zod.ZodType with get, set
            abstract list: Zod.ZodType with get, set

        type _metaAnnotationsTextType3 =
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract text: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type DefaultDescriptionItems834741e24 =
            [<EmitProperty("default")>]
            abstract ``default``: option<ResizeArray<string>> with get, set

            abstract maxItems: option<float> with get, set
            abstract minItems: option<float> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract items: AnyOf2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type AccessTokenExpiresF88d3e322 =
            [<EmitProperty("refresh_token")>]
            abstract refreshToken: Zod.ZodType with get, set

            abstract scope: Zod.ZodType with get, set

            [<EmitProperty("expires_in")>]
            abstract expiresIn: Zod.ZodType with get, set

            [<EmitProperty("token_type")>]
            abstract tokenType: Zod.ZodType with get, set

            [<EmitProperty("id_token")>]
            abstract idToken: Zod.ZodType with get, set

            [<EmitProperty("access_token")>]
            abstract accessToken: Zod.ZodType with get, set

        type EnumType =
            abstract enum: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams51 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type ArgsSchemaCallbackDescript5bdc466b<'Args> =
            abstract enabled: option<bool> with get, set
            abstract callback: option<U2<obj -> _metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>, _metaAuthInfo0d6b92d73 -> U2<TypesJs.GetPromptResult, Promise<TypesJs.GetPromptResult>>>> with get, set
            abstract argsSchema: option<'Args> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract name: option<string> with get, set

        type EndTurnMaxTokensStopSequenceToolUse =
            abstract toolUse: string with get, set
            abstract stopSequence: string with get, set
            abstract endTurn: string with get, set
            abstract maxTokens: string with get, set

        type HasMoreTotalValues2 =
            abstract hasMore: option<bool> with get, set
            abstract total: option<float> with get, set
            abstract values: ResizeArray<string> with get, set

        type ContentRole5<'T> =
            abstract content: Zod.ZodType with get, set
            abstract role: Zod.ZodType with get, set

        type MethodParams65 =
            abstract params: option<_metaCursor> with get, set
            abstract method: string with get, set

        type CreateMessage2 =
            abstract createMessage: Zod.ZodType with get, set

        type DescriptionNameRequired3 =
            abstract required: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set

        type _metaAnnotations83faa3cf<'InputArgs, 'OutputArgs, 'Args, 'Output> =
            abstract enabled: option<bool> with get, set
            abstract callback: option<U3<_def_e995c0ec<'Args, 'Output, obj, obj> -> obj -> U2<TypesJs.Result, Promise<obj>>, option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>> -> obj -> U2<TypesJs.Result, Promise<obj>>, obj -> U2<TypesJs.Result, Promise<obj>>>> with get, set
            abstract _meta: option<obj> with get, set
            abstract annotations: option<TypesJs.ToolAnnotations> with get, set
            abstract outputSchema: option<'OutputArgs> with get, set
            abstract paramsSchema: option<'InputArgs> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract name: option<string> with get, set

        type DefaultDescriptionItems834741e22 =
            [<EmitProperty("default")>]
            abstract ``default``: Zod.ZodType with get, set

            abstract items: Zod.ZodType with get, set
            abstract maxItems: Zod.ZodType with get, set
            abstract minItems: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract title: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type CancelledCompletedFailedAee391f7 =
            abstract cancelled: string with get, set
            abstract failed: string with get, set
            abstract completed: string with get, set

            [<EmitProperty("input_required")>]
            abstract inputRequired: string with get, set

            abstract working: string with get, set

        type _metaRoots =
            abstract roots: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type AnyOf2 =
            abstract anyOf: ResizeArray<ConstTitle2> with get, set

        type _metaLevel =
            abstract level: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type NameType2 =
            abstract name: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type MethodParams49 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaCreatedAtF1242d44 =
            abstract statusMessage: option<string> with get, set
            abstract pollInterval: option<float> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract lastUpdatedAt: string with get, set
            abstract createdAt: string with get, set
            abstract ttl: option<float> with get, set
            abstract status: TaskStatus with get, set
            abstract taskId: string with get, set

        type _metaAnnotationsDataMimeTypeType6 =
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract mimeType: string with get, set
            abstract data: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type TaskSupport4 =
            abstract taskSupport: option<LiteralUnions.ForbiddenOptionalRequired> with get, set

        type _metaNameUri =
            abstract _meta: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract uri: Zod.ZodType with get, set

        type _metaArgumentsE5be7b5f3 =
            abstract title: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract icons: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set
            abstract arguments: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set

        type _metaMimeTypeUri =
            abstract _meta: Zod.ZodType with get, set
            abstract mimeType: Zod.ZodType with get, set
            abstract uri: Zod.ZodType with get, set

        type _metaTask4 =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract task: CreatedAtLastUpdatedAtPoll0097a3012 with get, set

        type _metaArgumentsNameTask2 =
            abstract arguments: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract task: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaAnnotationsE7edce494 =
            abstract title: option<string> with get, set
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract mimeType: option<string> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set
            abstract uriTemplate: string with get, set

        type ClientIdClient68f9a9cd =
            [<EmitProperty("client_secret_expires_at")>]
            abstract clientSecretExpiresAt: option<float> with get, set

            [<EmitProperty("client_id_issued_at")>]
            abstract clientIdIssuedAt: option<float> with get, set

            [<EmitProperty("client_secret")>]
            abstract clientSecret: option<string> with get, set

            [<EmitProperty("software_statement")>]
            abstract softwareStatement: option<string> with get, set

            [<EmitProperty("software_version")>]
            abstract softwareVersion: option<string> with get, set

            [<EmitProperty("software_id")>]
            abstract softwareId: option<string> with get, set

            abstract jwks: option<obj> with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: option<string> with get, set

            [<EmitProperty("policy_uri")>]
            abstract policyUri: option<string> with get, set

            [<EmitProperty("tos_uri")>]
            abstract tosUri: option<string> with get, set

            abstract contacts: option<ResizeArray<string>> with get, set
            abstract scope: option<string> with get, set

            [<EmitProperty("logo_uri")>]
            abstract logoUri: option<string> with get, set

            [<EmitProperty("client_uri")>]
            abstract clientUri: option<string> with get, set

            [<EmitProperty("client_name")>]
            abstract clientName: option<string> with get, set

            [<EmitProperty("response_types")>]
            abstract responseTypes: option<ResizeArray<string>> with get, set

            [<EmitProperty("grant_types")>]
            abstract grantTypes: option<ResizeArray<string>> with get, set

            [<EmitProperty("token_endpoint_auth_method")>]
            abstract tokenEndpointAuthMethod: option<string> with get, set

            [<EmitProperty("client_id")>]
            abstract clientId: string with get, set

            [<EmitProperty("redirect_uris")>]
            abstract redirectUris: ResizeArray<string> with get, set

        type ListChanged2 =
            abstract listChanged: Zod.ZodType with get, set

        type _metaAnnotationsE7edce493 =
            abstract title: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract icons: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set
            abstract annotations: Zod.ZodType with get, set
            abstract mimeType: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract uriTemplate: Zod.ZodType with get, set

        type _metaNextCursorTools =
            abstract tools: Zod.ZodType with get, set
            abstract nextCursor: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type DefaultDescriptionOneOfTitleType2 =
            [<EmitProperty("default")>]
            abstract ``default``: option<string> with get, set

            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract oneOf: ResizeArray<ConstTitle2> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type MethodParams48 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaMimeTypeTextUri3 =
            abstract _meta: option<obj> with get, set
            abstract mimeType: option<string> with get, set
            abstract text: string with get, set
            abstract uri: string with get, set

        type _metaIncludeContextE2cd0689 =
            abstract toolChoice: option<Mode> with get, set
            abstract tools: option<ResizeArray<_metaAnnotations08cb50cb2>> with get, set
            abstract metadata: option<Erased.Empty> with get, set
            abstract stopSequences: option<ResizeArray<string>> with get, set
            abstract temperature: option<float> with get, set
            abstract includeContext: option<LiteralUnions.AllServersNoneThisServer> with get, set
            abstract systemPrompt: option<string> with get, set
            abstract modelPreferences: option<CostPriorityHintsIntelligeFaccff05> with get, set
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract maxTokens: float with get, set
            abstract messages: ResizeArray<_metaContentRole> with get, set

        type AuthorizationEndpointClien6e624a622 =
            [<EmitProperty("client_id_metadata_document_supported")>]
            abstract clientIdMetadataDocumentSupported: option<bool> with get, set

            [<EmitProperty("code_challenge_methods_supported")>]
            abstract codeChallengeMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("introspection_endpoint_auth_signing_alg_values_supported")>]
            abstract introspectionEndpointAuthSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("introspection_endpoint_auth_methods_supported")>]
            abstract introspectionEndpointAuthMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("introspection_endpoint")>]
            abstract introspectionEndpoint: option<string> with get, set

            [<EmitProperty("revocation_endpoint_auth_signing_alg_values_supported")>]
            abstract revocationEndpointAuthSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("revocation_endpoint_auth_methods_supported")>]
            abstract revocationEndpointAuthMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("revocation_endpoint")>]
            abstract revocationEndpoint: option<string> with get, set

            [<EmitProperty("service_documentation")>]
            abstract serviceDocumentation: option<string> with get, set

            [<EmitProperty("token_endpoint_auth_signing_alg_values_supported")>]
            abstract tokenEndpointAuthSigningAlgValuesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("token_endpoint_auth_methods_supported")>]
            abstract tokenEndpointAuthMethodsSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("grant_types_supported")>]
            abstract grantTypesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("response_modes_supported")>]
            abstract responseModesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: option<ResizeArray<string>> with get, set

            [<EmitProperty("registration_endpoint")>]
            abstract registrationEndpoint: option<string> with get, set

            [<EmitProperty("response_types_supported")>]
            abstract responseTypesSupported: ResizeArray<string> with get, set

            [<EmitProperty("token_endpoint")>]
            abstract tokenEndpoint: string with get, set

            [<EmitProperty("authorization_endpoint")>]
            abstract authorizationEndpoint: string with get, set

            abstract issuer: string with get, set

        type _metaDataLevelLogger =
            abstract logger: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract data: option<obj> with get, set
            abstract level: LoggingLevel with get, set

        type MethodParams61 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type MethodParams58 =
            abstract params: option<_metaCursor> with get, set
            abstract method: string with get, set

        type _metaAnnotations08cb50cb3 =
            abstract title: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract icons: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set
            abstract execution: Zod.ZodType with get, set
            abstract annotations: Zod.ZodType with get, set
            abstract outputSchema: Zod.ZodType with get, set
            abstract inputSchema: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set

        type AuthorizationDetailsTypesFc177e16 =
            [<EmitProperty("dpop_bound_access_tokens_required")>]
            abstract dpopBoundAccessTokensRequired: Zod.ZodType with get, set

            [<EmitProperty("dpop_signing_alg_values_supported")>]
            abstract dpopSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("authorization_details_types_supported")>]
            abstract authorizationDetailsTypesSupported: Zod.ZodType with get, set

            [<EmitProperty("tls_client_certificate_bound_access_tokens")>]
            abstract tlsClientCertificateBoundAccessTokens: Zod.ZodType with get, set

            [<EmitProperty("resource_tos_uri")>]
            abstract resourceTosUri: Zod.ZodType with get, set

            [<EmitProperty("resource_policy_uri")>]
            abstract resourcePolicyUri: Zod.ZodType with get, set

            [<EmitProperty("resource_documentation")>]
            abstract resourceDocumentation: Zod.ZodType with get, set

            [<EmitProperty("resource_name")>]
            abstract resourceName: Zod.ZodType with get, set

            [<EmitProperty("resource_signing_alg_values_supported")>]
            abstract resourceSigningAlgValuesSupported: Zod.ZodType with get, set

            [<EmitProperty("bearer_methods_supported")>]
            abstract bearerMethodsSupported: Zod.ZodType with get, set

            [<EmitProperty("scopes_supported")>]
            abstract scopesSupported: Zod.ZodType with get, set

            [<EmitProperty("jwks_uri")>]
            abstract jwksUri: Zod.ZodType with get, set

            [<EmitProperty("authorization_servers")>]
            abstract authorizationServers: Zod.ZodType with get, set

            abstract resource: Zod.ZodType with get, set

        type _metaAnnotationsE54c02694 =
            abstract title: option<string> with get, set
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract size: option<float> with get, set
            abstract mimeType: option<string> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set
            abstract uri: string with get, set

        type _metaIdInputNameType =
            abstract _meta: option<obj> with get, set
            abstract input: obj with get, set
            abstract id: string with get, set
            abstract name: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type AnyOf =
            abstract anyOf: Zod.ZodType with get, set

        type _metaElicitationId20f972a83 =
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract url: string with get, set
            abstract elicitationId: string with get, set
            abstract message: string with get, set
            abstract mode: string with get, set

        type _metaCapabilities64dfe9032 =
            abstract clientInfo: Zod.ZodType with get, set
            abstract capabilities: Zod.ZodType with get, set
            abstract protocolVersion: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type _metaDataLevelLogger2 =
            abstract data: Zod.ZodType with get, set
            abstract logger: Zod.ZodType with get, set
            abstract level: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type IdJsonrpcResult2 =
            abstract result: Zod.ZodType with get, set
            abstract id: Zod.ZodType with get, set
            abstract jsonrpc: Zod.ZodType with get, set

        type _metaBlobMimeTypeUri3 =
            abstract _meta: option<obj> with get, set
            abstract mimeType: option<string> with get, set
            abstract blob: string with get, set
            abstract uri: string with get, set

        type AccessTokenExpiresF88d3e32 =
            [<EmitProperty("refresh_token")>]
            abstract refreshToken: option<string> with get, set

            abstract scope: option<string> with get, set

            [<EmitProperty("expires_in")>]
            abstract expiresIn: option<float> with get, set

            [<EmitProperty("id_token")>]
            abstract idToken: option<string> with get, set

            [<EmitProperty("token_type")>]
            abstract tokenType: string with get, set

            [<EmitProperty("access_token")>]
            abstract accessToken: string with get, set

        type _metaAuthInfo0d6b92d72 =
            abstract requestInfo: option<RequestInfo> with get, set
            abstract taskRequestedTtl: option<float> with get, set
            abstract taskStore: option<RequestTaskStore> with get, set
            abstract taskId: option<string> with get, set
            abstract requestId: ProgressToken with get, set
            abstract _meta: option<TypesJs.RequestMeta> with get, set
            abstract sessionId: option<string> with get, set
            abstract authInfo: option<AuthInfo> with get, set
            abstract signal: CloudflareWorkersTypes.AbortSignal with get, set
            abstract sendNotification: notification: obj -> Promise<unit>
            abstract sendRequest: request: obj * resultSchema: obj * ?options: TaskRequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
            abstract closeSSEStream: unit -> unit
            abstract closeStandaloneSSEStream: unit -> unit

        type Arguments3 =
            abstract arguments: option<obj> with get, set

        type MethodParams34 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type _metaElicitationId20f972a82 =
            abstract url: Zod.ZodType with get, set
            abstract elicitationId: Zod.ZodType with get, set
            abstract message: Zod.ZodType with get, set
            abstract mode: Zod.ZodType with get, set
            abstract task: Zod.ZodType with get, set
            abstract _meta: Zod.ZodType with get, set

        type IoModelcontextprotocolRelaF70aae9a4 =
            [<EmitProperty("io.modelcontextprotocol/related-task")>]
            abstract ``io.modelcontextprotocol/relatedTask``: option<SharedLiterals.TaskId> with get, set

            abstract progressToken: option<U2<string, float>> with get, set

        type ClientIdClientDde5cb29 =
            [<EmitProperty("client_secret_expires_at")>]
            abstract clientSecretExpiresAt: option<float> with get, set

            [<EmitProperty("client_id_issued_at")>]
            abstract clientIdIssuedAt: option<float> with get, set

            [<EmitProperty("client_secret")>]
            abstract clientSecret: option<string> with get, set

            [<EmitProperty("client_id")>]
            abstract clientId: string with get, set

        type MethodParams13 =
            abstract params: _metaReasonRequestId with get, set
            abstract method: string with get, set

        type MessageProgressTotal2 =
            abstract message: Zod.ZodType with get, set
            abstract total: Zod.ZodType with get, set
            abstract progress: Zod.ZodType with get, set

        type _metaContent8ac9a7d5 =
            abstract _meta: option<obj> with get, set
            abstract isError: option<bool> with get, set
            abstract structuredContent: option<obj> with get, set
            abstract content: ResizeArray<ContentBlock> with get, set
            abstract toolUseId: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type MethodParams14 =
            abstract params: _metaMessage0e58bff5 with get, set
            abstract method: string with get, set

        type MethodParams8 =
            abstract params: option<_metaCursor> with get, set
            abstract method: string with get, set

        type IdJsonrpcMethodParams2 =
            abstract id: Zod.ZodType with get, set
            abstract jsonrpc: Zod.ZodType with get, set
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type MethodParams18 =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type MaxTotalTimeoutOnprogressOE4f494e2 =
            abstract onresumptiontoken: (option<string -> unit>) with get, set
            abstract resumptionToken: option<string> with get, set
            abstract relatedRequestId: option<U2<string, float>> with get, set
            abstract maxTotalTimeout: option<float> with get, set
            abstract resetTimeoutOnProgress: option<bool> with get, set
            abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
            abstract onprogress: option<ProgressCallback> with get, set
            abstract task: option<TypesJs.TaskCreationParams> with get, set
            abstract timeout: option<float> with get, set

        type _metaIncludeContextCaaaac63 =
            abstract stopSequences: option<ResizeArray<string>> with get, set
            abstract maxTokens: float with get, set
            abstract temperature: option<float> with get, set
            abstract includeContext: option<LiteralUnions.AllServersNoneThisServer> with get, set
            abstract systemPrompt: option<string> with get, set
            abstract modelPreferences: option<CostPriorityHintsIntelligeFaccff05> with get, set
            abstract messages: ResizeArray<_metaContentRole> with get, set
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract metadata: option<Erased.Empty> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        type CompletionsExperimentalExtA1abd3ff3 =
            abstract extensions: option<obj> with get, set
            abstract tasks: option<SharedLiterals.CancelListRequests2> with get, set
            abstract tools: option<SharedLiterals.ListChanged> with get, set
            abstract resources: option<SharedLiterals.ListChangedSubscribe> with get, set
            abstract prompts: option<SharedLiterals.ListChanged> with get, set
            abstract completions: option<Erased.Empty> with get, set
            abstract logging: option<Erased.Empty> with get, set
            abstract experimental: option<obj> with get, set

        type MethodParams37 =
            abstract params: Zod.ZodType with get, set
            abstract method: Zod.ZodType with get, set

        type DescriptionIconsName444a27cf3 =
            abstract title: Zod.ZodType with get, set
            abstract name: Zod.ZodType with get, set
            abstract icons: Zod.ZodType with get, set
            abstract description: Zod.ZodType with get, set
            abstract websiteUrl: Zod.ZodType with get, set
            abstract version: Zod.ZodType with get, set

        type AcceptCancelDecline =
            abstract decline: string with get, set
            abstract accept: string with get, set
            abstract cancel: string with get, set

        type CostPriorityHintsIntelligeFaccff05 =
            abstract intelligencePriority: option<float> with get, set
            abstract speedPriority: option<float> with get, set
            abstract costPriority: option<float> with get, set
            abstract hints: option<ResizeArray<Name>> with get, set

        type _metaAnnotations35478365<'Output> =
            abstract enabled: bool with get, set
            abstract handler: U3<obj -> U2<TypesJs.CallToolResult, Promise<TypesJs.CallToolResult>>, _def_e995c0ec<obj, 'Output, obj, obj> -> obj -> U2<TypesJs.CallToolResult, Promise<TypesJs.CallToolResult>>, ToolTaskHandler<option<ZodRawShapeCompat>>> with get, set
            abstract _meta: option<obj> with get, set
            abstract execution: option<TypesJs.ToolExecution> with get, set
            abstract annotations: option<TypesJs.ToolAnnotations> with get, set
            abstract outputSchema: option<AnySchema> with get, set
            abstract inputSchema: option<AnySchema> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract enable: unit -> unit
            abstract disable: unit -> unit
            abstract update: updates: _metaAnnotations83faa3cf<obj, obj, obj, 'Output> -> unit
            abstract remove: unit -> unit

    module StartAuthorization =
        type Case2 =
            abstract codeVerifier: string with get, set
            abstract authorizationUrl: CloudflareWorkersTypes.URL with get, set

    module StreamableHTTPClientTransport =
        module ResumeStream =
            type Options =
                abstract onresumptiontoken: token: string -> unit

        module Send =
            type Options =
                abstract resumptionToken: option<string> with get, set
                abstract onresumptiontoken: token: string -> unit

    module TypesJs =
        /// <deprecated>
        /// Use {@link JSONRPCErrorResponse} instead.<br/>
        /// <br/>
        /// Please note that spec types have renamed {@link JSONRPCError} to {@link JSONRPCErrorResponse} as per the updated JSON-RPC specification. (was previously just {@link JSONRPCError}) and future versions will remove {@link JSONRPCError}.
        /// </deprecated>
        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type JSONRPCError =
            abstract id: option<U2<string, float>> with get, set
            abstract error: SharedLiterals.CodeDataMessage with get, set
            abstract jsonrpc: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type InitializeRequestParams = interface end

        type ListTasksRequest =
            abstract params: option<SharedLiterals._metaCursor> with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CancelTaskRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type LoggingMessageNotificationParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type JSONRPCResultResponse = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type PingRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type GetPromptResult = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CallToolRequestParams = interface end

        type NumberSchema =
            [<EmitProperty("default")>]
            abstract ``default``: option<float> with get, set

            abstract maximum: option<float> with get, set
            abstract minimum: option<float> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: LiteralUnions.IntegerNumber with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ProgressNotification = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type TitledMultiSelectEnumSchema = interface end

        type ListRootsResult =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract roots: ResizeArray<SharedLiterals._metaNameUri2> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListResourceTemplatesRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CancelTaskResult = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type TextResourceContents = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Implementation = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CreateMessageResultWithTools = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ImageContent = interface end

        type BaseMetadata =
            abstract title: option<string> with get, set
            abstract name: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ToolAnnotations = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListPromptsResult = interface end

        type UnsubscribeRequest =
            abstract params: SharedLiterals._metaUri with get, set
            abstract method: string with get, set

        type ListToolsRequest =
            abstract params: option<SharedLiterals._metaCursor> with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ToolListChangedNotification = interface end

        type Icon =
            abstract theme: option<LiteralUnions.DarkLight> with get, set
            abstract sizes: option<ResizeArray<string>> with get, set
            abstract mimeType: option<string> with get, set
            abstract src: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CreateTaskResult = interface end

        type ToolResultContent =
            abstract _meta: option<obj> with get, set
            abstract isError: option<bool> with get, set
            abstract structuredContent: option<obj> with get, set
            abstract content: ResizeArray<ContentBlock> with get, set
            abstract toolUseId: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type ListChangedOptionsBaseSchema =
            abstract debounceMs: Zod.ZodType with get, set
            abstract autoRefresh: Zod.ZodType with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type TaskStatusNotification = interface end

        type EmptyResult =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CallToolResult = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ToolExecution = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type BooleanSchema = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type BlobResourceContents = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CancelledNotificationParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Prompt = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ReadResourceRequestParams = interface end

        type CallToolRequest =
            abstract params: SharedLiterals._metaArgumentsNameTask with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CompleteRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Result = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CompleteRequestPrompt = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ReadResourceRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ToolChoice = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type SetLevelRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListTasksResult = interface end

        /// <deprecated>
        /// Use ResourceTemplateReference instead
        /// </deprecated>
        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ResourceReference =
            abstract uri: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Tool = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type UnsubscribeRequestParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type PromptReference = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Notification = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListResourcesResult = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ElicitRequestFormParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "ExpandRecursively")>]
        type ExpandRecursively<'T> = interface end

        type StringSchema =
            [<EmitProperty("default")>]
            abstract ``default``: option<string> with get, set

            abstract format: option<LiteralUnions.DateDateTimeEmailUri> with get, set
            abstract maxLength: option<float> with get, set
            abstract minLength: option<float> with get, set
            abstract description: option<string> with get, set
            abstract title: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type PromptListChangedNotification = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Root = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Resource = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Annotations = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ResourceTemplate = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "Flatten")>]
        type Flatten<'T> = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type SetLevelRequestParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CreateMessageResult = interface end

        type GetTaskPayloadRequest =
            abstract params: SharedLiterals._metaTaskId with get, set
            abstract method: string with get, set

        type TaskAugmentedRequestParams =
            abstract task: option<SharedLiterals.Ttl> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type JSONRPCRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Request = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type LegacyTitledEnumSchema = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type TaskStatusNotificationParams = interface end

        type ElicitationCompleteNotification =
            abstract params: SharedLiterals._metaElicitationId with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ReadResourceResult = interface end

        type CancelledNotification =
            abstract params: SharedLiterals._metaReasonRequestId with get, set
            abstract method: string with get, set

        type GetPromptRequest =
            abstract params: SharedLiterals._metaArgumentsName2 with get, set
            abstract method: string with get, set

        type PaginatedRequest =
            abstract params: option<SharedLiterals._metaCursor> with get, set
            abstract method: string with get, set

        type ListResourceTemplatesResult =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract resourceTemplates: ResizeArray<SharedLiterals._metaAnnotationsE7edce494> with get, set

        type PromptMessage =
            abstract content: ContentBlock with get, set
            abstract role: Role with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type InitializeResult = interface end

        type InitializedNotification =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type SamplingMessage = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CreateMessageRequest = interface end

        type ResourceListChangedNotification =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ClientCapabilities = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type GetTaskPayloadResult = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListPromptsRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type JSONRPCNotification = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type RequestMeta = interface end

        type SubscribeRequest =
            abstract params: SharedLiterals._metaUri with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListResourcesRequest = interface end

        type NotificationParams =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ListToolsResult = interface end

        type ProgressNotificationParams =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract message: option<string> with get, set
            abstract total: option<float> with get, set
            abstract progress: float with get, set
            abstract progressToken: Zod.ZodType with get, set

        type Icons =
            abstract icons: option<ResizeArray<SharedLiterals.MimeTypeSizesSrcTheme>> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CompleteResult = interface end

        type RequestParams =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ResourceUpdatedNotificationParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CreateMessageRequestParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Progress = interface end

        type LoggingMessageNotification =
            abstract params: SharedLiterals._metaDataLevelLogger with get, set
            abstract method: string with get, set

        type PaginatedRequestParams =
            abstract cursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type UntitledMultiSelectEnumSchema = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type TitledSingleSelectEnumSchema = interface end

        type ResourceRequestParams =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract uri: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ModelHint = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type PromptArgument = interface end

        type Primitive = option<U3<string, bool, float>>

        type ListRootsRequest =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        type ToolUseContent =
            abstract _meta: option<obj> with get, set
            abstract input: obj with get, set
            abstract id: string with get, set
            abstract name: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ResourceLink = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ElicitRequestURLParams = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type GetTaskResult = interface end

        type GetPromptRequestParams =
            abstract arguments: option<obj> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract name: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type ServerCapabilities = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type InitializeRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CompleteRequestParams = interface end

        type PaginatedResult =
            abstract nextCursor: option<string> with get, set
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set

        type AudioContent =
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract mimeType: string with get, set
            abstract data: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type ResourceContents =
            abstract _meta: option<obj> with get, set
            abstract mimeType: option<string> with get, set
            abstract uri: string with get, set

        type RootsListChangedNotification =
            abstract params: option<SharedLiterals._meta2> with get, set
            abstract method: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type CompleteRequestResourceTemplate = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type SubscribeRequestParams = interface end

        type ModelPreferences =
            abstract intelligencePriority: option<float> with get, set
            abstract speedPriority: option<float> with get, set
            abstract costPriority: option<float> with get, set
            abstract hints: option<ResizeArray<SharedLiterals.Name>> with get, set

        type TextContent =
            abstract _meta: option<obj> with get, set
            abstract annotations: option<SharedLiterals.AudienceLastModifiedPriority> with get, set
            abstract text: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type RelatedTaskMetadata = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "Infer")>]
        type Infer<'Schema> = interface end

        type ElicitationCompleteNotificationParams =
            abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
            abstract elicitationId: string with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type EmbeddedResource = interface end

        type ResourceUpdatedNotification =
            abstract params: SharedLiterals._metaUri with get, set
            abstract method: string with get, set

        type UntitledSingleSelectEnumSchema =
            [<EmitProperty("default")>]
            abstract ``default``: option<string> with get, set

            abstract description: option<string> with get, set
            abstract title: option<string> with get, set
            abstract enum: ResizeArray<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type TaskMetadata =
            abstract ttl: option<float> with get, set

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type GetTaskRequest = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type Task = interface end

        [<Import("@modelcontextprotocol/sdk/types.js", "__type")>]
        type TaskCreationParams = interface end

        module CompleteRequestPrompt =
            type Params =
                abstract context: option<SharedLiterals.Arguments2> with get, set
                abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
                abstract argument: SharedLiterals.NameValue3 with get, set
                abstract ref: SharedLiterals.NameType3 with get, set

        module CompleteRequestResourceTemplate =
            type Params =
                abstract context: option<SharedLiterals.Arguments2> with get, set
                abstract _meta: option<SharedLiterals.IoModelcontextprotocolRelaF70aae9a> with get, set
                abstract argument: SharedLiterals.NameValue3 with get, set
                abstract ref: ResourceReference with get, set
