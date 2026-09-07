namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

type IAgents =
    /// <param name="namespace" >
    /// Agent namespace
    /// </param>
    /// <param name="name" >
    /// Name of the Agent instance
    /// </param>
    /// <param name="options" >
    /// Options for Agent creation
    /// </param>
    [<Import("agents", "getAgentByName")>]
    static member getAgentByName<'Env, 'T, 'Props>(``namespace``: obj, name: string, ?options: obj) : Promise<Agents.GetAgentByName.Case2> = JS.undefined

    [<Erase>]
    member _.genericObservability: Erased.Empty = JS.undefined

    [<Import("agents", "isDurableObjectMemoryLimitReset")>]
    static member isDurableObjectMemoryLimitReset(error: option<obj>) : bool = JS.undefined

    [<Erase>]
    member _.RPC_DO_PREFIX: string = JS.undefined

    /// <remarks>
    /// When using this schema with OpenAI models via the AI SDK, you must pass<br/>
    /// `providerOptions: { openai: { strictJsonSchema: false } }` to `generateObject`.<br/>
    /// This is because the schema uses a discriminated union which is not compatible<br/>
    /// with OpenAI's strict structured outputs mode.
    /// </remarks>
    /// <example>
    /// ```typescript<br/>
    /// import { generateObject } from "ai";<br/>
    /// import { scheduleSchema, getSchedulePrompt } from "agents/schedule";<br/>
    /// <br/>
    /// const result = await generateObject({<br/>
    ///   model,<br/>
    ///   prompt: `${getSchedulePrompt({ date: new Date() })} Input: "${userInput}"`,<br/>
    ///   schema: scheduleSchema,<br/>
    ///   // Required for OpenAI to avoid strict JSON schema validation errors<br/>
    ///   providerOptions: {<br/>
    ///     openai: { strictJsonSchema: false }<br/>
    ///   }<br/>
    /// });<br/>
    /// ```
    /// </example>
    [<Erase>]
    member _.scheduleSchema: Zod.ZodType = JS.undefined

    /// <example>
    /// ```ts<br/>
    /// const inbox = await getAgentByName(env.MyInbox, userId);<br/>
    /// const chat = await getSubAgentByName(inbox, MyChat, chatId);<br/>
    /// await chat.addMessage({ role: "user", content: "hi" });<br/>
    /// ```
    /// </example>
    [<Import("agents", "getSubAgentByName")>]
    static member getSubAgentByName<'T>(parent: option<obj>, cls: SharedLiterals.CtxEnv<obj>, name: string) : Promise<Agents.OptionalAgentMethods> = JS.undefined

    /// <deprecated>
    /// REMOVED due to security vulnerability (IDOR via spoofed headers).
    /// </deprecated>
    [<Import("agents", "createHeaderBasedEmailResolver")>]
    static member createHeaderBasedEmailResolver<'Env>(_: unit) : (CloudflareWorkersTypes.ForwardableEmailMessage -> obj -> Promise<option<Erased.Empty>>) = JS.undefined

    [<Erase>]
    member _.DEFAULT_EXEC_SWEEP_IDLE_MS: float = JS.undefined

    /// <example>
    /// ```typescript<br/>
    /// const headers = await signAgentHeaders(env.EMAIL_SECRET, "MyAgent", this.name);<br/>
    /// // Use these headers when sending outbound emails<br/>
    /// ```
    /// </example>
    /// <param name="secret" >
    /// - Secret key for HMAC signing (store in environment variables)
    /// </param>
    /// <param name="agentName" >
    /// - Name of the agent
    /// </param>
    /// <param name="agentId" >
    /// - ID of the agent instance
    /// </param>
    [<Import("agents", "signAgentHeaders")>]
    static member signAgentHeaders(secret: string, agentName: string, agentId: string) : Promise<Agents.OptionalAgentMethods> = JS.undefined

    [<Erase>]
    member _.MCP_SERVER_ID_MAX_LENGTH: int = JS.undefined

    [<Erase>]
    member _.DEFAULT_AGENT_STATIC_OPTIONS: Agents.DEFAULT_AGENT_STATIC_OPTIONS =
        JS.undefined

    [<Import("agents", "connectUrl")>]
    static member connectUrl(baseUrl: string, ?options: obj) : Promise<Erased.Empty> = JS.undefined

    [<CompiledName("ElicitRequestSchema")>]
    member _.elicitRequestSchema: Zod.ZodType = JS.undefined

    /// <example>
    /// ```typescript<br/>
    /// if (isAutoReplyEmail(parsed.headers)) {<br/>
    ///   // Skip processing auto-replies<br/>
    ///   return;<br/>
    /// }<br/>
    /// ```
    /// </example>
    /// <param name="headers" >
    /// - Headers array from postal-mime Email.headers or similar format
    /// </param>
    [<Import("agents", "isAutoReplyEmail")>]
    static member isAutoReplyEmail(headers: ResizeArray<Erased.Empty>) : bool = JS.undefined

    [<Import("agents", "browserSnapshot")>]
    static member browserSnapshot(browser: Erased.Empty, input: Erased.Empty) : Promise<Erased.Empty> = JS.undefined

    [<Import("agents", "browserExtract")>]
    static member browserExtract<'T>(browser: Erased.Empty, input: Erased.Empty) : Promise<obj> = JS.undefined

    [<Erase>]
    member _.AGENT_TOOL_PROGRESS_PART: string = JS.undefined

    [<CompiledName("__DO_NOT_USE_WILL_BREAK__agentContext")>]
    member _._dONOTUSEWILLBREAK_agentContext: option<obj> = JS.undefined

    [<Import("agents", "isDurableObjectCodeUpdateReset")>]
    static member isDurableObjectCodeUpdateReset(error: option<obj>) : bool = JS.undefined

    [<Import("agents", "isPlatformTransientError")>]
    static member isPlatformTransientError(error: option<obj>) : bool = JS.undefined

    [<Erase>]
    member _.DEFAULT_SWEEP_IDLE_MS: float = JS.undefined

    [<Import("agents", "createBrowserSession")>]
    static member createBrowserSession(browser: Erased.Empty, ?options: obj) : Promise<Erased.Empty> = JS.undefined

    /// <example>
    /// ```typescript<br/>
    /// // In your email handler<br/>
    /// const secureResolver = createSecureReplyEmailResolver(env.EMAIL_SECRET, {<br/>
    ///   maxAge: 7 * 24 * 60 * 60, // 7 days<br/>
    ///   onInvalidSignature: (email, reason) => {<br/>
    ///     console.warn(`Invalid signature from ${email.from}: ${reason}`);<br/>
    ///   }<br/>
    /// });<br/>
    /// const addressResolver = createAddressBasedEmailResolver("MyAgent");<br/>
    /// <br/>
    /// await routeAgentEmail(email, env, {<br/>
    ///   resolver: async (email, env) => {<br/>
    ///     // Try secure reply routing first<br/>
    ///     const replyRouting = await secureResolver(email, env);<br/>
    ///     if (replyRouting) return replyRouting;<br/>
    ///     // Fall back to address-based routing<br/>
    ///     return addressResolver(email, env);<br/>
    ///   }<br/>
    /// });<br/>
    /// ```
    /// </example>
    /// <param name="secret" >
    /// - Secret key for HMAC verification (must match the key used with signAgentHeaders)
    /// </param>
    /// <param name="options" >
    /// - Optional configuration for signature verification
    /// </param>
    [<Import("agents", "createSecureReplyEmailResolver")>]
    static member createSecureReplyEmailResolver<'Env>(secret: string, ?options: Erased.Empty) : (CloudflareWorkersTypes.ForwardableEmailMessage -> obj -> Promise<option<Erased.Empty>>) = JS.undefined

    [<Import("agents", "browserMarkdown")>]
    static member browserMarkdown(browser: Erased.Empty, input: Erased.Empty) : Promise<string> = JS.undefined

    /// <param name="request" >
    /// Request to route
    /// </param>
    /// <param name="env" >
    /// Environment containing Agent bindings
    /// </param>
    /// <param name="options" >
    /// Routing options
    /// </param>
    [<Import("agents", "routeAgentRequest")>]
    static member routeAgentRequest<'Env>(request: obj, env: obj, ?options: obj) : Promise<option<CloudflareWorkersTypes.Response>> = JS.undefined

    [<Erase>]
    member _.DEFAULT_MAX_AGE_SECONDS: float = JS.undefined

    /// <example>
    /// ```ts<br/>
    /// export default {<br/>
    ///   async fetch(req, env) {<br/>
    ///     const { parentName, rest } = myCustomParse(req.url);<br/>
    ///     const parent = await getAgentByName(env.Inbox, parentName);<br/>
    ///     return routeSubAgentRequest(req, parent, { fromPath: rest });<br/>
    ///   }<br/>
    /// };<br/>
    /// ```
    /// </example>
    [<Import("agents", "routeSubAgentRequest")>]
    static member routeSubAgentRequest(req: obj, parent: option<obj>, ?options: obj) : Promise<CloudflareWorkersTypes.Response> = JS.undefined

    [<Import("agents", "loadCdpSpec")>]
    static member loadCdpSpec(source: obj) : Promise<Agents.SearchableCdpSpec> = JS.undefined

    /// <param name="str" >
    /// The string to convert
    /// </param>
    [<Import("agents", "camelCaseToKebabCase")>]
    static member camelCaseToKebabCase(str: string) : string = JS.undefined

    [<Erase>]
    member _.channels: Agents.Channels = JS.undefined

    [<Import("agents", "isTerminalCloseEvent")>]
    static member isTerminalCloseEvent(event: obj) : bool = JS.undefined

    [<Import("agents", "browserScrape")>]
    static member browserScrape(browser: Erased.Empty, input: Erased.Empty) : Promise<ResizeArray<Erased.Empty>> = JS.undefined

    /// <param name="event" >
    /// - The event to get the schedule prompt for
    /// </param>
    /// <deprecated>
    /// this has been renamed to getSchedulePrompt, and unstable_getSchedulePrompt will be removed in the next major version
    /// </deprecated>
    [<Import("agents", "unstable_getSchedulePrompt")>]
    static member unstableGetSchedulePrompt(event: obj) : string = JS.undefined

    [<Import("agents", "listBrowserTargets")>]
    static member listBrowserTargets(browser: Erased.Empty, sessionId: string) : Promise<ResizeArray<Erased.Empty>> = JS.undefined

    /// <param name="metadata" >
    /// Optional metadata about the callable method
    /// </param>
    /// <deprecated>
    /// this has been renamed to callable, and unstable_callable will be removed in the next major version
    /// </deprecated>
    [<CompiledName("unstable_callable")>]
    member _.unstableCallable: option<Erased.Empty> -> (obj -> obj -> obj) -> obj -> obj -> obj -> obj =
        JS.undefined

    [<Import("agents", "getNamespacedData")>]
    static member getNamespacedData<'T>(mcpClients: obj, ``type``: obj) : proptypekey<Erased.Empty, obj> = JS.undefined

    [<Erase>]
    member _.AGENT_TOOL_MILESTONE_PART: string = JS.undefined

    [<Import("agents", "browserPdf")>]
    static member browserPdf(browser: Erased.Empty, input: Erased.Empty) : Promise<Erased.Empty> = JS.undefined

    [<Import("agents", "connectBrowser")>]
    static member connectBrowser(browser: Erased.Empty, ?options: U2<Erased.Empty, float>) : Promise<Erased.Empty> = JS.undefined

    /// <deprecated>
    /// This has been renamed to createMcpHandler, and experimental_createMcpHandler will be removed in the next major version
    /// </deprecated>
    [<Import("agents", "experimental_createMcpHandler")>]
    static member experimentalCreateMcpHandler(server: U2<obj, obj>, ?options: Erased.Empty) : (CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> option<obj> -> CloudflareWorkersTypes.ExecutionContext<option<obj>> -> Promise<CloudflareWorkersTypes.Response>) = JS.undefined

    [<Import("agents", "browserContent")>]
    static member browserContent(browser: Erased.Empty, input: Erased.Empty) : Promise<string> = JS.undefined

    [<Import("agents", "getMcpAuthContext")>]
    static member getMcpAuthContext(_: unit) : option<Erased.Empty> = JS.undefined

    [<Import("agents", "connectBrowserSession")>]
    static member connectBrowserSession(browser: Erased.Empty, sessionId: string, ?timeoutMs: float) : Promise<Erased.Empty> = JS.undefined

    [<Import("agents", "browserLinks")>]
    static member browserLinks(browser: Erased.Empty, input: Erased.Empty) : Promise<ResizeArray<string>> = JS.undefined

    [<Import("agents", "browserScreenshot")>]
    static member browserScreenshot(browser: Erased.Empty, input: Erased.Empty) : Promise<Erased.Empty> = JS.undefined

    [<Import("agents", "createStubProxy")>]
    static member createStubProxy<'T>(call: string -> ResizeArray<option<obj>> -> option<obj>) : obj = JS.undefined

    /// <example>
    /// normalizeServerId("my-supplied-id");  // "my-supplied-id"<br/>
    /// normalizeServerId("GitHub MCP!");     // "github-mcp"<br/>
    /// normalizeServerId("42-things");       // "id-42-things"
    /// </example>
    [<Import("agents", "normalizeServerId")>]
    static member normalizeServerId(input: string) : string = JS.undefined

    /// <param name="metadata" >
    /// Optional metadata about the callable method
    /// </param>
    [<Import("agents", "callable")>]
    static member callable(?metadata: Erased.Empty) : ((obj -> obj -> obj) -> obj -> obj -> obj -> obj) = JS.undefined

    [<Import("agents", "createMcpHandler")>]
    static member createMcpHandler(server: U2<obj, obj>, ?options: Erased.Empty) : (CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> option<obj> -> CloudflareWorkersTypes.ExecutionContext<option<obj>> -> Promise<CloudflareWorkersTypes.Response>) = JS.undefined

    /// <deprecated>
    /// this has been renamed to scheduleSchema, and unstable_scheduleSchema will be removed in the next major version
    /// </deprecated>
    [<CompiledName("unstable_scheduleSchema")>]
    member _.unstableScheduleSchema: Zod.ZodType = JS.undefined

    [<Import("agents", "runQuickAction")>]
    static member runQuickAction(browser: Erased.Empty, action: string, params: Erased.Empty) : Promise<CloudflareWorkersTypes.Response> = JS.undefined

    [<Import("agents", "runQuickAction")>]
    static member runQuickAction(browser: Erased.Empty, action: Erased.Empty, params: Erased.Empty) : Promise<CloudflareWorkersTypes.Response> = JS.undefined

    /// <param name="agentName" >
    /// The name of the agent to route the email to
    /// </param>
    /// <param name="agentId" >
    /// The id of the agent to route the email to
    /// </param>
    [<Import("agents", "createCatchAllEmailResolver")>]
    static member createCatchAllEmailResolver<'Env>(agentName: string, agentId: string) : (CloudflareWorkersTypes.ForwardableEmailMessage -> obj -> Promise<option<Erased.Empty>>) = JS.undefined

    [<Import("agents", "getBrowserRecording")>]
    static member getBrowserRecording(options: obj) : Promise<Erased.Empty> = JS.undefined

    [<Import("agents", "getCurrentAgent")>]
    static member getCurrentAgent<'T>(_: unit) : Agents.GetCurrentAgent = JS.undefined

    [<Erase>]
    member _.DEFAULT_CALL_TIMEOUT_MS: int = JS.undefined

    /// <param name="defaultAgentName" >
    /// The default agent name to use if the email address does not contain a sub-address
    /// </param>
    [<Import("agents", "createAddressBasedEmailResolver")>]
    static member createAddressBasedEmailResolver<'Env>(defaultAgentName: string) : (CloudflareWorkersTypes.ForwardableEmailMessage -> obj -> Promise<option<Erased.Empty>>) = JS.undefined

    /// <param name="event" >
    /// - The event to get the schedule prompt for
    /// </param>
    [<Import("agents", "getSchedulePrompt")>]
    static member getSchedulePrompt(event: obj) : string = JS.undefined

    [<Import("agents", "parseSubAgentPath")>]
    static member parseSubAgentPath(url: string, ?options: obj) : option<Erased.Empty> = JS.undefined

    /// <param name="email" >
    /// The email to route
    /// </param>
    /// <param name="env" >
    /// The environment containing the Agent bindings
    /// </param>
    /// <param name="options" >
    /// The options for routing the email
    /// </param>
    [<Import("agents", "routeAgentEmail")>]
    static member routeAgentEmail<'Env>(email: obj, env: obj, options: obj) : Promise<unit> = JS.undefined

    /// <param name="opts" >
    /// Connection options
    /// </param>
    /// <param name="init" >
    /// Request initialization options
    /// </param>
    [<Import("agents", "agentFetch")>]
    static member agentFetch(opts: obj, ?init: obj) : Promise<CloudflareWorkersTypes.Response> = JS.undefined

    [<Import("agents", "deleteBrowserSession")>]
    static member deleteBrowserSession(browser: Erased.Empty, sessionId: string) : Promise<unit> = JS.undefined

    [<Import("agents", "subscribe$1")>]
    static member ``subscribe$1``<'K>(channelKey: obj, callback: proptypekey<Erased.Empty, obj> -> unit) : (unit -> unit) = JS.undefined

    [<Erase>]
    member _.SUB_PREFIX: string = JS.undefined

module Agents =
    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MessageType =
        | [<CompiledName("cf_agent_mcp_servers")>] CF_AGENT_MCP_SERVERS
        | [<CompiledName("cf_mcp_agent_event")>] CF_MCP_AGENT_EVENT
        | [<CompiledName("cf_agent_state")>] CF_AGENT_STATE
        | [<CompiledName("cf_agent_state_error")>] CF_AGENT_STATE_ERROR
        | [<CompiledName("cf_agent_identity")>] CF_AGENT_IDENTITY
        | [<CompiledName("cf_agent_session")>] CF_AGENT_SESSION
        | [<CompiledName("cf_agent_session_error")>] CF_AGENT_SESSION_ERROR
        | [<CompiledName("rpc")>] RPC

    [<Import("agents", "BrowserRenderingError")>]
    type BrowserRenderingError =
        [<EmitConstructor>]
        abstract Create: message: string * status: float -> BrowserRenderingError

        abstract status: float with get

    [<Import("agents", "SearchableCdpSpec")>]
    type SearchableCdpSpec =
        abstract domains: ResizeArray<SharedLiterals.CommandsDescriptionEventsNameTypes> with get, set

    [<Import("agents", "AgentClient")>]
    type AgentClient<'AgentT, 'State> =
        [<EmitConstructor>]
        abstract Create: options: AgentClient.Options -> AgentClient<'AgentT, 'State>

        abstract _callImpl: option<obj> with get, set
        abstract _rejectPendingCalls: option<obj> with get, set
        abstract _resetReady: option<obj> with get, set
        abstract _previousAgent: option<obj> with get, set
        abstract _previousName: option<obj> with get, set
        abstract _resolveReady: option<obj> with get, set
        abstract _readyPromise: option<obj> with get, set
        abstract _pendingCalls: option<obj> with get, set
        abstract options: option<obj> with get, set
        abstract ready: Promise<unit> with get
        abstract connectionError: option<AgentConnectionError> with get, set
        abstract identified: bool with get, set
        abstract state: option<'State> with get, set
        abstract stub: U2<UntypedAgentStub, OptionalAgentMethods> with get, set
        abstract call: U2<UntypedAgentClientCall, AgentClient.Call> with get, set
        abstract name: string with get, set
        abstract agent: string with get, set
        abstract fetch: _opts: Partysocket.PartyFetchOptions -> Promise<CloudflareWorkersTypes.Response>
        abstract setState: state: 'State -> unit
        abstract close: ?code: float * ?reason: string -> unit

    [<Import("agents", "AgentToolEventState")>]
    type AgentToolEventState =
        abstract unboundRuns: ResizeArray<AgentToolEventState.UnboundRuns> with get, set
        abstract runsByToolCallId: OptionalAgentMethods with get, set
        abstract runsById: OptionalAgentMethods with get, set

    [<Import("agents", "WaitForApprovalOptions")>]
    type WaitForApprovalOptions =
        abstract eventType: option<string> with get, set
        abstract timeout: option<CloudflareWorkersTypes.WorkflowRetentionDuration> with get, set
        abstract stepName: option<string> with get, set

    [<Import("agents", "AgentToolFailure")>]
    type AgentToolFailure =
        abstract childStillRunning: option<bool> with get, set
        abstract reason: option<Erased.Empty> with get, set
        abstract retryable: bool with get, set
        abstract error: string with get, set
        abstract status: LiteralUnions.AbortedErrorInterrupted with get, set
        abstract ok: bool with get, set

    type Channels =
        abstract channel: option<obj> with get
        abstract email: option<obj> with get
        abstract mcp: option<obj> with get
        abstract workflow: option<obj> with get
        abstract lifecycle: option<obj> with get
        abstract schedule: option<obj> with get
        abstract agentTool: option<obj> with get
        abstract fiber: option<obj> with get
        abstract transcript: option<obj> with get
        abstract chat: option<obj> with get
        abstract message: option<obj> with get
        abstract rpc: option<obj> with get
        abstract state: option<obj> with get

    [<Import("agents", "RunWorkflowOptions")>]
    type RunWorkflowOptions =
        abstract agentBinding: option<string> with get, set
        abstract metadata: option<OptionalAgentMethods> with get, set
        abstract id: option<string> with get, set

    [<Import("agents", "BrowserConnectorOptions")>]
    type BrowserConnectorOptions =
        abstract timeout: option<float> with get, set

    [<Import("agents", "StoredBrowserSession")>]
    type StoredBrowserSession =
        abstract closedAt: option<float> with get, set
        abstract updatedAt: float with get, set
        abstract createdAt: float with get, set
        abstract sessionId: string with get, set

        [<EmitProperty("open")>]
        abstract ``open``: CloudflareWorkersTypes.Event with get, set

        abstract message: CloudflareWorkersTypes.MessageEvent with get, set
        /// <deprecated>
        /// Use `className` instead, which returns the Durable Object class name.<br/>
        /// In the next major version, `party` will return the class name instead of the kebab-case namespace.
        /// </deprecated>
        abstract party: string with get, set
        abstract routingRetry: option<U2<Partyserver.RoutingRetryOptions, bool>> with get, set
        abstract cors: option<U4<OptionalAgentMethods, CloudflareWorkersTypes.Headers, seq<seq<string>>, bool>> with get, set
        abstract locationHint: option<CloudflareWorkersTypes.DurableObjectLocationHint> with get, set
        abstract jurisdiction: option<CloudflareWorkersTypes.DurableObjectJurisdiction> with get, set
        abstract prefix: option<string> with get, set
        abstract className: option<string> with get, set
        abstract delayMs: float with get, set
        abstract attempt: float with get, set
        abstract binding: SharedLiterals.CallTool with get, set
        abstract bindings: option<OptionalAgentMethods> with get, set
        abstract modules: option<OptionalAgentMethods> with get, set
        abstract globalOutbound: option<SharedLiterals.ConnectFetch> with get, set
        abstract loader: CloudflareWorkersTypes.WorkerLoader with get, set
        abstract instructions: option<string> with get, set
        abstract client: CloudflareCodemode.SharedLiterals.CallTool2 with get, set
        abstract executionId: string with get, set
        abstract connectors: option<ResizeArray<string>> with get, set
        abstract savedAt: float with get, set
        abstract code: string with get, set
        abstract types: option<string> with get, set
        abstract server: ModelcontextprotocolSdk.McpServer with get, set
        abstract version: option<string> with get, set
        abstract spec: OptionalAgentMethods with get, set
        abstract rawBody: option<bool> with get, set
        abstract body: option<obj> with get, set
        abstract query: option<OptionalAgentMethods> with get, set
        abstract path: string with get, set
        abstract method: LiteralUnions.DELETE_GET_PATCH_POST_PUT with get, set
        abstract executor: option<Erased.Empty> with get, set
        abstract tools: U2<ResizeArray<CloudflareCodemode.ApprovalAwareJsonSchemaExecutableToolDescriptor>, CloudflareCodemode.ApprovalAwareJsonSchemaExecutableToolDescriptors> with get, set
        abstract logs: option<ResizeArray<string>> with get, set
        abstract error: option<string> with get, set
        abstract result: option<obj> with get, set
        abstract csp: option<string> with get, set
        abstract outputSchema: option<Erased.JsonSchema> with get, set
        abstract inputSchema: Erased.JsonSchema with get, set
        abstract prelude: option<string> with get, set
        abstract fns: OptionalAgentMethods with get, set
        abstract maxAlarmMemoryLimitStrikes: option<float> with get, set
        abstract detachedNoProgressBudgetMs: option<float> with get, set
        abstract detachedMaxBudgetMs: option<float> with get, set
        abstract agentToolReattachMaxWindowMs: option<float> with get, set
        abstract agentToolReattachNoProgressTimeoutMs: option<float> with get, set
        abstract fiberRecoveryMaxAgeMs: option<float> with get, set
        abstract fiberRecoveryScanDeadlineMs: option<float> with get, set
        abstract fiberRecoveryHookTimeoutMs: option<float> with get, set
        abstract retry: option<Erased.Empty> with get, set
        abstract keepAliveIntervalMs: option<float> with get, set
        abstract hungScheduleTimeoutSeconds: option<float> with get, set
        abstract sendIdentityOnConnect: option<bool> with get, set
        abstract hibernate: option<bool> with get, set
        abstract maxDelayMs: option<float> with get, set
        abstract baseDelayMs: option<float> with get, set
        abstract maxAttempts: option<float> with get, set
        abstract remainingPath: string with get, set
        abstract childName: string with get, set
        abstract childClass: string with get, set
        abstract transport: option<Erased.Empty> with get, set
        abstract authContext: option<Erased.Empty> with get, set
        abstract route: option<string> with get, set
        abstract props: option<OptionalAgentMethods> with get, set
        abstract name: string with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: CloudflareWorkersTypes.DurableObjectNamespace<obj> with get, set

        abstract timeout: option<float> with get, set
        abstract initializeParams: option<ModelcontextprotocolSdk.TypesJs.InitializeRequestParams> with get, set
        abstract initialized: bool with get, set
        abstract storage: option<Erased.Empty> with get, set
        abstract corsOptions: option<Erased.Empty> with get, set
        abstract serverId: option<string> with get, set
        abstract clientId: option<string> with get, set
        abstract authUrl: option<string> with get, set
        abstract key: option<string> with get, set
        abstract mode: option<LiteralUnions.DynamicOneShotReuse> with get, set
        abstract maxExecIdleMs: option<float> with get, set
        abstract maxIdleMs: option<float> with get, set
        abstract swept: ResizeArray<SharedLiterals.KeySessionId> with get, set
        abstract pageUrl: option<string> with get, set
        abstract expiresInMs: float with get, set
        abstract targetId: string with get, set
        abstract events: OptionalAgentMethods with get, set
        abstract duration: float with get, set
        abstract targets: option<ResizeArray<Erased.Empty>> with get, set
        abstract webSocketDebuggerUrl: option<string> with get, set
        abstract devtoolsFrontendUrl: option<string> with get, set
        abstract description: option<string> with get, set
        abstract title: option<string> with get, set
        abstract url: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract id: string with get, set
        abstract cdpHeaders: option<OptionalAgentMethods> with get, set
        abstract cdpUrl: option<string> with get, set
        abstract browser: option<Erased.Empty> with get, set
        abstract recording: option<bool> with get, set
        abstract includeTargets: option<bool> with get, set
        abstract keepAliveMs: option<float> with get, set
        abstract timeoutMs: option<float> with get, set
        abstract contentType: string with get, set
        abstract data: Uint8Array with get, set
        abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
        abstract userAgent: option<string> with get, set
        abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
        abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
        abstract cookies: option<ResizeArray<option<obj>>> with get, set
        abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
        abstract gotoOptions: option<Erased.Empty> with get, set
        abstract results: ResizeArray<SharedLiterals.AttributesHeightHtml7c0a12ab> with get, set
        abstract selector: string with get, set
        abstract screenshot: string with get, set
        abstract content: string with get, set
        abstract domains: ResizeArray<SharedLiterals.CommandsDescriptionEventsNameTypes> with get, set
        abstract quickAction: action: string * ?options: obj -> Promise<CloudflareWorkersTypes.Response>
        abstract acquireLock: key: string -> U2<Erased.Empty, Promise<Erased.Empty>>
        abstract get: key: string -> option<U2<StoredBrowserSession, Promise<option<StoredBrowserSession>>>>
        abstract set: key: string * session: StoredBrowserSession -> option<Promise<unit>>
        abstract delete: key: string -> option<Promise<unit>>
        abstract list: prefix: string -> U2<Map<string, StoredBrowserSession>, Promise<Map<string, StoredBrowserSession>>>
        abstract release: unit -> option<Promise<unit>>
        abstract fetch: input: U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>
        abstract emit: event: Erased.Empty -> unit
        abstract reportComplete: ?result: obj -> Promise<unit>
        abstract reportError: error: U2<exn, string> -> Promise<unit>
        abstract sendEvent: event: obj -> Promise<unit>
        abstract updateAgentState: ?state: obj -> Promise<unit>
        abstract mergeAgentState: partialState: OptionalAgentMethods -> Promise<unit>
        abstract resetAgentState: unit -> Promise<unit>
        abstract start: unit -> Promise<unit>
        abstract send: message: ModelcontextprotocolSdk.JSONRPCMessage * ?options: ModelcontextprotocolSdk.TransportSendOptions -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract onmessage: message: obj * ?extra: ModelcontextprotocolSdk.MessageExtraInfo -> unit
        abstract setProtocolVersion: version: string -> unit
        abstract checkState: state: string -> Promise<SharedLiterals.ErrorServerIdValid>
        abstract consumeState: state: string -> Promise<unit>
        abstract runWithCodeVerifierState: state: string * callback: (unit -> Promise<obj>) -> Promise<obj>
        abstract deleteCodeVerifier: unit -> Promise<unit>
        abstract storeEvent: streamId: string * message: ModelcontextprotocolSdk.JSONRPCMessage -> Promise<string>
        abstract getStreamIdForEventId: eventId: string -> Promise<option<string>>
        abstract replayEventsAfter: lastEventId: string * send: ModelcontextprotocolSdk.SharedLiterals.Send -> Promise<string>
        abstract clearStream: streamId: string -> Promise<unit>
        abstract onRetry: event: Partyserver.RoutingRetryEvent -> option<Promise<unit>>
        abstract execute: args: OptionalAgentMethods -> Promise<option<obj>>
        abstract execute: code: string * providersOrFns: U2<ResizeArray<Erased.Empty>, OptionalAgentMethods> * ?options: Erased.Empty -> Promise<Erased.Empty>
        abstract execute: args: Erased.Empty -> Promise<Erased.Empty>
        abstract request: options: CloudflareCodemode.RequestOptions * context: CloudflareCodemode.OpenApiMcpRequestContext -> Promise<option<obj>>
        abstract fetchTools: unit -> Promise<ResizeArray<ModelcontextprotocolSdk.TypesJs.Tool>>
        abstract tool: ?options: CloudflareCodemode.CodemodeRuntimeToolOptions -> CloudflareCodemode.CodemodeTool
        abstract approve: options: CloudflareCodemode.CodemodeApproveOptions -> Promise<CloudflareCodemode.ProxyToolOutput>
        abstract reject: options: CloudflareCodemode.CodemodeRejectOptions -> Promise<bool>
        abstract rollback: options: CloudflareCodemode.CodemodeRollbackOptions -> Promise<unit>
        abstract pending: ?executionId: string -> Promise<ResizeArray<CloudflareCodemode.PendingAction>>
        abstract expirePaused: ?options: CloudflareCodemode.CodemodeExpireOptions -> Promise<ResizeArray<string>>
        abstract executions: ?limit: float -> Promise<ResizeArray<CloudflareCodemode.ExecutionState>>
        abstract deleteExecution: id: string -> Promise<bool>
        abstract pruneExecutions: ?keep: float -> Promise<float>
        abstract saveSnippet: name: string * options: CloudflareCodemode.SaveSnippetOptions -> Promise<CloudflareCodemode.Snippet>
        abstract snippets: unit -> Promise<ResizeArray<CloudflareCodemode.Snippet>>
        abstract deleteSnippet: name: string -> Promise<bool>
        abstract onBeforeConnect: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * lobby: Partyserver.Lobby<obj> -> option<U3<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, Promise<option<U2<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>>>>>>
        abstract onBeforeRequest: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * lobby: Partyserver.Lobby<obj> -> option<U3<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, Promise<option<U2<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>>>>>>

    type RouteSubAgentRequest =
        abstract fromPath: option<string> with get, set

    [<Import("agents", "AgentWorkflowStep")>]
    type AgentWorkflowStep =
        inherit CloudflareWorkersTypes.CloudflareWorkersModule.WorkflowStep
        abstract reportComplete: ?result: obj -> Promise<unit>
        abstract reportError: error: U2<exn, string> -> Promise<unit>
        abstract sendEvent: event: obj -> Promise<unit>
        abstract updateAgentState: ?state: obj -> Promise<unit>
        abstract mergeAgentState: partialState: OptionalAgentMethods -> Promise<unit>
        abstract resetAgentState: unit -> Promise<unit>

    type ParseSubAgentPath =
        abstract knownClasses: option<System.Collections.Generic.IReadOnlyList<string>> with get, set

    [<Import("agents", "AgentMethods")>]
    type AgentMethods<'T> = interface end

    [<Import("agents", "CallOptions")>]
    type CallOptions =
        abstract stream: option<StreamOptions> with get, set
        abstract timeout: option<float> with get, set

    type IsAutoReplyEmail =
        abstract value: string with get, set
        abstract key: string with get, set

    [<Import("agents", "AgentToolEventMessage")>]
    type AgentToolEventMessage =
        abstract event: Erased.Empty with get, set
        abstract replay: option<bool> with get, set
        abstract sequence: float with get, set
        abstract parentToolCallId: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("agents", "ApprovalEventPayload")>]
    type ApprovalEventPayload =
        abstract metadata: option<OptionalAgentMethods> with get, set
        abstract reason: option<string> with get, set
        abstract approved: bool with get, set

    [<Import("agents", "CdpSpecSource")>]
    type CdpSpecSource =
        abstract cdpHeaders: option<OptionalAgentMethods> with get, set
        abstract cdpUrl: option<string> with get, set
        abstract browser: option<Erased.Empty> with get, set

    type GetAgentByName =
        abstract routingRetry: option<U2<Partyserver.RoutingRetryOptions, bool>> with get, set
        abstract props: option<OptionalAgentMethods> with get, set
        abstract locationHint: option<LiteralUnions.AfrApacApacNe83359f17> with get, set
        abstract jurisdiction: option<LiteralUnions.EuFedrampFedrampHigh> with get, set

    [<Import("agents", "MCPServerOptions")>]
    type MCPServerOptions =
        abstract retry: option<Erased.Empty> with get, set
        abstract transport: option<MCPServerOptions.Transport> with get, set
        abstract client: option<McpClientOptions> with get, set

    [<Import("agents", "RPCMethods")>]
    type RPCMethods =
        abstract Item: key: string -> option<obj>

    type OptionalArgsAgentClientCall = obj -> option<ResizeArray<option<obj>>> -> option<U2<CallOptions, StreamOptions>> -> option<U3<obj, Promise<option<U2<obj, obj>>>, obj>>

    [<Import("agents", "RPCRequest")>]
    type RPCRequest =
        abstract args: ResizeArray<option<obj>> with get, set
        abstract method: string with get, set
        abstract id: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("agents", "AgentWorkflowEvent")>]
    type AgentWorkflowEvent<'Params> = interface end

    [<Import("agents", "BrowserLiveView")>]
    type BrowserLiveView =
        abstract expiresInMs: float with get, set
        abstract targets: ResizeArray<BrowserLiveViewTarget> with get, set
        abstract sessionId: string with get, set

    [<Import("agents", "WorkflowTrackingRow")>]
    type WorkflowTrackingRow =
        [<EmitProperty("completed_at")>]
        abstract completedAt: option<float> with get, set

        [<EmitProperty("updated_at")>]
        abstract updatedAt: float with get, set

        [<EmitProperty("created_at")>]
        abstract createdAt: float with get, set

        [<EmitProperty("error_message")>]
        abstract errorMessage: option<string> with get, set

        [<EmitProperty("error_name")>]
        abstract errorName: option<string> with get, set

        abstract metadata: option<string> with get, set
        abstract status: CloudflareWorkersTypes.CloudflareWorkersModule.WorkflowInstanceStatus with get, set

        [<EmitProperty("workflow_name")>]
        abstract workflowName: string with get, set

        [<EmitProperty("workflow_id")>]
        abstract workflowId: string with get, set

        abstract id: string with get, set

    [<Import("agents", "__type")>]
    type Schedule = interface end

    [<Import("agents", "SSEEdgeClientTransport")>]
    type SSEEdgeClientTransport =
        interface
            [<EmitConstructor>]
            abstract Create: url: CloudflareWorkersTypes.URL * options: ModelcontextprotocolSdk.SSEClientTransportOptions -> SSEEdgeClientTransport
        end

    [<Import("agents", "CodemodeRuntime")>]
    type CodemodeRuntime =
        [<EmitConstructor>]
        abstract Create: ctx: Erased.Empty * ?env: obj -> CodemodeRuntime

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract ``begin``: code: string * ?options: CloudflareCodemode.BeginOptions -> Promise<string>
        abstract resume: id: string -> Promise<option<CloudflareCodemode.ExecutionState>>
        abstract decide: executionId: string * seq: float * connector: string * method: string * ?args: obj * requiresApproval: bool * ?ephemeral: bool -> Promise<CloudflareCodemode.ToolDecision>
        abstract recordResult: executionId: string * seq: float * ?result: obj -> Promise<unit>
        abstract complete: executionId: string * ?result: obj * ?logs: ResizeArray<string> -> Promise<unit>
        abstract fail: executionId: string * error: string * ?logs: ResizeArray<string> -> Promise<unit>
        abstract listPending: ?executionId: string -> Promise<ResizeArray<CloudflareCodemode.PendingAction>>
        abstract reject: seq: float * executionId: string -> Promise<bool>
        abstract expirePaused: ?maxAgeMs: float -> Promise<ResizeArray<string>>
        abstract actionsToRevert: executionId: string -> Promise<ResizeArray<CloudflareCodemode.ToolLogEntry>>
        abstract markReverted: seq: float * executionId: string -> Promise<unit>
        abstract markRolledBack: executionId: string -> Promise<unit>
        abstract getExecution: id: string -> Promise<option<CloudflareCodemode.ExecutionState>>
        abstract listExecutions: ?limit: float -> Promise<ResizeArray<CloudflareCodemode.ExecutionState>>
        abstract deleteExecution: id: string -> Promise<bool>
        abstract pruneExecutions: ?keep: float -> Promise<float>
        abstract saveSnippet: name: string * options: CloudflareCodemode.SaveSnippetOptions -> Promise<CloudflareCodemode.Snippet>
        abstract getSnippet: name: string -> Promise<option<CloudflareCodemode.Snippet>>
        abstract listSnippets: unit -> Promise<ResizeArray<CloudflareCodemode.Snippet>>
        abstract deleteSnippet: name: string -> Promise<bool>

    [<Import("agents", "WorkflowQueryCriteria")>]
    type WorkflowQueryCriteria =
        abstract cursor: option<string> with get, set
        abstract orderBy: option<LiteralUnions.AscDesc> with get, set
        abstract limit: option<float> with get, set
        abstract metadata: option<OptionalAgentMethods> with get, set
        abstract workflowName: option<string> with get, set
        abstract status: option<U2<LiteralUnions.CompleteErroredPaused4918918f, ResizeArray<CloudflareWorkersTypes.CloudflareWorkersModule.WorkflowInstanceStatus>>> with get, set

    [<Import("agents", "DurableObjectEventStore")>]
    type DurableObjectEventStore =
        [<EmitConstructor>]
        abstract Create: storage: CloudflareWorkersTypes.DurableObjectStorage -> DurableObjectEventStore

        abstract ensureSeqLoaded: option<obj> with get, set
        abstract seqInit: option<obj> with get
        abstract seqByStream: option<obj> with get
        abstract storage: option<obj> with get
        abstract REPLAY_LIMIT: option<obj> with get
        abstract DELETE_CHUNK: option<obj> with get
        abstract SEQ_PAD: option<obj> with get
        abstract EVENT_KEY_PREFIX: option<obj> with get
        abstract storeEvent: streamId: string * message: ModelcontextprotocolSdk.JSONRPCMessage -> Promise<string>
        abstract getStreamIdForEventId: eventId: string -> Promise<option<string>>
        abstract replayEventsAfter: lastEventId: string * send: ModelcontextprotocolSdk.SharedLiterals.Send -> Promise<string>
        abstract clearStream: streamId: string -> Promise<unit>

    [<Import("agents", "WorkflowPage")>]
    type WorkflowPage =
        abstract nextCursor: option<string> with get, set
        abstract total: float with get, set
        abstract workflows: ResizeArray<Erased.Empty> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type WorkflowStatus =
        | [<CompiledName("running")>] Running
        | [<CompiledName("paused")>] Paused
        | [<CompiledName("unknown")>] Unknown
        | [<CompiledName("terminated")>] Terminated
        | [<CompiledName("queued")>] Queued
        | [<CompiledName("errored")>] Errored
        | [<CompiledName("complete")>] Complete
        | [<CompiledName("waiting")>] Waiting
        | [<CompiledName("waitingForPause")>] WaitingForPause

    [<Import("agents", "AgentClientStub")>]
    type AgentClientStub<'AgentT> = interface end

    [<Import("agents", "BrowserLiveViewUrl")>]
    type BrowserLiveViewUrl =
        abstract expiresInMs: float with get, set
        abstract targetId: string with get, set
        abstract url: string with get, set

    type WorkflowCallback =
        abstract progress: obj with get, set
        abstract timestamp: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Erased.Empty with get, set

        abstract workflowId: string with get, set
        abstract workflowName: string with get, set

    [<Import("agents", "RPCServerTransport")>]
    type RPCServerTransport =
        [<EmitConstructor>]
        abstract Create: ?options: Erased.Empty -> RPCServerTransport

        abstract _appendContinuation: option<obj> with get, set
        abstract _completeContinuation: option<obj> with get, set
        abstract _appendRequest: option<obj> with get, set
        abstract _completeRequest: option<obj> with get, set
        abstract _completePending: option<obj> with get, set
        abstract _appendPending: option<obj> with get, set
        abstract _makeTimeout: option<obj> with get, set
        abstract sessionId: option<string> with get, set
        abstract _pendingContinuations: option<obj> with get, set
        abstract _pendingRequests: option<obj> with get, set
        abstract _timeout: option<obj> with get, set
        abstract _protocolVersion: option<obj> with get, set
        abstract _started: option<obj> with get, set
        abstract onclose: unit -> unit
        abstract onerror: error: exn -> unit
        abstract onmessage: message: ModelcontextprotocolSdk.JSONRPCMessage * ?extra: ModelcontextprotocolSdk.MessageExtraInfo -> unit
        abstract setProtocolVersion: version: string -> unit
        abstract getProtocolVersion: unit -> option<string>
        abstract start: unit -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract send: message: ModelcontextprotocolSdk.JSONRPCMessage * ?options: ModelcontextprotocolSdk.TransportSendOptions -> Promise<unit>
        abstract _awaitPendingResponse: unit -> Promise<option<U5<SharedLiterals.IdJsonrpcMethodParams, SharedLiterals.JsonrpcMethodParams, SharedLiterals.IdJsonrpcResult, ModelcontextprotocolSdk.TypesJs.JSONRPCError, ResizeArray<ModelcontextprotocolSdk.JSONRPCMessage>>>>
        abstract handle: message: U5<SharedLiterals.IdJsonrpcMethodParams, SharedLiterals.JsonrpcMethodParams, SharedLiterals.IdJsonrpcResult, ModelcontextprotocolSdk.TypesJs.JSONRPCError, ResizeArray<ModelcontextprotocolSdk.JSONRPCMessage>> -> Promise<option<U5<SharedLiterals.IdJsonrpcMethodParams, SharedLiterals.JsonrpcMethodParams, SharedLiterals.IdJsonrpcResult, ModelcontextprotocolSdk.TypesJs.JSONRPCError, ResizeArray<ModelcontextprotocolSdk.JSONRPCMessage>>>>

    [<Import("agents", "BrowserConnector")>]
    type BrowserConnector =
        [<EmitConstructor>]
        abstract Create: ctx: U2<Erased.Empty, CloudflareWorkersTypes.ExecutionContext<option<obj>>> * options: BrowserConnectorOptions -> BrowserConnector

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract name: unit -> string
        abstract instructions: unit -> string
        abstract tools: unit -> Erased.Empty
        abstract onPassEnd: executionId: string * _status: Erased.Empty -> Promise<unit>
        abstract disposeExecution: executionId: string * _status: Erased.Empty -> Promise<unit>
        abstract sessionInfo: unit -> Promise<option<Erased.Empty>>
        abstract liveView: ?options: BrowserConnector.LiveView.Options -> Promise<option<BrowserLiveView>>
        abstract closeSession: unit -> Promise<unit>
        abstract sweep: ?options: BrowserConnectorSweepOptions -> Promise<BrowserConnectorSweepResult>

    [<Import("agents", "WorkflowEventPayload")>]
    type WorkflowEventPayload =
        abstract payload: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type GetBrowserRecording =
        abstract fetchImpl: (option<U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> -> option<CloudflareWorkersTypes.RequestInit<CloudflareWorkersTypes.RequestInitCfProperties>> -> Promise<CloudflareWorkersTypes.Response>>) with get, set
        abstract sessionId: string with get, set
        abstract apiToken: string with get, set
        abstract accountId: string with get, set

    [<Import("agents", "AgentToolChildAdapter")>]
    type AgentToolChildAdapter =
        abstract startAgentToolRun: input: obj * options: AgentToolChildAdapter.StartAgentToolRun.Options -> Promise<AgentToolChildAdapter.StartAgentToolRun>
        abstract cancelAgentToolRun: runId: string * ?reason: obj -> Promise<unit>
        abstract inspectAgentToolRun: runId: string -> Promise<option<obj>>
        abstract getAgentToolChunks: runId: string * ?options: AgentToolChildAdapter.GetAgentToolChunks.Options -> Promise<ResizeArray<AgentToolStoredChunk>>
        abstract tailAgentToolRun: runId: string * ?options: AgentToolChildAdapter.TailAgentToolRun.Options -> Promise<CloudflareWorkersTypes.ReadableStream<AgentToolStoredChunk>>

    type Subscribe1 =
        abstract channel: U4<SharedLiterals.AgentNamePayloadTimestampType30, SharedLiterals.AgentNamePayloadTimestampType31, SharedLiterals.AgentNamePayloadTimestampType32, SharedLiterals.AgentNamePayloadTimestampType33> with get, set
        abstract email: U3<SharedLiterals.AgentNamePayloadTimestampType74, SharedLiterals.AgentNamePayloadTimestampType75, SharedLiterals.AgentNamePayloadTimestampType76> with get, set
        abstract mcp: Erased.Empty with get, set
        abstract workflow: U8<SharedLiterals.AgentNamePayloadTimestampType77, SharedLiterals.AgentNamePayloadTimestampType79, SharedLiterals.AgentNamePayloadTimestampType80, SharedLiterals.AgentNamePayloadTimestampType81, SharedLiterals.AgentNamePayloadTimestampType82, SharedLiterals.AgentNamePayloadTimestampType83, SharedLiterals.AgentNamePayloadTimestampType84, SharedLiterals.AgentNamePayloadTimestampType85> with get, set
        abstract lifecycle: U3<SharedLiterals.AgentNamePayloadTimestampType71, SharedLiterals.AgentNamePayloadTimestampType72, SharedLiterals.AgentNamePayloadTimestampType73> with get, set
        abstract schedule: U9<SharedLiterals.AgentNamePayloadTimestampType3, SharedLiterals.AgentNamePayloadTimestampType4, SharedLiterals.AgentNamePayloadTimestampType5, SharedLiterals.AgentNamePayloadTimestampType6, SharedLiterals.AgentNamePayloadTimestampType7, SharedLiterals.AgentNamePayloadTimestampType8, SharedLiterals.AgentNamePayloadTimestampType10, SharedLiterals.AgentNamePayloadTimestampType11, SharedLiterals.AgentNamePayloadTimestampType13> with get, set
        abstract agentTool: U8<SharedLiterals.AgentNamePayloadTimestampType62, SharedLiterals.AgentNamePayloadTimestampType63, SharedLiterals.AgentNamePayloadTimestampType64, SharedLiterals.AgentNamePayloadTimestampType65, SharedLiterals.AgentNamePayloadTimestampType66, SharedLiterals.AgentNamePayloadTimestampType68, SharedLiterals.AgentNamePayloadTimestampType69, SharedLiterals.AgentNamePayloadTimestampType70> with get, set
        abstract fiber: U9<SharedLiterals.AgentNamePayloadTimestampType35, SharedLiterals.AgentNamePayloadTimestampType36, SharedLiterals.AgentNamePayloadTimestampType37, SharedLiterals.AgentNamePayloadTimestampType38, SharedLiterals.AgentNamePayloadTimestampType39, SharedLiterals.AgentNamePayloadTimestampType40, SharedLiterals.AgentNamePayloadTimestampType41, SharedLiterals.AgentNamePayloadTimestampType42, SharedLiterals.AgentNamePayloadTimestampType43> with get, set
        abstract transcript: SharedLiterals.AgentNamePayloadTimestampType55 with get, set
        abstract chat: U15<SharedLiterals.AgentNamePayloadTimestampType44, SharedLiterals.AgentNamePayloadTimestampType46, SharedLiterals.AgentNamePayloadTimestampType47, SharedLiterals.AgentNamePayloadTimestampType48, SharedLiterals.AgentNamePayloadTimestampType49, SharedLiterals.AgentNamePayloadTimestampType50, SharedLiterals.AgentNamePayloadTimestampType51, SharedLiterals.AgentNamePayloadTimestampType52, SharedLiterals.AgentNamePayloadTimestampType53, SharedLiterals.AgentNamePayloadTimestampType54, SharedLiterals.AgentNamePayloadTimestampType57, SharedLiterals.AgentNamePayloadTimestampType58, SharedLiterals.AgentNamePayloadTimestampType59, SharedLiterals.AgentNamePayloadTimestampType60, SharedLiterals.AgentNamePayloadTimestampType61> with get, set
        abstract message: U22<SharedLiterals.AgentNamePayloadTimestampType34, SharedLiterals.AgentNamePayloadTimestampType45, SharedLiterals.AgentNamePayloadTimestampType56, SharedLiterals.AgentNamePayloadTimestampType67, SharedLiterals.AgentNamePayloadTimestampType78, SharedLiterals.AgentNamePayloadTimestampType89, SharedLiterals.AgentNamePayloadTimestampType2, SharedLiterals.AgentNamePayloadTimestampType14, SharedLiterals.AgentNamePayloadTimestampType15, SharedLiterals.AgentNamePayloadTimestampType16, SharedLiterals.AgentNamePayloadTimestampType17, SharedLiterals.AgentNamePayloadTimestampType18, SharedLiterals.AgentNamePayloadTimestampType19, SharedLiterals.AgentNamePayloadTimestampType20, SharedLiterals.AgentNamePayloadTimestampType21, SharedLiterals.AgentNamePayloadTimestampType22, SharedLiterals.AgentNamePayloadTimestampType24, SharedLiterals.AgentNamePayloadTimestampType25, SharedLiterals.AgentNamePayloadTimestampType26, SharedLiterals.AgentNamePayloadTimestampType27, SharedLiterals.AgentNamePayloadTimestampType28, SharedLiterals.AgentNamePayloadTimestampType29> with get, set
        abstract rpc: U2<SharedLiterals.AgentNamePayloadTimestampType12, SharedLiterals.AgentNamePayloadTimestampType23> with get, set
        abstract state: SharedLiterals.AgentNamePayloadTimestampType with get, set

    type GetNamespacedData =
        abstract resourceTemplates: ResizeArray<obj> with get, set
        abstract resources: ResizeArray<obj> with get, set
        abstract prompts: ResizeArray<obj> with get, set
        abstract tools: ResizeArray<obj> with get, set

    type RunQuickAction =
        abstract Invoke: browser: Erased.Empty * action: string * params: Erased.Empty -> Promise<CloudflareWorkersTypes.Response>
        abstract Invoke: browser: Erased.Empty * action: Erased.Empty * params: Erased.Empty -> Promise<CloudflareWorkersTypes.Response>

    type AgentFetch =
        abstract name: option<string> with get, set
        abstract agent: string with get, set
        abstract query: option<U2<Partysocket.Params, unit -> U2<Partysocket.Params, Promise<Partysocket.Params>>>> with get, set
        abstract basePath: option<string> with get, set
        abstract host: string with get, set
        abstract protocol: option<LiteralUnions.HttpHttps> with get, set
        abstract prefix: option<string> with get, set
        abstract path: option<string> with get, set
        abstract fetch: (option<U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> -> option<CloudflareWorkersTypes.RequestInit<CloudflareWorkersTypes.RequestInitCfProperties>> -> Promise<CloudflareWorkersTypes.Response>>) with get, set

    [<Import("agents", "UntypedAgentClientCall")>]
    type UntypedAgentClientCall =
        abstract Invoke: method: string * ?args: ResizeArray<option<Erased.Empty>> * ?options: U2<CallOptions, StreamOptions> -> Promise<obj>

    [<Import("agents", "ClearableEventStore")>]
    type ClearableEventStore =
        inherit ModelcontextprotocolSdk.EventStore
        abstract clearStream: streamId: string -> Promise<unit>

    [<Import("agents", "AgentClientCall")>]
    type AgentClientCall = interface end

    [<Import("agents", "AgentWorkflow")>]
    type AgentWorkflow<'AgentType, 'Params, 'ProgressType, 'Env> =
        [<EmitConstructor>]
        abstract Create: ctx: CloudflareWorkersTypes.ExecutionContext<option<obj>> * env: 'Env -> AgentWorkflow<'AgentType, 'Params, 'ProgressType, 'Env>

        /// <param name="err" >
        /// - The caught error
        /// </param>
        abstract _autoReportError: option<obj> with get, set
        abstract workflowName: string with get
        abstract workflowId: string with get
        abstract agent: AgentWorkflow.Agent with get
        abstract _wrapStep: option<obj> with get, set
        abstract _disposeAgent: option<obj> with get, set
        abstract _runWithErrorReporting: option<obj> with get, set
        abstract _initFacetAgent: option<obj> with get, set
        abstract _initAgent: option<obj> with get, set
        abstract _errorReported: option<obj> with get, set

        [<EmitProperty("__agentInitCalled")>]
        abstract _agentInitCalled: option<obj> with get, set

        abstract _workflowName: option<obj> with get, set
        abstract _workflowId: option<obj> with get, set
        abstract _agent: option<obj> with get, set

        [<EmitProperty("__agentOrigin")>]
        abstract _agentOrigin: option<Erased.Empty> with get, set

        [<EmitProperty("__agentBinding")>]
        abstract _agentBinding: string with get, set

        [<EmitProperty("__agentName")>]
        abstract _agentName: string with get, set

        abstract extendStep: step: AgentWorkflowStep * _event: CloudflareWorkersTypes.SharedLiterals.InstanceIdPayloadScheduleFdd336c4 -> AgentWorkflowStep
        abstract notifyAgent: callback: U4<WorkflowEventCallback, WorkflowErrorCallback, WorkflowCompleteCallback, obj> -> Promise<unit>
        abstract reportProgress: progress: 'ProgressType -> Promise<unit>
        abstract broadcastToClients: ?message: obj -> unit
        abstract waitForApproval: step: AgentWorkflowStep * ?options: WaitForApprovalOptions -> Promise<obj>

    [<Import("agents", "WorkflowRejectedError")>]
    type WorkflowRejectedError =
        [<EmitConstructor>]
        abstract Create: ?reason: string * ?workflowId: string -> WorkflowRejectedError

        abstract workflowId: option<string> with get
        abstract reason: option<string> with get

    [<Import("agents", "AgentToolRunState")>]
    type AgentToolRunState =
        abstract subAgent: SharedLiterals.AgentName with get, set
        abstract milestones: option<ResizeArray<Erased.Empty>> with get, set
        abstract progress: option<Erased.Empty> with get, set
        abstract childStillRunning: option<bool> with get, set
        abstract reason: option<Erased.Empty> with get, set
        abstract error: option<string> with get, set
        abstract summary: option<string> with get, set
        abstract parts: ResizeArray<obj> with get, set
        abstract status: LiteralUnions.AbortedCompletedError103eaa86 with get, set
        abstract display: option<Erased.Empty> with get, set
        abstract order: float with get, set
        abstract inputPreview: option<obj> with get, set
        abstract parentToolCallId: option<string> with get, set
        abstract agentType: string with get, set
        abstract runId: string with get, set

    [<Import("agents", "StreamableHTTPEdgeClientTransport")>]
    type StreamableHTTPEdgeClientTransport =
        interface
            [<EmitConstructor>]
            abstract Create: url: CloudflareWorkersTypes.URL * options: ModelcontextprotocolSdk.StreamableHTTPClientTransportOptions -> StreamableHTTPEdgeClientTransport
        end

    [<Import("agents", "AgentPromiseReturnType")>]
    type AgentPromiseReturnType<'T, 'K> = interface end

    [<Import("agents", "SqlError")>]
    type SqlError =
        [<EmitConstructor>]
        abstract Create: query: string * ?cause: obj -> SqlError

        abstract query: string with get

    type RouteAgentEmail =
        abstract routingRetry: option<U2<Partyserver.RoutingRetryOptions, bool>> with get, set
        abstract cors: option<U4<OptionalAgentMethods, CloudflareWorkersTypes.Headers, seq<seq<string>>, bool>> with get, set
        abstract props: option<obj> with get, set
        abstract locationHint: option<CloudflareWorkersTypes.DurableObjectLocationHint> with get, set
        abstract jurisdiction: option<CloudflareWorkersTypes.DurableObjectJurisdiction> with get, set
        abstract prefix: option<string> with get, set
        abstract onBeforeConnect: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * lobby: Partyserver.Lobby<obj> -> option<U3<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, Promise<option<U2<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>>>>>>
        abstract onBeforeRequest: req: CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> * lobby: Partyserver.Lobby<obj> -> option<U3<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, Promise<option<U2<CloudflareWorkersTypes.Response, CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>>>>>>
        abstract resolver: email: CloudflareWorkersTypes.ForwardableEmailMessage * env: obj -> Promise<option<Erased.Empty>>
        abstract onNoRoute: email: CloudflareWorkersTypes.ForwardableEmailMessage -> option<Promise<unit>>

    type GetCurrentAgent =
        abstract email: option<AgentEmail> with get, set
        abstract request: option<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>> with get, set
        abstract connection: option<obj> with get, set
        abstract agent: option<Erased.Empty> with get, set

    [<Import("agents", "StreamOptions")>]
    type StreamOptions =
        abstract onChunk: ?chunk: obj -> unit
        abstract onDone: ?finalChunk: obj -> unit
        abstract onError: error: string -> unit

    [<Import("agents", "WorkflowProgressCallback")>]
    type WorkflowProgressCallback<'P> =
        abstract progress: 'P with get, set
        abstract timestamp: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Erased.Empty with get, set

        abstract workflowId: string with get, set
        abstract workflowName: string with get, set

    [<Import("agents", "BrowserConnectorSessionOptions")>]
    type BrowserConnectorSessionOptions =
        abstract recording: option<bool> with get, set
        abstract keepAliveMs: option<float> with get, set
        abstract key: option<string> with get, set
        abstract mode: option<LiteralUnions.DynamicOneShotReuse> with get, set

    [<Import("agents", "AgentToolRunInspection")>]
    type AgentToolRunInspection =
        abstract milestones: option<ResizeArray<Erased.Empty>> with get, set
        abstract progress: option<Erased.Empty> with get, set
        abstract completedAt: option<float> with get, set
        abstract startedAt: float with get, set
        abstract error: option<string> with get, set
        abstract summary: option<string> with get, set
        abstract output: option<obj> with get, set
        abstract streamId: option<string> with get, set
        abstract requestId: option<string> with get, set
        abstract status: LiteralUnions.AbortedCompletedErrorRunningStarting with get, set
        abstract runId: string with get, set

    [<Import("agents", "WorkflowCompleteCallback")>]
    type WorkflowCompleteCallback =
        abstract result: option<obj> with get, set
        abstract timestamp: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Erased.Empty with get, set

        abstract workflowId: string with get, set
        abstract workflowName: string with get, set

    [<Import("agents", "BrowserConnectorSweepOptions")>]
    type BrowserConnectorSweepOptions =
        abstract maxExecIdleMs: option<float> with get, set
        abstract maxIdleMs: option<float> with get, set

    [<Import("agents", "AgentStub")>]
    type AgentStub =
        abstract Item: key: string -> option<obj>

    type McpClientOptions = option<ModelcontextprotocolSdk.ClientOptions>

    [<Import("agents", "TypedAgentClientCall")>]
    type TypedAgentClientCall =
        abstract Invoke: method: obj * ?args: ResizeArray<option<obj>> * ?options: U2<CallOptions, StreamOptions> -> option<U3<obj, Promise<option<U2<obj, obj>>>, obj>>

    [<Import("agents", "StateUpdateMessage")>]
    type StateUpdateMessage =
        abstract state: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: obj with get, set

    [<Import("agents", "__type")>]
    type ElicitRequest = interface end

    [<Import("agents", "RPCResponse")>]
    type RPCResponse =
        abstract id: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: obj with get, set

    [<Import("agents", "AgentToolStoredChunk")>]
    type AgentToolStoredChunk =
        abstract body: string with get, set
        abstract sequence: float with get, set

    type DEFAULT_AGENT_STATIC_OPTIONS =
        abstract maxAlarmMemoryLimitStrikes: float with get, set
        abstract detachedNoProgressBudgetMs: float with get, set
        abstract detachedMaxBudgetMs: float with get, set
        abstract agentToolReattachMaxWindowMs: float with get, set
        abstract agentToolReattachNoProgressTimeoutMs: float with get, set
        abstract fiberRecoveryMaxAgeMs: float with get, set
        abstract fiberRecoveryScanDeadlineMs: float with get, set
        abstract fiberRecoveryHookTimeoutMs: float with get, set
        abstract retry: DEFAULT_AGENT_STATIC_OPTIONS.Retry with get, set
        abstract keepAliveIntervalMs: float with get, set
        abstract hungScheduleTimeoutSeconds: float with get, set
        abstract sendIdentityOnConnect: bool with get, set
        abstract hibernate: bool with get, set

    [<Import("agents", "AgentEmail")>]
    type AgentEmail =
        abstract _secureRouted: option<bool> with get, set
        abstract rawSize: float with get, set
        abstract headers: CloudflareWorkersTypes.Headers with get, set

        [<EmitProperty("to")>]
        abstract ``to``: string with get, set

        abstract from: string with get, set
        abstract getRaw: unit -> Promise<Uint8Array>
        abstract setReject: reason: string -> unit
        abstract forward: rcptTo: string * ?headers: CloudflareWorkersTypes.Headers -> Promise<CloudflareWorkersTypes.EmailSendResult>
        abstract reply: options: SharedLiterals.FromRawTo -> Promise<CloudflareWorkersTypes.EmailSendResult>

    /// <deprecated>
    /// Use DurableObjectNamespace instead
    /// </deprecated>
    [<Import("agents", "AgentNamespace")>]
    type AgentNamespace<'Agentic> = interface end

    type UnstableScheduleSchema =
        [<EmitProperty("when")>]
        abstract ``when``: Zod.ZodType with get, set

        abstract description: Zod.ZodType with get, set

    [<Import("agents", "AgentClientFetchOptions")>]
    type AgentClientFetchOptions =
        abstract name: option<string> with get, set
        abstract agent: string with get, set
        abstract query: option<U2<Partysocket.Params, unit -> U2<Partysocket.Params, Promise<Partysocket.Params>>>> with get, set
        abstract basePath: option<string> with get, set
        abstract host: string with get, set
        abstract protocol: option<LiteralUnions.HttpHttps> with get, set
        abstract prefix: option<string> with get, set
        abstract path: option<string> with get, set
        abstract fetch: (option<U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> -> option<CloudflareWorkersTypes.RequestInit<CloudflareWorkersTypes.RequestInitCfProperties>> -> Promise<CloudflareWorkersTypes.Response>>) with get, set

    [<Import("agents", "DurableBrowserSessionStore")>]
    type DurableBrowserSessionStore =
        [<EmitConstructor>]
        abstract Create: storage: CloudflareWorkersTypes.DurableObjectStorage -> DurableBrowserSessionStore

        abstract storage: option<obj> with get

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract acquireLock: key: string -> Promise<Erased.Empty>
        abstract get: key: string -> Promise<option<StoredBrowserSession>>
        abstract set: key: string * session: StoredBrowserSession -> Promise<unit>
        abstract delete: key: string -> Promise<unit>
        abstract list: prefix: string -> Promise<Map<string, StoredBrowserSession>>

    [<Import("agents", "UntypedAgentStub")>]
    type UntypedAgentStub = interface end

    [<Import("agents", "RequiredAgentMethods")>]
    type RequiredAgentMethods<'T> = interface end

    [<Import("agents", "WorkflowErrorCallback")>]
    type WorkflowErrorCallback =
        abstract error: string with get, set
        abstract timestamp: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Erased.Empty with get, set

        abstract workflowId: string with get, set
        abstract workflowName: string with get, set

    [<Import("agents", "OptionalAgentMethods")>]
    type OptionalAgentMethods =
        abstract Item: key: string -> option<obj>

    [<Import("agents", "DurableObjectOAuthClientProvider")>]
    type DurableObjectOAuthClientProvider =
        [<EmitConstructor>]
        abstract Create: storage: CloudflareWorkersTypes.DurableObjectStorage * clientName: string * baseRedirectUrl: string -> DurableObjectOAuthClientProvider

        abstract codeVerifierForState: option<obj> with get, set
        abstract deleteExpiredChallengeCodeVerifiers: option<obj> with get, set
        abstract authUrl: option<string> with get
        abstract serverId: string with get
        abstract clientId: string with get
        abstract redirectUrl: string with get
        abstract clientUri: string with get
        abstract clientMetadata: ModelcontextprotocolSdk.Shared.AuthJs.OAuthClientMetadata with get
        abstract _clientId_: option<obj> with get, set
        abstract _serverId_: option<obj> with get, set
        abstract _authUrl_: option<obj> with get, set
        abstract baseRedirectUrl: string with get, set
        abstract clientName: string with get, set
        abstract storage: CloudflareWorkersTypes.DurableObjectStorage with get, set
        abstract keyPrefix: clientId: string -> string
        abstract clientInfoKey: clientId: string -> string
        abstract clientInformation: unit -> Promise<option<ModelcontextprotocolSdk.Shared.AuthJs.OAuthClientInformation>>
        abstract saveClientInformation: clientInformation: ModelcontextprotocolSdk.Shared.AuthJs.OAuthClientInformationFull -> Promise<unit>
        abstract tokenKey: clientId: string -> string
        abstract tokens: unit -> Promise<option<ModelcontextprotocolSdk.Shared.AuthJs.OAuthTokens>>
        abstract saveTokens: tokens: ModelcontextprotocolSdk.Shared.AuthJs.OAuthTokens -> Promise<unit>
        abstract stateKey: nonce: string -> string
        abstract state: unit -> Promise<string>
        abstract checkState: state: string -> Promise<SharedLiterals.ErrorServerIdValid>
        abstract consumeState: state: string -> Promise<unit>
        abstract redirectToAuthorization: authUrl: CloudflareWorkersTypes.URL -> Promise<unit>
        abstract invalidateCredentials: scope: LiteralUnions.AllClientTokensVerifier -> Promise<unit>
        abstract codeVerifierKey: clientId: string -> string
        abstract stateCodeVerifierPrefix: clientId: string -> string
        abstract stateCodeVerifierKey: clientId: string * nonce: string -> string
        abstract challengeCodeVerifierPrefix: clientId: string -> string
        abstract challengeCodeVerifierKey: clientId: string * codeChallenge: string -> string
        abstract codeVerifierKeys: clientId: string * ?options: DurableObjectOAuthClientProvider.CodeVerifierKeys.Options -> Promise<ResizeArray<string>>
        abstract saveCodeVerifier: verifier: string -> Promise<unit>
        abstract codeVerifier: unit -> Promise<string>
        abstract runWithCodeVerifierState: state: string * callback: (unit -> Promise<obj>) -> Promise<obj>
        abstract deleteCodeVerifier: unit -> Promise<unit>

    type CreateSecureReplyEmailResolver =
        abstract maxAge: option<float> with get, set
        abstract onInvalidSignature: email: CloudflareWorkersTypes.ForwardableEmailMessage * reason: Erased.Empty -> unit

    [<Import("agents", "AgentConnectionError")>]
    type AgentConnectionError =
        [<EmitConstructor>]
        abstract Create: event: CloudflareWorkersTypes.CloseEvent -> AgentConnectionError

        abstract wasClean: bool with get, set
        abstract reason: string with get, set
        abstract code: float with get, set

    type CreateBrowserSession =
        abstract recording: option<bool> with get, set
        abstract includeTargets: option<bool> with get, set
        abstract keepAliveMs: option<float> with get, set

    [<Import("agents", "BrowserLiveViewTarget")>]
    type BrowserLiveViewTarget =
        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set
        abstract pageUrl: option<string> with get, set
        abstract url: string with get, set
        abstract targetId: string with get, set

    type RequiredArgsAgentClientCall = obj -> option<ResizeArray<option<obj>>> -> option<U2<CallOptions, StreamOptions>> -> option<U3<obj, Promise<option<U2<obj, obj>>>, obj>>

    [<Import("agents", "AgentClientOptions")>]
    type AgentClientOptions =
        abstract defaultCallTimeout: option<float> with get, set
        abstract name: option<string> with get, set
        abstract agent: string with get, set
        abstract disableNameValidation: option<bool> with get, set
        abstract protocols: option<U4<ResizeArray<string>, unit -> option<U2<ResizeArray<string>, string>>, unit -> Promise<option<U2<ResizeArray<string>, string>>>, string>> with get, set
        abstract debugLogger: (option<ResizeArray<option<obj>> -> unit>) with get, set
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

        abstract query: option<U2<Partysocket.Params, unit -> U2<Partysocket.Params, Promise<Partysocket.Params>>>> with get, set
        abstract basePath: option<string> with get, set
        abstract host: string with get, set
        abstract protocol: option<LiteralUnions.WsWss> with get, set
        abstract prefix: option<string> with get, set
        abstract path: option<string> with get, set
        abstract id: option<string> with get, set
        abstract debug: option<bool> with get, set
        abstract shouldReconnectOnClose: event: CloudflareWorkersTypes.CloseEvent -> bool
        abstract onStateUpdate: state: obj * source: LiteralUnions.ClientServer -> unit
        abstract onStateUpdateError: error: string -> unit
        abstract onIdentity: name: string * agent: string -> unit
        abstract onIdentityChange: oldName: string * newName: string * oldAgent: string * newAgent: string -> unit
        abstract onConnectionError: error: AgentConnectionError -> unit

    [<Import("agents", "__type")>]
    type ElicitResult = interface end

    [<Import("agents", "WorkflowEventCallback")>]
    type WorkflowEventCallback =
        abstract event: option<obj> with get, set
        abstract timestamp: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Erased.Empty with get, set

        abstract workflowId: string with get, set
        abstract workflowName: string with get, set

    [<Import("agents", "MCPServerMessage")>]
    type MCPServerMessage =
        abstract mcp: Erased.Empty with get, set

        [<EmitProperty("type")>]
        abstract ``type``: obj with get, set

    [<Import("agents", "BrowserConnectorSweepResult")>]
    type BrowserConnectorSweepResult =
        abstract swept: ResizeArray<SharedLiterals.KeySessionId> with get, set

    type ConnectUrl =
        abstract headers: option<OptionalAgentMethods> with get, set
        abstract timeoutMs: option<float> with get, set

    [<Import("agents", "TerminalReconnectOptions")>]
    type TerminalReconnectOptions =
        abstract shouldReconnectOnClose: event: CloudflareWorkersTypes.CloseEvent -> bool

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LiveViewMode =
        | [<CompiledName("tab")>] Tab
        | [<CompiledName("devtools")>] Devtools

    module AgentClient =
        type Call =
            abstract Invoke: method: obj * ?args: ResizeArray<option<obj>> * ?options: U2<CallOptions, StreamOptions> -> option<U3<obj, Promise<option<U2<obj, obj>>>, obj>>

        type Options =
            abstract defaultCallTimeout: option<float> with get, set
            abstract name: option<string> with get, set
            abstract agent: string with get, set
            abstract disableNameValidation: option<bool> with get, set
            abstract protocols: option<U4<ResizeArray<string>, unit -> option<U2<ResizeArray<string>, string>>, unit -> Promise<option<U2<ResizeArray<string>, string>>>, string>> with get, set
            abstract debugLogger: (option<ResizeArray<option<obj>> -> unit>) with get, set
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

            abstract query: option<U2<Partysocket.Params, unit -> U2<Partysocket.Params, Promise<Partysocket.Params>>>> with get, set
            abstract basePath: option<string> with get, set
            abstract host: string with get, set
            abstract protocol: option<LiteralUnions.WsWss> with get, set
            abstract prefix: option<string> with get, set
            abstract path: option<string> with get, set
            abstract id: option<string> with get, set
            abstract debug: option<bool> with get, set
            abstract shouldReconnectOnClose: event: CloudflareWorkersTypes.CloseEvent -> bool
            abstract onStateUpdate: state: obj * source: LiteralUnions.ClientServer -> unit
            abstract onStateUpdateError: error: string -> unit
            abstract onIdentity: name: string * agent: string -> unit
            abstract onIdentityChange: oldName: string * newName: string * oldAgent: string * newAgent: string -> unit
            abstract onConnectionError: error: AgentConnectionError -> unit

    module AgentToolChildAdapter =
        type StartAgentToolRun =
            abstract milestones: option<ResizeArray<Erased.Empty>> with get, set
            abstract progress: option<Erased.Empty> with get, set
            abstract completedAt: option<float> with get, set
            abstract startedAt: float with get, set
            abstract error: option<string> with get, set
            abstract summary: option<string> with get, set
            abstract output: option<StartAgentToolRun.Output> with get, set
            abstract streamId: option<string> with get, set
            abstract requestId: option<string> with get, set
            abstract status: LiteralUnions.AbortedCompletedErrorRunningStarting with get, set
            abstract runId: string with get, set

        type GetAgentToolChunks =
            abstract body: string with get, set
            abstract sequence: float with get, set

        module GetAgentToolChunks =
            type Options =
                abstract afterSequence: option<float> with get, set

        module StartAgentToolRun =
            type Output = interface end

            type Options =
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract runId: string with get, set

        module TailAgentToolRun =
            type Options =
                abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                abstract afterSequence: option<float> with get, set

    module AgentToolEventState =
        type UnboundRuns =
            abstract subAgent: SharedLiterals.AgentName with get, set
            abstract milestones: option<ResizeArray<Erased.Empty>> with get, set
            abstract progress: option<Erased.Empty> with get, set
            abstract childStillRunning: option<bool> with get, set
            abstract reason: option<Erased.Empty> with get, set
            abstract error: option<string> with get, set
            abstract summary: option<string> with get, set
            abstract parts: ResizeArray<obj> with get, set
            abstract status: LiteralUnions.AbortedCompletedError103eaa86 with get, set
            abstract display: option<Erased.Empty> with get, set
            abstract order: float with get, set
            abstract parentToolCallId: option<string> with get, set
            abstract agentType: string with get, set
            abstract runId: string with get, set

        module UnboundRuns =
            type Display =
                abstract icon: option<string> with get, set
                abstract name: option<string> with get, set
                abstract Item: key: string -> option<obj>

    module AgentToolRunState =
        type Display =
            abstract icon: option<string> with get, set
            abstract name: option<string> with get, set
            abstract Item: key: string -> option<obj>

    module AgentWorkflow =
        type Agent =
            abstract name: option<string> with get
            abstract id: CloudflareWorkersTypes.DurableObjectId with get
            abstract Invoke: [<ParamArray>] args: CloudflareWorkersTypes.Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>
            abstract connect: address: U2<CloudflareWorkersTypes.SocketAddress, string> * ?options: CloudflareWorkersTypes.SocketOptions -> CloudflareWorkersTypes.Socket

        module WaitForApproval =
            type Options =
                abstract eventType: option<string> with get, set
                abstract timeout: option<CloudflareWorkersTypes.WorkflowRetentionDuration> with get, set
                abstract stepName: option<string> with get, set

    module BrowserConnector =
        type Options =
            abstract timeout: option<float> with get, set
            abstract cdpHeaders: option<unit> with get, set
            abstract cdpUrl: option<unit> with get, set
            abstract session: option<BrowserConnectorSessionOptions> with get, set
            abstract store: Erased.Empty with get, set
            abstract browser: Erased.Empty with get, set

        module LiveView =
            type Options =
                abstract mode: option<LiveViewMode> with get, set

        module Options =
            type Case2 =
                abstract timeout: option<float> with get, set
                abstract session: option<unit> with get, set
                abstract store: option<unit> with get, set
                abstract browser: option<unit> with get, set
                abstract cdpHeaders: option<OptionalAgentMethods> with get, set
                abstract cdpUrl: string with get, set

    module CodemodeRuntime =
        module Begin =
            type Options =
                abstract connectors: option<ResizeArray<string>> with get, set
                abstract maxExecutions: option<float> with get, set

    module DEFAULT_AGENT_STATIC_OPTIONS =
        type Retry =
            abstract maxDelayMs: float with get, set
            abstract baseDelayMs: float with get, set
            abstract maxAttempts: float with get, set

    module DurableObjectOAuthClientProvider =
        module CodeVerifierKeys =
            type Options =
                abstract includeChallengeKeys: option<bool> with get, set

    module GetAgentByName =
        type Case2 =
            abstract name: option<string> with get
            abstract id: CloudflareWorkersTypes.DurableObjectId with get
            abstract Invoke: [<ParamArray>] args: CloudflareWorkersTypes.Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<CloudflareWorkersTypes.Request<option<obj>, U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>>, CloudflareWorkersTypes.URL, string> * ?init: CloudflareWorkersTypes.RequestInit<U2<CloudflareWorkersTypes.RequestInitCfProperties, obj>> -> Promise<CloudflareWorkersTypes.Response>
            abstract connect: address: U2<CloudflareWorkersTypes.SocketAddress, string> * ?options: CloudflareWorkersTypes.SocketOptions -> CloudflareWorkersTypes.Socket

    module MCPServerMessage =
        module Type =
            module CF_AGENT_MCP_SERVERS =
                [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
                type CF_AGENT_MCP_SERVERS = | [<CompiledName("cf_agent_mcp_servers")>] CF_AGENT_MCP_SERVERS

    module MCPServerOptions =
        type Transport =
            abstract sessionId: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<Erased.Empty> with get, set

            abstract headers: option<CloudflareWorkersTypes.HeadersInit> with get, set

    module RPCResponse =
        module Type =
            module RPC =
                [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
                type RPC = | [<CompiledName("rpc")>] RPC

    module RunQuickAction =
        type Params =
            abstract selector: option<string> with get, set
            abstract screenshotOptions: option<OptionalAgentMethods> with get, set
            abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
            abstract userAgent: option<string> with get, set
            abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
            abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
            abstract cookies: option<ResizeArray<option<obj>>> with get, set
            abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
            abstract gotoOptions: option<Erased.Empty> with get, set
            abstract html: option<unit> with get, set
            abstract url: string with get, set

        module Params =
            type Case8 =
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract url: option<unit> with get, set
                abstract html: string with get, set

            type Case2 =
                abstract selector: option<string> with get, set
                abstract screenshotOptions: option<OptionalAgentMethods> with get, set
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract url: option<unit> with get, set
                abstract html: string with get, set

            type Case3 =
                abstract elements: ResizeArray<SharedLiterals.Selector> with get, set
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract html: option<unit> with get, set
                abstract url: string with get, set

            type Case4 =
                abstract elements: ResizeArray<SharedLiterals.Selector> with get, set
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract url: option<unit> with get, set
                abstract html: string with get, set

            type Case5 =
                [<EmitProperty("custom_ai")>]
                abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel>> with get, set

                [<EmitProperty("response_format")>]
                abstract responseFormat: option<SharedLiterals.SchemaType> with get, set

                abstract prompt: option<string> with get, set
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract html: option<unit> with get, set
                abstract url: string with get, set

            type Case6 =
                [<EmitProperty("custom_ai")>]
                abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel>> with get, set

                [<EmitProperty("response_format")>]
                abstract responseFormat: option<SharedLiterals.SchemaType> with get, set

                abstract prompt: option<string> with get, set
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract url: option<unit> with get, set
                abstract html: string with get, set

            type Case7 =
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract userAgent: option<string> with get, set
                abstract setExtraHTTPHeaders: option<OptionalAgentMethods> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract cookies: option<ResizeArray<option<obj>>> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHeightWidth> with get, set
                abstract gotoOptions: option<Erased.Empty> with get, set
                abstract html: option<unit> with get, set
                abstract url: string with get, set

    module Schedule =
        type When =
            abstract date: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        module When =
            type Case2 =
                abstract delayInSeconds: float with get, set

                [<EmitProperty("type")>]
                abstract ``type``: string with get, set

            type Case3 =
                abstract cron: string with get, set

                [<EmitProperty("type")>]
                abstract ``type``: string with get, set

            type Case4 =
                [<EmitProperty("type")>]
                abstract ``type``: string with get, set

    module SharedLiterals =
        type DescriptionIdName =
            abstract description: option<string> with get, set
            abstract name: string with get, set
            abstract id: string with get, set

        type ProgressType<'P> =
            abstract progress: 'P with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Type =
            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type CommandsDescriptionEventsNameTypes =
            abstract types: ResizeArray<CommandsDescriptionEventsNameTypes> with get, set
            abstract events: ResizeArray<CommandsDescriptionEventsNameTypes> with get, set
            abstract commands: ResizeArray<CommandsDescriptionEventsNameTypes> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set

        type TimestampTypeWorkflowIdWorkflowName =
            abstract timestamp: float with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Erased.Empty with get, set

            abstract workflowId: string with get, set
            abstract workflowName: string with get, set

        type DescriptionMethodName =
            abstract description: option<string> with get, set
            abstract method: string with get, set
            abstract name: string with get, set

        type ResultType =
            abstract result: option<obj> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type EventType =
            abstract event: option<obj> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type KeySessionId =
            abstract sessionId: string with get, set
            abstract key: string with get, set

        type MethodArgsOptionsMethodArgsOptions<'T> =
            abstract Invoke: method: string * ?args: ResizeArray<option<Erased.Empty>> * ?options: U2<CallOptions, StreamOptions> -> Promise<'T>

        type AgentName =
            abstract name: string with get, set
            abstract agent: string with get, set

        type Date =
            abstract date: Date with get, set

        type StreamTimeout =
            abstract stream: option<StreamOptions> with get, set
            abstract timeout: option<float> with get, set

        type DescriptionEventName =
            abstract description: option<string> with get, set
            abstract event: string with get, set
            abstract name: string with get, set

        type ErrorType =
            abstract error: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type DescriptionWhen =
            [<EmitProperty("when")>]
            abstract ``when``: Zod.ZodType with get, set

            abstract description: Zod.ZodType with get, set

        type DelayInSecondsType =
            abstract delayInSeconds: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type DateType =
            abstract date: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

        type OnChunkOnDoneOnError =
            abstract onChunk: ?chunk: obj -> unit
            abstract onDone: ?finalChunk: obj -> unit
            abstract onError: error: string -> unit

        type CronType =
            abstract cron: Zod.ZodType with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Zod.ZodType with get, set

    module StateUpdateMessage =
        module Type =
            module CF_AGENT_STATE =
                [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
                type CF_AGENT_STATE = | [<CompiledName("cf_agent_state")>] CF_AGENT_STATE

    module WorkflowCallback =
        type Case2 =
            abstract result: option<obj> with get, set
            abstract timestamp: float with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Erased.Empty with get, set

            abstract workflowId: string with get, set
            abstract workflowName: string with get, set

        type Case3 =
            abstract error: string with get, set
            abstract timestamp: float with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Erased.Empty with get, set

            abstract workflowId: string with get, set
            abstract workflowName: string with get, set

        type Case4 =
            abstract event: option<obj> with get, set
            abstract timestamp: float with get, set

            [<EmitProperty("type")>]
            abstract ``type``: Erased.Empty with get, set

            abstract workflowId: string with get, set
            abstract workflowName: string with get, set
