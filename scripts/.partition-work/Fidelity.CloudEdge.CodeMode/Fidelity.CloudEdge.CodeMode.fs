namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

type ICloudflareCodemode =
    [<Import("@cloudflare/codemode", "sanitizeToolName")>]
    static member sanitizeToolName(name: string) : string = JS.undefined

    [<Import("@cloudflare/codemode", "createBrowserCodeTool")>]
    static member createBrowserCodeTool(options: obj) : CloudflareCodemode.BrowserCodeToolDescriptor = JS.undefined

    [<Import("@cloudflare/codemode", "normalizeCode")>]
    static member normalizeCode(code: string) : string = JS.undefined

    [<Import("@cloudflare/codemode", "truncateResponse")>]
    static member truncateResponse(text: string, ?options: obj) : string = JS.undefined

    [<Erase>]
    member _.MAX_DURABLE_VALUE_BYTES: int = JS.undefined

    [<Erase>]
    member _.DEFAULT_MAX_EXECUTIONS: int = JS.undefined

    [<Import("@cloudflare/codemode", "generateTypesFromJsonSchema")>]
    static member generateTypesFromJsonSchema(tools: Erased.Empty) : string = JS.undefined

    [<Import("@cloudflare/codemode", "codeMcpServer")>]
    static member codeMcpServer(options: obj) : Promise<ModelcontextprotocolSdk.McpServer> = JS.undefined

    [<Import("@cloudflare/codemode", "jsonSchemaToType")>]
    static member jsonSchemaToType(schema: Erased.JsonSchema, typeName: string) : string = JS.undefined

    [<Import("@cloudflare/codemode", "runCode")>]
    static member runCode(codeexecutorprovidersconnectors: obj) : Promise<Erased.Empty> = JS.undefined

    [<Import("@cloudflare/codemode", "openApiMcpServer")>]
    static member openApiMcpServer(options: obj) : ModelcontextprotocolSdk.McpServer = JS.undefined

    [<Import("@cloudflare/codemode", "resolveProvider")>]
    static member resolveProvider(provider: Erased.Empty) : Erased.Empty = JS.undefined

    [<Import("@cloudflare/codemode", "truncateResult")>]
    static member truncateResult(value: option<obj>, ?options: obj) : option<obj> = JS.undefined

    [<Import("@cloudflare/codemode", "createCodemodeRuntime")>]
    static member createCodemodeRuntime(options: obj) : CloudflareCodemode.CodemodeRuntimeHandle = JS.undefined

    [<Erase>]
    member _.DEFAULT_PAUSED_TTL_MS: float = JS.undefined

module CloudflareCodemode =
    [<Import("@cloudflare/codemode", "CodemodeRollbackOptions")>]
    type CodemodeRollbackOptions =
        abstract executionId: string with get, set

    [<Import("@cloudflare/codemode", "CodemodeRuntimeToolOptions")>]
    type CodemodeRuntimeToolOptions =
        abstract connectorHints: option<obj> with get, set
        abstract description: option<string> with get, set

    [<Import("@cloudflare/codemode", "JsonSchemaExecutableToolDescriptor")>]
    type JsonSchemaExecutableToolDescriptor =
        abstract name: option<string> with get, set
        abstract execute: args: obj -> Promise<option<obj>>

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ExecutionStatus =
        | [<CompiledName("error")>] Error
        | [<CompiledName("running")>] Running
        | [<CompiledName("paused")>] Paused
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("rejected")>] Rejected
        | [<CompiledName("rolled_back")>] RolledBack

    [<Import("@cloudflare/codemode", "DescribeOutput")>]
    type DescribeOutput =
        abstract kind: LiteralUnions.ConnectorMethodSnippet with get, set
        abstract types: string with get, set
        abstract description: option<string> with get, set
        abstract path: string with get, set

    [<Import("@cloudflare/codemode", "McpConnectionLike")>]
    type McpConnectionLike =
        abstract tools: option<ResizeArray<ModelcontextprotocolSdk.TypesJs.Tool>> with get, set
        abstract instructions: option<string> with get, set
        abstract client: SharedLiterals.CallTool2 with get, set
        abstract name: option<string> with get, set
        abstract fetchTools: unit -> Promise<ResizeArray<ModelcontextprotocolSdk.TypesJs.Tool>>

    [<Import("@cloudflare/codemode", "SaveSnippetOptions")>]
    type SaveSnippetOptions =
        abstract executionId: string with get, set
        abstract inputSchema: option<obj> with get, set
        abstract description: option<string> with get, set

    [<Import("@cloudflare/codemode", "ProxyToolInputSchema")>]
    type ProxyToolInputSchema =
        [<EmitProperty("~standard")>]
        abstract ``~standard``: SharedLiterals.JsonSchemaValidateVendorVersion with get

    [<Import("@cloudflare/codemode", "Snippet")>]
    type Snippet =
        abstract connectors: option<ResizeArray<string>> with get, set
        abstract inputSchema: option<obj> with get, set
        abstract savedAt: float with get, set
        abstract code: string with get, set
        abstract description: string with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/codemode", "CodemodeExpireOptions")>]
    type CodemodeExpireOptions =
        abstract maxAgeMs: option<float> with get, set

    [<Import("@cloudflare/codemode", "DynamicWorkerExecutor")>]
    type DynamicWorkerExecutor =
        [<EmitConstructor>]
        abstract Create: options: DynamicWorkerExecutorOptions -> DynamicWorkerExecutor

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract execute: code: string * providersOrFns: U2<ResizeArray<Erased.Empty>, obj> * ?options: Erased.Empty -> Promise<Erased.Empty>

    [<Import("@cloudflare/codemode", "ApprovalAwareJsonSchemaExecutableToolDescriptor")>]
    type ApprovalAwareJsonSchemaExecutableToolDescriptor =
        inherit JsonSchemaExecutableToolDescriptor
        abstract needsApproval: option<U2<ResizeArray<option<obj>> -> option<obj>, bool>> with get, set

    [<Import("@cloudflare/codemode", "McpConnector")>]
    type McpConnector<'Env, 'Props> =
        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract createConnection: unit -> U2<Promise<McpConnectionLike>, McpConnectionLike>
        abstract toolName: tool: ModelcontextprotocolSdk.TypesJs.Tool -> string
        abstract getConnection: unit -> Promise<McpConnectionLike>
        abstract fetchTools: unit -> Promise<ResizeArray<ModelcontextprotocolSdk.TypesJs.Tool>>
        abstract describe: unit -> Promise<Erased.Empty>
        abstract tools: unit -> Promise<Erased.Empty>

    [<Import("@cloudflare/codemode", "CodemodeRejectOptions")>]
    type CodemodeRejectOptions =
        abstract executionId: string with get, set
        abstract seq: float with get, set

    [<Import("@cloudflare/codemode", "ToolDispatcher")>]
    type ToolDispatcher =
        [<EmitConstructor>]
        abstract Create: fns: obj -> ToolDispatcher

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract call: name: string * ?argsJson: string -> Promise<string>

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ToolLogEntryState =
        | [<CompiledName("executing")>] Executing
        | [<CompiledName("applied")>] Applied
        | [<CompiledName("pending")>] Pending
        | [<CompiledName("reverted")>] Reverted
        | [<CompiledName("error")>] Error

    [<Import("@cloudflare/codemode", "OpenApiMcpServerOptions")>]
    type OpenApiMcpServerOptions =
        abstract description: option<string> with get, set
        abstract version: option<string> with get, set
        abstract name: option<string> with get, set
        abstract executor: Erased.Empty with get, set
        abstract spec: obj with get, set
        abstract request: options: RequestOptions * context: OpenApiMcpRequestContext -> Promise<option<obj>>

    [<Import("@cloudflare/codemode", "ProxyToolInput")>]
    type ProxyToolInput =
        abstract code: string with get, set

    [<Import("@cloudflare/codemode", "BeginOptions")>]
    type BeginOptions =
        abstract connectors: option<ResizeArray<string>> with get, set
        abstract maxExecutions: option<float> with get, set

    [<Import("@cloudflare/codemode", "CodeMcpServerOptions")>]
    type CodeMcpServerOptions =
        abstract description: option<string> with get, set
        abstract executor: Erased.Empty with get, set
        abstract server: ModelcontextprotocolSdk.McpServer with get, set

    [<Import("@cloudflare/codemode", "CreateCodemodeRuntimeOptions")>]
    type CreateCodemodeRuntimeOptions =
        abstract maxExecutions: option<float> with get, set
        abstract name: option<string> with get, set
        abstract executor: Erased.Empty with get, set
        abstract connectors: ResizeArray<Erased.Empty> with get, set
        abstract ctx: Erased.Empty with get, set
        abstract transformResult: ?result: obj -> option<obj>

    [<Import("@cloudflare/codemode", "PendingAction")>]
    type PendingAction =
        abstract args: option<obj> with get, set
        abstract method: string with get, set
        abstract connector: string with get, set
        abstract seq: float with get, set
        abstract executionId: string with get, set

    [<Import("@cloudflare/codemode", "StandardSchemaIssue")>]
    type StandardSchemaIssue =
        abstract path: option<System.Collections.Generic.IReadOnlyList<obj>> with get
        abstract message: string with get

    [<Import("@cloudflare/codemode", "DynamicWorkerExecutorOptions")>]
    type DynamicWorkerExecutorOptions =
        abstract bindings: option<obj> with get, set
        abstract modules: option<obj> with get, set
        abstract globalOutbound: option<SharedLiterals.ConnectFetch> with get, set
        abstract timeout: option<float> with get, set
        abstract loader: CloudflareWorkersTypes.WorkerLoader with get, set

    [<Import("@cloudflare/codemode", "OpenApiMcpRequestContext")>]
    type OpenApiMcpRequestContext = interface end

    [<Import("@cloudflare/codemode", "IframeSandboxExecutor")>]
    type IframeSandboxExecutor =
        [<EmitConstructor>]
        abstract Create: ?options: IframeSandboxExecutorOptions -> IframeSandboxExecutor

        [<EmitProperty("#private")>]
        abstract ``#private``: option<obj> with get, set

        abstract execute: code: string * providersOrFns: U2<ResizeArray<Erased.Empty>, obj> -> Promise<Erased.Empty>

    [<Import("@cloudflare/codemode", "SearchOutput")>]
    type SearchOutput =
        abstract truncated: bool with get, set
        abstract total: float with get, set
        abstract results: ResizeArray<SearchResult> with get, set

    [<Import("@cloudflare/codemode", "ToolLogEntry")>]
    type ToolLogEntry =
        abstract state: ToolLogEntryState with get, set
        abstract ephemeral: option<bool> with get, set
        abstract requiresApproval: bool with get, set
        abstract result: option<obj> with get, set
        abstract args: option<obj> with get, set
        abstract method: string with get, set
        abstract connector: string with get, set
        abstract seq: float with get, set

    type CreateCodemodeRuntime =
        abstract maxExecutions: option<float> with get, set
        abstract name: option<string> with get, set
        abstract executor: Erased.Empty with get, set
        abstract connectors: ResizeArray<Erased.Empty> with get, set
        abstract ctx: Erased.Empty with get, set
        abstract transformResult: ?result: obj -> option<obj>

    [<Import("@cloudflare/codemode", "CreateBrowserCodeToolOptions")>]
    type CreateBrowserCodeToolOptions =
        abstract description: option<string> with get, set
        abstract executor: option<Erased.Empty> with get, set
        abstract tools: U2<ResizeArray<ApprovalAwareJsonSchemaExecutableToolDescriptor>, ApprovalAwareJsonSchemaExecutableToolDescriptors> with get, set

    [<Import("@cloudflare/codemode", "CodemodeTool")>]
    type CodemodeTool =
        abstract inputSchema: ProxyToolInputSchema with get, set
        abstract description: string with get, set
        abstract execute: input: ProxyToolInput * ?options: obj -> Promise<ProxyToolOutput>

    [<Import("@cloudflare/codemode", "IframeSandboxExecutorOptions")>]
    type IframeSandboxExecutorOptions =
        abstract csp: option<string> with get, set
        abstract timeout: option<float> with get, set

    [<Import("@cloudflare/codemode", "CodemodeApproveOptions")>]
    type CodemodeApproveOptions =
        abstract executionId: string with get, set

    [<Import("@cloudflare/codemode", "ExecutionState")>]
    type ExecutionState =
        abstract updatedAt: float with get, set
        abstract createdAt: float with get, set
        abstract connectors: option<ResizeArray<string>> with get, set
        abstract logs: option<ResizeArray<string>> with get, set
        abstract error: option<string> with get, set
        abstract result: option<obj> with get, set
        abstract log: ResizeArray<ToolLogEntry> with get, set
        abstract status: ExecutionStatus with get, set
        abstract code: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/codemode", "OpenApiRequestOptions")>]
    type OpenApiRequestOptions =
        abstract headers: option<obj> with get, set
        abstract body: option<obj> with get, set
        abstract params: option<obj> with get, set
        abstract method: option<string> with get, set
        abstract path: string with get, set

    [<Import("@cloudflare/codemode", "JsonSchemaExecutableToolDescriptors")>]
    type JsonSchemaExecutableToolDescriptors = interface end

    [<Import("@cloudflare/codemode", "BrowserCodeToolDescriptor")>]
    type BrowserCodeToolDescriptor =
        abstract outputSchema: SharedLiterals.PropertiesRequiredType5 with get, set
        abstract inputSchema: SharedLiterals.PropertiesRequiredType4 with get, set
        abstract description: string with get, set
        abstract name: string with get, set
        abstract execute: args: Erased.Empty -> Promise<Erased.Empty>

    type ToolDecision = U3<SharedLiterals.KindResult, SharedLiterals.KindSeq, SharedLiterals.KindSeq2>

    [<Import("@cloudflare/codemode", "RequestOptions")>]
    type RequestOptions =
        abstract rawBody: option<bool> with get, set
        abstract contentType: option<string> with get, set
        abstract body: option<obj> with get, set
        abstract query: option<obj> with get, set
        abstract path: string with get, set
        abstract method: LiteralUnions.DELETE_GET_PATCH_POST_PUT with get, set

    [<Import("@cloudflare/codemode", "ApprovalAwareJsonSchemaExecutableToolDescriptors")>]
    type ApprovalAwareJsonSchemaExecutableToolDescriptors = interface end

    type RunCode =
        abstract connectors: option<ResizeArray<Erased.Empty>> with get, set
        abstract providers: ResizeArray<Erased.Empty> with get, set
        abstract executor: Erased.Empty with get, set
        abstract code: string with get, set

    [<Import("@cloudflare/codemode", "CodemodeRuntimeHandle")>]
    type CodemodeRuntimeHandle =
        abstract tool: ?options: CodemodeRuntimeToolOptions -> CodemodeTool
        abstract approve: options: CodemodeApproveOptions -> Promise<ProxyToolOutput>
        abstract reject: options: CodemodeRejectOptions -> Promise<bool>
        abstract rollback: options: CodemodeRollbackOptions -> Promise<unit>
        abstract pending: ?executionId: string -> Promise<ResizeArray<PendingAction>>
        abstract expirePaused: ?options: CodemodeExpireOptions -> Promise<ResizeArray<string>>
        abstract executions: ?limit: float -> Promise<ResizeArray<ExecutionState>>
        abstract deleteExecution: id: string -> Promise<bool>
        abstract pruneExecutions: ?keep: float -> Promise<float>
        abstract saveSnippet: name: string * options: SaveSnippetOptions -> Promise<Snippet>
        abstract snippets: unit -> Promise<ResizeArray<Snippet>>
        abstract deleteSnippet: name: string -> Promise<bool>

    type TransformResult = option<obj> -> option<U2<Promise<option<obj>>, obj>>
    type ProxyToolOutput = U3<SharedLiterals.ExecutionIdLogsResultStatus, SharedLiterals.ExecutionIdPendingStatus, SharedLiterals.ErrorExecutionIdLogsStatus>

    [<Import("@cloudflare/codemode", "OpenApiConnector")>]
    type OpenApiConnector<'Env, 'Props> =
        abstract spec: unit -> U2<obj, Promise<obj>>
        abstract request: options: OpenApiRequestOptions -> Promise<option<obj>>
        abstract exposeSpec: unit -> bool
        abstract tools: unit -> Promise<Erased.Empty>

    [<Import("@cloudflare/codemode", "SearchResult")>]
    type SearchResult =
        abstract score: float with get, set
        abstract kind: LiteralUnions.MethodSnippet with get, set
        abstract description: option<string> with get, set
        abstract method: string with get, set
        abstract connector: string with get, set
        abstract path: string with get, set

    [<Import("@cloudflare/codemode", "TruncateOptions")>]
    type TruncateOptions =
        abstract maxTokens: option<float> with get, set
        abstract maxChars: option<float> with get, set

    module OpenApiConnector =
        module Request =
            type Options =
                abstract headers: option<obj> with get, set
                abstract body: option<obj> with get, set
                abstract params: option<obj> with get, set
                abstract method: option<string> with get, set
                abstract path: string with get, set

    module SearchOutput =
        type Results =
            abstract score: float with get, set
            abstract kind: LiteralUnions.MethodSnippet with get, set
            abstract description: option<string> with get, set
            abstract method: string with get, set
            abstract connector: string with get, set
            abstract path: string with get, set

    module SharedLiterals =
        type KindSeq2 =
            abstract seq: float with get, set
            abstract kind: string with get, set

        type Code2 =
            abstract code: string with get, set

        type MessagePath =
            abstract path: option<System.Collections.Generic.IReadOnlyList<obj>> with get
            abstract message: string with get

        type DescriptionItemsType =
            abstract description: string with get, set
            abstract items: Type2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type KindSeq =
            abstract seq: float with get, set
            abstract kind: string with get, set

        type JsonSchemaValidateVendorVersion =
            abstract jsonSchema: InputOutput with get
            abstract vendor: string with get
            abstract version: int with get
            abstract validate: ?value: obj -> U2<Value, Issues>

        type ExecutionIdPendingStatus =
            abstract pending: ResizeArray<PendingAction> with get, set
            abstract executionId: string with get, set
            abstract status: string with get, set

        type Value =
            abstract value: ProxyToolInput with get

        type LogsResult2 =
            abstract logs: option<ResizeArray<string>> with get, set
            abstract result: option<obj> with get, set

        type LogsResult =
            abstract logs: DescriptionItemsType with get, set
            abstract result: CloudflareWorkersTypes.AiImageToTextOutput with get, set

        type CallTool2 =
            abstract callTool: params: ModelcontextprotocolSdk.SharedLiterals._metaArgumentsNameTask * ?resultSchema: U2<Zod.ZodType, Zod.ZodType> * ?options: ModelcontextprotocolSdk.RequestOptions -> Promise<U2<SharedLiterals._metaContentIsErrorStructuredContent, SharedLiterals._metaToolResult>>

        type MaxCharsMaxTokens =
            abstract maxTokens: option<float> with get, set
            abstract maxChars: option<float> with get, set

        type DescriptionExecuteInputSchema =
            abstract inputSchema: ProxyToolInputSchema with get, set
            abstract description: string with get, set
            abstract execute: input: ProxyToolInput * ?options: obj -> Promise<ProxyToolOutput>

        type ExecutionIdSeq =
            abstract executionId: string with get, set
            abstract seq: float with get, set

        type ExecutionId =
            abstract executionId: string with get, set

        type ExecutionId2 =
            abstract executionId: string with get, set

        type _metaAuthInfo0d6b92d7 =
            abstract requestInfo: option<ModelcontextprotocolSdk.RequestInfo> with get, set
            abstract taskRequestedTtl: option<float> with get, set
            abstract taskStore: option<ModelcontextprotocolSdk.RequestTaskStore> with get, set
            abstract taskId: option<string> with get, set
            abstract requestId: ModelcontextprotocolSdk.ProgressToken with get, set
            abstract _meta: option<ModelcontextprotocolSdk.TypesJs.RequestMeta> with get, set
            abstract sessionId: option<string> with get, set
            abstract authInfo: option<ModelcontextprotocolSdk.AuthInfo> with get, set
            abstract signal: CloudflareWorkersTypes.AbortSignal with get, set
            abstract sendNotification: notification: obj -> Promise<unit>
            abstract sendRequest: request: obj * resultSchema: obj * ?options: ModelcontextprotocolSdk.TaskRequestOptions -> Promise<option<U3<proptypekey<obj, string>, proptypekey<proptypekey<obj, string>, string>, obj>>>
            abstract closeSSEStream: unit -> unit
            abstract closeStandaloneSSEStream: unit -> unit

        type ArgsConnectorEphemeral708e7fb4 =
            abstract state: ToolLogEntryState with get, set
            abstract ephemeral: option<bool> with get, set
            abstract requiresApproval: bool with get, set
            abstract result: option<obj> with get, set
            abstract args: option<obj> with get, set
            abstract method: string with get, set
            abstract connector: string with get, set
            abstract seq: float with get, set

        type ExecutionIdLogsResultStatus =
            abstract logs: option<ResizeArray<string>> with get, set
            abstract result: option<obj> with get, set
            abstract executionId: string with get, set
            abstract status: string with get, set

        type Type2 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type PropertiesRequiredType5 =
            abstract required: string with get, set
            abstract properties: LogsResult with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type ConnectorHintsDescription =
            abstract connectorHints: option<obj> with get, set
            abstract description: option<string> with get, set

        type Issues =
            abstract issues: System.Collections.Generic.IReadOnlyList<StandardSchemaIssue> with get

        type ArgsConnectorExecutionIdMethodSeq =
            abstract args: option<obj> with get, set
            abstract method: string with get, set
            abstract connector: string with get, set
            abstract seq: float with get, set
            abstract executionId: string with get, set

        type ErrorExecutionIdLogsStatus =
            abstract logs: option<ResizeArray<string>> with get, set
            abstract error: string with get, set
            abstract executionId: string with get, set
            abstract status: string with get, set

        type Code3 =
            abstract code: string with get, set

        type Target =
            abstract target: string with get

        type KindResult =
            abstract result: option<obj> with get, set
            abstract kind: string with get, set

        type MaxAgeMs =
            abstract maxAgeMs: option<float> with get, set

        type InputOutput =
            abstract input: options: Target -> obj
            abstract output: options: Target -> obj

        type PropertiesRequiredType4 =
            abstract required: string with get, set
            abstract properties: Code with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type CodeConnectorsCreatedAt60d72239 =
            abstract updatedAt: float with get, set
            abstract createdAt: float with get, set
            abstract connectors: option<ResizeArray<string>> with get, set
            abstract logs: option<ResizeArray<string>> with get, set
            abstract error: option<string> with get, set
            abstract result: option<obj> with get, set
            abstract log: ResizeArray<ToolLogEntry> with get, set
            abstract status: ExecutionStatus with get, set
            abstract code: string with get, set
            abstract id: string with get, set

        type ``~standard`` =
            [<EmitProperty("~standard")>]
            abstract ``~standard``: JsonSchemaValidateVendorVersion with get

        type Code =
            abstract code: DescriptionType with get, set

        type DescriptionType =
            abstract description: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        module CallTool2 =
            module CallTool =
                module CallTool =
                    type Options =
                        abstract resumptionToken: option<string> with get, set
                        abstract relatedRequestId: option<ModelcontextprotocolSdk.ProgressToken> with get, set
                        abstract relatedTask: option<ModelcontextprotocolSdk.TypesJs.RelatedTaskMetadata> with get, set
                        abstract task: option<ModelcontextprotocolSdk.TypesJs.TaskCreationParams> with get, set
                        abstract maxTotalTimeout: option<float> with get, set
                        abstract resetTimeoutOnProgress: option<bool> with get, set
                        abstract timeout: option<float> with get, set
                        abstract signal: option<CloudflareWorkersTypes.AbortSignal> with get, set
                        abstract onprogress: progress: ModelcontextprotocolSdk.TypesJs.Progress -> unit
                        abstract onresumptiontoken: token: string -> unit
