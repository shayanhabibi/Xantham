namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

type ICloudflareWorkersTypes =
    [<Erase>]
    member _.tracing: CloudflareWorkersTypes.Tracing = JS.undefined

    [<Import("@cloudflare/workers-types", "clearInterval")>]
    static member clearInterval(timeoutId: option<float>) : unit = JS.undefined

    [<Erase>]
    member _.__DURABLE_OBJECT_BRAND: string = JS.undefined

    [<Erase>]
    member _.__WORKFLOW_ENTRYPOINT_BRAND: string = JS.undefined

    [<Import("@cloudflare/workers-types", "fetch")>]
    static member fetch(input: U3<obj, obj, string>, ?init: obj) : Promise<CloudflareWorkersTypes.Response> = JS.undefined

    [<Import("@cloudflare/workers-types", "setTimeout")>]
    static member setTimeout<'Args>(callback: obj -> unit, msDelay: float, [<ParamArray>] args: obj) : float = JS.undefined

    [<Import("@cloudflare/workers-types", "setTimeout")>]
    static member setTimeout(callback: ResizeArray<option<obj>> -> unit, ?msDelay: float) : float = JS.undefined

    [<Import("@cloudflare/workers-types", "removeEventListener")>]
    static member removeEventListener<'Type>(``type``: obj, handler: U2<obj, obj>, ?options: U2<obj, bool>) : unit = JS.undefined

    [<CompiledName("EmailMessage")>]
    member _.emailMessage: CloudflareWorkersTypes.SharedLiterals.FromToRawPrototype =
        JS.undefined

    [<CompiledName("_EmailMessage")>]
    member _._emailMessage: CloudflareWorkersTypes.SharedLiterals.FromToRawPrototype =
        JS.undefined

    [<Erase>]
    member _.__RPC_TARGET_BRAND: string = JS.undefined

    [<Erase>]
    member _.__RPC_STUB_BRAND: string = JS.undefined

    [<Import("@cloudflare/workers-types", "reportError")>]
    static member reportError(error: option<obj>) : unit = JS.undefined

    [<Import("@cloudflare/workers-types", "queueMicrotask")>]
    static member queueMicrotask(task: obj) : unit = JS.undefined

    [<Erase>]
    member _.onRequest: CloudflareWorkersTypes.SharedLiterals.DataEnvFunctionPath23a88d11<obj, obj> -> U2<CloudflareWorkersTypes.Response, Promise<CloudflareWorkersTypes.Response>> =
        JS.undefined

    [<Import("@cloudflare/workers-types", "btoa")>]
    static member btoa(data: string) : string = JS.undefined

    [<Erase>]
    member _.__WORKER_ENTRYPOINT_BRAND: string = JS.undefined

    [<Import("@cloudflare/workers-types", "connect")>]
    static member _connect(address: U2<obj, string>, ?options: obj) : CloudflareWorkersTypes.Socket = JS.undefined

    [<CompiledName("Cloudflare")>]
    member _.cloudflare: option<obj> = JS.undefined

    [<CompiledName("RpcStub")>]
    member _.rpcStub: CloudflareWorkersTypes.RpcStub = JS.undefined

    [<Import("@cloudflare/workers-types", "addEventListener")>]
    static member addEventListener<'Type>(``type``: obj, handler: U2<obj, obj>, ?options: U2<obj, bool>) : unit = JS.undefined

    [<Import("@cloudflare/workers-types", "clearTimeout")>]
    static member clearTimeout(timeoutId: option<float>) : unit = JS.undefined

    [<Import("@cloudflare/workers-types", "setInterval")>]
    static member setInterval<'Args>(callback: obj -> unit, msDelay: float, [<ParamArray>] args: obj) : float = JS.undefined

    [<Import("@cloudflare/workers-types", "setInterval")>]
    static member setInterval(callback: ResizeArray<option<obj>> -> unit, ?msDelay: float) : float = JS.undefined

    [<CompiledName("Request")>]
    member _.request: CloudflareWorkersTypes.SharedLiterals.InputInitPrototype<obj, obj> =
        JS.undefined

    [<Erase>]
    member _.cache: CloudflareWorkersTypes.CacheContext = JS.undefined

    [<CompiledName("WebSocket")>]
    member _.webSocket: CloudflareWorkersTypes.SharedLiterals.CLOSEDCLOSINGCONNECTING7a08c077 =
        JS.undefined

    [<Import("@cloudflare/workers-types", "atob")>]
    static member atob(data: string) : string = JS.undefined

    [<Import("@cloudflare/workers-types", "dispatchEvent")>]
    static member dispatchEvent(event: U4<obj, obj, obj, obj>) : bool = JS.undefined

    [<Import("@cloudflare/workers-types", "structuredClone")>]
    static member structuredClone<'T>(value: obj, ?options: obj) : obj = JS.undefined

    [<Import("@cloudflare/workers-types", "connect")>]
    static member connect(address: U2<obj, string>, ?options: obj) : CloudflareWorkersTypes.Socket = JS.undefined

    [<CompiledName("Response")>]
    member _.response: CloudflareWorkersTypes.SharedLiterals.BodyInitErrorJsonPrototypeRedirect =
        JS.undefined

module CloudflareWorkersTypes =
    [<Import("@cloudflare/workers-types", "ResponseOutputRefusal")>]
    type ResponseOutputRefusal =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract refusal: string with get, set

    [<Import("@cloudflare/workers-types", "UniversalGatewayOptions")>]
    type UniversalGatewayOptions =
        abstract retries: option<GatewayRetries> with get, set
        abstract requestTimeoutMs: option<float> with get, set
        abstract eventId: option<string> with get, set
        abstract collectLog: option<bool> with get, set
        abstract metadata: option<obj> with get, set
        abstract skipCache: option<bool> with get, set
        abstract cacheTtl: option<float> with get, set
        abstract cacheKey: option<string> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseInputTextContent")>]
    type ResponseInputTextContent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "KVNamespace")>]
    type KVNamespace<'Key> =
        abstract get: key: 'Key * ?options: KVNamespace.Get.Options -> Promise<option<string>>
        abstract get: key: 'Key * ``type``: string -> U4<Promise<option<string>>, Promise<option<obj>>, Promise<option<ArrayBuffer>>, Promise<option<ReadableStream<option<obj>>>>>
        abstract get: key: 'Key * ?options: KVNamespaceGetOptions<string> -> U4<Promise<option<string>>, Promise<option<obj>>, Promise<option<ArrayBuffer>>, Promise<option<ReadableStream<option<obj>>>>>
        abstract get: key: ResizeArray<'Key> * ``type``: string -> U2<Promise<Map<string, option<string>>>, Promise<Map<string, option<obj>>>>
        abstract get: key: ResizeArray<'Key> * ?options: KVNamespace.Get.Options -> Promise<Map<string, option<string>>>
        abstract get: key: ResizeArray<'Key> * ?options: KVNamespaceGetOptions<string> -> U2<Promise<Map<string, option<string>>>, Promise<Map<string, option<obj>>>>
        abstract list: ?options: KVNamespaceListOptions -> Promise<U2<KVNamespace.List, KVNamespace.List.Case2>>
        abstract put: key: 'Key * value: U4<ArrayBuffer, obj, ReadableStream<option<obj>>, string> * ?options: KVNamespacePutOptions -> Promise<unit>
        abstract getWithMetadata: key: 'Key * ?options: obj -> Promise<KVNamespaceGetWithMetadataResult<string, obj>>
        abstract getWithMetadata: key: 'Key * ``type``: string -> U4<Promise<KVNamespaceGetWithMetadataResult<string, obj>>, Promise<KVNamespaceGetWithMetadataResult<obj, obj>>, Promise<KVNamespaceGetWithMetadataResult<ArrayBuffer, obj>>, Promise<KVNamespaceGetWithMetadataResult<ReadableStream<option<obj>>, obj>>>
        abstract getWithMetadata: key: 'Key * options: KVNamespaceGetOptions<string> -> U4<Promise<KVNamespaceGetWithMetadataResult<string, obj>>, Promise<KVNamespaceGetWithMetadataResult<obj, obj>>, Promise<KVNamespaceGetWithMetadataResult<ArrayBuffer, obj>>, Promise<KVNamespaceGetWithMetadataResult<ReadableStream<option<obj>>, obj>>>
        abstract getWithMetadata: key: ResizeArray<'Key> * ``type``: string -> U2<Promise<Map<string, KVNamespaceGetWithMetadataResult<string, obj>>>, Promise<Map<string, KVNamespaceGetWithMetadataResult<obj, obj>>>>
        abstract getWithMetadata: key: ResizeArray<'Key> * ?options: obj -> Promise<Map<string, KVNamespaceGetWithMetadataResult<string, obj>>>
        abstract getWithMetadata: key: ResizeArray<'Key> * ?options: KVNamespaceGetOptions<string> -> U2<Promise<Map<string, KVNamespaceGetWithMetadataResult<string, obj>>>, Promise<Map<string, KVNamespaceGetWithMetadataResult<obj, obj>>>>
        abstract delete: key: 'Key -> Promise<unit>

    [<Import("@cloudflare/workers-types", "AiSentenceSimilarityInput")>]
    type AiSentenceSimilarityInput =
        abstract sentences: ResizeArray<string> with get, set
        abstract source: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchMultiChatCompletionsRequest")>]
    type AiSearchMultiChatCompletionsRequest =
        [<EmitProperty("ai_search_options")>]
        abstract aiSearchOptions: AiSearchMultiSearchOptions with get, set

        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Google_Gemma_3_12B_It_Messages")>]
    type AiCfGoogleGemma312BItMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<AiCfGoogleGemma312BItMessages.Messages> with get, set

    [<Import("@cloudflare/workers-types", "ImageUploadOptions")>]
    type ImageUploadOptions =
        abstract encoding: option<string> with get, set
        abstract creator: option<string> with get, set
        abstract metadata: option<obj> with get, set
        abstract requireSignedURLs: option<bool> with get, set
        abstract filename: option<string> with get, set
        abstract id: option<string> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type WorkflowStepSensitivity = | [<CompiledName("output")>] WorkflowStepSensitivity

    [<Import("@cloudflare/workers-types", "BrowserRunSnapshotOptions")>]
    type BrowserRunSnapshotOptions =
        abstract screenshotOptions: option<SharedLiterals.CaptureBeyondViewportClipF9e2df771> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Iso3166Alpha2Code =
        | AF
        | AS
        | NA
        | SA
        | AD
        | AE
        | AG
        | AI
        | AL
        | AM
        | AO
        | AQ
        | AR
        | AT
        | AU
        | AW
        | AX
        | AZ
        | BA
        | BB
        | BD
        | BE
        | BF
        | BG
        | BH
        | BI
        | BJ
        | BL
        | BM
        | BN
        | BO
        | BQ
        | BR
        | BS
        | BT
        | BV
        | BW
        | BY
        | BZ
        | CA
        | CC
        | CD
        | CF
        | CG
        | CH
        | CI
        | CK
        | CL
        | CM
        | CN
        | CO
        | CR
        | CU
        | CV
        | CW
        | CX
        | CY
        | CZ
        | DE
        | DJ
        | DK
        | DM
        | DO
        | DZ
        | EC
        | EE
        | EG
        | EH
        | ER
        | ES
        | ET
        | FI
        | FJ
        | FK
        | FM
        | FO
        | FR
        | GA
        | GB
        | GD
        | GE
        | GF
        | GG
        | GH
        | GI
        | GL
        | GM
        | GN
        | GP
        | GQ
        | GR
        | GS
        | GT
        | GU
        | GW
        | GY
        | HK
        | HM
        | HN
        | HR
        | HT
        | HU
        | ID
        | IE
        | IL
        | IM
        | IN
        | IO
        | IQ
        | IR
        | IS
        | IT
        | JE
        | JM
        | JO
        | JP
        | KE
        | KG
        | KH
        | KI
        | KM
        | KN
        | KP
        | KR
        | KW
        | KY
        | KZ
        | LA
        | LB
        | LC
        | LI
        | LK
        | LR
        | LS
        | LT
        | LU
        | LV
        | LY
        | MA
        | MC
        | MD
        | ME
        | MF
        | MG
        | MH
        | MK
        | ML
        | MM
        | MN
        | MO
        | MP
        | MQ
        | MR
        | MS
        | MT
        | MU
        | MV
        | MW
        | MX
        | MY
        | MZ
        | NC
        | NE
        | NF
        | NG
        | NI
        | NL
        | NO
        | NP
        | NR
        | NU
        | NZ
        | OM
        | PA
        | PE
        | PF
        | PG
        | PH
        | PK
        | PL
        | PM
        | PN
        | PR
        | PS
        | PT
        | PW
        | PY
        | QA
        | RE
        | RO
        | RS
        | RU
        | RW
        | SB
        | SC
        | SD
        | SE
        | SG
        | SH
        | SI
        | SJ
        | SK
        | SL
        | SM
        | SN
        | SO
        | SR
        | SS
        | ST
        | SV
        | SX
        | SY
        | SZ
        | TC
        | TD
        | TF
        | TG
        | TH
        | TJ
        | TK
        | TL
        | TM
        | TN
        | TO
        | TR
        | TT
        | TV
        | TW
        | TZ
        | UA
        | UG
        | UM
        | US
        | UY
        | UZ
        | VA
        | VC
        | VE
        | VG
        | VI
        | VN
        | VU
        | WF
        | WS
        | YE
        | YT
        | ZA
        | ZM
        | ZW

    [<Import("@cloudflare/workers-types", "TraceItemQueueEventInfo")>]
    type TraceItemQueueEventInfo =
        abstract batchSize: float with get
        abstract queue: string with get

    [<Import("@cloudflare/workers-types", "ChatCompletionAudio")>]
    type ChatCompletionAudio =
        abstract transcript: string with get, set

        [<EmitProperty("expires_at")>]
        abstract expiresAt: float with get, set

        abstract data: string with get, set
        abstract id: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AgentMemoryMemoryType =
        | [<CompiledName("event")>] Event
        | [<CompiledName("task")>] Task
        | [<CompiledName("fact")>] Fact
        | [<CompiledName("instruction")>] Instruction

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesVaryHeader")>]
    type RequestInitCfPropertiesVaryHeader =
        abstract action: RequestInitCfPropertiesVaryAction with get, set

    [<Import("@cloudflare/workers-types", "CryptoKeyEllipticKeyAlgorithm")>]
    type CryptoKeyEllipticKeyAlgorithm =
        abstract namedCurve: string with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Queue")>]
    type Queue<'Body> =
        abstract metrics: unit -> Promise<QueueMetrics>
        abstract send: message: 'Body * ?options: QueueSendOptions -> Promise<QueueSendResponse>
        abstract sendBatch: messages: seq<MessageSendRequest<'Body>> * ?options: QueueSendBatchOptions -> Promise<QueueSendBatchResponse>

    [<Import("@cloudflare/workers-types", "ContainerSnapshotOptions")>]
    type ContainerSnapshotOptions =
        abstract name: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Baai_Bge_M3")>]
    type BaseAiCfBaaiBgeM3 =
        abstract postProcessedOutputs: AiCfBaaiBgeM3Output with get, set
        abstract inputs: AiCfBaaiBgeM3Input with get, set

    [<Import("@cloudflare/workers-types", "QueuingStrategyInit")>]
    type QueuingStrategyInit =
        abstract highWaterMark: float with get, set

    [<Import("@cloudflare/workers-types", "R2MultipartOptions")>]
    type R2MultipartOptions =
        abstract ssecKey: option<U2<ArrayBuffer, string>> with get, set
        abstract storageClass: option<string> with get, set
        abstract customMetadata: option<obj> with get, set
        abstract httpMetadata: option<U2<R2HTTPMetadata, Headers>> with get, set

    [<Import("@cloudflare/workers-types", "SecretsStoreSecret")>]
    type SecretsStoreSecret =
        abstract get: unit -> Promise<string>

    [<Import("@cloudflare/workers-types", "CryptoKeyPair")>]
    type CryptoKeyPair =
        abstract privateKey: CryptoKey with get, set
        abstract publicKey: CryptoKey with get, set

    [<Import("@cloudflare/workers-types", "AiSearchMultiSearchOptions")>]
    type AiSearchMultiSearchOptions =
        [<EmitProperty("instance_ids")>]
        abstract instanceIds: ResizeArray<string> with get, set

        abstract cache: option<SharedLiterals.CacheThresholdEnabled> with get, set
        abstract reranking: option<SharedLiterals.EnabledMatchThresholdModel> with get, set

        [<EmitProperty("query_rewrite")>]
        abstract queryRewrite: option<SharedLiterals.EnabledModelRewritePrompt> with get, set

        abstract retrieval: option<SharedLiterals.BoostByContextAef2ce5f> with get, set
        abstract Item: key: string -> option<obj>

    type EventListenerOrEventListenerObject<'EventType> = U2<EventListener, EventListenerObject<'EventType>>

    [<Import("@cloudflare/workers-types", "ChatCompletionsMessagesInput")>]
    type ChatCompletionsInput =
        abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

        [<EmitProperty("function_call")>]
        abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

        [<EmitProperty("web_search_options")>]
        abstract webSearchOptions: option<WebSearchOptions> with get, set

        abstract user: option<string> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

        [<EmitProperty("tool_choice")>]
        abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("stream_options")>]
        abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

        abstract stream: option<bool> with get, set
        abstract store: option<bool> with get, set
        abstract stop: option<U2<ResizeArray<string>, string>> with get, set

        [<EmitProperty("service_tier")>]
        abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<ResponseFormat> with get, set

        [<EmitProperty("chat_template_kwargs")>]
        abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

        [<EmitProperty("reasoning_effort")>]
        abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        abstract prediction: option<PredictionContent> with get, set

        [<EmitProperty("parallel_tool_calls")>]
        abstract parallelToolCalls: option<bool> with get, set

        abstract n: option<float> with get, set
        abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
        abstract metadata: option<obj> with get, set

        [<EmitProperty("max_completion_tokens")>]
        abstract maxCompletionTokens: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        [<EmitProperty("top_logprobs")>]
        abstract topLogprobs: option<float> with get, set

        abstract logprobs: option<bool> with get, set

        [<EmitProperty("logit_bias")>]
        abstract logitBias: option<obj> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        abstract audio: option<AudioParams> with get, set
        abstract model: option<string> with get, set
        abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

    type ResponseInputItem = U6<ResponseOutputMessage, ResponseFunctionToolCall, ResponseReasoningItem, EasyInputMessage, ResponseInputItemMessage, ResponseInputItemFunctionCallOutput>

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type StreamDownloadStatus =
        | [<CompiledName("error")>] Error
        | [<CompiledName("ready")>] Ready
        | [<CompiledName("inprogress")>] Inprogress

    type AiCfOpenaiWhisperTinyEnInput = U2<AiAutomaticSpeechRecognitionInput, string>

    [<Import("@cloudflare/workers-types", "__type")>]
    type MainModule = interface end

    [<Import("@cloudflare/workers-types", "Ai_Cf_Google_Gemma_3_12B_It_Output")>]
    type AiCfGoogleGemma312BItOutput =
        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract response: string with get, set

    [<Import("@cloudflare/workers-types", "AiTextGenerationToolLegacyInput")>]
    type AiTextGenerationToolLegacyInput =
        abstract parameters: option<SharedLiterals.PropertiesRequiredType7> with get, set
        abstract description: string with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemContentResult")>]
    type AiSearchItemContentResult =
        abstract size: float with get, set
        abstract filename: string with get, set
        abstract contentType: string with get, set
        abstract body: ReadableStream<option<obj>> with get, set

    [<Import("@cloudflare/workers-types", "LoopbackServiceStub")>]
    type LoopbackServiceStub<'T> =
        abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
        abstract Invoke: opts: SharedLiterals.Props<obj> -> obj
        abstract Invoke: opts: SharedLiterals.Props2 -> obj
        abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract Item: key: string -> option<obj>
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
        abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    [<Import("@cloudflare/workers-types", "StreamUrlUploadParams")>]
    type StreamUrlUploadParams =
        abstract watermarkId: option<string> with get, set
        abstract thumbnailTimestampPct: option<float> with get, set
        abstract scheduledDeletion: option<string> with get, set
        abstract requireSignedURLs: option<bool> with get, set
        abstract meta: option<obj> with get, set
        abstract creator: option<string> with get, set
        abstract allowedOrigins: option<ResizeArray<string>> with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRagAiSearchRequest")>]
    type AutoRagAiSearchRequest =
        [<EmitProperty("system_prompt")>]
        abstract systemPrompt: option<string> with get, set

        abstract stream: option<bool> with get, set

        [<EmitProperty("rewrite_query")>]
        abstract rewriteQuery: option<bool> with get, set

        abstract reranking: option<SharedLiterals.EnabledModel> with get, set

        [<EmitProperty("ranking_options")>]
        abstract rankingOptions: option<SharedLiterals.RankerScoreThreshold> with get, set

        [<EmitProperty("max_num_results")>]
        abstract maxNumResults: option<float> with get, set

        abstract filters: option<U2<CompoundFilter, ComparisonFilter>> with get, set
        abstract query: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Async_Batch")>]
    type AiCfAisingaporeGemmaSeaLionV427BItAsyncBatch =
        abstract requests: ResizeArray<U2<AiCfAisingaporeGemmaSeaLionV427BItPrompt1, AiCfAisingaporeGemmaSeaLionV427BItMessages1>> with get, set

    [<Import("@cloudflare/workers-types", "IdentityTransformStreamQueuingStrategy")>]
    type IdentityTransformStreamQueuingStrategy =
        abstract highWaterMark: option<float> with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesVary")>]
    type RequestInitCfPropertiesVary =
        abstract headers: option<RequestInitCfPropertiesVaryHeaders> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: RequestInitCfPropertiesVaryHeader with get, set

    [<Import("@cloudflare/workers-types", "TraceItemJsRpcEventInfo")>]
    type TraceItemJsRpcEventInfo =
        abstract rpcMethod: string with get

    [<Import("@cloudflare/workers-types", "SchedulerWaitOptions")>]
    type SchedulerWaitOptions =
        abstract signal: option<AbortSignal> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Openai_Whisper_Large_V3_Turbo_Output")>]
    type AiCfOpenaiWhisperLargeV3TurboOutput =
        abstract vtt: option<string> with get, set
        abstract segments: option<ResizeArray<AiCfOpenaiWhisperLargeV3TurboOutput.Segments>> with get, set

        [<EmitProperty("word_count")>]
        abstract wordCount: option<float> with get, set

        abstract text: string with get, set

        [<EmitProperty("transcription_info")>]
        abstract transcriptionInfo: option<AiCfOpenaiWhisperLargeV3TurboOutput.TranscriptionInfo> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunMarkdownSuccessResponse")>]
    type BrowserRunMarkdownSuccessResponse =
        abstract result: string with get, set
        abstract success: bool with get, set

    type ResponseItem = U4<ResponseOutputMessage, ResponseItem.Case1, ResponseFunctionToolCallItem, ResponseItem.Case3>

    [<Import("@cloudflare/workers-types", "AiAsyncBatchResponse")>]
    type AiAsyncBatchResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: string with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryListMemoriesResult")>]
    type AgentMemoryListMemoriesResult =
        abstract cursor: option<string> with get, set
        abstract memories: ResizeArray<AgentMemoryMemoryListEntry> with get, set

    [<Import("@cloudflare/workers-types", "BaseAiMultimodalEmbeddings")>]
    type BaseAiMultimodalEmbeddings =
        abstract postProcessedOutputs: AiImageTextToTextOutput with get, set
        abstract inputs: AiImageTextToTextInput with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContinentCode =
        | AF
        | AN
        | AS
        | EU
        | NA
        | OC
        | SA

    type AiCfBaaiBgeBaseEnV15Input = U2<SharedLiterals.PoolingText, SharedLiterals.Requests>

    [<Import("@cloudflare/workers-types", "ResponseFunctionCallArgumentsDoneEvent")>]
    type ResponseFunctionCallArgumentsDoneEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        abstract name: string with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        abstract arguments: string with get, set

    [<Import("@cloudflare/workers-types", "DeveloperMessage")>]
    type DeveloperMessage =
        abstract name: option<string> with get, set
        abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set
        abstract role: string with get, set

    [<Import("@cloudflare/workers-types", "Navigator")>]
    type Navigator =
        abstract languages: ResizeArray<string> with get
        abstract language: string with get
        abstract platform: string with get
        abstract hardwareConcurrency: float with get
        abstract userAgent: string with get
        abstract sendBeacon: url: string * ?body: BodyInit -> bool

    type ExportedHandlerQueueHandler = MessageBatch<obj> -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "ContainerDirectorySnapshotOptions")>]
    type ContainerDirectorySnapshotOptions =
        abstract name: option<string> with get, set
        abstract dir: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Meta_Llama_3_2_11B_Vision_Instruct")>]
    type BaseAiCfMetaLlama3211BVisionInstruct =
        abstract postProcessedOutputs: AiCfMetaLlama3211BVisionInstructOutput with get, set
        abstract inputs: AiCfMetaLlama3211BVisionInstructInput with get, set

    [<Import("@cloudflare/workers-types", "Workflow")>]
    type Workflow<'PARAMS> =
        abstract get: id: string -> Promise<WorkflowInstance>
        abstract create: ?options: WorkflowInstanceCreateOptions<'PARAMS> -> Promise<WorkflowInstance>
        abstract createBatch: batch: ResizeArray<WorkflowInstanceCreateOptions<'PARAMS>> -> Promise<ResizeArray<WorkflowInstance>>

    [<Import("@cloudflare/workers-types", "URLSearchParams")>]
    type URLSearchParams =
        [<EmitConstructor>]
        abstract Create: ?init: U3<seq<seq<string>>, obj, string> -> URLSearchParams

        abstract size: float with get
        abstract append: name: string * value: string -> unit
        abstract delete: name: string * ?value: string -> unit
        abstract get: name: string -> option<string>
        abstract getAll: name: string -> ResizeArray<string>
        abstract has: name: string * ?value: string -> bool
        abstract set: name: string * value: string -> unit
        abstract sort: unit -> unit
        abstract entries: unit -> seq<string * string>
        abstract keys: unit -> seq<string>
        abstract values: unit -> seq<string>
        abstract forEach: callback: (obj -> string -> string -> URLSearchParams -> unit) * ?thisArg: obj -> unit
        abstract toString: unit -> string
        abstract ``[symbol.iterator]``: unit -> seq<string * string>

    [<Import("@cloudflare/workers-types", "ColoLocalActorNamespace")>]
    type ColoLocalActorNamespace =
        abstract get: actorId: string -> SharedLiterals.ConnectFetch

    type AiCfQwenQwen330BA3BFp8Input = U3<AiCfQwenQwen330BA3BFp8Prompt, AiCfQwenQwen330BA3BFp8Messages, AiCfQwenQwen330BA3BFp8AsyncBatch>

    [<Import("@cloudflare/workers-types", "DurableObjectStub")>]
    type DurableObjectStub<'T> =
        abstract name: option<string> with get
        abstract id: DurableObjectId with get
        abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
        abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract Item: key: string -> option<obj>
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
        abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_Guard_3_8B_Input")>]
    type AiCfMetaLlamaGuard38BInput =
        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlamaGuard38BInput.ResponseFormat> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract messages: ResizeArray<AiCfMetaLlamaGuard38BInput.Messages> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchJobLog")>]
    type AiSearchJobLog =
        [<EmitProperty("created_at")>]
        abstract createdAt: float with get, set

        [<EmitProperty("message_type")>]
        abstract messageType: float with get, set

        abstract message: string with get, set
        abstract id: float with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_Base_En_V1_5_AsyncResponse")>]
    type AiCfBaaiBgeBaseEnV15AsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_2_Klein_4B_Output")>]
    type AiCfBlackForestLabsFlux2Klein4BOutput =
        abstract image: option<string> with get, set

    [<Import("@cloudflare/workers-types", "VectorizeIndex")>]
    type VectorizeIndex =
        abstract describe: unit -> Promise<VectorizeIndexDetails>
        abstract query: vector: U3<Float32Array, Float64Array, AiSentenceSimilarityOutput> * ?options: VectorizeQueryOptions -> Promise<VectorizeMatches>
        abstract insert: vectors: ResizeArray<VectorizeVector> -> Promise<VectorizeVectorMutation>
        abstract upsert: vectors: ResizeArray<VectorizeVector> -> Promise<VectorizeVectorMutation>
        abstract deleteByIds: ids: ResizeArray<string> -> Promise<VectorizeVectorMutation>
        abstract getByIds: ids: ResizeArray<string> -> Promise<ResizeArray<VectorizeVector>>

    [<Import("@cloudflare/workers-types", "DurableObjectPutOptions")>]
    type DurableObjectPutOptions =
        abstract noCache: option<bool> with get, set
        abstract allowUnconfirmed: option<bool> with get, set
        abstract allowConcurrency: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "KVNamespaceListKey")>]
    type KVNamespaceListKey<'Metadata, 'Key> =
        abstract metadata: option<'Metadata> with get, set
        abstract expiration: option<float> with get, set
        abstract name: 'Key with get, set

    type BrowserRunMarkdownOptions =
        abstract url: string with get, set
        abstract cacheTTL: option<float> with get, set
        abstract actionTimeout: option<float> with get, set
        abstract bestAttempt: option<bool> with get, set
        abstract waitForTimeout: option<float> with get, set
        abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
        abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
        abstract userAgent: option<string> with get, set
        abstract setJavaScriptEnabled: option<bool> with get, set
        abstract setExtraHTTPHeaders: option<obj> with get, set
        abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
        abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
        abstract allowRequestPattern: option<ResizeArray<string>> with get, set
        abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
        abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
        abstract emulateMediaType: option<string> with get, set
        abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
        abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
        abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
        abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

    [<Import("@cloudflare/workers-types", "TraceLog")>]
    type TraceLog =
        abstract message: option<obj> with get
        abstract level: string with get
        abstract timestamp: float with get

    [<Import("@cloudflare/workers-types", "AIGatewayUniversalRequest")>]
    type AIGatewayUniversalRequest =
        abstract query: option<obj> with get, set
        abstract headers: SharedLiterals.AuthorizationContentTypeC32cb5e3e with get, set
        abstract endpoint: string with get, set
        abstract provider: U2<LiteralUnions.AdobeFireflyAnthropicAws_7e6a892e, string> with get, set

    type BufferSource = U2<ArrayBuffer, obj>
    type ResponseFunctionCallOutputItem = U2<ResponseInputTextContent, ResponseInputImageContent>

    [<Import("@cloudflare/workers-types", "Logprob")>]
    type Logprob =
        [<EmitProperty("top_logprobs")>]
        abstract topLogprobs: option<ResizeArray<TopLogprob>> with get, set

        abstract logprob: float with get, set
        abstract token: string with get, set

    [<Import("@cloudflare/workers-types", "DynamicDispatchOptions")>]
    type DynamicDispatchOptions =
        abstract outbound: option<obj> with get, set
        abstract limits: option<DynamicDispatchLimits> with get, set

    [<Import("@cloudflare/workers-types", "CryptoKeyKeyAlgorithm")>]
    type CryptoKeyKeyAlgorithm =
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Openai_Gpt_Oss_120B")>]
    type BaseAiCfOpenaiGptOss120B =
        abstract postProcessedOutputs: U2<obj, obj> with get, set
        abstract inputs: U2<obj, obj> with get, set

    [<Import("@cloudflare/workers-types", "ResponseIncompleteEvent")>]
    type ResponseIncompleteEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        abstract response: Response with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionFunctionTool")>]
    type ChatCompletionFunctionTool =
        [<EmitProperty("function")>]
        abstract ``function``: FunctionDefinition with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseError")>]
    type ResponseError =
        abstract message: string with get, set
        abstract code: LiteralUnions.EmptyImageFileB06f83e3 with get, set

    [<Import("@cloudflare/workers-types", "FormData")>]
    type FormData =
        [<EmitConstructor>]
        abstract Create: unit -> FormData

        abstract append: name: string * value: U2<Blob, string> -> unit
        abstract append: name: string * value: string -> unit
        abstract append: name: string * value: Blob * ?filename: string -> unit
        abstract delete: name: string -> unit
        abstract get: name: string -> option<U2<File, string>>
        abstract getAll: name: string -> ResizeArray<U2<File, string>>
        abstract has: name: string -> bool
        abstract set: name: string * value: U2<Blob, string> -> unit
        abstract set: name: string * value: string -> unit
        abstract set: name: string * value: Blob * ?filename: string -> unit
        abstract entries: unit -> seq<string * U2<File, string>>
        abstract keys: unit -> seq<string>
        abstract values: unit -> seq<U2<File, string>>
        abstract forEach: callback: (obj -> U2<File, string> -> string -> FormData -> unit) * ?thisArg: obj -> unit
        abstract ``[symbol.iterator]``: unit -> seq<string * U2<File, string>>

    [<Import("@cloudflare/workers-types", "EventTarget")>]
    type EventTarget<'EventMap> =
        [<EmitConstructor>]
        abstract Create: unit -> EventTarget<'EventMap>

        abstract addEventListener: ``type``: obj * handler: U2<EventListener, EventListenerObject<proptypekey<'EventMap, obj>>> * ?options: U2<EventTargetAddEventListenerOptions, bool> -> unit
        abstract removeEventListener<'Type> : ``type``: 'Type * handler: U2<EventListener, EventListenerObject<proptypekey<'EventMap, 'Type>>> * ?options: U2<EventTargetEventListenerOptions, bool> -> unit
        abstract dispatchEvent: event: proptypekey<'EventMap, keyof<'EventMap>> -> bool

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesImage")>]
    type RequestInitCfPropertiesImage =
        inherit BasicImageTransformations
        abstract compression: option<string> with get, set

        [<EmitProperty("origin-auth")>]
        abstract originAuth: option<string> with get, set

        abstract draw: option<ResizeArray<RequestInitCfPropertiesImageDraw>> with get, set
        abstract metadata: option<LiteralUnions.CopyrightKeepNone> with get, set
        abstract anim: option<bool> with get, set
        abstract format: option<LiteralUnions.AvifBaselineJpegJpeg47a90ada> with get, set
        abstract quality: option<U2<LiteralUnions.HighLowMediumHighMediumLow, float>> with get, set

    [<Import("@cloudflare/workers-types", "SocketInfo")>]
    type SocketInfo =
        abstract localAddress: option<string> with get, set
        abstract remoteAddress: option<string> with get, set

    [<Import("@cloudflare/workers-types", "TextDecoderStreamTextDecoderStreamInit")>]
    type TextDecoderStreamTextDecoderStreamInit =
        abstract ignoreBOM: option<bool> with get, set
        abstract fatal: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "NotFoundError")>]
    type NotFoundError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Leonardo_Lucid_Origin_Input")>]
    type AiCfLeonardoLucidOriginInput =
        abstract steps: option<float> with get, set

        [<EmitProperty("num_steps")>]
        abstract numSteps: option<float> with get, set

        abstract width: option<float> with get, set
        abstract height: option<float> with get, set
        abstract seed: option<float> with get, set
        abstract guidance: option<float> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseInputItemMessage")>]
    type ResponseInputItemMessage =
        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
        abstract role: LiteralUnions.DeveloperSystemUser with get, set
        abstract content: ResponseInputMessageContentList with get, set

    [<Import("@cloudflare/workers-types", "R2Object")>]
    type R2Object =
        abstract ssecKeyMd5: option<string> with get
        abstract storageClass: string with get
        abstract range: option<R2Range> with get
        abstract customMetadata: option<obj> with get
        abstract httpMetadata: option<R2HTTPMetadata> with get
        abstract uploaded: Date with get
        abstract checksums: R2Checksums with get
        abstract httpEtag: string with get
        abstract etag: string with get
        abstract size: float with get
        abstract version: string with get
        abstract key: string with get
        abstract writeHttpMetadata: headers: Headers -> unit

    [<Import("@cloudflare/workers-types", "AiIMultimodalEmbeddingsOutput")>]
    type AiIMultimodalEmbeddingsOutput =
        abstract shape: AiSentenceSimilarityOutput with get, set
        abstract data: ResizeArray<AiSentenceSimilarityOutput> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen2_5_Coder_32B_Instruct_Messages")>]
    type AiCfQwenQwen25Coder32BInstructMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfQwenQwen25Coder32BInstructJSONMode1> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<AiCfQwenQwen25Coder32BInstructMessages.Messages> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchListItemsResponse")>]
    type AiSearchListItemsResponse =
        [<EmitProperty("result_info")>]
        abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

        abstract result: ResizeArray<AiSearchItemInfo> with get, set

    [<Import("@cloudflare/workers-types", "CryptoKeyArbitraryKeyAlgorithm")>]
    type CryptoKeyArbitraryKeyAlgorithm =
        abstract length: option<float> with get, set
        abstract namedCurve: option<string> with get, set
        abstract hash: option<CryptoKeyKeyAlgorithm> with get, set
        abstract name: string with get, set

    type KVNamespaceListResult<'Metadata, 'Key> = U2<SharedLiterals.CacheStatusCursorKeysListComplete<'Metadata, 'Key>, SharedLiterals.CacheStatusKeysListComplete<'Metadata, 'Key>>

    [<Import("@cloudflare/workers-types", "ImageTransform")>]
    type ImageTransform =
        abstract trim: option<U2<string, SharedLiterals.BorderBottomHeight5a030263>> with get, set
        abstract sharpen: option<float> with get, set
        abstract saturation: option<float> with get, set
        abstract rotate: option<LiteralUnions.I0I180I270I90> with get, set
        abstract gravity: option<U2<LiteralUnions.AutoBottomCenterB85bc937, SharedLiterals.ModeXY>> with get, set
        abstract segment: option<string> with get, set
        abstract gamma: option<float> with get, set
        abstract flip: option<LiteralUnions.HHvV> with get, set
        abstract fit: option<LiteralUnions.ContainCoverCrop8bc042f3> with get, set
        abstract contrast: option<float> with get, set
        abstract brightness: option<float> with get, set
        abstract border: option<U2<SharedLiterals.ColorWidth, SharedLiterals.BottomLeftRightTop2>> with get, set
        abstract blur: option<float> with get, set
        abstract background: option<string> with get, set
        abstract height: option<float> with get, set
        abstract width: option<float> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Baai_Bge_Small_En_V1_5")>]
    type BaseAiCfBaaiBgeSmallEnV15 =
        abstract postProcessedOutputs: AiCfBaaiBgeSmallEnV15Output with get, set
        abstract inputs: AiCfBaaiBgeBaseEnV15Input with get, set

    [<Import("@cloudflare/workers-types", "MediaTransformer")>]
    type MediaTransformer =
        abstract transform: ?transform: MediaTransformationInputOptions -> MediaTransformationGenerator
        abstract output: ?output: MediaTransformationOutputOptions -> MediaTransformationResult

    [<Import("@cloudflare/workers-types", "CloudflareAccessContext")>]
    type CloudflareAccessContext =
        abstract aud: string with get
        abstract getIdentity: unit -> Promise<option<CloudflareAccessIdentity>>

    [<Import("@cloudflare/workers-types", "DurableObjectNamespace")>]
    type DurableObjectNamespace<'T> =
        abstract newUniqueId: ?options: DurableObjectNamespaceNewUniqueIdOptions -> DurableObjectId
        abstract idFromName: name: string -> DurableObjectId
        abstract idFromString: id: string -> DurableObjectId
        abstract get: id: DurableObjectId * ?options: DurableObjectNamespaceGetDurableObjectOptions -> DurableObjectNamespace.Get
        abstract getByName: name: string * ?options: DurableObjectNamespaceGetDurableObjectOptions -> obj
        abstract jurisdiction: jurisdiction: DurableObjectJurisdiction -> DurableObjectNamespace<'T>

    [<Import("@cloudflare/workers-types", "DurableObjectStorage")>]
    type DurableObjectStorage =
        abstract kv: SyncKvStorage with get, set
        abstract sql: SqlStorage with get, set
        abstract get: key: string * ?options: DurableObjectGetOptions -> Promise<option<obj>>
        abstract get: keys: ResizeArray<string> * ?options: DurableObjectGetOptions -> Promise<Map<string, obj>>
        abstract list: ?options: DurableObjectListOptions -> Promise<Map<string, obj>>
        abstract put: key: string * value: obj * ?options: DurableObjectPutOptions -> Promise<unit>
        abstract put: entries: obj * ?options: DurableObjectPutOptions -> Promise<unit>
        abstract delete: key: string * ?options: DurableObjectPutOptions -> Promise<bool>
        abstract delete: keys: ResizeArray<string> * ?options: DurableObjectPutOptions -> Promise<float>
        abstract deleteAll: ?options: DurableObjectPutOptions -> Promise<unit>
        abstract transaction: closure: (DurableObjectTransaction -> Promise<obj>) -> Promise<obj>
        abstract getAlarm: ?options: DurableObjectGetAlarmOptions -> Promise<option<float>>
        abstract setAlarm: scheduledTime: U2<Date, float> * ?options: DurableObjectSetAlarmOptions -> Promise<unit>
        abstract deleteAlarm: ?options: DurableObjectSetAlarmOptions -> Promise<unit>
        abstract sync: unit -> Promise<unit>
        abstract transactionSync: closure: (unit -> obj) -> obj
        abstract getCurrentBookmark: unit -> Promise<string>
        abstract getBookmarkForTime: timestamp: U2<Date, float> -> Promise<string>
        abstract onNextSessionRestoreBookmark: bookmark: string -> Promise<string>

    [<Import("@cloudflare/workers-types", "AudioParams")>]
    type AudioParams =
        abstract format: LiteralUnions.AacFlacMp3OpusPcm16Wav with get, set
        abstract voice: U2<StreamDirectUploadWatermark, string> with get, set

    [<Import("@cloudflare/workers-types", "AiImageTextToTextOutput")>]
    type AiImageTextToTextOutput =
        abstract description: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemLogsResponse")>]
    type AiSearchItemLogsResponse =
        [<EmitProperty("result_info")>]
        abstract resultInfo: SharedLiterals.CountCursorPerPageTruncated with get, set

        abstract result: ResizeArray<AiSearchItemLog> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Leonardo_Phoenix_1_0_Input")>]
    type AiCfLeonardoPhoenix10Input =
        [<EmitProperty("negative_prompt")>]
        abstract negativePrompt: option<string> with get, set

        [<EmitProperty("num_steps")>]
        abstract numSteps: option<float> with get, set

        abstract width: option<float> with get, set
        abstract height: option<float> with get, set
        abstract seed: option<float> with get, set
        abstract guidance: option<float> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "WorkerLoaderModule")>]
    type WorkerLoaderModule =
        abstract wasm: option<ArrayBuffer> with get, set
        abstract py: option<string> with get, set
        abstract json: option<obj> with get, set
        abstract data: option<ArrayBuffer> with get, set
        abstract text: option<string> with get, set
        abstract cjs: option<string> with get, set
        abstract js: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AiTextGenerationFunctionsInput")>]
    type AiTextGenerationFunctionsInput =
        abstract code: string with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "StreamWatermarkCreateParams")>]
    type StreamWatermarkCreateParams =
        abstract position: option<StreamWatermarkPosition> with get, set
        abstract scale: option<float> with get, set
        abstract padding: option<float> with get, set
        abstract opacity: option<float> with get, set
        abstract name: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ContainerSnapshot")>]
    type ContainerSnapshot =
        abstract name: option<string> with get, set
        abstract size: float with get, set
        abstract id: string with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRAGNameNotSetError")>]
    type AutoRAGNameNotSetError = interface end

    type VectorizeIndexConfig = U2<SharedLiterals.DimensionsMetric, SharedLiterals.Preset>

    [<Import("@cloudflare/workers-types", "SubtleCryptoEncryptAlgorithm")>]
    type SubtleCryptoEncryptAlgorithm =
        abstract label: option<BufferSource> with get, set
        abstract length: option<float> with get, set
        abstract counter: option<BufferSource> with get, set
        abstract tagLength: option<float> with get, set
        abstract additionalData: option<BufferSource> with get, set
        abstract iv: option<BufferSource> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemChunk")>]
    type AiSearchItemChunk =
        abstract item: option<SharedLiterals.KeyMetadataTimestamp> with get, set

        [<EmitProperty("end_byte")>]
        abstract endByte: float with get, set

        [<EmitProperty("start_byte")>]
        abstract startByte: float with get, set

        abstract text: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "AiImageTextToTextInput")>]
    type AiImageTextToTextInput =
        abstract messages: option<ResizeArray<RoleScopedChatInput>> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        [<EmitProperty("ignore_eos")>]
        abstract ignoreEos: option<bool> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract prompt: option<string> with get, set
        abstract image: string with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesVaryHeaders")>]
    type RequestInitCfPropertiesVaryHeaders =
        [<EmitProperty("accept-language")>]
        abstract acceptLanguage: option<RequestInitCfPropertiesVaryAcceptLanguageHeader> with get, set

        abstract accept: option<RequestInitCfPropertiesVaryAcceptHeader> with get, set
        abstract Item: header: string -> option<U3<RequestInitCfPropertiesVaryHeader, RequestInitCfPropertiesVaryAcceptHeader, RequestInitCfPropertiesVaryAcceptLanguageHeader>>

    [<Import("@cloudflare/workers-types", "ErrorEventErrorEventInit")>]
    type ErrorEventErrorEventInit =
        abstract error: option<obj> with get, set
        abstract colno: option<float> with get, set
        abstract lineno: option<float> with get, set
        abstract filename: option<string> with get, set
        abstract message: option<string> with get, set

    [<Import("@cloudflare/workers-types", "TextDecoderStream")>]
    type TextDecoderStream =
        [<EmitConstructor>]
        abstract Create: ?label: string * ?options: TextDecoderStreamTextDecoderStreamInit -> TextDecoderStream

        inherit TransformStream<BufferSource, string>
        abstract ignoreBOM: bool with get
        abstract fatal: bool with get
        abstract encoding: string with get

    [<Import("@cloudflare/workers-types", "AiImageToTextInput")>]
    type AiImageToTextInput =
        abstract messages: option<ResizeArray<RoleScopedChatInput>> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract prompt: option<string> with get, set
        abstract image: AiSentenceSimilarityOutput with get, set

    [<Import("@cloudflare/workers-types", "QueueSendBatchMetadata")>]
    type QueueSendBatchMetadata =
        abstract metrics: QueueSendBatchMetrics with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_2_Dev_Input")>]
    type AiCfBlackForestLabsFlux2DevInput =
        abstract multipart: SharedLiterals.BodyContentType with get, set

    [<Import("@cloudflare/workers-types", "AiTextGenerationInput")>]
    type AiTextGenerationInput =
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract tools: option<U3<ResizeArray<AiTextGenerationToolInput>, ResizeArray<AiTextGenerationToolLegacyInput>, obj>> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiTextGenerationResponseFormat> with get, set

        abstract messages: option<ResizeArray<RoleScopedChatInput>> with get, set

        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set
        abstract prompt: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Pfnet_Plamo_Embedding_1B")>]
    type BaseAiCfPfnetPlamoEmbedding1B =
        abstract postProcessedOutputs: AiCfPfnetPlamoEmbedding1BOutput with get, set
        abstract inputs: AiCfPfnetPlamoEmbedding1BInput with get, set

    [<Import("@cloudflare/workers-types", "TraceItem")>]
    type TraceItem =
        abstract wallTime: float with get
        abstract cpuTime: float with get
        abstract truncated: bool with get
        abstract executionModel: string with get
        abstract outcome: string with get
        abstract durableObjectId: option<string> with get
        abstract preview: option<TracePreviewInfo> with get
        abstract tailAttributes: option<obj> with get
        abstract scriptTags: option<ResizeArray<string>> with get
        abstract dispatchNamespace: option<string> with get
        abstract scriptVersion: option<ScriptVersion> with get
        abstract entrypoint: option<string> with get
        abstract scriptName: option<string> with get
        abstract diagnosticsChannelEvents: ResizeArray<TraceDiagnosticChannelEvent> with get
        abstract exceptions: ResizeArray<TraceException> with get
        abstract logs: ResizeArray<TraceLog> with get
        abstract eventTimestamp: option<float> with get
        abstract event: option<U10<TraceItemFetchEventInfo, TraceItemJsRpcEventInfo, TraceItemConnectEventInfo, TraceItemScheduledEventInfo, TraceItemAlarmEventInfo, TraceItemQueueEventInfo, TraceItemEmailEventInfo, TraceItemTailEventInfo, TraceItemCustomEventInfo, TraceItemHibernatableWebSocketEventInfo>> with get

    [<Import("@cloudflare/workers-types", "TracePreviewInfo")>]
    type TracePreviewInfo =
        abstract name: string with get, set
        abstract slug: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Headers")>]
    type Headers =
        [<EmitConstructor>]
        abstract Create: ?init: HeadersInit -> Headers

        abstract get: name: string -> option<string>
        abstract getAll: name: string -> ResizeArray<string>
        abstract getSetCookie: unit -> ResizeArray<string>
        abstract has: name: string -> bool
        abstract set: name: string * value: string -> unit
        abstract append: name: string * value: string -> unit
        abstract delete: name: string -> unit
        abstract forEach: callback: (obj -> string -> string -> Headers -> unit) * ?thisArg: obj -> unit
        abstract entries: unit -> seq<string * string>
        abstract keys: unit -> seq<string>
        abstract values: unit -> seq<string>
        abstract ``[symbol.iterator]``: unit -> seq<string * string>

    [<Import("@cloudflare/workers-types", "AiSearchItemLog")>]
    type AiSearchItemLog =
        abstract errorType: option<string> with get, set
        abstract processingTimeMs: option<float> with get, set
        abstract chunkCount: option<float> with get, set
        abstract fileKey: option<string> with get, set
        abstract message: string with get, set
        abstract action: string with get, set
        abstract timestamp: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwq_32B_Messages")>]
    type AiCfQwenQwq32BMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRoleToolCallId> with get, set

    [<Import("@cloudflare/workers-types", "DurableObjectGetOptions")>]
    type DurableObjectGetOptions =
        abstract noCache: option<bool> with get, set
        abstract allowConcurrency: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Black_Forest_Labs_Flux_2_Klein_4B")>]
    type BaseAiCfBlackForestLabsFlux2Klein4B =
        abstract postProcessedOutputs: AiCfBlackForestLabsFlux2Klein4BOutput with get, set
        abstract inputs: AiCfBlackForestLabsFlux2Klein4BInput with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Nvidia_Nemotron_3_120B_A12B")>]
    type BaseAiCfNvidiaNemotron3120BA12B =
        abstract postProcessedOutputs: ChatCompletionsOutput with get, set
        abstract inputs: ChatCompletionsInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_JSON_Mode")>]
    type AiCfQwenQwen330BA3BFp8JSONMode =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_Async_Batch")>]
    type AiCfMetaLlama4Scout17B16EInstructAsyncBatch =
        abstract requests: ResizeArray<U2<AiCfMetaLlama4Scout17B16EInstructPromptInner, AiCfMetaLlama4Scout17B16EInstructMessagesInner>> with get, set

    [<Import("@cloudflare/workers-types", "CustomEventCustomEventInit")>]
    type CustomEventCustomEventInit =
        abstract detail: option<obj> with get, set
        abstract composed: option<bool> with get, set
        abstract cancelable: option<bool> with get, set
        abstract bubbles: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_Guard_3_8B_Output")>]
    type AiCfMetaLlamaGuard38BOutput =
        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract response: option<U2<AiCfMetaLlamaGuard38BOutput.Response, string>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Pipecat_Ai_Smart_Turn_V2_Output")>]
    type AiCfPipecatAiSmartTurnV2Output =
        abstract probability: option<float> with get, set

        [<EmitProperty("is_complete")>]
        abstract isComplete: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchJobInfo")>]
    type AiSearchJobInfo =
        [<EmitProperty("end_reason")>]
        abstract endReason: option<string> with get, set

        [<EmitProperty("ended_at")>]
        abstract endedAt: option<string> with get, set

        [<EmitProperty("started_at")>]
        abstract startedAt: option<string> with get, set

        [<EmitProperty("last_seen_at")>]
        abstract lastSeenAt: option<string> with get, set

        abstract description: option<string> with get, set
        abstract source: LiteralUnions.ScheduleUser with get, set
        abstract id: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BrowserRunLifecycleEvent =
        | [<CompiledName("load")>] Load
        | [<CompiledName("domcontentloaded")>] Domcontentloaded
        | [<CompiledName("networkidle0")>] Networkidle0
        | [<CompiledName("networkidle2")>] Networkidle2

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_1_Schnell_Input")>]
    type AiCfBlackForestLabsFlux1SchnellInput =
        abstract steps: option<float> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesTLSClientAuthPlaceholder")>]
    type IncomingRequestCfPropertiesTLSClientAuthPlaceholder =
        abstract certChainRFC9440TooLarge: bool with get, set
        abstract certChainRFC9440: string with get, set
        abstract certRFC9440TooLarge: bool with get, set
        abstract certRFC9440: string with get, set
        abstract certNotAfter: string with get, set
        abstract certNotBefore: string with get, set
        abstract certFingerprintSHA256: string with get, set
        abstract certFingerprintSHA1: string with get, set
        abstract certIssuerSKI: string with get, set
        abstract certSKI: string with get, set
        abstract certIssuerSerial: string with get, set
        abstract certSerial: string with get, set
        abstract certSubjectDNLegacy: string with get, set
        abstract certIssuerDNLegacy: string with get, set
        abstract certSubjectDNRFC2253: string with get, set
        abstract certIssuerDNRFC2253: string with get, set
        abstract certSubjectDN: string with get, set
        abstract certIssuerDN: string with get, set
        abstract certRevoked: string with get, set
        abstract certVerified: string with get, set
        abstract certPresented: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArtifactsErrorCode =
        | ALREADY_EXISTS
        | NOT_FOUND
        | IMPORT_IN_PROGRESS
        | FORK_IN_PROGRESS
        | INVALID_INPUT
        | INVALID_REPO_NAME
        | INVALID_TTL
        | INVALID_URL
        | REMOTE_AUTH_REQUIRED
        | UPSTREAM_UNAVAILABLE
        | MEMORY_LIMIT
        | INTERNAL_ERROR

    [<Import("@cloudflare/workers-types", "AiSearchMessage")>]
    type AiSearchMessage =
        abstract content: option<string> with get, set
        abstract role: LiteralUnions.AssistantDeveloperSystemToolUser with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type D1SessionConstraint =
        | [<CompiledName("first-primary")>] FirstPrimary
        | [<CompiledName("first-unconstrained")>] FirstUnconstrained

    [<Import("@cloudflare/workers-types", "ResponseCreatedEvent")>]
    type ResponseCreatedEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        abstract response: Response with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionContentPartInputAudio")>]
    type ChatCompletionContentPartInputAudio =
        [<EmitProperty("input_audio")>]
        abstract inputAudio: SharedLiterals.DataFormat2 with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "MediaTransformationInputOptions")>]
    type MediaTransformationInputOptions =
        abstract height: option<float> with get, set
        abstract width: option<float> with get, set
        abstract fit: option<LiteralUnions.ContainCoverScaleDown> with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRagSearchResponse")>]
    type AutoRagSearchResponse =
        [<EmitProperty("next_page")>]
        abstract nextPage: option<string> with get, set

        [<EmitProperty("has_more")>]
        abstract hasMore: bool with get, set

        abstract data: ResizeArray<SharedLiterals.AttributesContentFileF6de1a38> with get, set

        [<EmitProperty("search_query")>]
        abstract searchQuery: string with get, set

        abstract object: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Prompt_1")>]
    type AiCfAisingaporeGemmaSeaLionV427BItPrompt1 =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfAisingaporeGemmaSeaLionV427BItJSONMode2> with get, set

        abstract lora: option<string> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "Performance")>]
    type Performance =
        abstract timeOrigin: float with get
        abstract now: unit -> float
        abstract toJSON: unit -> obj

    type VectorizeVectorMetadata = U5<ResizeArray<string>, obj, string, float, bool>

    [<Import("@cloudflare/workers-types", "LoopbackColoLocalActorNamespace")>]
    type LoopbackColoLocalActorNamespace =
        interface
            inherit ColoLocalActorNamespace
        end

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type QueueContentType =
        | [<CompiledName("json")>] Json
        | [<CompiledName("text")>] Text
        | [<CompiledName("v8")>] V8
        | [<CompiledName("bytes")>] Bytes

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Google_Gemma_3_12B_It")>]
    type BaseAiCfGoogleGemma312BIt =
        abstract postProcessedOutputs: AiCfGoogleGemma312BItOutput with get, set
        abstract inputs: AiCfGoogleGemma312BItInput with get, set

    [<Import("@cloudflare/workers-types", "URLPattern")>]
    type URLPattern =
        [<EmitConstructor>]
        abstract Create: ?input: U2<URLPatternInit, string> * ?baseURL: U2<URLPatternOptions, string> * ?patternOptions: URLPatternOptions -> URLPattern

        abstract hasRegExpGroups: bool with get
        abstract hash: string with get
        abstract search: string with get
        abstract pathname: string with get
        abstract port: string with get
        abstract hostname: string with get
        abstract password: string with get
        abstract username: string with get
        abstract protocol: string with get
        abstract test: ?input: U2<URLPatternInit, string> * ?baseURL: string -> bool
        abstract exec: ?input: U2<URLPatternInit, string> * ?baseURL: string -> option<URLPatternResult>

    [<Import("@cloudflare/workers-types", "StreamBinding")>]
    type StreamBinding =
        abstract watermarks: StreamWatermarks with get, set
        abstract videos: StreamVideos with get, set
        abstract video: id: string -> StreamVideoHandle
        abstract upload: url: string * ?params: StreamUrlUploadParams -> Promise<StreamVideo>
        abstract createDirectUpload: params: StreamDirectUploadCreateParams -> Promise<StreamDirectUpload>

    [<Import("@cloudflare/workers-types", "WebSocketRequestResponsePair")>]
    type WebSocketRequestResponsePair =
        [<EmitConstructor>]
        abstract Create: request: string * response: string -> WebSocketRequestResponsePair

        abstract response: string with get
        abstract request: string with get

    type R2Range = U3<SharedLiterals.LengthOffset, SharedLiterals.LengthOffset2, SharedLiterals.Suffix>

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ResponseStatus =
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("failed")>] Failed
        | [<CompiledName("queued")>] Queued
        | [<CompiledName("cancelled")>] Cancelled
        | [<CompiledName("in_progress")>] InProgress
        | [<CompiledName("incomplete")>] Incomplete

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesCloudflareAccessOrApiShield")>]
    type IncomingRequestCfPropertiesCloudflareAccessOrApiShield =
        abstract tlsClientAuth: U2<IncomingRequestCfPropertiesTLSClientAuth, IncomingRequestCfPropertiesTLSClientAuthPlaceholder> with get, set

    [<Import("@cloudflare/workers-types", "ContainerExecOptions")>]
    type ContainerExecOptions =
        abstract stderr: option<LiteralUnions.CombinedIgnorePipe> with get, set
        abstract stdout: option<LiteralUnions.IgnorePipe> with get, set
        abstract stdin: option<U2<string, ReadableStream<option<obj>>>> with get, set
        abstract user: option<string> with get, set
        abstract env: option<obj> with get, set
        abstract cwd: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchMultiChatCompletionsResponse")>]
    type AiSearchMultiChatCompletionsResponse =
        abstract errors: option<ResizeArray<AiSearchMultiSearchError>> with get, set
        abstract chunks: ResizeArray<AiSearchMultiSearchChunk> with get, set
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "R2Bucket")>]
    type R2Bucket =
        abstract head: key: string -> Promise<option<R2Object>>
        abstract get: key: string * options: R2Bucket.Get.Options -> Promise<option<U2<R2Object, R2ObjectBody>>>
        abstract get: key: string * ?options: R2GetOptions -> Promise<option<R2ObjectBody>>
        abstract put: key: string * ?value: U5<ReadableStream<option<obj>>, ArrayBuffer, obj, Blob, string> * ?options: R2Bucket.Put.Options -> Promise<option<R2Object>>
        abstract put: key: string * ?value: U5<ReadableStream<option<obj>>, ArrayBuffer, obj, Blob, string> * ?options: R2PutOptions -> Promise<R2Object>
        abstract createMultipartUpload: key: string * ?options: R2MultipartOptions -> Promise<R2MultipartUpload>
        abstract resumeMultipartUpload: key: string * uploadId: string -> R2MultipartUpload
        abstract delete: keys: U2<ResizeArray<string>, string> -> Promise<unit>
        abstract list: ?options: R2ListOptions -> Promise<R2Objects>

    type ExportedHandlerTailStreamHandler = TailStream.TailEvent<TailStream.Onset> -> obj -> ExecutionContext<obj> -> U3<TailStream.TailEvent<obj> -> option<Promise<unit>>, TailStream.TailEventHandlerObject, Promise<TailStream.TailEventHandlerType>>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_2_Klein_9B_Output")>]
    type AiCfBlackForestLabsFlux2Klein9BOutput =
        abstract image: option<string> with get, set

    type AiCfBaaiBgeSmallEnV15Input = U2<SharedLiterals.PoolingText, SharedLiterals.Requests>

    [<Import("@cloudflare/workers-types", "TraceItemConnectEventInfo")>]
    type TraceItemConnectEventInfo = interface end

    [<Import("@cloudflare/workers-types", "QueueSendResponse")>]
    type QueueSendResponse =
        abstract metadata: QueueSendMetadata with get, set

    [<Import("@cloudflare/workers-types", "AiSearchInstance")>]
    type AiSearchInstance =
        abstract jobs: AiSearchJobs with get
        abstract items: AiSearchItems with get
        abstract search: params: AiSearchSearchRequest -> Promise<AiSearchSearchResponse>
        abstract chatCompletions: params: AiSearchInstance.ChatCompletions.Params -> Promise<ReadableStream<option<obj>>>
        abstract chatCompletions: params: AiSearchChatCompletionsRequest -> Promise<AiSearchChatCompletionsResponse>
        abstract update: config: AiSearchInstance.Update.Config -> Promise<AiSearchInstanceInfo>
        abstract info: unit -> Promise<AiSearchInstanceInfo>
        abstract stats: unit -> Promise<AiSearchStatsResponse>

    type ImageInfoResponse = U2<SharedLiterals.Format, SharedLiterals.FileSizeFormatHeightWidth>
    type ExportedHandlerTailHandler = ResizeArray<TraceItem> -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "ByteLengthQueuingStrategy")>]
    type ByteLengthQueuingStrategy =
        [<EmitConstructor>]
        abstract Create: init: QueuingStrategyInit -> ByteLengthQueuingStrategy

        inherit QueuingStrategy<obj>
        abstract highWaterMark: float with get
        abstract size: ?chunk: obj -> float

    [<Import("@cloudflare/workers-types", "ImageInputOptions")>]
    type ImageInputOptions =
        abstract encoding: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ExecProcess")>]
    type ExecProcess =
        abstract exitCode: Promise<float> with get
        abstract pid: float with get
        abstract stderr: option<ReadableStream<option<obj>>> with get
        abstract stdout: option<ReadableStream<option<obj>>> with get
        abstract stdin: option<WritableStream<option<obj>>> with get
        abstract output: unit -> Promise<ExecOutput>
        abstract kill: ?signal: float -> unit

    [<Import("@cloudflare/workers-types", "WebSearchSearchOptions")>]
    type WebSearchSearchOptions =
        abstract limit: option<float> with get, set
        abstract query: string with get, set

    [<Import("@cloudflare/workers-types", "WritableStreamDefaultWriter")>]
    type WritableStreamDefaultWriter<'W> =
        [<EmitConstructor>]
        abstract Create: stream: WritableStream<option<obj>> -> WritableStreamDefaultWriter<'W>

        abstract desiredSize: option<float> with get
        abstract ready: Promise<unit> with get
        abstract closed: Promise<unit> with get
        abstract abort: ?reason: obj -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract write: ?chunk: 'W -> Promise<unit>
        abstract releaseLock: unit -> unit

    [<Import("@cloudflare/workers-types", "Hyperdrive")>]
    type Hyperdrive =
        abstract database: string with get
        abstract password: string with get
        abstract user: string with get
        abstract port: float with get
        abstract host: string with get
        abstract connectionString: string with get
        abstract connect: unit -> Socket

    [<Import("@cloudflare/workers-types", "ArtifactsRepo")>]
    type ArtifactsRepo =
        inherit ArtifactsRepoInfo
        abstract createToken: ?scope: LiteralUnions.ReadWrite * ?ttl: float -> Promise<ArtifactsCreateTokenResult>
        abstract listTokens: unit -> Promise<ArtifactsTokenListResult>
        abstract revokeToken: tokenOrId: string -> Promise<bool>
        abstract fork: name: string * ?opts: ArtifactsRepo.Fork.Opts -> Promise<ArtifactsCreateRepoResult>

    [<Import("@cloudflare/workers-types", "Container")>]
    type Container =
        abstract running: bool with get
        abstract start: ?options: ContainerStartupOptions -> unit
        abstract monitor: unit -> Promise<unit>
        abstract destroy: ?error: obj -> Promise<unit>
        abstract signal: signo: float -> unit
        abstract getTcpPort: port: float -> SharedLiterals.ConnectFetch
        abstract setInactivityTimeout: durationMs: float -> Promise<unit>
        abstract interceptOutboundHttp: addr: string * binding: SharedLiterals.ConnectFetch -> Promise<unit>
        abstract interceptAllOutboundHttp: binding: SharedLiterals.ConnectFetch -> Promise<unit>
        abstract snapshotDirectory: options: ContainerDirectorySnapshotOptions -> Promise<ContainerDirectorySnapshot>
        abstract snapshotContainer: options: ContainerSnapshotOptions -> Promise<ContainerSnapshot>
        abstract interceptOutboundHttps: addr: string * binding: SharedLiterals.ConnectFetch -> Promise<unit>
        abstract exec: cmd: ResizeArray<string> * ?options: ContainerExecOptions -> Promise<ExecProcess>

    [<Import("@cloudflare/workers-types", "ForbiddenError")>]
    type ForbiddenError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Text")>]
    type Text =
        abstract removed: bool with get
        abstract lastInTextNode: bool with get
        abstract text: string with get
        abstract before: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Text
        abstract after: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Text
        abstract replace: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Text
        abstract remove: unit -> Text

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_JSON_Mode_1")>]
    type AiCfQwenQwen330BA3BFp8JSONMode1 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchNotFoundError")>]
    type AiSearchNotFoundError = interface end

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Text_Completion_Response")>]
    type AiCfAisingaporeGemmaSeaLionV427BItTextCompletionResponse =
        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract choices: option<ResizeArray<SharedLiterals.FinishReasonIndexFc31d0af>> with get, set
        abstract model: option<string> with get, set
        abstract created: option<float> with get, set
        abstract object: option<string> with get, set
        abstract id: option<string> with get, set

    [<Import("@cloudflare/workers-types", "R2UploadedPart")>]
    type R2UploadedPart =
        abstract etag: string with get, set
        abstract partNumber: float with get, set

    [<Import("@cloudflare/workers-types", "AiSummarizationOutput")>]
    type AiSummarizationOutput =
        abstract summary: string with get, set

    [<Import("@cloudflare/workers-types", "MediaTransformationGenerator")>]
    type MediaTransformationGenerator =
        abstract output: ?output: MediaTransformationOutputOptions -> MediaTransformationResult

    [<Import("@cloudflare/workers-types", "D1Meta")>]
    type D1Meta =
        [<EmitProperty("total_attempts")>]
        abstract totalAttempts: option<float> with get, set

        abstract timings: option<SharedLiterals.SqlDurationMs> with get, set

        [<EmitProperty("served_by_primary")>]
        abstract servedByPrimary: option<bool> with get, set

        [<EmitProperty("served_by_colo")>]
        abstract servedByColo: option<string> with get, set

        [<EmitProperty("served_by_region")>]
        abstract servedByRegion: option<string> with get, set

        abstract changes: float with get, set

        [<EmitProperty("changed_db")>]
        abstract changedDb: bool with get, set

        [<EmitProperty("last_row_id")>]
        abstract lastRowId: float with get, set

        [<EmitProperty("rows_written")>]
        abstract rowsWritten: float with get, set

        [<EmitProperty("rows_read")>]
        abstract rowsRead: float with get, set

        [<EmitProperty("size_after")>]
        abstract sizeAfter: float with get, set

        abstract duration: float with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionToolChoiceFunction")>]
    type ChatCompletionToolChoiceFunction =
        [<EmitProperty("function")>]
        abstract ``function``: SharedLiterals.Name2 with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwq_32B_Output")>]
    type AiCfQwenQwq32BOutput =
        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract response: string with get, set

    [<Import("@cloudflare/workers-types", "WorkflowInstanceCreateOptions")>]
    type WorkflowInstanceCreateOptions<'PARAMS> =
        abstract retention: option<WorkflowInstanceCreateOptions.Retention> with get, set
        abstract params: option<'PARAMS> with get, set
        abstract id: option<string> with get, set

    type EventListener = obj -> unit

    [<Import("@cloudflare/workers-types", "ArtifactsTokenListResult")>]
    type ArtifactsTokenListResult =
        abstract total: float with get, set
        abstract tokens: ResizeArray<ArtifactsTokenInfo> with get, set

    [<Import("@cloudflare/workers-types", "ReadableStreamBYOBRequest")>]
    type ReadableStreamBYOBRequest =
        abstract atLeast: option<float> with get
        abstract view: option<Uint8Array> with get
        abstract respond: bytesWritten: float -> unit
        abstract respondWithNewView: view: BufferSource -> unit

    [<Import("@cloudflare/workers-types", "AiSearchListInstancesParams")>]
    type AiSearchListInstancesParams =
        [<EmitProperty("order_by_direction")>]
        abstract orderByDirection: option<LiteralUnions.AscDesc> with get, set

        [<EmitProperty("order_by")>]
        abstract orderBy: option<string> with get, set

        abstract search: option<string> with get, set

        [<EmitProperty("per_page")>]
        abstract perPage: option<float> with get, set

        abstract page: option<float> with get, set

    [<Import("@cloudflare/workers-types", "Blob")>]
    type Blob =
        [<EmitConstructor>]
        abstract Create: ?bits: ResizeArray<U4<ArrayBuffer, obj, Blob, string>> * ?options: BlobOptions -> Blob

        [<EmitProperty("type")>]
        abstract ``type``: string with get

        abstract size: float with get
        abstract slice: ?start: float * ?``end``: float * ?``type``: string -> Blob
        abstract arrayBuffer: unit -> Promise<ArrayBuffer>
        abstract bytes: unit -> Promise<Uint8Array>
        abstract text: unit -> Promise<string>
        abstract stream: unit -> ReadableStream<option<obj>>

    [<Import("@cloudflare/workers-types", "MediaTransformationOutputOptions")>]
    type MediaTransformationOutputOptions =
        abstract format: option<LiteralUnions.JpgM4aPng> with get, set
        abstract imageCount: option<float> with get, set
        abstract duration: option<string> with get, set
        abstract time: option<string> with get, set
        abstract audio: option<bool> with get, set
        abstract mode: option<LiteralUnions.AudioFrameSpritesheetVideo> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DurableObjectLocationHint =
        | [<CompiledName("wnam")>] Wnam
        | [<CompiledName("enam")>] Enam
        | [<CompiledName("sam")>] Sam
        | [<CompiledName("weur")>] Weur
        | [<CompiledName("eeur")>] Eeur
        | [<CompiledName("apac")>] Apac
        | [<CompiledName("apac-ne")>] ApacNe
        | [<CompiledName("apac-se")>] ApacSe
        | [<CompiledName("oc")>] Oc
        | [<CompiledName("afr")>] Afr
        | [<CompiledName("me")>] Me

    [<Import("@cloudflare/workers-types", "BaseAiTranslation")>]
    type BaseAiTranslation =
        abstract postProcessedOutputs: AiTranslationOutput with get, set
        abstract inputs: AiTranslationInput with get, set

    [<Import("@cloudflare/workers-types", "QueueEvent")>]
    type QueueEvent<'Body> =
        inherit ExtendableEvent
        abstract metadata: MessageBatchMetadata with get
        abstract queue: string with get
        abstract messages: System.Collections.Generic.IReadOnlyList<Message<'Body>> with get
        abstract retryAll: ?options: QueueRetryOptions -> unit
        abstract ackAll: unit -> unit

    [<Import("@cloudflare/workers-types", "Ai_Cf_Mistralai_Mistral_Small_3_1_24B_Instruct_Messages")>]
    type AiCfMistralaiMistralSmall3124BInstructMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRoleToolCallId> with get, set

    [<Import("@cloudflare/workers-types", "R2PutOptions")>]
    type R2PutOptions =
        abstract ssecKey: option<U2<ArrayBuffer, string>> with get, set
        abstract storageClass: option<string> with get, set
        abstract sha512: option<U3<ArrayBuffer, obj, string>> with get, set
        abstract sha384: option<U3<ArrayBuffer, obj, string>> with get, set
        abstract sha256: option<U3<ArrayBuffer, obj, string>> with get, set
        abstract sha1: option<U3<ArrayBuffer, obj, string>> with get, set
        abstract md5: option<U3<ArrayBuffer, obj, string>> with get, set
        abstract customMetadata: option<obj> with get, set
        abstract httpMetadata: option<U2<R2HTTPMetadata, Headers>> with get, set
        abstract onlyIf: option<U2<R2Conditional, Headers>> with get, set

    [<Import("@cloudflare/workers-types", "UnsafeTraceMetrics")>]
    type UnsafeTraceMetrics =
        abstract fromTrace: item: TraceItem -> TraceMetrics

    [<Import("@cloudflare/workers-types", "R2MultipartUpload")>]
    type R2MultipartUpload =
        abstract uploadId: string with get
        abstract key: string with get
        abstract uploadPart: partNumber: float * value: U5<ReadableStream<option<obj>>, ArrayBuffer, obj, Blob, string> * ?options: R2UploadPartOptions -> Promise<R2UploadedPart>
        abstract abort: unit -> Promise<unit>
        abstract complete: uploadedParts: ResizeArray<R2UploadedPart> -> Promise<R2Object>

    [<Import("@cloudflare/workers-types", "ResponseUsage")>]
    type ResponseUsage =
        [<EmitProperty("total_tokens")>]
        abstract totalTokens: float with get, set

        [<EmitProperty("output_tokens")>]
        abstract outputTokens: float with get, set

        [<EmitProperty("input_tokens")>]
        abstract inputTokens: float with get, set

    [<Import("@cloudflare/workers-types", "TraceDiagnosticChannelEvent")>]
    type TraceDiagnosticChannelEvent =
        abstract message: option<obj> with get
        abstract channel: string with get
        abstract timestamp: float with get

    [<Import("@cloudflare/workers-types", "BaseAiSummarization")>]
    type BaseAiSummarization =
        abstract postProcessedOutputs: AiSummarizationOutput with get, set
        abstract inputs: AiSummarizationInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_Output")>]
    type AiCfMetaLlama4Scout17B16EInstructOutput =
        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<SharedLiterals.FunctionIdType2>> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract response: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Deepgram_Aura_2_En")>]
    type BaseAiCfDeepgramAura2En =
        abstract postProcessedOutputs: string with get, set
        abstract inputs: AiCfDeepgramAura2EnInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Messages_1")>]
    type AiCfQwenQwen330BA3BFp8Messages1 =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfQwenQwen330BA3BFp8JSONMode3> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRole> with get, set

    [<Import("@cloudflare/workers-types", "ArtifactsTokenInfo")>]
    type ArtifactsTokenInfo =
        abstract expiresAt: string with get, set
        abstract createdAt: string with get, set
        abstract state: LiteralUnions.ActiveExpiredRevoked with get, set
        abstract scope: LiteralUnions.ReadWrite with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Input_QueryAnd_Contexts_1")>]
    type AiCfBaaiBgeM3InputQueryAndContexts1 =
        [<EmitProperty("truncate_inputs")>]
        abstract truncateInputs: option<bool> with get, set

        abstract contexts: ResizeArray<SharedLiterals.Text> with get, set
        abstract query: option<string> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunErrorResponse")>]
    type BrowserRunErrorResponse =
        abstract errors: ResizeArray<SharedLiterals.CodeDetailMessagePath> with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "FlagshipEvaluationDetails")>]
    type FlagshipEvaluationDetails<'T> =
        abstract errorMessage: option<string> with get, set
        abstract errorCode: option<string> with get, set
        abstract reason: option<string> with get, set
        abstract variant: option<string> with get, set
        abstract value: obj with get, set
        abstract flagKey: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseFormatJSONObject")>]
    type ResponseFormatJSONObject =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionTokenLogprob")>]
    type ChatCompletionTokenLogprob =
        [<EmitProperty("top_logprobs")>]
        abstract topLogprobs: ResizeArray<ChatCompletionTopLogprob> with get, set

        abstract bytes: option<AiSentenceSimilarityOutput> with get, set
        abstract logprob: float with get, set
        abstract token: string with get, set

    [<Import("@cloudflare/workers-types", "URL")>]
    type URL =
        [<EmitConstructor>]
        abstract Create: url: U2<URL, string> * ?``base``: U2<URL, string> -> URL

        abstract searchParams: URLSearchParams with get
        abstract hash: string with get
        abstract search: string with get
        abstract pathname: string with get
        abstract port: string with get
        abstract hostname: string with get
        abstract host: string with get
        abstract password: string with get
        abstract username: string with get
        abstract protocol: string with get
        abstract href: string with get
        abstract origin: string with get
        abstract toJSON: unit -> string
        abstract toString: unit -> string
        abstract canParse: url: string * ?``base``: string -> bool
        abstract parse: url: string * ?``base``: string -> option<URL>
        abstract createObjectURL: object: U2<File, Blob> -> string
        abstract revokeObjectURL: objectUrl: string -> unit

    [<Import("@cloudflare/workers-types", "FacetStartupOptions")>]
    type FacetStartupOptions<'T> =
        [<EmitProperty("class")>]
        abstract ``class``: DurableObjectClass<'T> with get, set

        abstract id: option<U2<DurableObjectId, string>> with get, set

    type ResponseOutputItem = U3<ResponseOutputMessage, ResponseFunctionToolCall, ResponseReasoningItem>

    [<Import("@cloudflare/workers-types", "KVNamespacePutOptions")>]
    type KVNamespacePutOptions =
        abstract metadata: option<obj> with get, set
        abstract expirationTtl: option<float> with get, set
        abstract expiration: option<float> with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesCloudflareForSaaSEnterprise")>]
    type IncomingRequestCfPropertiesCloudflareForSaaSEnterprise<'HostMetadata> =
        abstract hostMetadata: option<'HostMetadata> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AgentMemoryThinkingLevel =
        | [<CompiledName("low")>] Low
        | [<CompiledName("high")>] High
        | [<CompiledName("medium")>] Medium

    [<Import("@cloudflare/workers-types", "ChatCompletionContentPartImage")>]
    type ChatCompletionContentPartImage =
        [<EmitProperty("image_url")>]
        abstract imageUrl: SharedLiterals.DetailUrl2 with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "AiTextEmbeddingsOutput")>]
    type AiTextEmbeddingsOutput =
        abstract data: ResizeArray<AiSentenceSimilarityOutput> with get, set
        abstract shape: AiSentenceSimilarityOutput with get, set

    [<Import("@cloudflare/workers-types", "ResponseOutputMessage")>]
    type ResponseOutputMessage =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract status: LiteralUnions.CompletedInProgressIncomplete with get, set
        abstract role: string with get, set
        abstract content: ResizeArray<U2<ResponseOutputText, ResponseOutputRefusal>> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_Reranker_Base_Input")>]
    type AiCfBaaiBgeRerankerBaseInput =
        abstract contexts: ResizeArray<SharedLiterals.Text> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

    [<Import("@cloudflare/workers-types", "LoopbackDurableObjectNamespace")>]
    type LoopbackDurableObjectNamespace =
        interface
            inherit DurableObjectNamespace<unit>
        end

    [<Import("@cloudflare/workers-types", "EventContext")>]
    type EventContext =
        abstract data: obj with get, set
        abstract params: obj with get, set
        abstract env: EventContext.Env with get, set
        abstract functionPath: string with get, set
        abstract request: Request<option<obj>, EventContext.Request> with get, set
        abstract waitUntil: promise: Promise<option<obj>> -> unit
        abstract passThroughOnException: unit -> unit
        abstract next: ?input: U2<Request<option<obj>, U2<RequestInitCfProperties, obj>>, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>

    [<Import("@cloudflare/workers-types", "StreamUpdateVideoParams")>]
    type StreamUpdateVideoParams =
        abstract thumbnailTimestampPct: option<float> with get, set
        abstract scheduledDeletion: option<string> with get, set
        abstract requireSignedURLs: option<bool> with get, set
        abstract meta: option<obj> with get, set
        abstract maxDurationSeconds: option<float> with get, set
        abstract creator: option<string> with get, set
        abstract allowedOrigins: option<ResizeArray<string>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Chat_Completion_Response")>]
    type AiCfAisingaporeGemmaSeaLionV427BItChatCompletionResponse =
        [<EmitProperty("prompt_logprobs")>]
        abstract promptLogprobs: option<MainModule> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract choices: option<ResizeArray<SharedLiterals.FinishReasonIndexE81dd86d>> with get, set
        abstract model: option<string> with get, set
        abstract created: option<float> with get, set
        abstract object: option<string> with get, set
        abstract id: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ExecutionContext")>]
    type ExecutionContext<'Props> =
        abstract tracing: Tracing with get, set
        abstract access: option<CloudflareAccessContext> with get
        abstract cache: option<CacheContext> with get, set
        abstract props: 'Props with get
        abstract exports: Cloudflare.Exports with get
        abstract waitUntil: promise: Promise<option<obj>> -> unit
        abstract passThroughOnException: unit -> unit

    [<Import("@cloudflare/workers-types", "PredictionContent")>]
    type PredictionContent =
        abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "AnalyticsEngineDataPoint")>]
    type AnalyticsEngineDataPoint =
        abstract blobs: option<ResizeArray<option<U2<ArrayBuffer, string>>>> with get, set
        abstract doubles: option<AiSentenceSimilarityOutput> with get, set
        abstract indexes: option<ResizeArray<option<U2<ArrayBuffer, string>>>> with get, set

    type SqlStorageValue = option<U3<ArrayBuffer, string, float>>
    type ChatCompletionCustomToolFormat = U2<ChatCompletionCustomToolTextFormat, ChatCompletionCustomToolGrammarFormat>

    [<Import("@cloudflare/workers-types", "CachePurgeResult")>]
    type CachePurgeResult =
        abstract errors: ResizeArray<CachePurgeError> with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Black_Forest_Labs_Flux_2_Dev")>]
    type BaseAiCfBlackForestLabsFlux2Dev =
        abstract postProcessedOutputs: AiCfBlackForestLabsFlux2DevOutput with get, set
        abstract inputs: AiCfBlackForestLabsFlux2DevInput with get, set

    [<Import("@cloudflare/workers-types", "StructuredSerializeOptions")>]
    type StructuredSerializeOptions =
        abstract transfer: option<ResizeArray<option<obj>>> with get, set

    type AiCfBaaiBgeLargeEnV15Input = U2<SharedLiterals.PoolingText, SharedLiterals.Requests>

    [<Import("@cloudflare/workers-types", "ResponseCustomToolCallOutput")>]
    type ResponseCustomToolCallOutput =
        abstract id: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract output: U2<ResizeArray<ResponseInputContent>, string> with get, set

        [<EmitProperty("call_id")>]
        abstract callId: string with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunScrapeSuccessResponse")>]
    type BrowserRunScrapeSuccessResponse =
        abstract result: ResizeArray<BrowserRunScrapeSuccessResponse.Result> with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "CacheQueryOptions")>]
    type CacheQueryOptions =
        abstract ignoreMethod: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionTopLogprob")>]
    type ChatCompletionTopLogprob =
        abstract bytes: option<AiSentenceSimilarityOutput> with get, set
        abstract logprob: float with get, set
        abstract token: string with get, set

    type AiCfBaaiBgeM3Output = U4<AiCfBaaiBgeM3OutputQuery, AiCfBaaiBgeM3OutputEmbeddingForContexts, AiCfBaaiBgeM3OutputEmbedding, AiCfBaaiBgeM3AsyncResponse>

    [<Import("@cloudflare/workers-types", "WorkerLoader")>]
    type WorkerLoader =
        abstract get: ?name: string * getCode: (unit -> U2<WorkerLoaderWorkerCode, Promise<WorkerLoaderWorkerCode>>) -> WorkerStub
        abstract load: code: WorkerLoaderWorkerCode -> WorkerStub

    [<Import("@cloudflare/workers-types", "AlarmInvocationInfo")>]
    type AlarmInvocationInfo =
        abstract scheduledTime: float with get
        abstract retryCount: float with get
        abstract isRetry: bool with get

    [<Import("@cloudflare/workers-types", "Comment")>]
    type Comment =
        abstract removed: bool with get
        abstract text: string with get, set
        abstract before: content: string * ?options: ContentOptions -> Comment
        abstract after: content: string * ?options: ContentOptions -> Comment
        abstract replace: content: string * ?options: ContentOptions -> Comment
        abstract remove: unit -> Comment

    [<Import("@cloudflare/workers-types", "ChatCompletionsOutput")>]
    type ChatCompletionsOutput =
        [<EmitProperty("service_tier")>]
        abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

        [<EmitProperty("system_fingerprint")>]
        abstract systemFingerprint: option<string> with get, set

        abstract usage: option<CompletionUsage> with get, set
        abstract choices: ResizeArray<ChatCompletionChoice> with get, set
        abstract model: string with get, set
        abstract created: float with get, set
        abstract object: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "QueueSendMetrics")>]
    type QueueSendMetrics =
        abstract oldestMessageTimestamp: option<Date> with get, set
        abstract backlogBytes: float with get, set
        abstract backlogCount: float with get, set

    [<Import("@cloudflare/workers-types", "VectorizeMatch")>]
    type VectorizeMatch =
        abstract score: float with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: option<string> with get, set

        abstract metadata: option<obj> with get, set
        abstract id: string with get, set
        abstract values: option<U3<AiSentenceSimilarityOutput, Float32Array, Float64Array>> with get, set

    [<Import("@cloudflare/workers-types", "ToMarkdownService")>]
    type ToMarkdownService =
        abstract transform: files: ResizeArray<MarkdownDocument> * ?options: ConversionRequestOptions -> Promise<ResizeArray<ConversionResponse>>
        abstract transform: files: MarkdownDocument * ?options: ConversionRequestOptions -> Promise<ConversionResponse>
        abstract supported: unit -> Promise<ResizeArray<SupportedFileFormat>>

    [<Import("@cloudflare/workers-types", "EmailSendResult")>]
    type EmailSendResult =
        abstract messageId: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_Large_En_V1_5_AsyncResponse")>]
    type AiCfBaaiBgeLargeEnV15AsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesImageDraw")>]
    type RequestInitCfPropertiesImageDraw =
        inherit BasicImageTransformations
        abstract right: option<float> with get, set
        abstract bottom: option<float> with get, set
        abstract left: option<float> with get, set
        abstract top: option<float> with get, set
        abstract composite: option<ImageCompositeMode> with get, set
        abstract repeat: option<U2<LiteralUnions.XY, bool>> with get, set
        abstract opacity: option<float> with get, set
        abstract url: string with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunJsonOptions")>]
    type BrowserRunJsonOptions =
        [<EmitProperty("custom_ai")>]
        abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel2>> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItems")>]
    type AiSearchItems =
        abstract list: ?params: AiSearchListItemsParams -> Promise<AiSearchListItemsResponse>
        abstract upload: name: string * content: U3<ReadableStream<option<obj>>, Blob, string> * ?options: AiSearchUploadItemOptions -> Promise<AiSearchItemInfo>
        abstract uploadAndPoll: name: string * content: U3<ReadableStream<option<obj>>, Blob, string> * ?options: AiSearchItems.UploadAndPoll.Options -> Promise<AiSearchItemInfo>
        abstract get: itemId: string -> AiSearchItem
        abstract delete: itemId: string -> Promise<unit>

    [<Import("@cloudflare/workers-types", "ToolMessage")>]
    type ToolMessage =
        [<EmitProperty("tool_call_id")>]
        abstract toolCallId: string with get, set

        abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set
        abstract role: string with get, set

    type ChatCompletionMessageToolCall = U2<ChatCompletionMessageFunctionToolCall, ChatCompletionMessageCustomToolCall>

    [<Import("@cloudflare/workers-types", "VectorizeAsyncMutation")>]
    type VectorizeAsyncMutation =
        abstract mutationId: string with get, set

    [<Import("@cloudflare/workers-types", "StreamDownload")>]
    type StreamDownload =
        abstract url: option<string> with get, set
        abstract status: StreamDownloadStatus with get, set
        abstract percentComplete: float with get, set

    [<Import("@cloudflare/workers-types", "EmbeddedImageConversionOptions")>]
    type EmbeddedImageConversionOptions =
        abstract maxConvertedImages: option<float> with get, set
        abstract convert: option<bool> with get, set
        abstract descriptionLanguage: option<LiteralUnions.DeEnEsFrItPt> with get, set

    [<Import("@cloudflare/workers-types", "QueueMetrics")>]
    type QueueMetrics =
        abstract oldestMessageTimestamp: option<Date> with get, set
        abstract backlogBytes: float with get, set
        abstract backlogCount: float with get, set

    [<Import("@cloudflare/workers-types", "HTMLRewriter")>]
    type HTMLRewriter =
        [<EmitConstructor>]
        abstract Create: unit -> HTMLRewriter

        abstract on: selector: string * handlers: HTMLRewriterElementContentHandlers -> HTMLRewriter
        abstract onDocument: handlers: HTMLRewriterDocumentContentHandlers -> HTMLRewriter
        abstract transform: response: Response -> Response

    [<Import("@cloudflare/workers-types", "MediaTransformationResult")>]
    type MediaTransformationResult =
        abstract media: unit -> Promise<ReadableStream<Uint8Array>>
        abstract response: unit -> Promise<Response>
        abstract contentType: unit -> Promise<string>

    [<Import("@cloudflare/workers-types", "R2ListOptions")>]
    type R2ListOptions =
        abstract include: option<ResizeArray<LiteralUnions.CustomMetadataHttpMetadata>> with get, set
        abstract startAfter: option<string> with get, set
        abstract delimiter: option<string> with get, set
        abstract cursor: option<string> with get, set
        abstract prefix: option<string> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunLinksOptions")>]
    type BrowserRunLinksOptions =
        abstract excludeExternalLinks: option<bool> with get, set
        abstract visibleLinksOnly: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "QuotaReachedError")>]
    type QuotaReachedError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "AiMultimodalEmbeddingsInput")>]
    type AiMultimodalEmbeddingsInput =
        abstract text: ResizeArray<string> with get, set
        abstract image: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type WorkflowDurationLabel =
        | [<CompiledName("second")>] Second
        | [<CompiledName("minute")>] Minute
        | [<CompiledName("hour")>] Hour
        | [<CompiledName("day")>] Day
        | [<CompiledName("week")>] Week
        | [<CompiledName("month")>] Month
        | [<CompiledName("year")>] Year

    [<Import("@cloudflare/workers-types", "UnderlyingByteSource")>]
    type UnderlyingByteSource =
        abstract autoAllocateChunkSize: option<float> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract start: controller: ReadableByteStreamController -> option<Promise<unit>>
        abstract pull: controller: ReadableByteStreamController -> option<Promise<unit>>
        abstract cancel: ?reason: obj -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "ArtifactsRepoInfo")>]
    type ArtifactsRepoInfo =
        abstract remote: string with get, set
        abstract readOnly: bool with get, set
        abstract source: option<string> with get, set
        abstract lastPushAt: option<string> with get, set
        abstract updatedAt: string with get, set
        abstract createdAt: string with get, set
        abstract defaultBranch: string with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Moonshotai_Kimi_K2_5")>]
    type BaseAiCfMoonshotaiKimiK25 =
        abstract postProcessedOutputs: ChatCompletionsOutput with get, set
        abstract inputs: ChatCompletionsInput with get, set

    [<Import("@cloudflare/workers-types", "GatewayRetries")>]
    type GatewayRetries =
        abstract backoff: option<CloudflareWorkersModule.WorkflowBackoff> with get, set
        abstract retryDelayMs: option<float> with get, set
        abstract maxAttempts: option<LiteralUnions.I1I2I3I4I5> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchChatCompletionsRequest")>]
    type AiSearchChatCompletionsRequest =
        [<EmitProperty("ai_search_options")>]
        abstract aiSearchOptions: option<AiSearchOptions> with get, set

        abstract stream: option<bool> with get, set
        abstract model: option<string> with get, set
        abstract messages: ResizeArray<AiSearchMessage> with get, set
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "AiModelsSearchObject")>]
    type AiModelsSearchObject =
        abstract properties: ResizeArray<SharedLiterals.PropertyIdValue> with get, set
        abstract tags: ResizeArray<string> with get, set
        abstract task: SharedLiterals.DescriptionIdName2 with get, set
        abstract description: string with get, set
        abstract name: string with get, set
        abstract source: float with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ImageDrawOptions")>]
    type ImageDrawOptions =
        abstract right: option<float> with get, set
        abstract bottom: option<float> with get, set
        abstract left: option<float> with get, set
        abstract top: option<float> with get, set
        abstract composite: option<ImageCompositeMode> with get, set
        abstract repeat: option<U2<bool, string>> with get, set
        abstract opacity: option<float> with get, set

    [<Import("@cloudflare/workers-types", "VectorizeVectorMetadataFilter")>]
    type VectorizeVectorMetadataFilter =
        abstract Item: field: string -> option<U5<SharedLiterals.EqGtGteLtLteNe, SharedLiterals.InNin, string, float, bool>>

    [<Import("@cloudflare/workers-types", "Params")>]
    type Params<'P> = interface end

    type AiSearchSearchRequest = U2<SharedLiterals.AiSearchOptionsMessagesQuery, SharedLiterals.AiSearchOptionsMessagesQuery2>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Unum_Uform_Gen2_Qwen_500M_Output")>]
    type AiCfUnumUformGen2Qwen500MOutput =
        abstract description: option<string> with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRagSearchRequest")>]
    type AutoRagSearchRequest =
        [<EmitProperty("rewrite_query")>]
        abstract rewriteQuery: option<bool> with get, set

        abstract reranking: option<SharedLiterals.EnabledModel> with get, set

        [<EmitProperty("ranking_options")>]
        abstract rankingOptions: option<SharedLiterals.RankerScoreThreshold> with get, set

        [<EmitProperty("max_num_results")>]
        abstract maxNumResults: option<float> with get, set

        abstract filters: option<U2<CompoundFilter, ComparisonFilter>> with get, set
        abstract query: string with get, set

    type AiCfBaaiBgeLargeEnV15Output = U2<SharedLiterals.DataPoolingShape, AiCfBaaiBgeLargeEnV15AsyncResponse>

    [<Import("@cloudflare/workers-types", "RequestInit")>]
    type RequestInit<'Cf> =
        abstract encodeResponseBody: option<LiteralUnions.AutomaticManual> with get, set
        abstract signal: option<AbortSignal> with get, set
        abstract integrity: option<string> with get, set
        abstract cache: option<LiteralUnions.NoCacheNoStore> with get, set
        abstract cf: option<'Cf> with get, set
        abstract fetcher: option<SharedLiterals.ConnectFetch> with get, set
        abstract redirect: option<string> with get, set
        abstract body: option<U9<ReadableStream<Uint8Array>, ArrayBuffer, obj, Blob, URLSearchParams, FormData, seq<BufferSource>, AsyncIterable<BufferSource>, string>> with get, set
        abstract headers: option<HeadersInit> with get, set
        abstract method: option<string> with get, set

    [<Import("@cloudflare/workers-types", "SupportedFileFormat")>]
    type SupportedFileFormat =
        abstract extension: string with get, set
        abstract mimeType: string with get, set

    [<Import("@cloudflare/workers-types", "WorkerGlobalScope")>]
    type WorkerGlobalScope =
        inherit EventTarget<WorkerGlobalScopeEventMap>

        [<EmitProperty("EventTarget")>]
        abstract eventTarget: WorkerGlobalScope.EventTarget with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Moonshotai_Kimi_K2_6")>]
    type BaseAiCfMoonshotaiKimiK26 =
        abstract postProcessedOutputs: ChatCompletionsOutput with get, set
        abstract inputs: ChatCompletionsInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Nova_3_Input")>]
    type AiCfDeepgramNova3Input =
        [<EmitProperty("utterance_end_ms")>]
        abstract utteranceEndMs: option<bool> with get, set

        [<EmitProperty("vad_events")>]
        abstract vadEvents: option<bool> with get, set

        abstract endpointing: option<string> with get, set

        [<EmitProperty("interim_results")>]
        abstract interimResults: option<bool> with get, set

        abstract channels: option<float> with get, set

        [<EmitProperty("utt_split")>]
        abstract uttSplit: option<float> with get, set

        abstract utterances: option<bool> with get, set
        abstract topics: option<bool> with get, set

        [<EmitProperty("smart_format")>]
        abstract smartFormat: option<bool> with get, set

        abstract sentiment: option<bool> with get, set
        abstract search: option<string> with get, set
        abstract replace: option<string> with get, set
        abstract redact: option<string> with get, set
        abstract punctuate: option<bool> with get, set

        [<EmitProperty("profanity_filter")>]
        abstract profanityFilter: option<bool> with get, set

        abstract paragraphs: option<bool> with get, set
        abstract numerals: option<bool> with get, set
        abstract multichannel: option<bool> with get, set
        abstract mode: option<LiteralUnions.FinanceGeneralMedical> with get, set

        [<EmitProperty("mip_opt_out")>]
        abstract mipOptOut: option<bool> with get, set

        abstract measurements: option<bool> with get, set
        abstract language: option<string> with get, set
        abstract keywords: option<string> with get, set
        abstract keyterm: option<string> with get, set

        [<EmitProperty("filler_words")>]
        abstract fillerWords: option<bool> with get, set

        abstract extra: option<string> with get, set
        abstract encoding: option<LiteralUnions.AmrNbAmrWbFlac807e0cb2> with get, set
        abstract dictation: option<bool> with get, set
        abstract diarize: option<bool> with get, set

        [<EmitProperty("detect_language")>]
        abstract detectLanguage: option<bool> with get, set

        [<EmitProperty("detect_entities")>]
        abstract detectEntities: option<bool> with get, set

        [<EmitProperty("custom_intent")>]
        abstract customIntent: option<string> with get, set

        [<EmitProperty("custom_intent_mode")>]
        abstract customIntentMode: option<LiteralUnions.ExtendedStrict> with get, set

        [<EmitProperty("custom_topic")>]
        abstract customTopic: option<string> with get, set

        [<EmitProperty("custom_topic_mode")>]
        abstract customTopicMode: option<LiteralUnions.ExtendedStrict> with get, set

        abstract audio: SharedLiterals.BodyContentType2 with get, set

    [<Import("@cloudflare/workers-types", "WebSearch")>]
    type WebSearch =
        abstract search: options: WebSearchSearchOptions -> Promise<WebSearchSearchResponse>

    [<Import("@cloudflare/workers-types", "ResponseFunctionCallOutputItemList")>]
    type ResponseFunctionCallOutputItemList = interface end

    [<Import("@cloudflare/workers-types", "EmailEvent")>]
    type EmailEvent =
        inherit ExtendableEvent
        abstract message: ForwardableEmailMessage with get

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Zai_Org_Glm_4_7_Flash")>]
    type BaseAiCfZaiOrgGlm47Flash =
        abstract postProcessedOutputs: ChatCompletionsOutput with get, set
        abstract inputs: ChatCompletionsInput with get, set

    type AiCfGoogleGemma312BItInput = U2<AiCfGoogleGemma312BItPrompt, AiCfGoogleGemma312BItMessages>

    [<Import("@cloudflare/workers-types", "AiTextEmbeddingsInput")>]
    type AiTextEmbeddingsInput =
        abstract text: U2<ResizeArray<string>, string> with get, set

    [<Import("@cloudflare/workers-types", "D1ExecResult")>]
    type D1ExecResult =
        abstract duration: float with get, set
        abstract count: float with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_AsyncResponse")>]
    type AiCfMetaLlama3370BInstructFp8FastAsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "TextEncoderEncodeIntoResult")>]
    type TextEncoderEncodeIntoResult =
        abstract written: float with get, set
        abstract read: float with get, set

    [<Import("@cloudflare/workers-types", "CloseEventInit")>]
    type CloseEventInit =
        abstract wasClean: option<bool> with get, set
        abstract reason: option<string> with get, set
        abstract code: option<float> with get, set

    type AiCfQwenQwq32BInput = U2<AiCfQwenQwq32BPrompt, AiCfQwenQwq32BMessages>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Output_Query")>]
    type AiCfBaaiBgeM3OutputQuery =
        abstract response: option<ResizeArray<SharedLiterals.IdScore>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Openai_Whisper_Tiny_En_Output")>]
    type AiCfOpenaiWhisperTinyEnOutput =
        abstract vtt: option<string> with get, set
        abstract words: option<ResizeArray<SharedLiterals.EndStartWord2>> with get, set

        [<EmitProperty("word_count")>]
        abstract wordCount: option<float> with get, set

        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Deepgram_Aura_1")>]
    type BaseAiCfDeepgramAura1 =
        abstract postProcessedOutputs: string with get, set
        abstract inputs: AiCfDeepgramAura1Input with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Ai4Bharat_Indictrans2_En_Indic_1B")>]
    type BaseAiCfAi4BharatIndictrans2EnIndic1B =
        abstract postProcessedOutputs: AiCfAi4BharatIndictrans2EnIndic1BOutput with get, set
        abstract inputs: AiCfAi4BharatIndictrans2EnIndic1BInput with get, set

    [<Import("@cloudflare/workers-types", "VectorizeQueryOptions")>]
    type VectorizeQueryOptions =
        abstract filter: option<VectorizeVectorMetadataFilter> with get, set
        abstract returnMetadata: option<U2<LiteralUnions.AllIndexedNone, bool>> with get, set
        abstract returnValues: option<bool> with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: option<string> with get, set

        abstract topK: option<float> with get, set

    [<Import("@cloudflare/workers-types", "JsonWebKeyWithKid")>]
    type JsonWebKeyWithKid =
        inherit JsonWebKey
        abstract kid: string with get

    [<Import("@cloudflare/workers-types", "CryptoKeyHmacKeyAlgorithm")>]
    type CryptoKeyHmacKeyAlgorithm =
        abstract length: float with get, set
        abstract hash: CryptoKeyKeyAlgorithm with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "WebSearchOptions")>]
    type WebSearchOptions =
        [<EmitProperty("user_location")>]
        abstract userLocation: option<WebSearchUserLocation> with get, set

        [<EmitProperty("search_context_size")>]
        abstract searchContextSize: option<AgentMemoryThinkingLevel> with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryRecallOptions")>]
    type AgentMemoryRecallOptions =
        abstract referenceDate: option<U2<Date, string>> with get, set
        abstract responseLength: option<AgentMemoryResponseLength> with get, set
        abstract thinkingLevel: option<AgentMemoryThinkingLevel> with get, set

    [<Import("@cloudflare/workers-types", "D1PreparedStatement")>]
    type D1PreparedStatement =
        abstract bind: [<ParamArray>] values: ResizeArray<option<obj>> -> D1PreparedStatement
        abstract first: colName: string -> Promise<option<obj>>
        abstract first: unit -> Promise<option<obj>>
        abstract run: unit -> Promise<D1PreparedStatement.Run>
        abstract all<'T> : unit -> Promise<obj>
        abstract raw: options: D1PreparedStatement.Raw.Options -> Promise<ResizeArray<string> * ResizeArray<obj>>
        abstract raw: ?options: D1PreparedStatement.Raw.Options.Case2 -> Promise<ResizeArray<obj>>

    [<Import("@cloudflare/workers-types", "ChatCompletionsResponseFormatText")>]
    type ChatCompletionsResponseFormatText =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "QueueSendBatchResponse")>]
    type QueueSendBatchResponse =
        abstract metadata: QueueSendBatchMetadata with get, set

    [<Import("@cloudflare/workers-types", "AiGatewayInternalError")>]
    type AiGatewayInternalError = interface end

    type AiCfPipecatAiSmartTurnV2Input = U2<SharedLiterals.AudioDtype, SharedLiterals.AudioDtype2>

    [<Import("@cloudflare/workers-types", "TraceItemHibernatableWebSocketEventInfoMessage")>]
    type TraceItemHibernatableWebSocketEventInfoMessage =
        abstract webSocketEventType: string with get

    [<Import("@cloudflare/workers-types", "BasicImageTransformations")>]
    type BasicImageTransformations =
        abstract upscale: option<LiteralUnions.GenerateInterpolate> with get, set
        abstract segment: option<string> with get, set
        abstract border: option<U2<BasicImageTransformations.Border, BasicImageTransformations.Border.Case2>> with get, set
        abstract dpr: option<float> with get, set
        abstract saturation: option<float> with get, set
        abstract gamma: option<float> with get, set
        abstract brightness: option<float> with get, set
        abstract contrast: option<float> with get, set
        abstract blur: option<float> with get, set
        abstract sharpen: option<float> with get, set
        abstract rotate: option<LiteralUnions.I0I180I270I360I90> with get, set
        abstract flip: option<LiteralUnions.HHvV> with get, set
        abstract background: option<string> with get, set
        abstract trim: option<U2<string, SharedLiterals.BorderBottomHeight5a030263>> with get, set
        abstract fit: option<LiteralUnions.ContainCoverCrop0093f896> with get, set
        abstract zoom: option<float> with get, set
        abstract gravity: option<U2<LiteralUnions.AutoBottomCenterB85bc937, BasicImageTransformationsGravityCoordinates>> with get, set
        abstract height: option<float> with get, set
        abstract width: option<float> with get, set

    [<Import("@cloudflare/workers-types", "FlagshipEvaluationContext")>]
    type FlagshipEvaluationContext = interface end

    [<Import("@cloudflare/workers-types", "ChatCompletionMessageCustomToolCall")>]
    type ChatCompletionMessageCustomToolCall =
        abstract custom: SharedLiterals.InputName with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ForwardableEmailMessage")>]
    type ForwardableEmailMessage =
        inherit EmailMessage
        abstract rawSize: float with get
        abstract headers: Headers with get
        abstract raw: ReadableStream<Uint8Array> with get
        abstract setReject: reason: string -> unit
        abstract forward: rcptTo: string * ?headers: Headers -> Promise<EmailSendResult>
        abstract reply: message: EmailMessage -> Promise<EmailSendResult>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Nova_3_Output")>]
    type AiCfDeepgramNova3Output =
        abstract results: option<AiCfDeepgramNova3Output.Results> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Messages_1")>]
    type AiCfAisingaporeGemmaSeaLionV427BItMessages1 =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfAisingaporeGemmaSeaLionV427BItJSONMode3> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRole> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Qwen_Qwen2_5_Coder_32B_Instruct")>]
    type BaseAiCfQwenQwen25Coder32BInstruct =
        abstract postProcessedOutputs: AiCfQwenQwen25Coder32BInstructOutput with get, set
        abstract inputs: AiCfQwenQwen25Coder32BInstructInput with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Deepgram_Flux")>]
    type BaseAiCfDeepgramFlux =
        abstract postProcessedOutputs: AiCfDeepgramFluxOutput with get, set
        abstract inputs: AiCfDeepgramFluxInput with get, set

    [<Import("@cloudflare/workers-types", "EndTag")>]
    type EndTag =
        abstract name: string with get, set
        abstract before: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> EndTag
        abstract after: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> EndTag
        abstract remove: unit -> EndTag

    [<Import("@cloudflare/workers-types", "ConversionOptions")>]
    type ConversionOptions =
        abstract pdf: option<SharedLiterals.ImagesMetadata> with get, set
        abstract image: option<ImageConversionOptions> with get, set
        abstract docx: option<SharedLiterals.Images> with get, set
        abstract html: option<SharedLiterals.CssSelectorHostnameImages> with get, set

    [<Import("@cloudflare/workers-types", "ScheduledController")>]
    type ScheduledController =
        abstract cron: string with get
        abstract scheduledTime: float with get
        abstract noRetry: unit -> unit

    [<Import("@cloudflare/workers-types", "Doctype")>]
    type Doctype =
        abstract systemId: option<string> with get
        abstract publicId: option<string> with get
        abstract name: option<string> with get

    [<Import("@cloudflare/workers-types", "AgentMemoryIncomingMemory")>]
    type AgentMemoryIncomingMemory =
        abstract sessionId: option<string> with get, set
        abstract content: string with get, set

    [<Import("@cloudflare/workers-types", "ConversionRequestOptions")>]
    type ConversionRequestOptions =
        abstract conversionOptions: option<ConversionOptions> with get, set
        abstract extraHeaders: option<obj> with get, set
        abstract gateway: option<GatewayOptions> with get, set

    [<Import("@cloudflare/workers-types", "URLPatternComponentResult")>]
    type URLPatternComponentResult =
        abstract groups: obj with get, set
        abstract input: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseContentReasoningText")>]
    type ResponseContentReasoningText =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "StreamScopedDownloads")>]
    type StreamScopedDownloads =
        abstract generate: ?downloadType: StreamDownloadType -> Promise<StreamDownloadGetResponse>
        abstract get: unit -> Promise<StreamDownloadGetResponse>
        abstract delete: ?downloadType: StreamDownloadType -> Promise<unit>

    [<Import("@cloudflare/workers-types", "VectorizeVectorMutation")>]
    type VectorizeVectorMutation =
        abstract count: float with get, set
        abstract ids: ResizeArray<string> with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRagAiSearchResponse")>]
    type AutoRagAiSearchResponse =
        abstract response: string with get, set

        [<EmitProperty("next_page")>]
        abstract nextPage: option<string> with get, set

        [<EmitProperty("has_more")>]
        abstract hasMore: bool with get, set

        abstract data: ResizeArray<SharedLiterals.AttributesContentFileF6de1a38> with get, set

        [<EmitProperty("search_query")>]
        abstract searchQuery: string with get, set

        abstract object: string with get, set

    [<Import("@cloudflare/workers-types", "UnderlyingSource")>]
    type UnderlyingSource<'R> =
        abstract expectedLength: option<float> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract start: controller: ReadableStreamDefaultController<'R> -> option<Promise<unit>>
        abstract pull: controller: ReadableStreamDefaultController<'R> -> option<Promise<unit>>
        abstract cancel: ?reason: obj -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "AiModels")>]
    type AiModels =
        [<EmitProperty("@cf/google/gemma-4-26b-a4b-it")>]
        abstract ``@cf/google/gemma426bA4bIt``: BaseAiCfGoogleGemma426BA4BIT with get, set

        [<EmitProperty("@cf/nvidia/nemotron-3-120b-a12b")>]
        abstract ``@cf/nvidia/nemotron3120bA12b``: BaseAiCfNvidiaNemotron3120BA12B with get, set

        [<EmitProperty("@cf/moonshotai/kimi-k2.6")>]
        abstract ``@cf/moonshotai/kimiK2.6``: BaseAiCfMoonshotaiKimiK26 with get, set

        [<EmitProperty("@cf/moonshotai/kimi-k2.5")>]
        abstract ``@cf/moonshotai/kimiK2.5``: BaseAiCfMoonshotaiKimiK25 with get, set

        [<EmitProperty("@cf/zai-org/glm-4.7-flash")>]
        abstract ``@cf/zaiOrg/glm4.7Flash``: BaseAiCfZaiOrgGlm47Flash with get, set

        [<EmitProperty("@cf/black-forest-labs/flux-2-klein-9b")>]
        abstract ``@cf/blackForestLabs/flux2Klein9b``: BaseAiCfBlackForestLabsFlux2Klein9B with get, set

        [<EmitProperty("@cf/black-forest-labs/flux-2-klein-4b")>]
        abstract ``@cf/blackForestLabs/flux2Klein4b``: BaseAiCfBlackForestLabsFlux2Klein4B with get, set

        [<EmitProperty("@cf/black-forest-labs/flux-2-dev")>]
        abstract ``@cf/blackForestLabs/flux2Dev``: BaseAiCfBlackForestLabsFlux2Dev with get, set

        [<EmitProperty("@cf/deepgram/aura-2-es")>]
        abstract ``@cf/deepgram/aura2Es``: BaseAiCfDeepgramAura2Es with get, set

        [<EmitProperty("@cf/deepgram/aura-2-en")>]
        abstract ``@cf/deepgram/aura2En``: BaseAiCfDeepgramAura2En with get, set

        [<EmitProperty("@cf/deepgram/flux")>]
        abstract ``@cf/deepgram/flux``: BaseAiCfDeepgramFlux with get, set

        [<EmitProperty("@cf/pfnet/plamo-embedding-1b")>]
        abstract ``@cf/pfnet/plamoEmbedding1b``: BaseAiCfPfnetPlamoEmbedding1B with get, set

        [<EmitProperty("@cf/aisingapore/gemma-sea-lion-v4-27b-it")>]
        abstract ``@cf/aisingapore/gemmaSeaLionV427bIt``: BaseAiCfAisingaporeGemmaSeaLionV427BIt with get, set

        [<EmitProperty("@cf/ai4bharat/indictrans2-en-indic-1B")>]
        abstract ``@cf/ai4bharat/indictrans2EnIndic1B``: BaseAiCfAi4BharatIndictrans2EnIndic1B with get, set

        [<EmitProperty("@cf/deepgram/aura-1")>]
        abstract ``@cf/deepgram/aura1``: BaseAiCfDeepgramAura1 with get, set

        [<EmitProperty("@cf/leonardo/lucid-origin")>]
        abstract ``@cf/leonardo/lucidOrigin``: BaseAiCfLeonardoLucidOrigin with get, set

        [<EmitProperty("@cf/leonardo/phoenix-1.0")>]
        abstract ``@cf/leonardo/phoenix1.0``: BaseAiCfLeonardoPhoenix10 with get, set

        [<EmitProperty("@cf/openai/gpt-oss-20b")>]
        abstract ``@cf/openai/gptOss20b``: BaseAiCfOpenaiGptOss20B with get, set

        [<EmitProperty("@cf/openai/gpt-oss-120b")>]
        abstract ``@cf/openai/gptOss120b``: BaseAiCfOpenaiGptOss120B with get, set

        [<EmitProperty("@cf/pipecat-ai/smart-turn-v2")>]
        abstract ``@cf/pipecatAi/smartTurnV2``: BaseAiCfPipecatAiSmartTurnV2 with get, set

        [<EmitProperty("@cf/qwen/qwen3-embedding-0.6b")>]
        abstract ``@cf/qwen/qwen3Embedding0.6b``: BaseAiCfQwenQwen3Embedding06B with get, set

        [<EmitProperty("@cf/deepgram/nova-3")>]
        abstract ``@cf/deepgram/nova3``: BaseAiCfDeepgramNova3 with get, set

        [<EmitProperty("@cf/qwen/qwen3-30b-a3b-fp8")>]
        abstract ``@cf/qwen/qwen330bA3bFp8``: BaseAiCfQwenQwen330BA3BFp8 with get, set

        [<EmitProperty("@cf/meta/llama-4-scout-17b-16e-instruct")>]
        abstract ``@cf/meta/llama4Scout17b16eInstruct``: BaseAiCfMetaLlama4Scout17B16EInstruct with get, set

        [<EmitProperty("@cf/google/gemma-3-12b-it")>]
        abstract ``@cf/google/gemma312bIt``: BaseAiCfGoogleGemma312BIt with get, set

        [<EmitProperty("@cf/mistralai/mistral-small-3.1-24b-instruct")>]
        abstract ``@cf/mistralai/mistralSmall3.124bInstruct``: BaseAiCfMistralaiMistralSmall3124BInstruct with get, set

        [<EmitProperty("@cf/qwen/qwq-32b")>]
        abstract ``@cf/qwen/qwq32b``: BaseAiCfQwenQwq32B with get, set

        [<EmitProperty("@cf/qwen/qwen2.5-coder-32b-instruct")>]
        abstract ``@cf/qwen/qwen2.5Coder32bInstruct``: BaseAiCfQwenQwen25Coder32BInstruct with get, set

        [<EmitProperty("@cf/baai/bge-reranker-base")>]
        abstract ``@cf/baai/bgeRerankerBase``: BaseAiCfBaaiBgeRerankerBase with get, set

        [<EmitProperty("@cf/meta/llama-guard-3-8b")>]
        abstract ``@cf/meta/llamaGuard38b``: BaseAiCfMetaLlamaGuard38B with get, set

        [<EmitProperty("@cf/meta/llama-3.3-70b-instruct-fp8-fast")>]
        abstract ``@cf/meta/llama3.370bInstructFp8Fast``: BaseAiCfMetaLlama3370BInstructFp8Fast with get, set

        [<EmitProperty("@cf/meta/llama-3.2-11b-vision-instruct")>]
        abstract ``@cf/meta/llama3.211bVisionInstruct``: BaseAiCfMetaLlama3211BVisionInstruct with get, set

        [<EmitProperty("@cf/black-forest-labs/flux-1-schnell")>]
        abstract ``@cf/blackForestLabs/flux1Schnell``: BaseAiCfBlackForestLabsFlux1Schnell with get, set

        [<EmitProperty("@cf/baai/bge-m3")>]
        abstract ``@cf/baai/bgeM3``: BaseAiCfBaaiBgeM3 with get, set

        [<EmitProperty("@cf/openai/whisper-large-v3-turbo")>]
        abstract ``@cf/openai/whisperLargeV3Turbo``: BaseAiCfOpenaiWhisperLargeV3Turbo with get, set

        [<EmitProperty("@cf/openai/whisper-tiny-en")>]
        abstract ``@cf/openai/whisperTinyEn``: BaseAiCfOpenaiWhisperTinyEn with get, set

        [<EmitProperty("@cf/unum/uform-gen2-qwen-500m")>]
        abstract ``@cf/unum/uformGen2Qwen500m``: BaseAiCfUnumUformGen2Qwen500M with get, set

        [<EmitProperty("@cf/baai/bge-large-en-v1.5")>]
        abstract ``@cf/baai/bgeLargeEnV1.5``: BaseAiCfBaaiBgeLargeEnV15 with get, set

        [<EmitProperty("@cf/baai/bge-small-en-v1.5")>]
        abstract ``@cf/baai/bgeSmallEnV1.5``: BaseAiCfBaaiBgeSmallEnV15 with get, set

        [<EmitProperty("@cf/meta/m2m100-1.2b")>]
        abstract ``@cf/meta/m2m1001.2b``: BaseAiCfMetaM2M10012B with get, set

        [<EmitProperty("@cf/openai/whisper")>]
        abstract ``@cf/openai/whisper``: BaseAiCfOpenaiWhisper with get, set

        [<EmitProperty("@cf/baai/bge-base-en-v1.5")>]
        abstract ``@cf/baai/bgeBaseEnV1.5``: BaseAiCfBaaiBgeBaseEnV15 with get, set

        [<EmitProperty("@cf/llava-hf/llava-1.5-7b-hf")>]
        abstract ``@cf/llavaHf/llava1.57bHf``: BaseAiImageToText with get, set

        [<EmitProperty("@cf/facebook/bart-large-cnn")>]
        abstract ``@cf/facebook/bartLargeCnn``: BaseAiSummarization with get, set

        [<EmitProperty("@cf/ibm-granite/granite-4.0-h-micro")>]
        abstract ``@cf/ibmGranite/granite4.0HMicro``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/deepseek-ai/deepseek-r1-distill-qwen-32b")>]
        abstract ``@cf/deepseekAi/deepseekR1DistillQwen32b``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-3.2-1b-instruct")>]
        abstract ``@cf/meta/llama3.21bInstruct``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-3.2-3b-instruct")>]
        abstract ``@cf/meta/llama3.23bInstruct``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-3.1-8b-instruct-awq")>]
        abstract ``@cf/meta/llama3.18bInstructAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-3.1-8b-instruct-fp8")>]
        abstract ``@cf/meta/llama3.18bInstructFp8``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-3-8b-instruct-awq")>]
        abstract ``@cf/meta/llama38bInstructAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/fblgit/una-cybertron-7b-v2-bf16")>]
        abstract ``@cf/fblgit/unaCybertron7bV2Bf16``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-3-8b-instruct")>]
        abstract ``@cf/meta/llama38bInstruct``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/mistral/mistral-7b-instruct-v0.2")>]
        abstract ``@hf/mistral/mistral7bInstructV0.2``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/google/gemma-7b-it-lora")>]
        abstract ``@cf/google/gemma7bItLora``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/google/gemma-2b-it-lora")>]
        abstract ``@cf/google/gemma2bItLora``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta-llama/llama-2-7b-chat-hf-lora")>]
        abstract ``@cf/metaLlama/llama27bChatHfLora``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/google/gemma-7b-it")>]
        abstract ``@hf/google/gemma7bIt``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/nexusflow/starling-lm-7b-beta")>]
        abstract ``@hf/nexusflow/starlingLm7bBeta``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/nousresearch/hermes-2-pro-mistral-7b")>]
        abstract ``@hf/nousresearch/hermes2ProMistral7b``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/mistral/mistral-7b-instruct-v0.2-lora")>]
        abstract ``@cf/mistral/mistral7bInstructV0.2Lora``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/qwen/qwen1.5-1.8b-chat")>]
        abstract ``@cf/qwen/qwen1.51.8bChat``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/microsoft/phi-2")>]
        abstract ``@cf/microsoft/phi2``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/tinyllama/tinyllama-1.1b-chat-v1.0")>]
        abstract ``@cf/tinyllama/tinyllama1.1bChatV1.0``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/qwen/qwen1.5-14b-chat-awq")>]
        abstract ``@cf/qwen/qwen1.514bChatAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/qwen/qwen1.5-7b-chat-awq")>]
        abstract ``@cf/qwen/qwen1.57bChatAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/qwen/qwen1.5-0.5b-chat")>]
        abstract ``@cf/qwen/qwen1.50.5bChat``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/thebloke/discolm-german-7b-v1-awq")>]
        abstract ``@cf/thebloke/discolmGerman7bV1Awq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/tiiuae/falcon-7b-instruct")>]
        abstract ``@cf/tiiuae/falcon7bInstruct``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/openchat/openchat-3.5-0106")>]
        abstract ``@cf/openchat/openchat3.50106``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/defog/sqlcoder-7b-2")>]
        abstract ``@cf/defog/sqlcoder7b2``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/deepseek-ai/deepseek-math-7b-instruct")>]
        abstract ``@cf/deepseekAi/deepseekMath7bInstruct``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/deepseek-coder-6.7b-instruct-awq")>]
        abstract ``@hf/thebloke/deepseekCoder6.7bInstructAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/deepseek-coder-6.7b-base-awq")>]
        abstract ``@hf/thebloke/deepseekCoder6.7bBaseAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/neural-chat-7b-v3-1-awq")>]
        abstract ``@hf/thebloke/neuralChat7bV31Awq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/openhermes-2.5-mistral-7b-awq")>]
        abstract ``@hf/thebloke/openhermes2.5Mistral7bAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/zephyr-7b-beta-awq")>]
        abstract ``@hf/thebloke/zephyr7bBetaAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/mistral-7b-instruct-v0.1-awq")>]
        abstract ``@hf/thebloke/mistral7bInstructV0.1Awq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@hf/thebloke/llama-2-13b-chat-awq")>]
        abstract ``@hf/thebloke/llama213bChatAwq``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-2-7b-chat-fp16")>]
        abstract ``@cf/meta/llama27bChatFp16``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/mistral/mistral-7b-instruct-v0.1")>]
        abstract ``@cf/mistral/mistral7bInstructV0.1``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/meta/llama-2-7b-chat-int8")>]
        abstract ``@cf/meta/llama27bChatInt8``: BaseAiTextGeneration with get, set

        [<EmitProperty("@cf/microsoft/resnet-50")>]
        abstract ``@cf/microsoft/resnet50``: BaseAiImageClassification with get, set

        [<EmitProperty("@cf/google/embeddinggemma-300m")>]
        abstract ``@cf/google/embeddinggemma300m``: BaseAiTextEmbeddings with get, set

        [<EmitProperty("@cf/myshell-ai/melotts")>]
        abstract ``@cf/myshellAi/melotts``: BaseAiTextToSpeech with get, set

        [<EmitProperty("@cf/bytedance/stable-diffusion-xl-lightning")>]
        abstract ``@cf/bytedance/stableDiffusionXlLightning``: BaseAiTextToImage with get, set

        [<EmitProperty("@cf/lykon/dreamshaper-8-lcm")>]
        abstract ``@cf/lykon/dreamshaper8Lcm``: BaseAiTextToImage with get, set

        [<EmitProperty("@cf/runwayml/stable-diffusion-v1-5-img2img")>]
        abstract ``@cf/runwayml/stableDiffusionV15Img2img``: BaseAiTextToImage with get, set

        [<EmitProperty("@cf/runwayml/stable-diffusion-v1-5-inpainting")>]
        abstract ``@cf/runwayml/stableDiffusionV15Inpainting``: BaseAiTextToImage with get, set

        [<EmitProperty("@cf/stabilityai/stable-diffusion-xl-base-1.0")>]
        abstract ``@cf/stabilityai/stableDiffusionXlBase1.0``: BaseAiTextToImage with get, set

        [<EmitProperty("@cf/huggingface/distilbert-sst-2-int8")>]
        abstract ``@cf/huggingface/distilbertSst2Int8``: BaseAiTextClassification with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunPuppeteerScreenshotOptions")>]
    type BrowserRunPuppeteerScreenshotOptions =
        abstract fromSurface: option<bool> with get, set
        abstract captureBeyondViewport: option<bool> with get, set
        abstract optimizeForSpeed: option<bool> with get, set
        abstract omitBackground: option<bool> with get, set
        abstract clip: option<SharedLiterals.HeightScaleWidthXY> with get, set
        abstract fullPage: option<bool> with get, set
        abstract quality: option<float> with get, set
        abstract encoding: option<LiteralUnions.Base64Binary> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JpegPngWebp> with get, set

    [<Import("@cloudflare/workers-types", "URLPatternOptions")>]
    type URLPatternOptions =
        abstract ignoreCase: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "TraceItemFetchEventInfo")>]
    type TraceItemFetchEventInfo =
        abstract request: TraceItemFetchEventInfoRequest with get
        abstract response: option<TraceItemFetchEventInfoResponse> with get

    [<Import("@cloudflare/workers-types", "SubtleCryptoImportKeyAlgorithm")>]
    type SubtleCryptoImportKeyAlgorithm =
        abstract compressed: option<bool> with get, set
        abstract namedCurve: option<string> with get, set
        abstract length: option<float> with get, set
        abstract hash: option<U2<SubtleCryptoHashAlgorithm, string>> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "InferenceUpstreamError")>]
    type InferenceUpstreamError = interface end

    [<Import("@cloudflare/workers-types", "ResponsesOutput")>]
    type ResponsesOutput =
        abstract usage: option<ResponseUsage> with get, set
        abstract truncation: option<LiteralUnions.AutoDisabled> with get, set
        abstract text: option<ResponseTextConfig> with get, set
        abstract status: option<ResponseStatus> with get, set

        [<EmitProperty("service_tier")>]
        abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

        [<EmitProperty("safety_identifier")>]
        abstract safetyIdentifier: option<string> with get, set

        abstract reasoning: option<Reasoning> with get, set
        abstract prompt: option<ResponsePrompt> with get, set

        [<EmitProperty("previous_response_id")>]
        abstract previousResponseId: option<string> with get, set

        [<EmitProperty("max_output_tokens")>]
        abstract maxOutputTokens: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract tools: option<ResizeArray<Tool>> with get, set

        [<EmitProperty("tool_choice")>]
        abstract toolChoice: option<U2<string, ToolChoiceFunction>> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("parallel_tool_calls")>]
        abstract parallelToolCalls: option<bool> with get, set

        abstract output: option<ResizeArray<ResponseOutputItem>> with get, set
        abstract object: option<string> with get, set
        abstract instructions: option<U2<ResizeArray<ResponseInputItem>, string>> with get, set

        [<EmitProperty("incomplete_details")>]
        abstract incompleteDetails: option<ResponseIncompleteDetails> with get, set

        abstract error: option<ResponseError> with get, set

        [<EmitProperty("output_text")>]
        abstract outputText: option<string> with get, set

        [<EmitProperty("created_at")>]
        abstract createdAt: option<float> with get, set

        abstract id: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ImageList")>]
    type ImageList =
        abstract listComplete: bool with get, set
        abstract cursor: option<string> with get, set
        abstract images: ResizeArray<ImageMetadata> with get, set

    [<Import("@cloudflare/workers-types", "AbortSignal")>]
    type AbortSignal =
        inherit EventTarget<obj>
        abstract onabort: option<obj> with get
        abstract reason: option<obj> with get
        abstract aborted: bool with get
        abstract abort: ?reason: obj -> AbortSignal
        abstract timeout: delay: float -> AbortSignal
        abstract any: signals: ResizeArray<AbortSignal> -> AbortSignal
        abstract throwIfAborted: unit -> unit

    [<Import("@cloudflare/workers-types", "ReadableByteStreamController")>]
    type ReadableByteStreamController =
        abstract desiredSize: option<float> with get
        abstract byobRequest: option<ReadableStreamBYOBRequest> with get
        abstract close: unit -> unit
        abstract enqueue: chunk: BufferSource -> unit
        abstract error: ?reason: obj -> unit

    [<Import("@cloudflare/workers-types", "IncomingRequestCfProperties")>]
    type IncomingRequestCfProperties<'HostMetadata> =
        abstract tlsClientAuth: U2<IncomingRequestCfPropertiesTLSClientAuth, IncomingRequestCfPropertiesTLSClientAuthPlaceholder> with get, set
        /// <example>
        /// "635"
        /// </example>
        abstract metroCode: option<string> with get, set
        /// <example>
        /// "TX"
        /// </example>
        abstract regionCode: option<string> with get, set
        /// <example>
        /// "Texas"
        /// </example>
        abstract region: option<string> with get, set
        /// <example>
        /// "America/Chicago"
        /// </example>
        abstract timezone: option<string> with get, set
        /// <example>
        /// "-97.74260"
        /// </example>
        abstract longitude: option<string> with get, set
        /// <example>
        /// "30.27130"
        /// </example>
        abstract latitude: option<string> with get, set
        /// <example>
        /// "78701"
        /// </example>
        abstract postalCode: option<string> with get, set
        /// <example>
        /// "Austin"
        /// </example>
        abstract city: option<string> with get, set
        /// <example>
        /// "AN"
        /// </example>
        abstract continent: option<ContinentCode> with get, set
        /// <example>
        /// "1"
        /// </example>
        abstract isEUCountry: option<string> with get, set
        /// <example>
        /// "GB"
        /// </example>
        abstract country: option<LiteralUnions.ADAEAFAda77976> with get, set
        abstract hostMetadata: option<'HostMetadata> with get, set
        /// <deprecated />
        abstract clientTrustScore: float with get, set
        abstract botManagement: IncomingRequestCfProperties.BotManagement with get, set
        abstract tlsExportedAuthenticator: option<IncomingRequestCfPropertiesExportedAuthenticatorMetadata> with get, set
        /// <example>
        /// "AEAD-AES128-GCM-SHA256"
        /// </example>
        abstract tlsCipher: string with get, set
        /// <example>
        /// "TLSv1.3"
        /// </example>
        abstract tlsVersion: string with get, set
        /// <example>
        /// "weight=192;exclusive=0;group=3;group-weight=127"
        /// </example>
        abstract requestPriority: string with get, set
        /// <example>
        /// "HTTP/2"
        /// </example>
        abstract httpProtocol: string with get, set
        /// <example>
        /// 3
        /// </example>
        abstract edgeRequestKeepAliveStatus: IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus with get, set
        /// <example>
        /// "DFW"
        /// </example>
        abstract colo: string with get, set
        /// <example>
        /// 22
        /// </example>
        abstract clientTcpRtt: option<float> with get, set
        /// <example>
        /// "gzip, deflate, br"
        /// </example>
        abstract clientAcceptEncoding: option<string> with get, set
        /// <example>
        /// "Google Cloud"
        /// </example>
        abstract asOrganization: option<string> with get, set
        /// <example>
        /// 395747
        /// </example>
        abstract asn: option<float> with get, set
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_2_11B_Vision_Instruct_Output")>]
    type AiCfMetaLlama3211BVisionInstructOutput =
        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

        abstract response: option<string> with get, set

    [<Import("@cloudflare/workers-types", "TextEncoderStream")>]
    type TextEncoderStream =
        [<EmitConstructor>]
        abstract Create: unit -> TextEncoderStream

        inherit TransformStream<string, Uint8Array>
        abstract encoding: string with get

    [<Import("@cloudflare/workers-types", "AiSummarizationInput")>]
    type AiSummarizationInput =
        [<EmitProperty("max_length")>]
        abstract maxLength: option<float> with get, set

        [<EmitProperty("input_text")>]
        abstract inputText: string with get, set

    [<Import("@cloudflare/workers-types", "RateLimitOutcome")>]
    type RateLimitOutcome =
        abstract success: bool with get, set

    type RpcStub =
        abstract Create: value: obj -> obj

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AIGatewayProviders =
        | [<CompiledName("workers-ai")>] WorkersAi
        | [<CompiledName("anthropic")>] Anthropic
        | [<CompiledName("aws-bedrock")>] AwsBedrock
        | [<CompiledName("azure-openai")>] AzureOpenai
        | [<CompiledName("google-vertex-ai")>] GoogleVertexAi
        | [<CompiledName("huggingface")>] Huggingface
        | [<CompiledName("openai")>] Openai
        | [<CompiledName("perplexity-ai")>] PerplexityAi
        | [<CompiledName("replicate")>] Replicate
        | [<CompiledName("groq")>] Groq
        | [<CompiledName("cohere")>] Cohere
        | [<CompiledName("google-ai-studio")>] GoogleAiStudio
        | [<CompiledName("mistral")>] Mistral
        | [<CompiledName("grok")>] Grok
        | [<CompiledName("openrouter")>] Openrouter
        | [<CompiledName("deepseek")>] Deepseek
        | [<CompiledName("cerebras")>] Cerebras
        | [<CompiledName("cartesia")>] Cartesia
        | [<CompiledName("elevenlabs")>] Elevenlabs
        | [<CompiledName("adobe-firefly")>] AdobeFirefly

    [<Import("@cloudflare/workers-types", "ResponseInputItemFunctionCallOutput")>]
    type ResponseInputItemFunctionCallOutput =
        abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
        abstract id: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract output: U2<ResponseFunctionCallOutputItemList, string> with get, set

        [<EmitProperty("call_id")>]
        abstract callId: string with get, set

    [<Import("@cloudflare/workers-types", "DynamicDispatchLimits")>]
    type DynamicDispatchLimits =
        abstract subRequests: option<float> with get, set
        abstract cpuMs: option<float> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunResponseMeta")>]
    type BrowserRunResponseMeta =
        abstract title: string with get, set
        abstract status: float with get, set

    type AiCfMetaLlama4Scout17B16EInstructInput = U3<AiCfMetaLlama4Scout17B16EInstructPrompt, AiCfMetaLlama4Scout17B16EInstructMessages, AiCfMetaLlama4Scout17B16EInstructAsyncBatch>

    [<Import("@cloudflare/workers-types", "AiObjectDetectionInput")>]
    type AiObjectDetectionInput =
        abstract image: AiSentenceSimilarityOutput with get, set

    [<Import("@cloudflare/workers-types", "StreamPublicDetails")>]
    type StreamPublicDetails =
        abstract logo: option<string> with get, set

        [<EmitProperty("channel_link")>]
        abstract channelLink: option<string> with get, set

        [<EmitProperty("share_link")>]
        abstract shareLink: option<string> with get, set

        abstract title: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Pfnet_Plamo_Embedding_1B_Output")>]
    type AiCfPfnetPlamoEmbedding1BOutput =
        abstract shape: float * float with get, set
        abstract data: ResizeArray<AiSentenceSimilarityOutput> with get, set

    [<Import("@cloudflare/workers-types", "ImageListOptions")>]
    type ImageListOptions =
        abstract creator: option<string> with get, set
        abstract sortOrder: option<LiteralUnions.AscDesc> with get, set
        abstract cursor: option<string> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "ComparisonFilter")>]
    type ComparisonFilter =
        abstract value: U3<bool, float, string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: LiteralUnions.EqGtGteLtLteNe with get, set

        abstract key: string with get, set

    [<Import("@cloudflare/workers-types", "BrowserRun")>]
    type BrowserRun =
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunScreenshotOptions -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunPDFOptions -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunMarkdownOptions -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunScrapeOptions -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunLinksOptions -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunSnapshotOptions -> Promise<Response>
        abstract quickAction: action: string * options: BrowserRunJsonOptions -> Promise<Response>

    [<Import("@cloudflare/workers-types", "SocketOptions")>]
    type SocketOptions =
        abstract highWaterMark: option<float> with get, set
        abstract allowHalfOpen: bool with get, set
        abstract secureTransport: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AiImageClassificationInput")>]
    type AiImageClassificationInput =
        abstract image: AiSentenceSimilarityOutput with get, set

    [<Import("@cloudflare/workers-types", "QueueSendBatchMetrics")>]
    type QueueSendBatchMetrics =
        abstract oldestMessageTimestamp: option<Date> with get, set
        abstract backlogBytes: float with get, set
        abstract backlogCount: float with get, set

    type ExportedHandlerTestHandler = TestController -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_JSON_Mode_2")>]
    type AiCfMetaLlama3370BInstructFp8FastJSONMode2 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "TestController")>]
    type TestController = interface end

    [<Import("@cloudflare/workers-types", "Flagship")>]
    type Flagship =
        abstract get: flagKey: string * ?defaultValue: obj * ?context: FlagshipEvaluationContext -> Promise<option<obj>>
        abstract getBooleanValue: flagKey: string * defaultValue: bool * ?context: FlagshipEvaluationContext -> Promise<bool>
        abstract getStringValue: flagKey: string * defaultValue: string * ?context: FlagshipEvaluationContext -> Promise<string>
        abstract getNumberValue: flagKey: string * defaultValue: float * ?context: FlagshipEvaluationContext -> Promise<float>
        abstract getObjectValue: flagKey: string * defaultValue: obj * ?context: FlagshipEvaluationContext -> Promise<obj>
        abstract getBooleanDetails: flagKey: string * defaultValue: bool * ?context: FlagshipEvaluationContext -> Promise<FlagshipEvaluationDetails<bool>>
        abstract getStringDetails: flagKey: string * defaultValue: string * ?context: FlagshipEvaluationContext -> Promise<FlagshipEvaluationDetails<string>>
        abstract getNumberDetails: flagKey: string * defaultValue: float * ?context: FlagshipEvaluationContext -> Promise<FlagshipEvaluationDetails<float>>
        abstract getObjectDetails: flagKey: string * defaultValue: obj * ?context: FlagshipEvaluationContext -> Promise<FlagshipEvaluationDetails<obj>>

    type SetInterval =
        abstract Invoke: callback: (ResizeArray<option<obj>> -> unit) * ?msDelay: float -> float
        abstract Invoke: callback: (obj -> unit) * msDelay: float * [<ParamArray>] args: obj -> float

    [<Import("@cloudflare/workers-types", "CachePurgeError")>]
    type CachePurgeError =
        abstract message: string with get, set
        abstract code: float with get, set

    [<Import("@cloudflare/workers-types", "CfProperties")>]
    type CfProperties<'HostMetadata> = interface end

    [<Import("@cloudflare/workers-types", "Ai_Cf_Leonardo_Lucid_Origin_Output")>]
    type AiCfLeonardoLucidOriginOutput =
        abstract image: option<string> with get, set

    [<Import("@cloudflare/workers-types", "RsaOtherPrimesInfo")>]
    type RsaOtherPrimesInfo =
        abstract t: option<string> with get, set
        abstract d: option<string> with get, set
        abstract r: option<string> with get, set

    [<Import("@cloudflare/workers-types", "MediaError")>]
    type MediaError =
        abstract stack: option<string> with get
        abstract message: string with get
        abstract code: float with get

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesBotManagement")>]
    type IncomingRequestCfPropertiesBotManagement =
        /// <deprecated />
        abstract clientTrustScore: float with get, set
        abstract botManagement: IncomingRequestCfPropertiesBotManagementBase with get, set

    type AiCfBaaiBgeBaseEnV15Output = U2<SharedLiterals.DataPoolingShape, AiCfBaaiBgeBaseEnV15AsyncResponse>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwq_32B_Prompt")>]
    type AiCfQwenQwq32BPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "KVNamespaceGetWithMetadataResult")>]
    type KVNamespaceGetWithMetadataResult<'Value, 'Metadata> =
        abstract cacheStatus: option<string> with get, set
        abstract metadata: option<'Metadata> with get, set
        abstract value: option<'Value> with get, set

    [<Import("@cloudflare/workers-types", "ResponseTextDeltaEvent")>]
    type ResponseTextDeltaEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        abstract logprobs: ResizeArray<Logprob> with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        abstract delta: string with get, set

        [<EmitProperty("content_index")>]
        abstract contentIndex: float with get, set

    [<Import("@cloudflare/workers-types", "AiTextToSpeechInput")>]
    type AiTextToSpeechInput =
        abstract lang: option<string> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "DurableObjectNamespaceNewUniqueIdOptions")>]
    type DurableObjectNamespaceNewUniqueIdOptions =
        abstract jurisdiction: option<DurableObjectJurisdiction> with get, set

    [<Import("@cloudflare/workers-types", "R2Objects")>]
    type R2Objects =
        abstract delimitedPrefixes: ResizeArray<string> with get, set
        abstract objects: ResizeArray<R2Object> with get, set

    [<Import("@cloudflare/workers-types", "EventTargetHandlerObject")>]
    type EventTargetHandlerObject =
        abstract handleEvent: event: Event -> option<obj>

    [<Import("@cloudflare/workers-types", "ContainerDirectorySnapshot")>]
    type ContainerDirectorySnapshot =
        abstract name: option<string> with get, set
        abstract dir: string with get, set
        abstract size: float with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_JSON_Mode")>]
    type AiCfMetaLlama4Scout17B16EInstructJSONMode =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "AssistantMessageContentPart")>]
    type AssistantMessageContentPart =
        abstract refusal: option<string> with get, set
        abstract text: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: LiteralUnions.RefusalText with get, set

    [<Import("@cloudflare/workers-types", "ResponseOutputItemAddedEvent")>]
    type ResponseOutputItemAddedEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        abstract item: ResponseOutputItem with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type VectorizeDistanceMetric =
        | [<CompiledName("euclidean")>] Euclidean
        | [<CompiledName("cosine")>] Cosine
        | [<CompiledName("dot-product")>] DotProduct

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CertVerificationStatus =
        | FAILED
        | SUCCESS
        | NONE
        | [<CompiledName("FAILED:self signed certificate")>] ``FAILED:self signed certificate``
        | [<CompiledName("FAILED:unable to verify the first certificate")>] ``FAILED:unable to verify the first certificate``
        | [<CompiledName("FAILED:certificate is not yet valid")>] ``FAILED:certificate is not yet valid``
        | [<CompiledName("FAILED:certificate has expired")>] ``FAILED:certificate has expired``

    [<Import("@cloudflare/workers-types", "BaseAiAutomaticSpeechRecognition")>]
    type BaseAiAutomaticSpeechRecognition =
        abstract postProcessedOutputs: AiAutomaticSpeechRecognitionOutput with get, set
        abstract inputs: AiAutomaticSpeechRecognitionInput with get, set

    [<Import("@cloudflare/workers-types", "ResponseRefusalDoneEvent")>]
    type ResponseRefusalDoneEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        abstract refusal: string with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        [<EmitProperty("content_index")>]
        abstract contentIndex: float with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesImageMinify")>]
    type RequestInitCfPropertiesImageMinify =
        abstract html: option<bool> with get, set
        abstract css: option<bool> with get, set
        abstract javascript: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionResponseMessage")>]
    type ChatCompletionResponseMessage =
        [<EmitProperty("function_call")>]
        abstract functionCall: option<SharedLiterals.ArgumentsName> with get, set

        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<ChatCompletionMessageToolCall>> with get, set

        abstract audio: option<ChatCompletionAudio> with get, set
        abstract annotations: option<ResizeArray<ChatCompletionUrlCitation>> with get, set
        abstract refusal: option<string> with get, set
        abstract content: option<string> with get, set
        abstract role: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ReasoningEffort =
        | [<CompiledName("minimal")>] Minimal
        | [<CompiledName("low")>] Low
        | [<CompiledName("medium")>] Medium
        | [<CompiledName("high")>] High

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Meta_Llama_Guard_3_8B")>]
    type BaseAiCfMetaLlamaGuard38B =
        abstract postProcessedOutputs: AiCfMetaLlamaGuard38BOutput with get, set
        abstract inputs: AiCfMetaLlamaGuard38BInput with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AgentMemoryResponseLength =
        | [<CompiledName("medium")>] Medium
        | [<CompiledName("short")>] Short
        | [<CompiledName("long")>] Long

    [<Import("@cloudflare/workers-types", "AiSearchMultiSearchChunk")>]
    type AiSearchMultiSearchChunk =
        [<EmitProperty("instance_id")>]
        abstract instanceId: string with get, set

        [<EmitProperty("scoring_details")>]
        abstract scoringDetails: option<SharedLiterals.FusionMethodKeyword6c9246d8> with get, set

        abstract item: SharedLiterals.KeyMetadataTimestamp with get, set
        abstract text: string with get, set
        abstract score: float with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchListJobsParams")>]
    type AiSearchListJobsParams =
        [<EmitProperty("per_page")>]
        abstract perPage: option<float> with get, set

        abstract page: option<float> with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesR2")>]
    type RequestInitCfPropertiesR2 =
        abstract bucketColoId: option<float> with get, set

    [<Import("@cloudflare/workers-types", "Body")>]
    type Body =
        abstract bodyUsed: bool with get
        abstract body: option<ReadableStream<option<obj>>> with get
        abstract arrayBuffer: unit -> Promise<ArrayBuffer>
        abstract bytes: unit -> Promise<Uint8Array>
        abstract text: unit -> Promise<string>
        abstract json: unit -> Promise<obj>
        abstract formData: unit -> Promise<FormData>
        abstract blob: unit -> Promise<Blob>

    [<Import("@cloudflare/workers-types", "ArtifactsRepoListResult")>]
    type ArtifactsRepoListResult =
        abstract cursor: option<string> with get, set
        abstract total: float with get, set
        abstract repos: ResizeArray<ArtifactsRepoListResult.Repos> with get, set

    type ConversionResponse = U2<SharedLiterals.DataFormatIdMimeTypeNameTokens, SharedLiterals.ErrorFormatIdMimeTypeName>

    [<Import("@cloudflare/workers-types", "Ai")>]
    type Ai<'AiModelList> =
        abstract aiGatewayLogId: option<string> with get, set
        abstract gateway: gatewayId: string -> AiGateway
        abstract aiSearch: unit -> AiSearchNamespace
        abstract autorag: autoragId: string -> AutoRAG
        abstract run: model: obj * inputs: Ai.Run.Inputs * options: Ai.Run.Options -> Promise<AiAsyncBatchResponse>
        abstract run: model: obj * inputs: proptypekey<proptypekey<'AiModelList, obj>, string> * options: Ai.Run.Options.Case2 -> Promise<Response>
        abstract run: model: obj * inputs: proptypekey<proptypekey<'AiModelList, obj>, string> * options: Ai.Run.Options.Case3 -> Promise<Response>
        abstract run: model: obj * inputs: Ai.Run.Inputs.Case2 * ?options: AiOptions -> Promise<ReadableStream<option<obj>>>
        abstract run<'Name> : model: 'Name * inputs: proptypekey<proptypekey<'AiModelList, 'Name>, string> * ?options: AiOptions -> Promise<proptypekey<proptypekey<'AiModelList, 'Name>, string>>
        abstract run<'Model> : ?model: string * inputs: obj * ?options: AiOptions -> Promise<obj>
        abstract models: ?params: AiModelsSearchParams -> Promise<ResizeArray<AiModelsSearchObject>>
        abstract toMarkdown: unit -> ToMarkdownService
        abstract toMarkdown: files: ResizeArray<MarkdownDocument> * ?options: ConversionRequestOptions -> Promise<ResizeArray<ConversionResponse>>
        abstract toMarkdown: files: MarkdownDocument * ?options: ConversionRequestOptions -> Promise<ConversionResponse>

    type AiCfMetaLlama3370BInstructFp8FastOutput = U3<AiCfQwenQwen25Coder32BInstructOutput, AiCfMetaLlama3370BInstructFp8FastAsyncResponse, string>

    [<Import("@cloudflare/workers-types", "MessageEventInit")>]
    type MessageEventInit =
        abstract data: U2<ArrayBuffer, string> with get, set

    [<Import("@cloudflare/workers-types", "MessageBatchMetrics")>]
    type MessageBatchMetrics =
        abstract oldestMessageTimestamp: option<Date> with get, set
        abstract backlogBytes: float with get, set
        abstract backlogCount: float with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Mistralai_Mistral_Small_3_1_24B_Instruct_Output")>]
    type AiCfMistralaiMistralSmall3124BInstructOutput =
        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract response: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseFormatJSONSchema")>]
    type ResponseFormatJSONSchema =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: SharedLiterals.DescriptionNameSchemaStrict with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "VectorizeMatches")>]
    type VectorizeMatches =
        abstract count: float with get, set
        abstract matches: ResizeArray<VectorizeMatch> with get, set

    [<Import("@cloudflare/workers-types", "ResponseReasoningItem")>]
    type ResponseReasoningItem =
        abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set

        [<EmitProperty("encrypted_content")>]
        abstract encryptedContent: option<string> with get, set

        abstract content: option<ResizeArray<ResponseReasoningContentItem>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract summary: ResizeArray<ResponseReasoningSummaryItem> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ReadableStreamGetReaderOptions")>]
    type ReadableStreamGetReaderOptions =
        abstract mode: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_JSON_Mode_1")>]
    type AiCfMetaLlama3370BInstructFp8FastJSONMode1 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "HelloWorldBinding")>]
    type HelloWorldBinding =
        abstract get: unit -> Promise<HelloWorldBinding.Get>
        abstract set: value: string -> Promise<unit>

    type VectorizeVectorMetadataValue = U4<ResizeArray<string>, string, float, bool>

    [<Import("@cloudflare/workers-types", "Event")>]
    type Event =
        [<EmitConstructor>]
        abstract Create: ``type``: string * ?init: EventInit -> Event

        abstract BUBBLING_PHASE: float with get
        abstract AT_TARGET: float with get
        abstract CAPTURING_PHASE: float with get
        abstract NONE: float with get
        abstract cancelBubble: bool with get
        abstract isTrusted: bool with get
        abstract timeStamp: float with get
        abstract srcElement: option<EventTarget<obj>> with get
        abstract target: option<EventTarget<obj>> with get
        abstract currentTarget: option<EventTarget<obj>> with get
        abstract returnValue: bool with get
        abstract defaultPrevented: bool with get
        abstract cancelable: bool with get
        abstract bubbles: bool with get
        abstract composed: bool with get
        abstract eventPhase: float with get

        [<EmitProperty("type")>]
        abstract ``type``: string with get

        abstract stopImmediatePropagation: unit -> unit
        abstract preventDefault: unit -> unit
        abstract stopPropagation: unit -> unit
        abstract composedPath: unit -> ResizeArray<EventTarget<obj>>

    [<Import("@cloudflare/workers-types", "PromptTokensDetails")>]
    type PromptTokensDetails =
        [<EmitProperty("audio_tokens")>]
        abstract audioTokens: option<float> with get, set

        [<EmitProperty("cached_tokens")>]
        abstract cachedTokens: option<float> with get, set

    [<Import("@cloudflare/workers-types", "StreamOptions")>]
    type StreamOptions =
        [<EmitProperty("include_obfuscation")>]
        abstract includeObfuscation: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Deepgram_Nova_3")>]
    type BaseAiCfDeepgramNova3 =
        abstract postProcessedOutputs: AiCfDeepgramNova3Output with get, set
        abstract inputs: AiCfDeepgramNova3Input with get, set

    type AiCfMetaM2M10012BOutput = U2<AiTranslationOutput, AiCfMetaM2M10012BAsyncResponse>
    type ChatCompletionToolChoiceOption = U4<LiteralUnions.AutoNoneRequired, ChatCompletionToolChoiceFunction, ChatCompletionToolChoiceCustom, ChatCompletionToolChoiceAllowedTools>

    [<Import("@cloudflare/workers-types", "TransformStreamDefaultController")>]
    type TransformStreamDefaultController<'O> =
        abstract desiredSize: option<float> with get
        abstract enqueue: ?chunk: 'O -> unit
        abstract error: ?reason: obj -> unit
        abstract terminate: unit -> unit

    [<Import("@cloudflare/workers-types", "WritableStream")>]
    type WritableStream<'W> =
        [<EmitConstructor>]
        abstract Create: ?underlyingSink: UnderlyingSink<option<obj>> * ?queuingStrategy: QueuingStrategy<option<obj>> -> WritableStream<'W>

        abstract locked: bool with get
        abstract abort: ?reason: obj -> Promise<unit>
        abstract close: unit -> Promise<unit>
        abstract getWriter: unit -> WritableStreamDefaultWriter<'W>

    [<Import("@cloudflare/workers-types", "DigestStream")>]
    type DigestStream =
        [<EmitConstructor>]
        abstract Create: algorithm: U2<SubtleCryptoHashAlgorithm, string> -> DigestStream

        inherit WritableStream<BufferSource>
        abstract bytesWritten: float with get
        abstract digest: Promise<ArrayBuffer> with get

    [<Import("@cloudflare/workers-types", "WebSocket")>]
    type WebSocket =
        inherit EventTarget<WebSocketEventMap>
        abstract binaryType: LiteralUnions.ArraybufferBlob with get, set
        abstract extensions: option<string> with get, set
        abstract protocol: option<string> with get, set
        abstract url: option<string> with get, set
        abstract readyState: float with get, set
        abstract accept: ?options: WebSocketAcceptOptions -> unit
        abstract send: message: U3<ArrayBuffer, obj, string> -> unit
        abstract close: ?code: float * ?reason: string -> unit
        abstract serializeAttachment: ?attachment: obj -> unit
        abstract deserializeAttachment: unit -> option<obj>

    [<Import("@cloudflare/workers-types", "BrowserRunScrapeOptions")>]
    type BrowserRunScrapeOptions =
        abstract elements: ResizeArray<SharedLiterals.Selector> with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryGetSummaryOptions")>]
    type AgentMemoryGetSummaryOptions =
        abstract sessionId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ResponseFormatText")>]
    type ResponseFormatText =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "StreamVideoStatus")>]
    type StreamVideoStatus =
        abstract errorReasonText: string with get, set
        abstract errorReasonCode: string with get, set
        abstract pctComplete: option<string> with get, set
        abstract step: option<string> with get, set
        abstract state: string with get, set

    [<Import("@cloudflare/workers-types", "ReadableStream")>]
    type ReadableStream<'R> =
        abstract locked: bool with get
        abstract cancel: ?reason: obj -> Promise<unit>
        abstract getReader: unit -> ReadableStreamDefaultReader<'R>
        abstract getReader: options: ReadableStreamGetReaderOptions -> ReadableStreamBYOBReader
        abstract pipeThrough: transform: ReadableWritablePair<obj, 'R> * ?options: StreamPipeOptions -> ReadableStream<obj>
        abstract pipeTo: destination: WritableStream<'R> * ?options: StreamPipeOptions -> Promise<unit>
        abstract tee: unit -> ReadableStream<'R> * ReadableStream<'R>
        abstract values: ?options: ReadableStreamValuesOptions -> seq<'R>
        abstract ``[symbol.asyncIterator]``: ?options: ReadableStreamValuesOptions -> seq<'R>

    [<Import("@cloudflare/workers-types", "BaseAiObjectDetection")>]
    type BaseAiObjectDetection =
        abstract postProcessedOutputs: AiObjectDetectionOutput with get, set
        abstract inputs: AiObjectDetectionInput with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Deepgram_Aura_2_Es")>]
    type BaseAiCfDeepgramAura2Es =
        abstract postProcessedOutputs: string with get, set
        abstract inputs: AiCfDeepgramAura2EsInput with get, set

    [<Import("@cloudflare/workers-types", "SubtleCryptoSignAlgorithm")>]
    type SubtleCryptoSignAlgorithm =
        abstract saltLength: option<float> with get, set
        abstract dataLength: option<float> with get, set
        abstract hash: option<U2<SubtleCryptoHashAlgorithm, string>> with get, set
        abstract name: string with get, set

    type AiCfUnumUformGen2Qwen500MInput = U2<SharedLiterals.FrequencyPenaltyImageFb6819ce, string>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Ai4Bharat_Indictrans2_En_Indic_1B_Output")>]
    type AiCfAi4BharatIndictrans2EnIndic1BOutput =
        abstract translations: ResizeArray<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_M2M100_1_2B_AsyncResponse")>]
    type AiCfMetaM2M10012BAsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionToolChoiceAllowedTools")>]
    type ChatCompletionToolChoiceAllowedTools =
        [<EmitProperty("allowed_tools")>]
        abstract allowedTools: SharedLiterals.ModeTools with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AiSearchMultiSearchRequest = U2<SharedLiterals.AiSearchOptionsMessagesQuery3, SharedLiterals.AiSearchOptionsMessagesQuery4>

    [<Import("@cloudflare/workers-types", "ResponseInputImageContent")>]
    type ResponseInputImageContent =
        [<EmitProperty("image_url")>]
        abstract imageUrl: option<string> with get, set

        abstract detail: option<LiteralUnions.AutoHighLow> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type VectorizeMetadataRetrievalLevel =
        | [<CompiledName("all")>] All
        | [<CompiledName("none")>] None
        | [<CompiledName("indexed")>] Indexed

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_Prompt")>]
    type AiCfMetaLlama4Scout17B16EInstructPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlama4Scout17B16EInstructJSONMode> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "DocumentEnd")>]
    type DocumentEnd =
        abstract append: content: string * ?options: ContentOptions -> DocumentEnd

    [<Import("@cloudflare/workers-types", "AiTranslationOutput")>]
    type AiTranslationOutput =
        [<EmitProperty("translated_text")>]
        abstract translatedText: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ResponseFunctionToolCall")>]
    type ResponseFunctionToolCall =
        abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
        abstract id: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract name: string with get, set

        [<EmitProperty("call_id")>]
        abstract callId: string with get, set

        abstract arguments: string with get, set

    [<Import("@cloudflare/workers-types", "Request")>]
    type Request<'CfHostMetadata, 'Cf> =
        inherit Body
        abstract cache: option<LiteralUnions.NoCacheNoStore> with get, set
        abstract keepalive: bool with get, set
        abstract integrity: string with get, set
        abstract cf: option<'Cf> with get, set
        abstract signal: AbortSignal with get, set
        abstract fetcher: option<SharedLiterals.ConnectFetch> with get, set
        abstract redirect: string with get, set
        abstract headers: Headers with get, set
        abstract url: string with get, set
        abstract method: string with get, set
        abstract clone: unit -> Request<'CfHostMetadata, 'Cf>

    [<Import("@cloudflare/workers-types", "BaseAiTextClassification")>]
    type BaseAiTextClassification =
        abstract postProcessedOutputs: AiTextClassificationOutput with get, set
        abstract inputs: AiTextClassificationInput with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Black_Forest_Labs_Flux_2_Klein_9B")>]
    type BaseAiCfBlackForestLabsFlux2Klein9B =
        abstract postProcessedOutputs: AiCfBlackForestLabsFlux2Klein9BOutput with get, set
        abstract inputs: AiCfBlackForestLabsFlux2Klein9BInput with get, set

    [<Import("@cloudflare/workers-types", "ResponseFailedEvent")>]
    type ResponseFailedEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        abstract response: Response with get, set

    [<Import("@cloudflare/workers-types", "ResponseRefusalDeltaEvent")>]
    type ResponseRefusalDeltaEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        abstract delta: string with get, set

        [<EmitProperty("content_index")>]
        abstract contentIndex: float with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesBase")>]
    type IncomingRequestCfPropertiesBase =
        abstract tlsExportedAuthenticator: option<IncomingRequestCfPropertiesExportedAuthenticatorMetadata> with get, set
        /// <example>
        /// "AEAD-AES128-GCM-SHA256"
        /// </example>
        abstract tlsCipher: string with get, set
        /// <example>
        /// "TLSv1.3"
        /// </example>
        abstract tlsVersion: string with get, set
        /// <example>
        /// "weight=192;exclusive=0;group=3;group-weight=127"
        /// </example>
        abstract requestPriority: string with get, set
        /// <example>
        /// "HTTP/2"
        /// </example>
        abstract httpProtocol: string with get, set
        /// <example>
        /// 3
        /// </example>
        abstract edgeRequestKeepAliveStatus: IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus with get, set
        /// <example>
        /// "DFW"
        /// </example>
        abstract colo: string with get, set
        /// <example>
        /// 22
        /// </example>
        abstract clientTcpRtt: option<float> with get, set
        /// <example>
        /// "gzip, deflate, br"
        /// </example>
        abstract clientAcceptEncoding: option<string> with get, set
        /// <example>
        /// "Google Cloud"
        /// </example>
        abstract asOrganization: option<string> with get, set
        /// <example>
        /// 395747
        /// </example>
        abstract asn: option<float> with get, set

    type ResponseFormat = U3<ChatCompletionsResponseFormatText, ChatCompletionsResponseFormatJSONObject, ResponseFormatJSONSchema>

    [<Import("@cloudflare/workers-types", "Response")>]
    type Response =
        inherit Body

        [<EmitProperty("type")>]
        abstract ``type``: LiteralUnions.DefaultError with get, set

        abstract cf: option<obj> with get, set
        abstract webSocket: option<WebSocket> with get, set
        abstract url: string with get, set
        abstract redirected: bool with get, set
        abstract ok: bool with get, set
        abstract headers: Headers with get, set
        abstract statusText: string with get, set
        abstract status: float with get, set
        abstract clone: unit -> Response

    [<Import("@cloudflare/workers-types", "LoopbackDurableObjectClass")>]
    type LoopbackDurableObjectClass<'T> =
        abstract Invoke: opts: SharedLiterals.Props<obj> -> DurableObjectClass<'T>
        abstract Invoke: opts: SharedLiterals.Props2 -> DurableObjectClass<'T>

    [<Import("@cloudflare/workers-types", "SubtleCryptoGenerateKeyAlgorithm")>]
    type SubtleCryptoGenerateKeyAlgorithm =
        abstract namedCurve: option<string> with get, set
        abstract length: option<float> with get, set
        abstract publicExponent: option<BufferSource> with get, set
        abstract modulusLength: option<float> with get, set
        abstract hash: option<U2<SubtleCryptoHashAlgorithm, string>> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "TraceItemScheduledEventInfo")>]
    type TraceItemScheduledEventInfo =
        abstract cron: string with get
        abstract scheduledTime: float with get

    [<Import("@cloudflare/workers-types", "TraceItemEmailEventInfo")>]
    type TraceItemEmailEventInfo =
        abstract rawSize: float with get
        abstract rcptTo: string with get
        abstract mailFrom: string with get

    [<Import("@cloudflare/workers-types", "DurableObjectNamespaceGetDurableObjectOptions")>]
    type DurableObjectNamespaceGetDurableObjectOptions =
        abstract routingMode: option<string> with get, set
        abstract locationHint: option<DurableObjectLocationHint> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Openai_Whisper_Tiny_En")>]
    type BaseAiCfOpenaiWhisperTinyEn =
        abstract postProcessedOutputs: AiCfOpenaiWhisperTinyEnOutput with get, set
        abstract inputs: AiCfOpenaiWhisperInput with get, set

    [<Import("@cloudflare/workers-types", "MessagePortPostMessageOptions")>]
    type MessagePortPostMessageOptions =
        abstract transfer: option<ResizeArray<option<obj>>> with get, set

    [<Import("@cloudflare/workers-types", "MessageChannel")>]
    type MessageChannel =
        [<EmitConstructor>]
        abstract Create: unit -> MessageChannel

        abstract port2: MessagePort with get
        abstract port1: MessagePort with get

    [<Import("@cloudflare/workers-types", "TraceItemFetchEventInfoResponse")>]
    type TraceItemFetchEventInfoResponse =
        abstract status: float with get

    type PagesPluginFunction =
        abstract pluginArgs: obj with get, set
        abstract data: obj with get, set
        abstract params: obj with get, set
        abstract env: PagesPluginFunction.Env with get, set
        abstract functionPath: string with get, set
        abstract request: Request<option<obj>, obj> with get, set
        abstract waitUntil: promise: Promise<option<obj>> -> unit
        abstract passThroughOnException: unit -> unit
        abstract next: ?input: U2<Request<option<obj>, U2<RequestInitCfProperties, obj>>, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>

    [<Import("@cloudflare/workers-types", "SubtleCryptoDeriveKeyAlgorithm")>]
    type SubtleCryptoDeriveKeyAlgorithm =
        abstract info: option<BufferSource> with get, set

        [<EmitProperty("$public")>]
        abstract ``$public``: option<CryptoKey> with get, set

        abstract hash: option<U2<SubtleCryptoHashAlgorithm, string>> with get, set
        abstract iterations: option<float> with get, set
        abstract salt: option<BufferSource> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "AiTextToImageInput")>]
    type AiTextToImageInput =
        abstract seed: option<float> with get, set
        abstract guidance: option<float> with get, set
        abstract strength: option<float> with get, set

        [<EmitProperty("num_steps")>]
        abstract numSteps: option<float> with get, set

        abstract mask: option<AiSentenceSimilarityOutput> with get, set

        [<EmitProperty("image_b64")>]
        abstract imageB64: option<string> with get, set

        abstract image: option<AiSentenceSimilarityOutput> with get, set
        abstract width: option<float> with get, set
        abstract height: option<float> with get, set

        [<EmitProperty("negative_prompt")>]
        abstract negativePrompt: option<string> with get, set

        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "EventTargetAddEventListenerOptions")>]
    type EventTargetAddEventListenerOptions =
        abstract signal: option<AbortSignal> with get, set
        abstract once: option<bool> with get, set
        abstract passive: option<bool> with get, set
        abstract capture: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "ResponsesFunctionTool")>]
    type Tool =
        abstract description: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract strict: option<bool> with get, set
        abstract parameters: option<System.Collections.Generic.IDictionary<string, option<obj>>> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen2_5_Coder_32B_Instruct_Prompt")>]
    type AiCfQwenQwen25Coder32BInstructPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfQwenQwen25Coder32BInstructJSONMode> with get, set

        abstract lora: option<string> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "ChatTemplateKwargs")>]
    type ChatTemplateKwargs =
        [<EmitProperty("clear_thinking")>]
        abstract clearThinking: option<bool> with get, set

        [<EmitProperty("enable_thinking")>]
        abstract enableThinking: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "AssistantMessage")>]
    type AssistantMessage =
        [<EmitProperty("function_call")>]
        abstract functionCall: option<SharedLiterals.ArgumentsName> with get, set

        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<ChatCompletionMessageToolCall>> with get, set

        abstract audio: option<StreamDirectUploadWatermark> with get, set
        abstract name: option<string> with get, set
        abstract refusal: option<string> with get, set
        abstract content: option<U2<ResizeArray<AssistantMessageContentPart>, string>> with get, set
        abstract role: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemLogsParams")>]
    type AiSearchItemLogsParams =
        abstract cursor: option<string> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "AiTextGenerationResponseFormat")>]
    type AiTextGenerationResponseFormat =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "HTMLRewriterElementContentHandlers")>]
    type HTMLRewriterElementContentHandlers =
        abstract element: element: Element -> option<Promise<unit>>
        abstract comments: comment: Comment -> option<Promise<unit>>
        abstract text: element: Text -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "AgentMemoryScoredCandidate")>]
    type AgentMemoryScoredCandidate =
        abstract score: float with get, set
        abstract sessionId: option<string> with get, set
        abstract summary: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "AIGatewayHeaders")>]
    type AIGatewayHeaders =
        [<EmitProperty("Content-Type")>]
        abstract contentType: string with get, set

        [<EmitProperty("Authorization")>]
        abstract authorization: string with get, set

        [<EmitProperty("cf-aig-collect-log")>]
        abstract cfAigCollectLog: U2<bool, string> with get, set

        [<EmitProperty("cf-aig-backoff")>]
        abstract cfAigBackoff: string with get, set

        [<EmitProperty("cf-aig-retry-delay")>]
        abstract cfAigRetryDelay: Zod.ZodType with get, set

        [<EmitProperty("cf-aig-max-attempts")>]
        abstract cfAigMaxAttempts: Zod.ZodType with get, set

        [<EmitProperty("cf-aig-request-timeout")>]
        abstract cfAigRequestTimeout: Zod.ZodType with get, set

        [<EmitProperty("cf-aig-event-id")>]
        abstract cfAigEventId: string with get, set

        [<EmitProperty("cf-aig-cache-key")>]
        abstract cfAigCacheKey: string with get, set

        [<EmitProperty("cf-aig-skip-cache")>]
        abstract cfAigSkipCache: U2<bool, string> with get, set

        [<EmitProperty("cf-aig-cache-ttl")>]
        abstract cfAigCacheTtl: Zod.ZodType with get, set

        [<EmitProperty("cf-aig-custom-cost")>]
        abstract cfAigCustomCost: U3<SharedLiterals.PerTokenInPerTokenOut, SharedLiterals.TotalCost, string> with get, set

        [<EmitProperty("cf-aig-metadata")>]
        abstract cfAigMetadata: U2<obj, string> with get, set

        abstract Item: key: string -> U4<string, float, bool, obj>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_Small_En_V1_5_AsyncResponse")>]
    type AiCfBaaiBgeSmallEnV15AsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_1_Schnell_Output")>]
    type AiCfBlackForestLabsFlux1SchnellOutput =
        abstract image: option<string> with get, set

    [<Import("@cloudflare/workers-types", "WorkerGlobalScopeEventMap")>]
    type WorkerGlobalScopeEventMap =
        abstract rejectionhandled: PromiseRejectionEvent with get, set
        abstract unhandledrejection: PromiseRejectionEvent with get, set
        abstract queue: QueueEvent<option<obj>> with get, set
        abstract scheduled: ScheduledEvent with get, set
        abstract fetch: FetchEvent with get, set

    [<Import("@cloudflare/workers-types", "BaseAiImageToText")>]
    type BaseAiImageToText =
        abstract postProcessedOutputs: AiImageToTextOutput with get, set
        abstract inputs: AiImageToTextInput with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunLinksSuccessResponse")>]
    type BrowserRunLinksSuccessResponse =
        abstract result: ResizeArray<string> with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "AiModelsSearchParams")>]
    type AiModelsSearchParams =
        abstract task: option<string> with get, set
        abstract source: option<float> with get, set
        abstract search: option<string> with get, set

        [<EmitProperty("per_page")>]
        abstract perPage: option<float> with get, set

        abstract page: option<float> with get, set

        [<EmitProperty("hide_experimental")>]
        abstract hideExperimental: option<bool> with get, set

        abstract author: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ResponseReasoningTextDoneEvent")>]
    type ResponseReasoningTextDoneEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        [<EmitProperty("content_index")>]
        abstract contentIndex: float with get, set

    [<Import("@cloudflare/workers-types", "ResponseTextConfig")>]
    type ResponseTextConfig =
        abstract verbosity: option<LiteralUnions.HighLowMedium> with get, set
        abstract format: option<ResponseFormatTextConfig> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Openai_Whisper_Output")>]
    type AiCfOpenaiWhisperOutput =
        abstract vtt: option<string> with get, set
        abstract words: option<ResizeArray<SharedLiterals.EndStartWord2>> with get, set

        [<EmitProperty("word_count")>]
        abstract wordCount: option<float> with get, set

        abstract text: string with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRAGNotFoundError")>]
    type AutoRAGNotFoundError = interface end

    type WorkflowRetentionDuration = U15<WorkflowRetentionDuration.Case0, WorkflowRetentionDuration.Case1, WorkflowRetentionDuration.Case2, WorkflowRetentionDuration.Case3, WorkflowRetentionDuration.Case4, WorkflowRetentionDuration.Case5, WorkflowRetentionDuration.Case6, WorkflowRetentionDuration.Case7, WorkflowRetentionDuration.Case8, WorkflowRetentionDuration.Case9, WorkflowRetentionDuration.Case10, WorkflowRetentionDuration.Case11, WorkflowRetentionDuration.Case12, WorkflowRetentionDuration.Case13, float>

    [<Import("@cloudflare/workers-types", "TopLogprob")>]
    type TopLogprob =
        abstract logprob: option<float> with get, set
        abstract token: option<string> with get, set

    [<Import("@cloudflare/workers-types", "StreamScopedCaptions")>]
    type StreamScopedCaptions =
        abstract upload: language: string * input: ReadableStream<option<obj>> -> Promise<StreamCaption>
        abstract generate: language: string -> Promise<StreamCaption>
        abstract list: ?language: string -> Promise<ResizeArray<StreamCaption>>
        abstract delete: language: string -> Promise<unit>

    [<Import("@cloudflare/workers-types", "D1Result")>]
    type D1Result =
        abstract results: ResizeArray<obj> with get, set
        abstract error: option<unit> with get, set
        abstract meta: D1Result.Meta with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunJsonErrorResponse")>]
    type BrowserRunJsonErrorResponse =
        abstract rawAiResponse: option<string> with get, set
        abstract errors: ResizeArray<SharedLiterals.CodeDetailMessagePath> with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "BaseAiTextToImage")>]
    type BaseAiTextToImage =
        abstract postProcessedOutputs: AiTextToImageOutput with get, set
        abstract inputs: AiTextToImageInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Flux_Input")>]
    type AiCfDeepgramFluxInput =
        abstract tag: option<string> with get, set

        [<EmitProperty("mip_opt_out")>]
        abstract mipOptOut: option<LiteralUnions.FalseTrue> with get, set

        abstract keyterm: option<string> with get, set

        [<EmitProperty("eot_timeout_ms")>]
        abstract eotTimeoutMs: option<string> with get, set

        [<EmitProperty("eot_threshold")>]
        abstract eotThreshold: option<string> with get, set

        [<EmitProperty("eager_eot_threshold")>]
        abstract eagerEotThreshold: option<string> with get, set

        [<EmitProperty("sample_rate")>]
        abstract sampleRate: string with get, set

        abstract encoding: string with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionsResponseFormatJSONObject")>]
    type ChatCompletionsResponseFormatJSONObject =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "AiModelListType")>]
    type AiModelListType = interface end

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen2_5_Coder_32B_Instruct_JSON_Mode_1")>]
    type AiCfQwenQwen25Coder32BInstructJSONMode1 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "ResponseReasoningContentItem")>]
    type ResponseReasoningContentItem =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

    type AiCfQwenQwen330BA3BFp8Output = U4<AiCfQwenQwen330BA3BFp8ChatCompletionResponse, AiCfQwenQwen330BA3BFp8TextCompletionResponse, AiCfQwenQwen330BA3BFp8AsyncResponse, string>

    [<Import("@cloudflare/workers-types", "CompletionUsage")>]
    type CompletionUsage =
        [<EmitProperty("completion_tokens_details")>]
        abstract completionTokensDetails: option<CompletionTokensDetails> with get, set

        [<EmitProperty("prompt_tokens_details")>]
        abstract promptTokensDetails: option<PromptTokensDetails> with get, set

        [<EmitProperty("total_tokens")>]
        abstract totalTokens: float with get, set

        [<EmitProperty("completion_tokens")>]
        abstract completionTokens: float with get, set

        [<EmitProperty("prompt_tokens")>]
        abstract promptTokens: float with get, set

    [<Import("@cloudflare/workers-types", "ImageMetadata")>]
    type ImageMetadata =
        abstract creator: option<string> with get, set
        abstract draft: option<bool> with get, set
        abstract variants: ResizeArray<string> with get, set
        abstract meta: option<obj> with get, set
        abstract requireSignedURLs: bool with get, set
        abstract uploaded: option<string> with get, set
        abstract filename: option<string> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_Embedding_0_6B_Output")>]
    type AiCfQwenQwen3Embedding06BOutput =
        abstract shape: option<AiSentenceSimilarityOutput> with get, set
        abstract data: option<ResizeArray<AiSentenceSimilarityOutput>> with get, set

    [<Import("@cloudflare/workers-types", "WritableStreamDefaultController")>]
    type WritableStreamDefaultController =
        abstract signal: AbortSignal with get
        abstract error: ?reason: obj -> unit

    [<Import("@cloudflare/workers-types", "ArtifactsCreateRepoResult")>]
    type ArtifactsCreateRepoResult =
        abstract tokenExpiresAt: string with get, set
        abstract token: string with get, set
        abstract remote: string with get, set
        abstract defaultBranch: string with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseReasoningSummaryItem")>]
    type ResponseReasoningSummaryItem =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

    type ReadableStreamReadResult<'R> = U2<SharedLiterals.DoneValue2<'R>, SharedLiterals.DoneValue>

    [<Import("@cloudflare/workers-types", "FileOptions")>]
    type FileOptions =
        abstract lastModified: option<float> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    type AutoRagListResponse = ResizeArray<SharedLiterals.EnableIdPaused9f74c307>

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Pipecat_Ai_Smart_Turn_V2")>]
    type BaseAiCfPipecatAiSmartTurnV2 =
        abstract postProcessedOutputs: AiCfPipecatAiSmartTurnV2Output with get, set
        abstract inputs: AiCfPipecatAiSmartTurnV2Input with get, set

    [<Import("@cloudflare/workers-types", "AiOptions")>]
    type AiOptions =
        abstract signal: option<AbortSignal> with get, set
        abstract extraHeaders: option<obj> with get, set
        abstract prefix: option<string> with get, set
        abstract returnRawResponse: option<bool> with get, set
        abstract gateway: option<GatewayOptions> with get, set
        abstract tags: option<ResizeArray<string>> with get, set
        abstract websocket: option<bool> with get, set
        abstract queueRequest: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "AiTextToImageOutput")>]
    type AiTextToImageOutput = interface end

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesBotManagementBase")>]
    type IncomingRequestCfPropertiesBotManagementBase =
        abstract detectionIds: AiSentenceSimilarityOutput with get, set
        abstract staticResource: bool with get, set
        abstract corporateProxy: bool with get, set
        abstract verifiedBot: bool with get, set
        /// <example>
        /// 54
        /// </example>
        abstract score: float with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Input_Embedding")>]
    type AiCfBaaiBgeM3InputEmbedding =
        [<EmitProperty("truncate_inputs")>]
        abstract truncateInputs: option<bool> with get, set

        abstract text: U2<ResizeArray<string>, string> with get, set

    [<Import("@cloudflare/workers-types", "ResponseOutputText")>]
    type ResponseOutputText =
        abstract logprobs: option<ResizeArray<Logprob>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseInput")>]
    type ResponseInput = interface end

    type AiCfMetaLlama3211BVisionInstructInput = U2<AiCfMetaLlama3211BVisionInstructPrompt, AiCfMetaLlama3211BVisionInstructMessages>

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It")>]
    type BaseAiCfAisingaporeGemmaSeaLionV427BIt =
        abstract postProcessedOutputs: AiCfAisingaporeGemmaSeaLionV427BItOutput with get, set
        abstract inputs: AiCfAisingaporeGemmaSeaLionV427BItInput with get, set

    [<Import("@cloudflare/workers-types", "Span")>]
    type Span =
        abstract isTraced: bool with get
        abstract setAttribute: key: string * ?value: U3<bool, float, string> -> unit
        abstract ``end``: unit -> unit

    [<Import("@cloudflare/workers-types", "ImageOutputOptions")>]
    type ImageOutputOptions =
        abstract anim: option<bool> with get, set
        abstract background: option<string> with get, set
        abstract quality: option<float> with get, set
        abstract format: LiteralUnions.ImageAvifImageGifImageJ6516ed8b with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Async_Batch")>]
    type AiCfQwenQwen330BA3BFp8AsyncBatch =
        abstract requests: ResizeArray<U2<AiCfQwenQwen330BA3BFp8Prompt1, AiCfQwenQwen330BA3BFp8Messages1>> with get, set

    [<Import("@cloudflare/workers-types", "AiTextGenerationToolLegacyOutput")>]
    type AiTextGenerationToolLegacyOutput =
        abstract arguments: option<obj> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "R2UploadPartOptions")>]
    type R2UploadPartOptions =
        abstract ssecKey: option<U2<ArrayBuffer, string>> with get, set

    [<Import("@cloudflare/workers-types", "WebSearchResult")>]
    type WebSearchResult =
        abstract faviconUrl: option<string> with get, set
        abstract imageUrl: option<string> with get, set
        abstract lastModifiedDate: option<string> with get, set
        abstract description: option<string> with get, set
        abstract title: string with get, set
        abstract url: string with get, set

    [<Import("@cloudflare/workers-types", "ToolChoiceFunction")>]
    type ToolChoiceFunction =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseInputImage")>]
    type ResponseInputImage =
        [<EmitProperty("image_url")>]
        abstract imageUrl: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract detail: LiteralUnions.AutoHighLow with get, set

    [<Import("@cloudflare/workers-types", "SqlStorage")>]
    type SqlStorage =
        [<EmitProperty("Statement")>]
        abstract statement: SqlStorage.Statement with get, set

        [<EmitProperty("Cursor")>]
        abstract cursor: SqlStorage.Cursor with get, set

        abstract databaseSize: float with get
        abstract exec: query: string * [<ParamArray>] bindings: ResizeArray<option<obj>> -> SqlStorageCursor<obj>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Aura_1_Input")>]
    type AiCfDeepgramAura1Input =
        [<EmitProperty("bit_rate")>]
        abstract bitRate: option<float> with get, set

        [<EmitProperty("sample_rate")>]
        abstract sampleRate: option<float> with get, set

        abstract text: string with get, set
        abstract container: option<LiteralUnions.NoneOggWav> with get, set
        abstract encoding: option<LiteralUnions.AacAlawFlacLinear16Mp3MulawOpus> with get, set
        abstract speaker: option<LiteralUnions.AngusArcasAsteriaB111950c> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchListResponse")>]
    type AiSearchListResponse =
        [<EmitProperty("result_info")>]
        abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

        abstract result: ResizeArray<AiSearchInstanceInfo> with get, set

    [<Import("@cloudflare/workers-types", "BadRequestError")>]
    type BadRequestError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Console")>]
    type Console =
        abstract ``assert``: condition: bool * [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract clear: unit -> unit
        abstract count: ?label: string -> unit
        abstract countReset: ?label: string -> unit
        abstract debug: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract dir: ?item: obj * ?options: obj -> unit
        abstract dirxml: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract error: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract group: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract groupCollapsed: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract groupEnd: unit -> unit
        abstract info: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract log: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract table: ?tabularData: obj * ?properties: ResizeArray<string> -> unit
        abstract time: ?label: string -> unit
        abstract timeEnd: ?label: string -> unit
        abstract timeLog: label: string * [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract timeStamp: ?label: string -> unit
        abstract trace: [<ParamArray>] data: ResizeArray<option<obj>> -> unit
        abstract warn: [<ParamArray>] data: ResizeArray<option<obj>> -> unit

    [<Import("@cloudflare/workers-types", "AiSearchConfig")>]
    type AiSearchConfig =
        abstract metadata: option<obj> with get, set

        [<EmitProperty("sync_interval")>]
        abstract syncInterval: option<LiteralUnions.I14400I21600I3600I43200I7200I86400> with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: option<string> with get, set

        [<EmitProperty("custom_metadata")>]
        abstract customMetadata: option<ResizeArray<SharedLiterals.DataTypeFieldName>> with get, set

        [<EmitProperty("cache_threshold")>]
        abstract cacheThreshold: option<LiteralUnions.AnythingGoesClose50b421b1> with get, set

        abstract cache: option<bool> with get, set

        [<EmitProperty("max_num_results")>]
        abstract maxNumResults: option<float> with get, set

        [<EmitProperty("score_threshold")>]
        abstract scoreThreshold: option<float> with get, set

        [<EmitProperty("chunk_overlap")>]
        abstract chunkOverlap: option<float> with get, set

        [<EmitProperty("chunk_size")>]
        abstract chunkSize: option<float> with get, set

        abstract chunk: option<bool> with get, set

        [<EmitProperty("retrieval_options")>]
        abstract retrievalOptions: option<SharedLiterals.BoostByKeywordMatchMode> with get, set

        [<EmitProperty("indexing_options")>]
        abstract indexingOptions: option<SharedLiterals.KeywordTokenizer> with get, set

        [<EmitProperty("fusion_method")>]
        abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

        [<EmitProperty("index_method")>]
        abstract indexMethod: option<SharedLiterals.KeywordVector> with get, set

        /// <deprecated>
        /// Use index_method instead.
        /// </deprecated>
        [<EmitProperty("hybrid_search_enabled")>]
        abstract hybridSearchEnabled: option<bool> with get, set

        [<EmitProperty("reranking_model")>]
        abstract rerankingModel: option<string> with get, set

        [<EmitProperty("rewrite_model")>]
        abstract rewriteModel: option<string> with get, set

        [<EmitProperty("ai_search_model")>]
        abstract aiSearchModel: option<string> with get, set

        [<EmitProperty("embedding_model")>]
        abstract embeddingModel: option<string> with get, set

        abstract reranking: option<bool> with get, set

        [<EmitProperty("rewrite_query")>]
        abstract rewriteQuery: option<bool> with get, set

        [<EmitProperty("ai_gateway_id")>]
        abstract aiGatewayId: option<string> with get, set

        [<EmitProperty("token_id")>]
        abstract tokenId: option<string> with get, set

        [<EmitProperty("source_params")>]
        abstract sourceParams: option<obj> with get, set

        abstract source: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<U2<LiteralUnions.R2WebCrawler, string>> with get, set

        abstract id: string with get, set
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "AiSearchNamespace")>]
    type AiSearchNamespace =
        abstract get: name: string -> AiSearchInstance
        abstract list: ?params: AiSearchListInstancesParams -> Promise<AiSearchListResponse>
        abstract create: config: AiSearchConfig -> Promise<AiSearchInstance>
        abstract delete: name: string -> Promise<unit>
        abstract search: params: AiSearchMultiSearchRequest -> Promise<AiSearchMultiSearchResponse>
        abstract chatCompletions: params: AiSearchNamespace.ChatCompletions.Params -> Promise<ReadableStream<option<obj>>>
        abstract chatCompletions: params: AiSearchMultiChatCompletionsRequest -> Promise<AiSearchMultiChatCompletionsResponse>

    [<Import("@cloudflare/workers-types", "R2Checksums")>]
    type R2Checksums =
        abstract sha512: option<ArrayBuffer> with get
        abstract sha384: option<ArrayBuffer> with get
        abstract sha256: option<ArrayBuffer> with get
        abstract sha1: option<ArrayBuffer> with get
        abstract md5: option<ArrayBuffer> with get
        abstract toJSON: unit -> R2StringChecksums

    type AiObjectDetectionOutput = ResizeArray<SharedLiterals.LabelScore>

    [<Import("@cloudflare/workers-types", "AutoRAG")>]
    type AutoRAG =
        abstract list: unit -> Promise<AutoRagListResponse>
        abstract search: params: AutoRagSearchRequest -> Promise<AutoRagSearchResponse>
        abstract aiSearch: params: AutoRagAiSearchRequestStreaming -> Promise<Response>
        abstract aiSearch: params: AutoRagAiSearchRequest -> U2<Promise<AutoRagAiSearchResponse>, Promise<U2<AutoRagAiSearchResponse, Response>>>

    [<Import("@cloudflare/workers-types", "CryptoKey")>]
    type CryptoKey =
        abstract usages: ResizeArray<string> with get
        abstract algorithm: U6<CryptoKeyKeyAlgorithm, CryptoKeyAesKeyAlgorithm, CryptoKeyHmacKeyAlgorithm, CryptoKeyRsaKeyAlgorithm, CryptoKeyEllipticKeyAlgorithm, CryptoKeyArbitraryKeyAlgorithm> with get
        abstract extractable: bool with get

        [<EmitProperty("type")>]
        abstract ``type``: string with get

    [<Import("@cloudflare/workers-types", "TraceItemHibernatableWebSocketEventInfo")>]
    type TraceItemHibernatableWebSocketEventInfo =
        abstract getWebSocketEvent: U3<TraceItemHibernatableWebSocketEventInfoMessage, TraceItemHibernatableWebSocketEventInfoClose, TraceItemHibernatableWebSocketEventInfoError> with get

    [<Import("@cloudflare/workers-types", "URLPatternInit")>]
    type URLPatternInit =
        abstract baseURL: option<string> with get, set
        abstract hash: option<string> with get, set
        abstract search: option<string> with get, set
        abstract pathname: option<string> with get, set
        abstract port: option<string> with get, set
        abstract hostname: option<string> with get, set
        abstract password: option<string> with get, set
        abstract username: option<string> with get, set
        abstract protocol: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionChoice")>]
    type ChatCompletionChoice =
        abstract logprobs: option<ChatCompletionLogprobs> with get, set

        [<EmitProperty("finish_reason")>]
        abstract finishReason: LiteralUnions.ContentFilterFunction33a35c6e with get, set

        abstract message: ChatCompletionResponseMessage with get, set
        abstract index: float with get, set

    [<Import("@cloudflare/workers-types", "ResponseConversationParam")>]
    type ResponseConversationParam =
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "BaseAiTextEmbeddings")>]
    type BaseAiTextEmbeddings =
        abstract postProcessedOutputs: AiTextEmbeddingsOutput with get, set
        abstract inputs: AiTextEmbeddingsInput with get, set

    [<Import("@cloudflare/workers-types", "MediaBinding")>]
    type MediaBinding =
        abstract input: media: ReadableStream<Uint8Array> -> MediaTransformer

    [<Import("@cloudflare/workers-types", "DurableObjectTransaction")>]
    type DurableObjectTransaction =
        abstract get: key: string * ?options: DurableObjectGetOptions -> Promise<option<obj>>
        abstract get: keys: ResizeArray<string> * ?options: DurableObjectGetOptions -> Promise<Map<string, obj>>
        abstract list: ?options: DurableObjectListOptions -> Promise<Map<string, obj>>
        abstract put: key: string * value: obj * ?options: DurableObjectPutOptions -> Promise<unit>
        abstract put: entries: obj * ?options: DurableObjectPutOptions -> Promise<unit>
        abstract delete: key: string * ?options: DurableObjectPutOptions -> Promise<bool>
        abstract delete: keys: ResizeArray<string> * ?options: DurableObjectPutOptions -> Promise<float>
        abstract rollback: unit -> unit
        abstract getAlarm: ?options: DurableObjectGetAlarmOptions -> Promise<option<float>>
        abstract setAlarm: scheduledTime: U2<Date, float> * ?options: DurableObjectSetAlarmOptions -> Promise<unit>
        abstract deleteAlarm: ?options: DurableObjectSetAlarmOptions -> Promise<unit>

    [<Import("@cloudflare/workers-types", "MaxFileSizeError")>]
    type MaxFileSizeError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "SyncKvStorage")>]
    type SyncKvStorage =
        abstract get: key: string -> option<obj>
        abstract list: ?options: SyncKvListOptions -> seq<string * obj>
        abstract put: key: string * value: obj -> unit
        abstract delete: key: string -> bool

    [<Import("@cloudflare/workers-types", "MessageSendRequest")>]
    type MessageSendRequest<'Body> =
        abstract delaySeconds: option<float> with get, set
        abstract contentType: option<QueueContentType> with get, set
        abstract body: 'Body with get, set

    [<Import("@cloudflare/workers-types", "CustomEvent")>]
    type CustomEvent<'T> =
        [<EmitConstructor>]
        abstract Create: ``type``: string * ?init: CustomEventCustomEventInit -> CustomEvent<'T>

        inherit Event
        abstract detail: 'T with get

    [<Import("@cloudflare/workers-types", "ReadableStreamDefaultController")>]
    type ReadableStreamDefaultController<'R> =
        abstract desiredSize: option<float> with get
        abstract close: unit -> unit
        abstract enqueue: ?chunk: 'R -> unit
        abstract error: ?reason: obj -> unit

    [<Import("@cloudflare/workers-types", "StreamDirectUploadCreateParams")>]
    type StreamDirectUploadCreateParams =
        abstract watermark: option<StreamDirectUploadWatermark> with get, set
        abstract scheduledDeletion: option<string> with get, set
        abstract thumbnailTimestampPct: option<float> with get, set
        abstract requireSignedURLs: option<bool> with get, set
        abstract allowedOrigins: option<ResizeArray<string>> with get, set
        abstract meta: option<obj> with get, set
        abstract creator: option<string> with get, set
        abstract expiry: option<string> with get, set
        abstract maxDurationSeconds: float with get, set

    [<Import("@cloudflare/workers-types", "ResponseOutputItemDoneEvent")>]
    type ResponseOutputItemDoneEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        abstract item: ResponseOutputItem with get, set

    [<Import("@cloudflare/workers-types", "TraceItemTailEventInfo")>]
    type TraceItemTailEventInfo =
        abstract consumedEvents: ResizeArray<TraceItemTailEventInfoTailItem> with get

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRAGUnauthorizedError")>]
    type AutoRAGUnauthorizedError = interface end

    [<Import("@cloudflare/workers-types", "SendEmail")>]
    type SendEmail =
        abstract send: message: EmailMessage -> Promise<EmailSendResult>
        abstract send: builder: SendEmail.Send.Builder -> Promise<EmailSendResult>

    [<Import("@cloudflare/workers-types", "workerdResourceLimits")>]
    type WorkerdResourceLimits =
        abstract subRequests: option<float> with get, set
        abstract cpuMs: option<float> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionContentPartFile")>]
    type ChatCompletionContentPartFile =
        abstract file: SharedLiterals.FileDataFileIdFilename with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "AiAutomaticSpeechRecognitionInput")>]
    type AiAutomaticSpeechRecognitionInput =
        abstract audio: AiSentenceSimilarityOutput with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryGetSummaryResponse")>]
    type AgentMemoryGetSummaryResponse =
        abstract summary: string with get, set

    [<Import("@cloudflare/workers-types", "Transformer")>]
    type Transformer<'I, 'O> =
        abstract expectedLength: option<float> with get, set
        abstract writableType: option<string> with get, set
        abstract readableType: option<string> with get, set
        abstract start: controller: TransformStreamDefaultController<'O> -> option<Promise<unit>>
        abstract transform: chunk: 'I * controller: TransformStreamDefaultController<'O> -> option<Promise<unit>>
        abstract flush: controller: TransformStreamDefaultController<'O> -> option<Promise<unit>>
        abstract cancel: ?reason: obj -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "Fetcher")>]
    type Fetcher<'T, 'Reserved> =
        abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
        abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract Item: key: string -> option<obj>
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
        abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    [<Import("@cloudflare/workers-types", "AiGatewayLogNotFound")>]
    type AiGatewayLogNotFound = interface end

    [<Import("@cloudflare/workers-types", "TraceItemHibernatableWebSocketEventInfoError")>]
    type TraceItemHibernatableWebSocketEventInfoError =
        abstract webSocketEventType: string with get

    type EmailAttachment = U2<SharedLiterals.ContentContentIdDispositio8d6362d4, SharedLiterals.ContentContentIdDispositio8d6362d42>

    [<Import("@cloudflare/workers-types", "AgentMemoryMemoryListEntry")>]
    type AgentMemoryMemoryListEntry = interface end

    [<Import("@cloudflare/workers-types", "StreamPipeOptions")>]
    type StreamPipeOptions =
        abstract signal: option<AbortSignal> with get, set
        abstract preventClose: option<bool> with get, set
        abstract preventCancel: option<bool> with get, set
        abstract preventAbort: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "BaseAiImageClassification")>]
    type BaseAiImageClassification =
        abstract postProcessedOutputs: AiImageClassificationOutput with get, set
        abstract inputs: AiImageClassificationInput with get, set

    [<Import("@cloudflare/workers-types", "AiSearchInternalError")>]
    type AiSearchInternalError = interface end

    [<Import("@cloudflare/workers-types", "R2HTTPMetadata")>]
    type R2HTTPMetadata =
        abstract cacheExpiry: option<Date> with get, set
        abstract cacheControl: option<string> with get, set
        abstract contentEncoding: option<string> with get, set
        abstract contentDisposition: option<string> with get, set
        abstract contentLanguage: option<string> with get, set
        abstract contentType: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ExtendableEvent")>]
    type ExtendableEvent =
        inherit Event
        abstract waitUntil: promise: Promise<option<obj>> -> unit

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Prompt")>]
    type AiCfQwenQwen330BA3BFp8Prompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfQwenQwen330BA3BFp8JSONMode> with get, set

        abstract lora: option<string> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchJobLogsParams")>]
    type AiSearchJobLogsParams =
        [<EmitProperty("per_page")>]
        abstract perPage: option<float> with get, set

        abstract page: option<float> with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesVaryAcceptLanguageHeader")>]
    type RequestInitCfPropertiesVaryAcceptLanguageHeader =
        inherit RequestInitCfPropertiesVaryHeader
        abstract languages: option<ResizeArray<string>> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Baai_Bge_Large_En_V1_5")>]
    type BaseAiCfBaaiBgeLargeEnV15 =
        abstract postProcessedOutputs: AiCfBaaiBgeLargeEnV15Output with get, set
        abstract inputs: AiCfBaaiBgeBaseEnV15Input with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Openai_Gpt_Oss_20B")>]
    type BaseAiCfOpenaiGptOss20B =
        abstract postProcessedOutputs: U2<BaseAiCfOpenaiGptOss20B.PostProcessedOutputs, BaseAiCfOpenaiGptOss20B.PostProcessedOutputs.Case2> with get, set
        abstract inputs: U2<BaseAiCfOpenaiGptOss20B.Inputs, BaseAiCfOpenaiGptOss20B.Inputs.Case2> with get, set

    type AiCfAisingaporeGemmaSeaLionV427BItInput = U3<AiCfAisingaporeGemmaSeaLionV427BItPrompt, AiCfAisingaporeGemmaSeaLionV427BItMessages, AiCfAisingaporeGemmaSeaLionV427BItAsyncBatch>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_Reranker_Base_Output")>]
    type AiCfBaaiBgeRerankerBaseOutput =
        abstract response: option<ResizeArray<SharedLiterals.IdScore>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Openai_Whisper_Large_V3_Turbo_Input")>]
    type AiCfOpenaiWhisperLargeV3TurboInput =
        [<EmitProperty("hallucination_silence_threshold")>]
        abstract hallucinationSilenceThreshold: option<float> with get, set

        [<EmitProperty("log_prob_threshold")>]
        abstract logProbThreshold: option<float> with get, set

        [<EmitProperty("compression_ratio_threshold")>]
        abstract compressionRatioThreshold: option<float> with get, set

        [<EmitProperty("no_speech_threshold")>]
        abstract noSpeechThreshold: option<float> with get, set

        [<EmitProperty("condition_on_previous_text")>]
        abstract conditionOnPreviousText: option<bool> with get, set

        [<EmitProperty("beam_size")>]
        abstract beamSize: option<float> with get, set

        abstract prefix: option<string> with get, set

        [<EmitProperty("initial_prompt")>]
        abstract initialPrompt: option<string> with get, set

        [<EmitProperty("vad_filter")>]
        abstract vadFilter: option<bool> with get, set

        abstract language: option<string> with get, set
        abstract task: option<string> with get, set
        abstract audio: U2<SharedLiterals.BodyContentType, string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_Embedding_0_6B_Input")>]
    type AiCfQwenQwen3Embedding06BInput =
        abstract text: option<U2<ResizeArray<string>, string>> with get, set
        abstract documents: option<U2<ResizeArray<string>, string>> with get, set
        abstract instruction: option<string> with get, set
        abstract queries: option<U2<ResizeArray<string>, string>> with get, set

    [<Import("@cloudflare/workers-types", "PubSubMessage")>]
    type PubSubMessage =
        abstract payload: U2<Uint8Array, string> with get, set
        abstract payloadFormatIndicator: float with get
        abstract contentType: string with get
        abstract receivedAt: float with get
        abstract jti: option<string> with get
        abstract clientId: string with get
        abstract topic: string with get
        abstract broker: string with get
        abstract mid: float with get

    [<Import("@cloudflare/workers-types", "RateLimitedError")>]
    type RateLimitedError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "StreamDirectUpload")>]
    type StreamDirectUpload =
        abstract scheduledDeletion: option<string> with get, set
        abstract watermark: option<StreamWatermark> with get, set
        abstract id: string with get, set
        abstract uploadURL: string with get, set

    [<Import("@cloudflare/workers-types", "DecompressionStream")>]
    type DecompressionStream =
        interface
            [<EmitConstructor>]
            abstract Create: format: LiteralUnions.DeflateDeflateRawGzip -> DecompressionStream

            inherit TransformStream<BufferSource, Uint8Array>
        end

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen2_5_Coder_32B_Instruct_Output")>]
    type AiCfQwenQwen25Coder32BInstructOutput =
        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract response: string with get, set

    type AiTextToSpeechOutput = U2<Uint8Array, SharedLiterals.Audio2>

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ResponseIncludable =
        | [<CompiledName("message.input_image.image_url")>] MessageInputImageImageUrl
        | [<CompiledName("message.output_text.logprobs")>] MessageOutputTextLogprobs

    type HeadersInit = U3<Headers, seq<seq<string>>, obj>

    [<Import("@cloudflare/workers-types", "EmailAddress")>]
    type EmailAddress =
        abstract email: string with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Aura_2_Es_Input")>]
    type AiCfDeepgramAura2EsInput =
        [<EmitProperty("bit_rate")>]
        abstract bitRate: option<float> with get, set

        [<EmitProperty("sample_rate")>]
        abstract sampleRate: option<float> with get, set

        abstract text: string with get, set
        abstract container: option<LiteralUnions.NoneOggWav> with get, set
        abstract encoding: option<LiteralUnions.AacAlawFlacLinear16Mp3MulawOpus> with get, set
        abstract speaker: option<LiteralUnions.AlvaroAquilaCarina8376d988> with get, set

    [<Import("@cloudflare/workers-types", "HTMLRewriterDocumentContentHandlers")>]
    type HTMLRewriterDocumentContentHandlers =
        abstract doctype: doctype: Doctype -> option<Promise<unit>>
        abstract comments: comment: Comment -> option<Promise<unit>>
        abstract text: text: Text -> option<Promise<unit>>
        abstract ``end``: ``end``: DocumentEnd -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "AiImageToTextOutput")>]
    type AiImageToTextOutput =
        abstract description: string with get, set

    [<Import("@cloudflare/workers-types", "WorkerLoaderWorkerCode")>]
    type WorkerLoaderWorkerCode =
        abstract streamingTails: option<ResizeArray<SharedLiterals.ConnectFetch>> with get, set
        abstract tails: option<ResizeArray<SharedLiterals.ConnectFetch>> with get, set
        abstract globalOutbound: option<SharedLiterals.ConnectFetch> with get, set
        abstract env: option<obj> with get, set
        abstract modules: obj with get, set
        abstract mainModule: string with get, set
        abstract limits: option<WorkerdResourceLimits> with get, set
        abstract allowExperimental: option<bool> with get, set
        abstract compatibilityFlags: option<ResizeArray<string>> with get, set
        abstract compatibilityDate: string with get, set

    [<Import("@cloudflare/workers-types", "D1DatabaseSession")>]
    type D1DatabaseSession =
        abstract prepare: query: string -> D1PreparedStatement
        abstract batch: statements: ResizeArray<D1PreparedStatement> -> Promise<ResizeArray<D1DatabaseSession.Batch>>
        abstract getBookmark: unit -> option<string>

    [<Import("@cloudflare/workers-types", "ImageTransformationResult")>]
    type ImageTransformationResult =
        abstract response: unit -> Response
        abstract contentType: unit -> string
        abstract image: ?options: ImageTransformationOutputOptions -> ReadableStream<Uint8Array>

    [<Import("@cloudflare/workers-types", "StreamVideosListParams")>]
    type StreamVideosListParams =
        abstract afterComp: option<StreamPaginationComparison> with get, set
        abstract after: option<string> with get, set
        abstract beforeComp: option<StreamPaginationComparison> with get, set
        abstract before: option<string> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchChatCompletionsResponse")>]
    type AiSearchChatCompletionsResponse =
        abstract chunks: ResizeArray<SharedLiterals.IdItemScoreScoringDetailsTextType> with get, set
        abstract choices: ResizeArray<SharedLiterals.IndexMessage> with get, set
        abstract model: option<string> with get, set
        abstract object: option<string> with get, set
        abstract id: option<string> with get, set
        abstract Item: key: string -> option<obj>

    type ChatCompletionMessageParam = U6<DeveloperMessage, SystemMessage, UserMessage, AssistantMessage, ToolMessage, FunctionMessage>

    [<Import("@cloudflare/workers-types", "StreamVideos")>]
    type StreamVideos =
        abstract list: ?params: StreamVideosListParams -> Promise<ResizeArray<StreamVideo>>

    [<Import("@cloudflare/workers-types", "ExportedHandler")>]
    type ExportedHandler<'Env, 'QueueHandlerMessage, 'CfHostMetadata, 'Props> =
        abstract queue: (option<MessageBatch<obj> -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract email: (option<ForwardableEmailMessage -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract test: (option<TestController -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract scheduled: (option<ScheduledController -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract tailStream: (option<TailStream.TailEvent<TailStream.Onset> -> 'Env -> ExecutionContext<'Props> -> U3<TailStream.TailEvent<obj> -> option<Promise<unit>>, TailStream.TailEventHandlerObject, Promise<TailStream.TailEventHandlerType>>>) with get, set
        abstract trace: (option<ResizeArray<TraceItem> -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract tail: (option<ResizeArray<TraceItem> -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract connect: (option<Socket -> 'Env -> ExecutionContext<'Props> -> option<Promise<unit>>>) with get, set
        abstract fetch: (option<Request<'CfHostMetadata, ExportedHandler.Fetch> -> 'Env -> ExecutionContext<'Props> -> U2<Response, Promise<Response>>>) with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type RequestInitCfPropertiesVaryAction =
        | [<CompiledName("normalize")>] Normalize
        | [<CompiledName("passthrough")>] Passthrough
        | [<CompiledName("bypass")>] Bypass

    type Service =
        abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
        abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract Item: key: string -> option<obj>
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
        abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    [<Import("@cloudflare/workers-types", "AiInternalError")>]
    type AiInternalError = interface end

    [<Import("@cloudflare/workers-types", "AiSearchMultiSearchError")>]
    type AiSearchMultiSearchError =
        abstract message: string with get, set

        [<EmitProperty("instance_id")>]
        abstract instanceId: string with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionCustomToolGrammarFormat")>]
    type ChatCompletionCustomToolGrammarFormat =
        abstract grammar: SharedLiterals.DefinitionSyntax with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Messages")>]
    type AiCfQwenQwen330BA3BFp8Messages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfQwenQwen330BA3BFp8JSONMode1> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRole> with get, set

    [<Import("@cloudflare/workers-types", "WorkflowInstanceRestartOptions")>]
    type WorkflowInstanceRestartOptions =
        abstract from: option<WorkflowInstanceRestartOptions.From> with get, set

    [<Import("@cloudflare/workers-types", "TextEncoder")>]
    type TextEncoder =
        [<EmitConstructor>]
        abstract Create: unit -> TextEncoder

        abstract encoding: string with get
        abstract encode: ?input: string -> Uint8Array
        abstract encodeInto: input: string * buffer: Uint8Array -> TextEncoderEncodeIntoResult

    [<Import("@cloudflare/workers-types", "AiTextGenerationToolOutput")>]
    type AiTextGenerationToolOutput =
        [<EmitProperty("function")>]
        abstract ``function``: SharedLiterals.ArgumentsName with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "KVNamespaceListOptions")>]
    type KVNamespaceListOptions =
        abstract cursor: option<string> with get, set
        abstract prefix: option<string> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "TraceMetrics")>]
    type TraceMetrics =
        abstract wallTime: float with get
        abstract cpuTime: float with get

    type RequestInfo<'CfHostMetadata, 'Cf> = U2<Request<'CfHostMetadata, 'Cf>, string>

    [<Import("@cloudflare/workers-types", "BaseAiImageTextToText")>]
    type BaseAiImageTextToText =
        abstract postProcessedOutputs: AiImageTextToTextOutput with get, set
        abstract inputs: AiImageTextToTextInput with get, set

    [<Import("@cloudflare/workers-types", "VectorizeVector")>]
    type VectorizeVector =
        abstract metadata: option<obj> with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: option<string> with get, set

        abstract values: U3<Float32Array, Float64Array, AiSentenceSimilarityOutput> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ReadableWritablePair")>]
    type ReadableWritablePair<'R, 'W> =
        abstract writable: WritableStream<'W> with get, set
        abstract readable: ReadableStream<'R> with get, set

    [<Import("@cloudflare/workers-types", "ResponseReasoningTextDeltaEvent")>]
    type ResponseReasoningTextDeltaEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        abstract delta: string with get, set

        [<EmitProperty("content_index")>]
        abstract contentIndex: float with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Qwen_Qwen3_Embedding_0_6B")>]
    type BaseAiCfQwenQwen3Embedding06B =
        abstract postProcessedOutputs: AiCfQwenQwen3Embedding06BOutput with get, set
        abstract inputs: AiCfQwenQwen3Embedding06BInput with get, set

    [<Import("@cloudflare/workers-types", "TraceItemCustomEventInfo")>]
    type TraceItemCustomEventInfo = interface end

    [<Import("@cloudflare/workers-types", "AgentMemoryListMemoriesOptions")>]
    type AgentMemoryListMemoriesOptions =
        [<EmitProperty("type")>]
        abstract ``type``: option<AgentMemoryMemoryType> with get, set

        abstract sessionId: option<string> with get, set
        abstract cursor: option<string> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "InternalError")>]
    type InternalError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseCompletedEvent")>]
    type ResponseCompletedEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        abstract response: Response with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type StreamDownloadType =
        | [<CompiledName("default")>] Default
        | [<CompiledName("audio")>] Audio

    [<Import("@cloudflare/workers-types", "QueueRetryOptions")>]
    type QueueRetryOptions =
        abstract delaySeconds: option<float> with get, set

    [<Import("@cloudflare/workers-types", "IdentityTransformStream")>]
    type IdentityTransformStream =
        interface
            [<EmitConstructor>]
            abstract Create: ?queuingStrategy: IdentityTransformStreamQueuingStrategy -> IdentityTransformStream

            inherit TransformStream<BufferSource, Uint8Array>
        end

    [<Import("@cloudflare/workers-types", "Reasoning")>]
    type Reasoning =
        abstract summary: option<LiteralUnions.AutoConciseDetailed> with get, set

        [<EmitProperty("generate_summary")>]
        abstract generateSummary: option<LiteralUnions.AutoConciseDetailed> with get, set

        abstract effort: option<LiteralUnions.HighLowMediumMinimal> with get, set

    [<Import("@cloudflare/workers-types", "ExecOutput")>]
    type ExecOutput =
        abstract exitCode: float with get
        abstract stderr: ArrayBuffer with get
        abstract stdout: ArrayBuffer with get

    [<Import("@cloudflare/workers-types", "ArtifactsError")>]
    type ArtifactsError =
        abstract numericCode: float with get
        abstract code: ArtifactsErrorCode with get
        abstract name: string with get

    [<Import("@cloudflare/workers-types", "AgentMemoryMessage")>]
    type AgentMemoryMessage =
        abstract timestamp: option<Date> with get, set
        abstract content: string with get, set
        abstract role: LiteralUnions.AssistantSystemUser with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Text_Completion_Response")>]
    type AiCfQwenQwen330BA3BFp8TextCompletionResponse =
        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract choices: option<ResizeArray<SharedLiterals.FinishReasonIndexFc31d0af>> with get, set
        abstract model: option<string> with get, set
        abstract created: option<float> with get, set
        abstract object: option<string> with get, set
        abstract id: option<string> with get, set

    [<Import("@cloudflare/workers-types", "EventInit")>]
    type EventInit =
        abstract composed: option<bool> with get, set
        abstract cancelable: option<bool> with get, set
        abstract bubbles: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "StreamWatermarks")>]
    type StreamWatermarks =
        abstract generate: input: ReadableStream<option<obj>> * params: StreamWatermarkCreateParams -> Promise<StreamWatermark>
        abstract generate: url: string * params: StreamWatermarkCreateParams -> Promise<StreamWatermark>
        abstract list: unit -> Promise<ResizeArray<StreamWatermark>>
        abstract get: watermarkId: string -> Promise<StreamWatermark>
        abstract delete: watermarkId: string -> Promise<unit>

    [<Import("@cloudflare/workers-types", "StreamCaption")>]
    type StreamCaption =
        abstract status: option<StreamDownloadStatus> with get, set
        abstract language: string with get, set
        abstract label: string with get, set
        abstract generated: option<bool> with get, set

    type AiSentenceSimilarityOutput = ResizeArray<float>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Prompt")>]
    type AiCfAisingaporeGemmaSeaLionV427BItPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfAisingaporeGemmaSeaLionV427BItJSONMode> with get, set

        abstract lora: option<string> with get, set
        abstract prompt: string with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type VectorizeVectorMetadataFilterCollectionOp =
        | [<CompiledName("$in")>] In
        | [<CompiledName("$nin")>] Nin

    [<Import("@cloudflare/workers-types", "ExportedHandlerFetchHandler")>]
    type ExportedHandlerFetchHandler =
        abstract Invoke: request: Request<obj, obj> * env: obj * ctx: ExecutionContext<obj> -> U2<Response, Promise<Response>>

    [<Import("@cloudflare/workers-types", "WorkflowError")>]
    type WorkflowError =
        abstract message: string with get, set
        abstract code: option<float> with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesExportedAuthenticatorMetadata")>]
    type IncomingRequestCfPropertiesExportedAuthenticatorMetadata =
        /// <example>
        /// "084ee802fe1348f688220e2a6040a05b2199a761f33cf753abb1b006792d3f8b"
        /// </example>
        abstract serverFinished: string with get, set
        /// <example>
        /// "084ee802fe1348f688220e2a6040a05b2199a761f33cf753abb1b006792d3f8b"
        /// </example>
        abstract clientFinished: string with get, set
        /// <example>
        /// "44372ba35fa1270921d318f34c12f155dc87b682cf36a790cfaa3ba8737a1b5d"
        /// </example>
        abstract serverHandshake: string with get, set
        /// <example>
        /// "44372ba35fa1270921d318f34c12f155dc87b682cf36a790cfaa3ba8737a1b5d"
        /// </example>
        abstract clientHandshake: string with get, set

    [<Import("@cloudflare/workers-types", "DurableObjectId")>]
    type DurableObjectId =
        abstract jurisdiction: option<string> with get
        abstract name: option<string> with get
        abstract toString: unit -> string
        abstract equals: other: DurableObjectId -> bool

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen2_5_Coder_32B_Instruct_JSON_Mode")>]
    type AiCfQwenQwen25Coder32BInstructJSONMode =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunContentSuccessResponse")>]
    type BrowserRunContentSuccessResponse =
        abstract meta: BrowserRunResponseMeta with get, set
        abstract result: string with get, set
        abstract success: bool with get, set

    type AiCfOpenaiWhisperInput = U2<AiAutomaticSpeechRecognitionInput, string>

    [<Import("@cloudflare/workers-types", "ResponseInputMessageContentList")>]
    type ResponseInputMessageContentList = interface end

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_Prompt_Inner")>]
    type AiCfMetaLlama4Scout17B16EInstructPromptInner =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlama4Scout17B16EInstructJSONMode> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "R2StringChecksums")>]
    type R2StringChecksums =
        abstract sha512: option<string> with get, set
        abstract sha384: option<string> with get, set
        abstract sha256: option<string> with get, set
        abstract sha1: option<string> with get, set
        abstract md5: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ImagesError")>]
    type ImagesError =
        abstract stack: option<string> with get
        abstract message: string with get
        abstract code: float with get

    type VectorFloatArray = U2<Float32Array, Float64Array>

    [<Import("@cloudflare/workers-types", "VectorizeIndexInfo")>]
    type VectorizeIndexInfo =
        abstract processedUpToMutation: float with get, set
        abstract processedUpToDatetime: float with get, set
        abstract dimensions: float with get, set
        abstract vectorCount: float with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_JSON_Mode_1")>]
    type AiCfAisingaporeGemmaSeaLionV427BItJSONMode1 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionsCommonOptions")>]
    type ChatCompletionsCommonOptions =
        abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

        [<EmitProperty("function_call")>]
        abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

        [<EmitProperty("web_search_options")>]
        abstract webSearchOptions: option<WebSearchOptions> with get, set

        abstract user: option<string> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

        [<EmitProperty("tool_choice")>]
        abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("stream_options")>]
        abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

        abstract stream: option<bool> with get, set
        abstract store: option<bool> with get, set
        abstract stop: option<U2<ResizeArray<string>, string>> with get, set

        [<EmitProperty("service_tier")>]
        abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<ResponseFormat> with get, set

        [<EmitProperty("chat_template_kwargs")>]
        abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

        [<EmitProperty("reasoning_effort")>]
        abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        abstract prediction: option<PredictionContent> with get, set

        [<EmitProperty("parallel_tool_calls")>]
        abstract parallelToolCalls: option<bool> with get, set

        abstract n: option<float> with get, set
        abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
        abstract metadata: option<obj> with get, set

        [<EmitProperty("max_completion_tokens")>]
        abstract maxCompletionTokens: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        [<EmitProperty("top_logprobs")>]
        abstract topLogprobs: option<float> with get, set

        abstract logprobs: option<bool> with get, set

        [<EmitProperty("logit_bias")>]
        abstract logitBias: option<obj> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        abstract audio: option<AudioParams> with get, set
        abstract model: option<string> with get, set

    [<Import("@cloudflare/workers-types", "DispatchNamespace")>]
    type DispatchNamespace =
        abstract get: name: string * ?args: obj * ?options: DynamicDispatchOptions -> SharedLiterals.ConnectFetch

    [<Import("@cloudflare/workers-types", "ContainerStartupOptions")>]
    type ContainerStartupOptions =
        abstract containerSnapshot: option<ContainerSnapshot> with get, set
        abstract directorySnapshots: option<ResizeArray<ContainerDirectorySnapshotRestoreParams>> with get, set
        abstract labels: option<obj> with get, set
        abstract env: option<obj> with get, set
        abstract enableInternet: bool with get, set
        abstract entrypoint: option<ResizeArray<string>> with get, set

    [<Import("@cloudflare/workers-types", "SqlStorageCursor")>]
    type SqlStorageCursor<'T> =
        abstract rowsWritten: float with get
        abstract rowsRead: float with get
        abstract columnNames: ResizeArray<string> with get, set
        abstract next: unit -> U2<SqlStorageCursor.Next, SqlStorageCursor.Next.Case2>
        abstract toArray: unit -> ResizeArray<'T>
        abstract one: unit -> 'T
        abstract raw: unit -> seq<obj>
        abstract ``[symbol.iterator]``: unit -> seq<'T>

    [<Import("@cloudflare/workers-types", "AiTextGenerationOutput")>]
    type AiTextGenerationOutput =
        abstract usage: option<UsageTags> with get, set

        [<EmitProperty("tool_calls")>]
        abstract toolCalls: option<AiTextGenerationOutput.ToolCalls> with get, set

        abstract response: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ReadableStreamBYOBReaderReadableStreamBYOBReaderReadOptions")>]
    type ReadableStreamBYOBReaderReadableStreamBYOBReaderReadOptions =
        abstract min: option<float> with get, set

    [<Import("@cloudflare/workers-types", "RateLimit")>]
    type RateLimit =
        abstract limit: options: RateLimitOptions -> Promise<RateLimitOutcome>

    [<Import("@cloudflare/workers-types", "ResponseInit")>]
    type ResponseInit =
        abstract encodeBody: option<LiteralUnions.AutomaticManual> with get, set
        abstract webSocket: option<WebSocket> with get, set
        abstract cf: option<obj> with get, set
        abstract headers: option<HeadersInit> with get, set
        abstract statusText: option<string> with get, set
        abstract status: option<float> with get, set

    [<Import("@cloudflare/workers-types", "MessageBatch")>]
    type MessageBatch<'Body> =
        abstract metadata: MessageBatchMetadata with get
        abstract queue: string with get
        abstract messages: System.Collections.Generic.IReadOnlyList<Message<'Body>> with get
        abstract retryAll: ?options: QueueRetryOptions -> unit
        abstract ackAll: unit -> unit

    [<Import("@cloudflare/workers-types", "CachePurgeOptions")>]
    type CachePurgeOptions =
        abstract purgeEverything: option<bool> with get, set
        abstract pathPrefixes: option<ResizeArray<string>> with get, set
        abstract tags: option<ResizeArray<string>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Google_Gemma_3_12B_It_Prompt")>]
    type AiCfGoogleGemma312BItPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "URLPatternResult")>]
    type URLPatternResult =
        abstract hash: URLPatternComponentResult with get, set
        abstract search: URLPatternComponentResult with get, set
        abstract pathname: URLPatternComponentResult with get, set
        abstract port: URLPatternComponentResult with get, set
        abstract hostname: URLPatternComponentResult with get, set
        abstract password: URLPatternComponentResult with get, set
        abstract username: URLPatternComponentResult with get, set
        abstract protocol: URLPatternComponentResult with get, set
        abstract inputs: ResizeArray<U2<URLPatternInit, string>> with get, set

    [<Import("@cloudflare/workers-types", "BaseAiSentenceSimilarity")>]
    type BaseAiSentenceSimilarity =
        abstract postProcessedOutputs: AiSentenceSimilarityOutput with get, set
        abstract inputs: AiSentenceSimilarityInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_2_Klein_9B_Input")>]
    type AiCfBlackForestLabsFlux2Klein9BInput =
        abstract multipart: SharedLiterals.BodyContentType with get, set

    [<Import("@cloudflare/workers-types", "QueueSendOptions")>]
    type QueueSendOptions =
        abstract delaySeconds: option<float> with get, set
        abstract contentType: option<QueueContentType> with get, set

    [<Import("@cloudflare/workers-types", "ResponseInputMessageItem")>]
    type ResponseInputMessageItem =
        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
        abstract role: LiteralUnions.DeveloperSystemUser with get, set
        abstract content: ResponseInputMessageContentList with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Google_Gemma_4_26B_A4B_IT")>]
    type BaseAiCfGoogleGemma426BA4BIT =
        abstract postProcessedOutputs: ChatCompletionsOutput with get, set
        abstract inputs: ChatCompletionsInput with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionCustomToolTextFormat")>]
    type ChatCompletionCustomToolTextFormat =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItem")>]
    type AiSearchItem =
        abstract info: unit -> Promise<AiSearchItemInfo>
        abstract download: unit -> Promise<AiSearchItemContentResult>
        abstract sync: unit -> Promise<AiSearchItemInfo>
        abstract logs: ?params: AiSearchItemLogsParams -> Promise<AiSearchItemLogsResponse>
        abstract chunks: ?params: AiSearchItemChunksParams -> Promise<AiSearchItemChunksResponse>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_JSON_Mode")>]
    type AiCfAisingaporeGemmaSeaLionV427BItJSONMode =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunScreenshotOptions")>]
    type BrowserRunScreenshotOptions =
        abstract screenshotOptions: option<BrowserRunPuppeteerScreenshotOptions> with get, set
        abstract scrollPage: option<bool> with get, set
        abstract selector: option<string> with get, set

    [<Import("@cloudflare/workers-types", "SubtleCryptoHashAlgorithm")>]
    type SubtleCryptoHashAlgorithm =
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "QueueSendMetadata")>]
    type QueueSendMetadata =
        abstract metrics: QueueSendMetrics with get, set

    [<Import("@cloudflare/workers-types", "R2ObjectBody")>]
    type R2ObjectBody =
        inherit R2Object
        abstract bodyUsed: bool with get
        abstract body: ReadableStream<option<obj>> with get
        abstract arrayBuffer: unit -> Promise<ArrayBuffer>
        abstract bytes: unit -> Promise<Uint8Array>
        abstract text: unit -> Promise<string>
        abstract json: unit -> Promise<obj>
        abstract blob: unit -> Promise<Blob>

    [<Import("@cloudflare/workers-types", "AiSearchUploadItemOptions")>]
    type AiSearchUploadItemOptions =
        abstract metadata: option<obj> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_Messages")>]
    type AiCfMetaLlama4Scout17B16EInstructMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlama4Scout17B16EInstructJSONMode> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRoleToolCallId> with get, set

    type AiCfMistralaiMistralSmall3124BInstructInput = U2<AiCfMistralaiMistralSmall3124BInstructPrompt, AiCfMistralaiMistralSmall3124BInstructMessages>
    type AiTextClassificationOutput = ResizeArray<SharedLiterals.LabelScore>

    [<Import("@cloudflare/workers-types", "CryptoKeyRsaKeyAlgorithm")>]
    type CryptoKeyRsaKeyAlgorithm =
        abstract hash: option<CryptoKeyKeyAlgorithm> with get, set
        abstract publicExponent: BufferSource with get, set
        abstract modulusLength: float with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Openai_Whisper_Large_V3_Turbo")>]
    type BaseAiCfOpenaiWhisperLargeV3Turbo =
        abstract postProcessedOutputs: AiCfOpenaiWhisperLargeV3TurboOutput with get, set
        abstract inputs: AiCfOpenaiWhisperLargeV3TurboInput with get, set

    [<Import("@cloudflare/workers-types", "ResponseTextDoneEvent")>]
    type ResponseTextDoneEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        abstract logprobs: ResizeArray<Logprob> with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        [<EmitProperty("content_index")>]
        abstract contentIndex: float with get, set

    [<Import("@cloudflare/workers-types", "SubtleCrypto")>]
    type SubtleCrypto =
        abstract encrypt: algorithm: U2<SubtleCryptoEncryptAlgorithm, string> * key: CryptoKey * plainText: BufferSource -> Promise<ArrayBuffer>
        abstract decrypt: algorithm: U2<SubtleCryptoEncryptAlgorithm, string> * key: CryptoKey * cipherText: BufferSource -> Promise<ArrayBuffer>
        abstract sign: algorithm: U2<SubtleCryptoSignAlgorithm, string> * key: CryptoKey * data: BufferSource -> Promise<ArrayBuffer>
        abstract verify: algorithm: U2<SubtleCryptoSignAlgorithm, string> * key: CryptoKey * signature: BufferSource * data: BufferSource -> Promise<bool>
        abstract digest: algorithm: U2<SubtleCryptoHashAlgorithm, string> * data: BufferSource -> Promise<ArrayBuffer>
        abstract generateKey: algorithm: U2<SubtleCryptoGenerateKeyAlgorithm, string> * extractable: bool * keyUsages: ResizeArray<string> -> Promise<U2<CryptoKey, CryptoKeyPair>>
        abstract deriveKey: algorithm: U2<SubtleCryptoDeriveKeyAlgorithm, string> * baseKey: CryptoKey * derivedKeyAlgorithm: U2<SubtleCryptoImportKeyAlgorithm, string> * extractable: bool * keyUsages: ResizeArray<string> -> Promise<CryptoKey>
        abstract deriveBits: algorithm: U2<SubtleCryptoDeriveKeyAlgorithm, string> * baseKey: CryptoKey * ?length: float -> Promise<ArrayBuffer>
        abstract importKey: format: string * keyData: U3<ArrayBuffer, obj, JsonWebKey> * algorithm: U2<SubtleCryptoImportKeyAlgorithm, string> * extractable: bool * keyUsages: ResizeArray<string> -> Promise<CryptoKey>
        abstract exportKey: format: string * key: CryptoKey -> Promise<U2<ArrayBuffer, JsonWebKey>>
        abstract wrapKey: format: string * key: CryptoKey * wrappingKey: CryptoKey * wrapAlgorithm: U2<SubtleCryptoEncryptAlgorithm, string> -> Promise<ArrayBuffer>
        abstract unwrapKey: format: string * wrappedKey: BufferSource * unwrappingKey: CryptoKey * unwrapAlgorithm: U2<SubtleCryptoEncryptAlgorithm, string> * unwrappedKeyAlgorithm: U2<SubtleCryptoImportKeyAlgorithm, string> * extractable: bool * keyUsages: ResizeArray<string> -> Promise<CryptoKey>
        abstract timingSafeEqual: a: BufferSource * b: BufferSource -> bool

    [<Import("@cloudflare/workers-types", "KVNamespaceGetOptions")>]
    type KVNamespaceGetOptions<'Type> =
        abstract cacheTtl: option<float> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: 'Type with get, set

    [<Import("@cloudflare/workers-types", "EventPluginContext")>]
    type EventPluginContext =
        abstract pluginArgs: obj with get, set
        abstract data: obj with get, set
        abstract params: obj with get, set
        abstract env: EventPluginContext.Env with get, set
        abstract functionPath: string with get, set
        abstract request: Request<option<obj>, obj> with get, set
        abstract waitUntil: promise: Promise<option<obj>> -> unit
        abstract passThroughOnException: unit -> unit
        abstract next: ?input: U2<Request<option<obj>, U2<RequestInitCfProperties, obj>>, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>

    [<Import("@cloudflare/workers-types", "EventListenerObject")>]
    type EventListenerObject<'EventType> =
        abstract handleEvent: event: 'EventType -> unit

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_Messages")>]
    type AiCfMetaLlama3370BInstructFp8FastMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlama3370BInstructFp8FastJSONMode1> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRole> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchListJobsResponse")>]
    type AiSearchListJobsResponse =
        [<EmitProperty("result_info")>]
        abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

        abstract result: ResizeArray<AiSearchJobInfo> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Leonardo_Phoenix_1_0")>]
    type BaseAiCfLeonardoPhoenix10 =
        abstract postProcessedOutputs: string with get, set
        abstract inputs: AiCfLeonardoPhoenix10Input with get, set

    [<Import("@cloudflare/workers-types", "ImageTransformer")>]
    type ImageTransformer =
        abstract transform: transform: ImageTransform -> ImageTransformer
        abstract draw: image: U2<ReadableStream<Uint8Array>, ImageTransformer> * ?options: ImageDrawOptions -> ImageTransformer
        abstract output: options: ImageOutputOptions -> Promise<ImageTransformationResult>

    [<Import("@cloudflare/workers-types", "ArtifactsCreateTokenResult")>]
    type ArtifactsCreateTokenResult =
        abstract expiresAt: string with get, set
        abstract scope: LiteralUnions.ReadWrite with get, set
        abstract plaintext: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "TraceException")>]
    type TraceException =
        abstract stack: option<string> with get
        abstract name: string with get
        abstract message: string with get
        abstract timestamp: float with get

    [<Import("@cloudflare/workers-types", "AiTranslationInput")>]
    type AiTranslationInput =
        [<EmitProperty("source_lang")>]
        abstract sourceLang: option<string> with get, set

        [<EmitProperty("target_lang")>]
        abstract targetLang: string with get, set

        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "TailEvent")>]
    type TailEvent =
        inherit ExtendableEvent
        abstract traces: ResizeArray<TraceItem> with get
        abstract events: ResizeArray<TraceItem> with get

    [<Import("@cloudflare/workers-types", "Scheduler")>]
    type Scheduler =
        abstract wait: delay: float * ?maybeOptions: SchedulerWaitOptions -> Promise<unit>

    [<Import("@cloudflare/workers-types", "R2Conditional")>]
    type R2Conditional =
        abstract secondsGranularity: option<bool> with get, set
        abstract uploadedAfter: option<Date> with get, set
        abstract uploadedBefore: option<Date> with get, set
        abstract etagDoesNotMatch: option<string> with get, set
        abstract etagMatches: option<string> with get, set

    [<Import("@cloudflare/workers-types", "DurableObjectClass")>]
    type DurableObjectClass<'_T> = interface end

    [<Import("@cloudflare/workers-types", "FlagshipEvaluationError")>]
    type FlagshipEvaluationError = interface end

    [<Import("@cloudflare/workers-types", "MessageEvent")>]
    type MessageEvent =
        [<EmitConstructor>]
        abstract Create: ``type``: string * initializer: MessageEventInit -> MessageEvent

        inherit Event
        abstract ports: ResizeArray<MessagePort> with get
        abstract source: option<MessagePort> with get
        abstract lastEventId: string with get
        abstract origin: option<string> with get
        abstract data: option<obj> with get

    [<Import("@cloudflare/workers-types", "WebSearchSearchResponse")>]
    type WebSearchSearchResponse =
        abstract metadata: WebSearchResponseMetadata with get, set
        abstract items: ResizeArray<WebSearchResult> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunSnapshotSuccessResponse")>]
    type BrowserRunSnapshotSuccessResponse =
        abstract meta: BrowserRunResponseMeta with get, set
        abstract result: BrowserRunSnapshotSuccessResponse.Result with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "RequestInitCfProperties")>]
    type RequestInitCfProperties =
        abstract resolveOverride: option<string> with get, set
        abstract r2: option<RequestInitCfPropertiesR2> with get, set
        abstract polish: option<LiteralUnions.LosslessLossyOff> with get, set
        abstract mirage: option<bool> with get, set
        abstract minify: option<RequestInitCfPropertiesImageMinify> with get, set
        abstract image: option<RequestInitCfPropertiesImage> with get, set
        abstract grpcWeb: option<LiteralUnions.ConvertPassthrough> with get, set
        abstract apps: option<bool> with get, set
        abstract scrapeShield: option<bool> with get, set
        abstract cacheReserveMinimumFileSize: option<float> with get, set
        abstract cacheDeceptionArmor: option<bool> with get, set
        abstract stripLastModified: option<bool> with get, set
        abstract stripEtags: option<bool> with get, set
        abstract respectStrongEtag: option<bool> with get, set
        abstract cacheReserveEligible: option<bool> with get, set
        abstract cacheControl: option<string> with get, set
        abstract vary: option<RequestInitCfPropertiesVary> with get, set
        abstract cacheTtlByStatus: option<obj> with get, set
        abstract cacheTtl: option<float> with get, set
        abstract cacheTags: option<ResizeArray<string>> with get, set
        abstract cacheKey: option<string> with get, set
        abstract cacheEverything: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "ReadableStreamBYOBReader")>]
    type ReadableStreamBYOBReader =
        [<EmitConstructor>]
        abstract Create: stream: ReadableStream<option<obj>> -> ReadableStreamBYOBReader

        abstract closed: Promise<unit> with get
        abstract cancel: ?reason: obj -> Promise<unit>
        abstract read: view: obj -> Promise<U2<SharedLiterals.DoneValue, ReadableStreamBYOBReader.Read>>
        abstract releaseLock: unit -> unit
        abstract readAtLeast: minElements: float * view: obj -> Promise<U2<SharedLiterals.DoneValue, obj>>

    [<Import("@cloudflare/workers-types", "Message")>]
    type Message<'Body> =
        abstract attempts: float with get
        abstract body: 'Body with get
        abstract timestamp: Date with get
        abstract id: string with get
        abstract retry: ?options: QueueRetryOptions -> unit
        abstract ack: unit -> unit

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_2_11B_Vision_Instruct_Prompt")>]
    type AiCfMetaLlama3211BVisionInstructPrompt =
        abstract lora: option<string> with get, set

        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set
        abstract image: option<U2<AiSentenceSimilarityOutput, AiCfMetaLlama3211BVisionInstructPrompt.Image>> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_2_Dev_Output")>]
    type AiCfBlackForestLabsFlux2DevOutput =
        abstract image: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ErrorEvent")>]
    type ErrorEvent =
        [<EmitConstructor>]
        abstract Create: ``type``: string * ?init: ErrorEventErrorEventInit -> ErrorEvent

        inherit Event
        abstract error: option<obj> with get
        abstract colno: float with get
        abstract lineno: float with get
        abstract message: string with get
        abstract filename: string with get

    type EmailExportedHandler = ForwardableEmailMessage -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "DurableObjectListOptions")>]
    type DurableObjectListOptions =
        abstract noCache: option<bool> with get, set
        abstract allowConcurrency: option<bool> with get, set
        abstract limit: option<float> with get, set
        abstract reverse: option<bool> with get, set
        abstract prefix: option<string> with get, set

        [<EmitProperty("end")>]
        abstract ``end``: option<string> with get, set

        abstract startAfter: option<string> with get, set
        abstract start: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchInstanceInfo")>]
    type AiSearchInstanceInfo =
        abstract metadata: option<obj> with get, set

        [<EmitProperty("sync_interval")>]
        abstract syncInterval: option<LiteralUnions.I14400I21600I3600I43200I7200I86400> with get, set

        [<EmitProperty("custom_metadata")>]
        abstract customMetadata: option<ResizeArray<SharedLiterals.DataTypeFieldName>> with get, set

        [<EmitProperty("cache_threshold")>]
        abstract cacheThreshold: option<LiteralUnions.AnythingGoesClose50b421b1> with get, set

        abstract cache: option<bool> with get, set

        [<EmitProperty("max_num_results")>]
        abstract maxNumResults: option<float> with get, set

        [<EmitProperty("score_threshold")>]
        abstract scoreThreshold: option<float> with get, set

        [<EmitProperty("chunk_overlap")>]
        abstract chunkOverlap: option<float> with get, set

        [<EmitProperty("chunk_size")>]
        abstract chunkSize: option<float> with get, set

        abstract chunk: option<bool> with get, set

        [<EmitProperty("retrieval_options")>]
        abstract retrievalOptions: option<SharedLiterals.BoostByKeywordMatchMode> with get, set

        [<EmitProperty("indexing_options")>]
        abstract indexingOptions: option<SharedLiterals.KeywordTokenizer> with get, set

        [<EmitProperty("fusion_method")>]
        abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

        [<EmitProperty("index_method")>]
        abstract indexMethod: option<SharedLiterals.KeywordVector> with get, set

        /// <deprecated>
        /// Use index_method instead.
        /// </deprecated>
        [<EmitProperty("hybrid_search_enabled")>]
        abstract hybridSearchEnabled: option<bool> with get, set

        [<EmitProperty("reranking_model")>]
        abstract rerankingModel: option<string> with get, set

        [<EmitProperty("rewrite_model")>]
        abstract rewriteModel: option<string> with get, set

        [<EmitProperty("ai_search_model")>]
        abstract aiSearchModel: option<string> with get, set

        [<EmitProperty("embedding_model")>]
        abstract embeddingModel: option<string> with get, set

        abstract reranking: option<bool> with get, set

        [<EmitProperty("rewrite_query")>]
        abstract rewriteQuery: option<bool> with get, set

        [<EmitProperty("ai_gateway_id")>]
        abstract aiGatewayId: option<string> with get, set

        [<EmitProperty("token_id")>]
        abstract tokenId: option<string> with get, set

        [<EmitProperty("modified_at")>]
        abstract modifiedAt: option<string> with get, set

        [<EmitProperty("created_at")>]
        abstract createdAt: option<string> with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: option<string> with get, set

        abstract status: option<string> with get, set
        abstract paused: option<bool> with get, set

        [<EmitProperty("source_params")>]
        abstract sourceParams: option<obj> with get, set

        abstract source: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<U2<LiteralUnions.R2WebCrawler, string>> with get, set

        abstract id: string with get, set
        abstract Item: key: string -> option<obj>

    type AiCfBaaiBgeSmallEnV15Output = U2<SharedLiterals.DataPoolingShape, AiCfBaaiBgeSmallEnV15AsyncResponse>

    [<Import("@cloudflare/workers-types", "ServiceWorkerGlobalScope")>]
    type ServiceWorkerGlobalScope =
        inherit WorkerGlobalScope

        [<EmitProperty("HTMLRewriter")>]
        abstract hTMLRewriter: ServiceWorkerGlobalScope.HTMLRewriter with get, set

        [<EmitProperty("IdentityTransformStream")>]
        abstract identityTransformStream: ServiceWorkerGlobalScope.IdentityTransformStream with get, set

        [<EmitProperty("FixedLengthStream")>]
        abstract fixedLengthStream: ServiceWorkerGlobalScope.FixedLengthStream with get, set

        [<EmitProperty("Cache")>]
        abstract cache: ServiceWorkerGlobalScope.Cache with get, set

        [<EmitProperty("CacheStorage")>]
        abstract cacheStorage: ServiceWorkerGlobalScope.CacheStorage with get, set

        [<EmitProperty("CryptoKey")>]
        abstract cryptoKey: ServiceWorkerGlobalScope.CryptoKey with get, set

        [<EmitProperty("SubtleCrypto")>]
        abstract subtleCrypto: ServiceWorkerGlobalScope.SubtleCrypto with get, set

        [<EmitProperty("FormData")>]
        abstract formData: ServiceWorkerGlobalScope.FormData with get, set

        [<EmitProperty("File")>]
        abstract file: ServiceWorkerGlobalScope.File with get, set

        [<EmitProperty("Blob")>]
        abstract blob: ServiceWorkerGlobalScope.Blob with get, set

        [<EmitProperty("URLPattern")>]
        abstract uRLPattern: ServiceWorkerGlobalScope.URLPattern with get, set

        [<EmitProperty("URLSearchParams")>]
        abstract uRLSearchParams: ServiceWorkerGlobalScope.URLSearchParams with get, set

        abstract URL: ServiceWorkerGlobalScope.URL with get, set
        abstract navigator: Navigator with get, set

        [<EmitProperty("TextEncoder")>]
        abstract textEncoder: ServiceWorkerGlobalScope.TextEncoder with get, set

        [<EmitProperty("TextDecoder")>]
        abstract textDecoder: ServiceWorkerGlobalScope.TextDecoder with get, set

        [<EmitProperty("AbortSignal")>]
        abstract abortSignal: ServiceWorkerGlobalScope.AbortSignal with get, set

        [<EmitProperty("AbortController")>]
        abstract abortController: ServiceWorkerGlobalScope.AbortController with get, set

        [<EmitProperty("WebSocketRequestResponsePair")>]
        abstract webSocketRequestResponsePair: ServiceWorkerGlobalScope.WebSocketRequestResponsePair with get, set

        [<EmitProperty("WebSocketPair")>]
        abstract webSocketPair: ServiceWorkerGlobalScope.WebSocketPair with get, set

        [<EmitProperty("WebSocket")>]
        abstract webSocket: SharedLiterals.CLOSEDCLOSINGCONNECTING7a08c077 with get, set

        [<EmitProperty("Response")>]
        abstract response: SharedLiterals.BodyInitErrorJsonPrototypeRedirect with get, set

        [<EmitProperty("Request")>]
        abstract request: SharedLiterals.InputInitPrototype<obj, obj> with get, set

        [<EmitProperty("Body")>]
        abstract body: ServiceWorkerGlobalScope.Body with get, set

        [<EmitProperty("Headers")>]
        abstract headers: ServiceWorkerGlobalScope.Headers with get, set

        [<EmitProperty("TextDecoderStream")>]
        abstract textDecoderStream: ServiceWorkerGlobalScope.TextDecoderStream with get, set

        [<EmitProperty("TextEncoderStream")>]
        abstract textEncoderStream: ServiceWorkerGlobalScope.TextEncoderStream with get, set

        [<EmitProperty("DecompressionStream")>]
        abstract decompressionStream: ServiceWorkerGlobalScope.DecompressionStream with get, set

        [<EmitProperty("CompressionStream")>]
        abstract compressionStream: ServiceWorkerGlobalScope.CompressionStream with get, set

        [<EmitProperty("TransformStreamDefaultController")>]
        abstract transformStreamDefaultController: ServiceWorkerGlobalScope.TransformStreamDefaultController with get, set

        [<EmitProperty("WritableStreamDefaultController")>]
        abstract writableStreamDefaultController: ServiceWorkerGlobalScope.WritableStreamDefaultController with get, set

        [<EmitProperty("ReadableByteStreamController")>]
        abstract readableByteStreamController: ServiceWorkerGlobalScope.ReadableByteStreamController with get, set

        [<EmitProperty("ReadableStreamDefaultController")>]
        abstract readableStreamDefaultController: ServiceWorkerGlobalScope.ReadableStreamDefaultController with get, set

        [<EmitProperty("ReadableStreamBYOBRequest")>]
        abstract readableStreamBYOBRequest: ServiceWorkerGlobalScope.ReadableStreamBYOBRequest with get, set

        [<EmitProperty("EventSource")>]
        abstract eventSource: ServiceWorkerGlobalScope.EventSource with get, set

        [<EmitProperty("MessagePort")>]
        abstract messagePort: ServiceWorkerGlobalScope.MessagePort with get, set

        [<EmitProperty("MessageChannel")>]
        abstract messageChannel: ServiceWorkerGlobalScope.MessageChannel with get, set

        [<EmitProperty("ErrorEvent")>]
        abstract errorEvent: ServiceWorkerGlobalScope.ErrorEvent with get, set

        [<EmitProperty("CountQueuingStrategy")>]
        abstract countQueuingStrategy: ServiceWorkerGlobalScope.CountQueuingStrategy with get, set

        [<EmitProperty("ByteLengthQueuingStrategy")>]
        abstract byteLengthQueuingStrategy: ServiceWorkerGlobalScope.ByteLengthQueuingStrategy with get, set

        [<EmitProperty("TransformStream")>]
        abstract transformStream: ServiceWorkerGlobalScope.TransformStream with get, set

        [<EmitProperty("WritableStreamDefaultWriter")>]
        abstract writableStreamDefaultWriter: ServiceWorkerGlobalScope.WritableStreamDefaultWriter with get, set

        [<EmitProperty("WritableStream")>]
        abstract writableStream: ServiceWorkerGlobalScope.WritableStream with get, set

        [<EmitProperty("ReadableStream")>]
        abstract readableStream: ServiceWorkerGlobalScope.ReadableStream with get, set

        [<EmitProperty("ReadableStreamBYOBReader")>]
        abstract readableStreamBYOBReader: ServiceWorkerGlobalScope.ReadableStreamBYOBReader with get, set

        [<EmitProperty("ReadableStreamDefaultReader")>]
        abstract readableStreamDefaultReader: ServiceWorkerGlobalScope.ReadableStreamDefaultReader with get, set

        [<EmitProperty("CloseEvent")>]
        abstract closeEvent: ServiceWorkerGlobalScope.CloseEvent with get, set

        [<EmitProperty("MessageEvent")>]
        abstract messageEvent: ServiceWorkerGlobalScope.MessageEvent with get, set

        [<EmitProperty("ScheduledEvent")>]
        abstract scheduledEvent: ServiceWorkerGlobalScope.ScheduledEvent with get, set

        [<EmitProperty("TraceEvent")>]
        abstract traceEvent: obj with get, set

        [<EmitProperty("TailEvent")>]
        abstract tailEvent: ServiceWorkerGlobalScope.TailEvent with get, set

        [<EmitProperty("FetchEvent")>]
        abstract fetchEvent: ServiceWorkerGlobalScope.FetchEvent with get, set

        [<EmitProperty("PromiseRejectionEvent")>]
        abstract promiseRejectionEvent: ServiceWorkerGlobalScope.PromiseRejectionEvent with get, set

        [<EmitProperty("CustomEvent")>]
        abstract customEvent: ServiceWorkerGlobalScope.CustomEvent with get, set

        [<EmitProperty("ExtendableEvent")>]
        abstract extendableEvent: ServiceWorkerGlobalScope.ExtendableEvent with get, set

        [<EmitProperty("Event")>]
        abstract event: ServiceWorkerGlobalScope.Event with get, set

        abstract origin: string with get

        [<EmitProperty("Cloudflare")>]
        abstract cloudflare: option<obj> with get, set

        abstract performance: Performance with get, set
        abstract scheduler: Scheduler with get, set
        abstract caches: CacheStorage with get, set
        abstract crypto: Crypto with get, set
        abstract self: ServiceWorkerGlobalScope with get, set

        [<EmitProperty("WorkerGlobalScope")>]
        abstract workerGlobalScope: ServiceWorkerGlobalScope.WorkerGlobalScope with get, set

        [<EmitProperty("DOMException")>]
        abstract dOMException: ServiceWorkerGlobalScope.DOMException with get, set

        abstract btoa: data: string -> string
        abstract atob: data: string -> string
        abstract setTimeout: callback: (ResizeArray<option<obj>> -> unit) * ?msDelay: float -> float
        abstract setTimeout: callback: (obj -> unit) * msDelay: float * [<ParamArray>] args: obj -> float
        abstract clearTimeout: ?timeoutId: float -> unit
        abstract setInterval: callback: (ResizeArray<option<obj>> -> unit) * ?msDelay: float -> float
        abstract setInterval: callback: (obj -> unit) * msDelay: float * [<ParamArray>] args: obj -> float
        abstract clearInterval: ?timeoutId: float -> unit
        abstract queueMicrotask: task: obj -> unit
        abstract structuredClone: value: obj * ?options: StructuredSerializeOptions -> obj
        abstract reportError: ?error: obj -> unit
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<RequestInitCfProperties> -> Promise<Response>

    type SetTimeout =
        abstract Invoke: callback: (ResizeArray<option<obj>> -> unit) * ?msDelay: float -> float
        abstract Invoke: callback: (obj -> unit) * msDelay: float * [<ParamArray>] args: obj -> float

    [<Import("@cloudflare/workers-types", "EmailMessage")>]
    type EmailMessage =
        [<EmitProperty("to")>]
        abstract ``to``: string with get

        abstract from: string with get

    [<Import("@cloudflare/workers-types", "CloseEvent")>]
    type CloseEvent =
        [<EmitConstructor>]
        abstract Create: ``type``: string * ?initializer: CloseEventInit -> CloseEvent

        inherit Event
        abstract wasClean: bool with get
        abstract reason: string with get
        abstract code: float with get

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BrowserRunResourceType =
        | [<CompiledName("fetch")>] Fetch
        | [<CompiledName("image")>] Image
        | [<CompiledName("ping")>] Ping
        | [<CompiledName("document")>] Document
        | [<CompiledName("stylesheet")>] Stylesheet
        | [<CompiledName("media")>] Media
        | [<CompiledName("font")>] Font
        | [<CompiledName("script")>] Script
        | [<CompiledName("texttrack")>] Texttrack
        | [<CompiledName("xhr")>] Xhr
        | [<CompiledName("prefetch")>] Prefetch
        | [<CompiledName("eventsource")>] Eventsource
        | [<CompiledName("websocket")>] Websocket
        | [<CompiledName("manifest")>] Manifest
        | [<CompiledName("signedexchange")>] Signedexchange
        | [<CompiledName("cspviolationreport")>] Cspviolationreport
        | [<CompiledName("preflight")>] Preflight
        | [<CompiledName("other")>] Other

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Black_Forest_Labs_Flux_1_Schnell")>]
    type BaseAiCfBlackForestLabsFlux1Schnell =
        abstract postProcessedOutputs: AiCfBlackForestLabsFlux1SchnellOutput with get, set
        abstract inputs: AiCfBlackForestLabsFlux1SchnellInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_JSON_Mode_3")>]
    type AiCfQwenQwen330BA3BFp8JSONMode3 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "TransformStream")>]
    type TransformStream<'I, 'O> =
        [<EmitConstructor>]
        abstract Create: ?transformer: Transformer<'I, 'O> * ?writableStrategy: QueuingStrategy<'I> * ?readableStrategy: QueuingStrategy<'O> -> TransformStream<'I, 'O>

        abstract writable: WritableStream<'I> with get
        abstract readable: ReadableStream<'O> with get

    [<Import("@cloudflare/workers-types", "ImagesBinding")>]
    type ImagesBinding =
        abstract hosted: HostedImagesBinding with get
        abstract info: stream: ReadableStream<Uint8Array> * ?options: ImageInputOptions -> Promise<ImageInfoResponse>
        abstract input: stream: ReadableStream<Uint8Array> * ?options: ImageInputOptions -> ImageTransformer

    type XOR =
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "Cache")>]
    type Cache =
        abstract delete: request: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?options: CacheQueryOptions -> Promise<bool>
        abstract ``match``: request: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?options: CacheQueryOptions -> Promise<option<Response>>
        abstract put: request: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * response: Response -> Promise<unit>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Pfnet_Plamo_Embedding_1B_Input")>]
    type AiCfPfnetPlamoEmbedding1BInput =
        abstract text: U2<ResizeArray<string>, string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_JSON_Mode_3")>]
    type AiCfAisingaporeGemmaSeaLionV427BItJSONMode3 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "Tracing")>]
    type Tracing =
        [<EmitProperty("Span")>]
        abstract span: Tracing.Span with get, set

        abstract enterSpan: name: string * callback: (Span -> obj -> obj) * [<ParamArray>] args: obj -> obj
        abstract startActiveSpan: name: string * callback: (Span -> obj -> obj) * [<ParamArray>] args: obj -> obj

    type ExportedHandlerConnectHandler = Socket -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "WebSocketEventMap")>]
    type WebSocketEventMap =
        abstract error: ErrorEvent with get, set

        [<EmitProperty("open")>]
        abstract ``open``: Event with get, set

        abstract message: MessageEvent with get, set
        abstract close: CloseEvent with get, set

    [<Import("@cloudflare/workers-types", "EventSource")>]
    type EventSource =
        [<EmitConstructor>]
        abstract Create: url: string * ?init: EventSourceEventSourceInit -> EventSource

        inherit EventTarget<obj>
        abstract CLOSED: float with get
        abstract OPEN: float with get
        abstract CONNECTING: float with get
        abstract onerror: option<obj> with get
        abstract onmessage: option<obj> with get
        abstract onopen: option<obj> with get
        abstract readyState: float with get
        abstract withCredentials: bool with get
        abstract url: string with get
        abstract close: unit -> unit
        abstract from: stream: ReadableStream<option<obj>> -> EventSource

    type ResponseContent = U5<ResponseInputText, ResponseInputImage, ResponseOutputText, ResponseOutputRefusal, ResponseContent.Case4>

    [<Import("@cloudflare/workers-types", "ResponseErrorEvent")>]
    type ResponseErrorEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        abstract param: option<string> with get, set
        abstract message: string with get, set
        abstract code: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Black_Forest_Labs_Flux_2_Klein_4B_Input")>]
    type AiCfBlackForestLabsFlux2Klein4BInput =
        abstract multipart: SharedLiterals.BodyContentType with get, set

    type AiCfQwenQwen25Coder32BInstructInput = U2<AiCfQwenQwen25Coder32BInstructPrompt, AiCfQwenQwen25Coder32BInstructMessages>

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Baai_Bge_Reranker_Base")>]
    type BaseAiCfBaaiBgeRerankerBase =
        abstract postProcessedOutputs: AiCfBaaiBgeRerankerBaseOutput with get, set
        abstract inputs: AiCfBaaiBgeRerankerBaseInput with get, set

    [<Import("@cloudflare/workers-types", "TextDecoderConstructorOptions")>]
    type TextDecoderConstructorOptions =
        abstract ignoreBOM: bool with get, set
        abstract fatal: bool with get, set

    [<Import("@cloudflare/workers-types", "RateLimitOptions")>]
    type RateLimitOptions =
        abstract key: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchJob")>]
    type AiSearchJob =
        abstract info: unit -> Promise<AiSearchJobInfo>
        abstract logs: ?params: AiSearchJobLogsParams -> Promise<AiSearchJobLogsResponse>
        abstract cancel: unit -> Promise<AiSearchJobInfo>

    [<Import("@cloudflare/workers-types", "AbortController")>]
    type AbortController =
        [<EmitConstructor>]
        abstract Create: unit -> AbortController

        abstract signal: AbortSignal with get
        abstract abort: ?reason: obj -> unit

    type LoopbackForExport =
        abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
        abstract Invoke: opts: SharedLiterals.Props<obj> -> obj
        abstract Invoke: opts: SharedLiterals.Props2 -> obj
        abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
        abstract Item: key: string -> option<obj>
        abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
        abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    [<Import("@cloudflare/workers-types", "DurableObjectGetAlarmOptions")>]
    type DurableObjectGetAlarmOptions =
        abstract allowConcurrency: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "D1Database")>]
    type D1Database =
        abstract prepare: query: string -> D1PreparedStatement
        abstract batch: statements: ResizeArray<D1PreparedStatement> -> Promise<ResizeArray<obj>>
        abstract exec: query: string -> Promise<D1ExecResult>
        abstract withSession: ?constraintOrBookmark: U2<LiteralUnions.FirstPrimaryFirstUnconstrained, string> -> D1DatabaseSession
        abstract dump: unit -> Promise<ArrayBuffer>

    [<Import("@cloudflare/workers-types", "TraceItemHibernatableWebSocketEventInfoClose")>]
    type TraceItemHibernatableWebSocketEventInfoClose =
        abstract wasClean: bool with get
        abstract code: float with get
        abstract webSocketEventType: string with get

    [<Import("@cloudflare/workers-types", "AiSearchJobs")>]
    type AiSearchJobs =
        abstract list: ?params: AiSearchListJobsParams -> Promise<AiSearchListJobsResponse>
        abstract create: ?params: AiSearchCreateJobParams -> Promise<AiSearchJobInfo>
        abstract get: jobId: string -> AiSearchJob

    [<Import("@cloudflare/workers-types", "TraceItemTailEventInfoTailItem")>]
    type TraceItemTailEventInfoTailItem =
        abstract scriptName: option<string> with get

    [<Import("@cloudflare/workers-types", "CountQueuingStrategy")>]
    type CountQueuingStrategy =
        [<EmitConstructor>]
        abstract Create: init: QueuingStrategyInit -> CountQueuingStrategy

        inherit QueuingStrategy<option<obj>>
        abstract highWaterMark: float with get
        abstract size: ?chunk: obj -> float

    [<Import("@cloudflare/workers-types", "HostedImagesBinding")>]
    type HostedImagesBinding =
        abstract image: imageId: string -> ImageHandle
        abstract upload: image: U2<ReadableStream<Uint8Array>, ArrayBuffer> * ?options: ImageUploadOptions -> Promise<ImageMetadata>
        abstract list: ?options: ImageListOptions -> Promise<ImageList>

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Mistralai_Mistral_Small_3_1_24B_Instruct")>]
    type BaseAiCfMistralaiMistralSmall3124BInstruct =
        abstract postProcessedOutputs: AiCfMistralaiMistralSmall3124BInstructOutput with get, set
        abstract inputs: AiCfMistralaiMistralSmall3124BInstructInput with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesTLSClientAuth")>]
    type IncomingRequestCfPropertiesTLSClientAuth =
        abstract certChainRFC9440TooLarge: bool with get, set
        abstract certChainRFC9440: string with get, set
        abstract certRFC9440TooLarge: bool with get, set
        abstract certRFC9440: string with get, set
        /// <example>
        /// "Dec 22 19:39:00 2018 GMT"
        /// </example>
        abstract certNotAfter: string with get, set
        /// <example>
        /// "Dec 22 19:39:00 2018 GMT"
        /// </example>
        abstract certNotBefore: string with get, set
        /// <example>
        /// "acf77cf37b4156a2708e34c4eb755f9b5dbbe5ebb55adfec8f11493438d19e6ad3f157f81fa3b98278453d5652b0c1fd1d71e5695ae4d709803a4d3f39de9dea"
        /// </example>
        abstract certFingerprintSHA256: string with get, set
        /// <example>
        /// "6b9109f323999e52259cda7373ff0b4d26bd232e"
        /// </example>
        abstract certFingerprintSHA1: string with get, set
        /// <example>
        /// "BB:AF:7E:02:3D:FA:A6:F1:3C:84:8E:AD:EE:38:98:EC:D9:32:32:D4"
        /// </example>
        abstract certIssuerSKI: string with get, set
        /// <example>
        /// "BB:AF:7E:02:3D:FA:A6:F1:3C:84:8E:AD:EE:38:98:EC:D9:32:32:D4"
        /// </example>
        abstract certSKI: string with get, set
        /// <example>
        /// "2489002934BDFEA34"
        /// </example>
        abstract certIssuerSerial: string with get, set
        /// <example>
        /// "00936EACBE07F201DF"
        /// </example>
        abstract certSerial: string with get, set
        abstract certSubjectDNLegacy: string with get, set
        abstract certIssuerDNLegacy: string with get, set
        /// <example>
        /// "CN=*.cloudflareaccess.com, C=US, ST=Texas, L=Austin, O=Cloudflare"
        /// </example>
        abstract certSubjectDNRFC2253: string with get, set
        /// <example>
        /// "CN=cloudflareaccess.com, C=US, ST=Texas, L=Austin, O=Cloudflare"
        /// </example>
        abstract certIssuerDNRFC2253: string with get, set
        /// <example>
        /// "CN=*.cloudflareaccess.com, C=US, ST=Texas, L=Austin, O=Cloudflare"
        /// </example>
        abstract certSubjectDN: string with get, set
        /// <example>
        /// "CN=cloudflareaccess.com, C=US, ST=Texas, L=Austin, O=Cloudflare"
        /// </example>
        abstract certIssuerDN: string with get, set
        abstract certRevoked: LiteralUnions.``01`` with get, set
        /// <example>
        /// "FAILED:self signed certificate"
        /// </example>
        abstract certVerified: LiteralUnions.``FAILEDFAILED:certificate haA37cab5c`` with get, set
        abstract certPresented: string with get, set

    [<Import("@cloudflare/workers-types", "FixedLengthStream")>]
    type FixedLengthStream =
        interface
            [<EmitConstructor>]
            abstract Create: expectedLength: float * ?queuingStrategy: IdentityTransformStreamQueuingStrategy -> FixedLengthStream

            inherit IdentityTransformStream
        end

    [<Import("@cloudflare/workers-types", "TooManyWatermarksError")>]
    type TooManyWatermarksError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "ContainerDirectorySnapshotRestoreParams")>]
    type ContainerDirectorySnapshotRestoreParams =
        abstract mountPoint: option<string> with get, set
        abstract snapshot: ContainerDirectorySnapshot with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_Messages")>]
    type AiCfAisingaporeGemmaSeaLionV427BItMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfAisingaporeGemmaSeaLionV427BItJSONMode1> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRole> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchStatsResponse")>]
    type AiSearchStatsResponse =
        abstract engine: option<SharedLiterals.R2Vectorize> with get, set

        [<EmitProperty("last_activity")>]
        abstract lastActivity: option<string> with get, set

        abstract outdated: option<float> with get, set
        abstract skipped: option<float> with get, set
        abstract error: option<float> with get, set
        abstract completed: option<float> with get, set
        abstract running: option<float> with get, set
        abstract queued: option<float> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8")>]
    type BaseAiCfQwenQwen330BA3BFp8 =
        abstract postProcessedOutputs: AiCfQwenQwen330BA3BFp8Output with get, set
        abstract inputs: AiCfQwenQwen330BA3BFp8Input with get, set

    type AiCfMetaM2M10012BInput = U2<SharedLiterals.SourceLangTargetLangText, SharedLiterals.Requests3>

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ImageCompositeMode =
        | [<CompiledName("out")>] Out
        | [<CompiledName("in")>] In
        | [<CompiledName("over")>] Over
        | [<CompiledName("atop")>] Atop
        | [<CompiledName("xor")>] Xor
        | [<CompiledName("lighter")>] Lighter

    [<Import("@cloudflare/workers-types", "RequestInitCfPropertiesVaryAcceptHeader")>]
    type RequestInitCfPropertiesVaryAcceptHeader =
        inherit RequestInitCfPropertiesVaryHeader

        [<EmitProperty("media_types")>]
        abstract mediaTypes: option<ResizeArray<string>> with get, set

    [<Import("@cloudflare/workers-types", "AiGatewayLog")>]
    type AiGatewayLog =
        [<EmitProperty("created_at")>]
        abstract createdAt: Date with get, set

        [<EmitProperty("response_head_complete")>]
        abstract responseHeadComplete: bool with get, set

        [<EmitProperty("response_head")>]
        abstract responseHead: option<string> with get, set

        [<EmitProperty("response_size")>]
        abstract responseSize: float with get, set

        [<EmitProperty("request_head_complete")>]
        abstract requestHeadComplete: bool with get, set

        [<EmitProperty("request_head")>]
        abstract requestHead: option<string> with get, set

        [<EmitProperty("request_size")>]
        abstract requestSize: float with get, set

        [<EmitProperty("custom_cost")>]
        abstract customCost: option<bool> with get, set

        abstract cost: option<float> with get, set
        abstract step: option<float> with get, set
        abstract metadata: option<obj> with get, set

        [<EmitProperty("tokens_out")>]
        abstract tokensOut: option<float> with get, set

        [<EmitProperty("tokens_in")>]
        abstract tokensIn: option<float> with get, set

        abstract cached: bool with get, set
        abstract success: bool with get, set

        [<EmitProperty("response_content_type")>]
        abstract responseContentType: option<string> with get, set

        [<EmitProperty("status_code")>]
        abstract statusCode: float with get, set

        [<EmitProperty("request_content_type")>]
        abstract requestContentType: option<string> with get, set

        [<EmitProperty("request_type")>]
        abstract requestType: option<string> with get, set

        abstract duration: float with get, set
        abstract path: string with get, set

        [<EmitProperty("model_type")>]
        abstract modelType: option<string> with get, set

        abstract model: string with get, set
        abstract provider: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunJsonSuccessResponse")>]
    type BrowserRunJsonSuccessResponse =
        abstract result: obj with get, set
        abstract success: bool with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunJsonBaseOptions")>]
    type BrowserRunJsonBaseOptions =
        [<EmitProperty("custom_ai")>]
        abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel2>> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Meta_M2M100_1_2B")>]
    type BaseAiCfMetaM2M10012B =
        abstract postProcessedOutputs: AiCfMetaM2M10012BOutput with get, set
        abstract inputs: AiCfMetaM2M10012BInput with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryIngestOptions")>]
    type AgentMemoryIngestOptions =
        abstract sessionId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemInfo")>]
    type AiSearchItemInfo =
        abstract metadata: option<obj> with get, set

        [<EmitProperty("created_at")>]
        abstract createdAt: option<string> with get, set

        [<EmitProperty("last_seen_at")>]
        abstract lastSeenAt: option<string> with get, set

        [<EmitProperty("source_id")>]
        abstract sourceId: option<string> with get, set

        [<EmitProperty("file_size")>]
        abstract fileSize: option<float> with get, set

        [<EmitProperty("chunks_count")>]
        abstract chunksCount: option<float> with get, set

        [<EmitProperty("namespace")>]
        abstract ``namespace``: option<string> with get, set

        abstract checksum: option<string> with get, set
        abstract error: option<string> with get, set

        [<EmitProperty("next_action")>]
        abstract nextAction: option<LiteralUnions.DELETE_INDEX> with get, set

        abstract status: LiteralUnions.CompletedErrorOutdated22d6dfc3 with get, set
        abstract key: string with get, set
        abstract id: string with get, set
        abstract Item: key: string -> option<obj>

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRAGInternalError")>]
    type AutoRAGInternalError = interface end

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Leonardo_Lucid_Origin")>]
    type BaseAiCfLeonardoLucidOrigin =
        abstract postProcessedOutputs: AiCfLeonardoLucidOriginOutput with get, set
        abstract inputs: AiCfLeonardoLucidOriginInput with get, set

    type ExportedHandlerTraceHandler = ResizeArray<TraceItem> -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "InvalidURLError")>]
    type InvalidURLError =
        inherit StreamError
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemChunksParams")>]
    type AiSearchItemChunksParams =
        abstract offset: option<float> with get, set
        abstract limit: option<float> with get, set

    [<Import("@cloudflare/workers-types", "D1Response")>]
    type D1Response =
        abstract error: option<unit> with get, set
        abstract meta: D1Response.Meta with get, set
        abstract success: bool with get, set

    type PagesFunction = SharedLiterals.DataEnvFunctionPath23a88d11<obj, obj> -> U2<Response, Promise<Response>>

    [<Import("@cloudflare/workers-types", "StreamVideoInput")>]
    type StreamVideoInput =
        abstract height: float with get, set
        abstract width: float with get, set

    [<Import("@cloudflare/workers-types", "FetchEvent")>]
    type FetchEvent =
        inherit ExtendableEvent
        abstract request: Request<option<obj>, U2<RequestInitCfProperties, obj>> with get
        abstract respondWith: promise: U2<Response, Promise<Response>> -> unit
        abstract passThroughOnException: unit -> unit

    [<Import("@cloudflare/workers-types", "ResponseInputText")>]
    type ResponseInputText =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionContentPartText")>]
    type ChatCompletionContentPartText =
        abstract text: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "AnalyticsEngineDataset")>]
    type AnalyticsEngineDataset =
        abstract writeDataPoint: ?event: AnalyticsEngineDataPoint -> unit

    [<Import("@cloudflare/workers-types", "MessagePort")>]
    type MessagePort =
        inherit EventTarget<obj>
        abstract onmessage: option<obj> with get
        abstract postMessage: ?data: obj * ?options: U2<ResizeArray<option<obj>>, MessagePortPostMessageOptions> -> unit
        abstract close: unit -> unit
        abstract start: unit -> unit

    [<Import("@cloudflare/workers-types", "DurableObjectState")>]
    type DurableObjectState<'Props> =
        abstract facets: DurableObjectFacets with get, set
        abstract container: option<Container> with get, set
        abstract storage: DurableObjectStorage with get
        abstract id: DurableObjectId with get
        abstract props: 'Props with get
        abstract exports: Cloudflare.Exports with get
        abstract waitUntil: promise: Promise<option<obj>> -> unit
        abstract blockConcurrencyWhile: callback: (unit -> Promise<obj>) -> Promise<obj>
        abstract acceptWebSocket: ws: WebSocket * ?tags: ResizeArray<string> -> unit
        abstract getWebSockets: ?tag: string -> ResizeArray<WebSocket>
        abstract setWebSocketAutoResponse: ?maybeReqResp: WebSocketRequestResponsePair -> unit
        abstract getWebSocketAutoResponse: unit -> option<WebSocketRequestResponsePair>
        abstract getWebSocketAutoResponseTimestamp: ws: WebSocket -> option<Date>
        abstract setHibernatableWebSocketEventTimeout: ?timeoutMs: float -> unit
        abstract getHibernatableWebSocketEventTimeout: unit -> option<float>
        abstract getTags: ws: WebSocket -> ResizeArray<string>
        abstract abort: ?reason: string -> unit

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Input_QueryAnd_Contexts")>]
    type AiCfBaaiBgeM3InputQueryAndContexts =
        [<EmitProperty("truncate_inputs")>]
        abstract truncateInputs: option<bool> with get, set

        abstract contexts: ResizeArray<SharedLiterals.Text> with get, set
        abstract query: option<string> with get, set

    [<Import("@cloudflare/workers-types", "BaseAiTextToSpeech")>]
    type BaseAiTextToSpeech =
        abstract postProcessedOutputs: AiTextToSpeechOutput with get, set
        abstract inputs: AiTextToSpeechInput with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionUrlCitation")>]
    type ChatCompletionUrlCitation =
        [<EmitProperty("url_citation")>]
        abstract urlCitation: SharedLiterals.EndIndexStartIndexTitleUrl with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "Without")>]
    type Without =
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_JSON_Mode_2")>]
    type AiCfQwenQwen330BA3BFp8JSONMode2 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct_Messages_Inner")>]
    type AiCfMetaLlama4Scout17B16EInstructMessagesInner =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlama4Scout17B16EInstructJSONMode> with get, set

        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRoleToolCallId> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionCustomTool")>]
    type ChatCompletionCustomTool =
        abstract custom: SharedLiterals.DescriptionFormatName with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "CompressionStream")>]
    type CompressionStream =
        interface
            [<EmitConstructor>]
            abstract Create: format: LiteralUnions.DeflateDeflateRawGzip -> CompressionStream

            inherit TransformStream<BufferSource, Uint8Array>
        end

    [<Import("@cloudflare/workers-types", "TextDecoder")>]
    type TextDecoder =
        [<EmitConstructor>]
        abstract Create: ?label: string * ?options: TextDecoderConstructorOptions -> TextDecoder

        abstract ignoreBOM: bool with get
        abstract fatal: bool with get
        abstract encoding: string with get
        abstract decode: ?input: BufferSource * ?options: TextDecoderDecodeOptions -> string

    [<Import("@cloudflare/workers-types", "SyncKvListOptions")>]
    type SyncKvListOptions =
        abstract limit: option<float> with get, set
        abstract reverse: option<bool> with get, set
        abstract prefix: option<string> with get, set

        [<EmitProperty("end")>]
        abstract ``end``: option<string> with get, set

        abstract startAfter: option<string> with get, set
        abstract start: option<string> with get, set

    [<Import("@cloudflare/workers-types", "BlobOptions")>]
    type BlobOptions =
        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

    type ExportedHandlerScheduledHandler = ScheduledController -> obj -> ExecutionContext<obj> -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "AiSearchCreateJobParams")>]
    type AiSearchCreateJobParams =
        abstract description: option<string> with get, set

    [<Import("@cloudflare/workers-types", "WorkflowInstance")>]
    type WorkflowInstance =
        abstract id: string with get, set
        abstract pause: unit -> Promise<unit>
        abstract resume: unit -> Promise<unit>
        abstract terminate: unit -> Promise<unit>
        abstract restart: ?options: WorkflowInstanceRestartOptions -> Promise<unit>
        abstract status: unit -> Promise<InstanceStatus>
        abstract sendEvent: typepayload: obj -> Promise<unit>

    [<Import("@cloudflare/workers-types", "WorkerVersionMetadata")>]
    type WorkerVersionMetadata =
        abstract timestamp: string with get, set
        abstract tag: string with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "CryptoKeyAesKeyAlgorithm")>]
    type CryptoKeyAesKeyAlgorithm =
        abstract length: float with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "StreamDownloadGetResponse")>]
    type StreamDownloadGetResponse =
        [<EmitProperty("default")>]
        abstract ``default``: option<StreamDownload> with get, set

        abstract audio: option<StreamDownload> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DurableObjectRoutingMode = | [<CompiledName("primary-only")>] DurableObjectRoutingMode

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type StreamPaginationComparison =
        | [<CompiledName("eq")>] Eq
        | [<CompiledName("gt")>] Gt
        | [<CompiledName("gte")>] Gte
        | [<CompiledName("lt")>] Lt
        | [<CompiledName("lte")>] Lte

    [<Import("@cloudflare/workers-types", "ResponseFormatTextJSONSchemaConfig")>]
    type ResponseFormatTextJSONSchemaConfig =
        abstract strict: option<bool> with get, set
        abstract description: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract schema: System.Collections.Generic.IDictionary<string, option<obj>> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "DurableObjectSetAlarmOptions")>]
    type DurableObjectSetAlarmOptions =
        abstract allowUnconfirmed: option<bool> with get, set
        abstract allowConcurrency: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "DurableObject")>]
    type DurableObject =
        abstract fetch: request: Request<option<obj>, U2<RequestInitCfProperties, obj>> -> U2<Response, Promise<Response>>
        abstract connect: socket: Socket -> option<Promise<unit>>
        abstract alarm: ?alarmInfo: AlarmInvocationInfo -> option<Promise<unit>>
        abstract webSocketMessage: ws: WebSocket * message: U2<ArrayBuffer, string> -> option<Promise<unit>>
        abstract webSocketClose: ws: WebSocket * code: float * reason: string * wasClean: bool -> option<Promise<unit>>
        abstract webSocketError: ws: WebSocket * ?error: obj -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Prompt_1")>]
    type AiCfQwenQwen330BA3BFp8Prompt1 =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfQwenQwen330BA3BFp8JSONMode2> with get, set

        abstract lora: option<string> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchSearchResponse")>]
    type AiSearchSearchResponse =
        abstract chunks: ResizeArray<SharedLiterals.IdItemScoreScoringDetailsTextType> with get, set

        [<EmitProperty("search_query")>]
        abstract searchQuery: string with get, set

    [<Import("@cloudflare/workers-types", "File")>]
    type File =
        [<EmitConstructor>]
        abstract Create: ?bits: ResizeArray<U4<ArrayBuffer, obj, Blob, string>> * name: string * ?options: FileOptions -> File

        inherit Blob
        abstract lastModified: float with get
        abstract name: string with get

    [<Import("@cloudflare/workers-types", "TlsOptions")>]
    type TlsOptions =
        abstract expectedServerHostname: option<string> with get, set

    [<Import("@cloudflare/workers-types", "WebSearchUserLocation")>]
    type WebSearchUserLocation =
        abstract approximate: SharedLiterals.CityCountryRegionTimezone with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "StreamError")>]
    type StreamError =
        abstract stack: option<string> with get
        abstract message: string with get
        abstract statusCode: float with get
        abstract code: float with get

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Input_Embedding_1")>]
    type AiCfBaaiBgeM3InputEmbedding1 =
        [<EmitProperty("truncate_inputs")>]
        abstract truncateInputs: option<bool> with get, set

        abstract text: U2<ResizeArray<string>, string> with get, set

    [<Import("@cloudflare/workers-types", "ImageHandle")>]
    type ImageHandle =
        abstract details: unit -> Promise<option<ImageMetadata>>
        abstract bytes: unit -> Promise<option<ReadableStream<Uint8Array>>>
        abstract update: options: ImageUpdateOptions -> Promise<ImageMetadata>
        abstract delete: unit -> Promise<bool>

    [<Import("@cloudflare/workers-types", "VectorizeError")>]
    type VectorizeError =
        abstract error: string with get, set
        abstract code: option<float> with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Baai_Bge_Base_En_V1_5")>]
    type BaseAiCfBaaiBgeBaseEnV15 =
        abstract postProcessedOutputs: AiCfBaaiBgeBaseEnV15Output with get, set
        abstract inputs: AiCfBaaiBgeBaseEnV15Input with get, set

    type AiCfMetaLlama3370BInstructFp8FastInput = U3<AiCfMetaLlama3370BInstructFp8FastPrompt, AiCfMetaLlama3370BInstructFp8FastMessages, AiCfMetaLlama3370BInstructFp8FastAsyncBatch>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Output_Embedding")>]
    type AiCfBaaiBgeM3OutputEmbedding =
        abstract pooling: option<LiteralUnions.ClsMean> with get, set
        abstract data: option<ResizeArray<AiSentenceSimilarityOutput>> with get, set
        abstract shape: option<AiSentenceSimilarityOutput> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_JSON_Mode_2")>]
    type AiCfAisingaporeGemmaSeaLionV427BItJSONMode2 =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "ScriptVersion")>]
    type ScriptVersion =
        abstract message: option<string> with get, set
        abstract tag: option<string> with get, set
        abstract id: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_Async_Batch")>]
    type AiCfMetaLlama3370BInstructFp8FastAsyncBatch =
        abstract requests: option<ResizeArray<AiCfMetaLlama3370BInstructFp8FastAsyncBatch.Requests>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_Prompt")>]
    type AiCfMetaLlama3370BInstructFp8FastPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("response_format")>]
        abstract responseFormat: option<AiCfMetaLlama3370BInstructFp8FastJSONMode> with get, set

        abstract lora: option<string> with get, set
        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "CompletionTokensDetails")>]
    type CompletionTokensDetails =
        [<EmitProperty("rejected_prediction_tokens")>]
        abstract rejectedPredictionTokens: option<float> with get, set

        [<EmitProperty("accepted_prediction_tokens")>]
        abstract acceptedPredictionTokens: option<float> with get, set

        [<EmitProperty("audio_tokens")>]
        abstract audioTokens: option<float> with get, set

        [<EmitProperty("reasoning_tokens")>]
        abstract reasoningTokens: option<float> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type VectorizeVectorMetadataFilterOp =
        | [<CompiledName("$eq")>] Eq
        | [<CompiledName("$ne")>] Ne
        | [<CompiledName("$lt")>] Lt
        | [<CompiledName("$lte")>] Lte
        | [<CompiledName("$gt")>] Gt
        | [<CompiledName("$gte")>] Gte

    [<Import("@cloudflare/workers-types", "QueueSendBatchOptions")>]
    type QueueSendBatchOptions =
        abstract delaySeconds: option<float> with get, set

    [<Import("@cloudflare/workers-types", "SocketAddress")>]
    type SocketAddress =
        abstract port: float with get, set
        abstract hostname: string with get, set

    [<Import("@cloudflare/workers-types", "StreamDirectUploadWatermark")>]
    type StreamDirectUploadWatermark =
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchMultiSearchResponse")>]
    type AiSearchMultiSearchResponse =
        abstract errors: option<ResizeArray<AiSearchMultiSearchError>> with get, set
        abstract chunks: ResizeArray<AiSearchMultiSearchChunk> with get, set

        [<EmitProperty("search_query")>]
        abstract searchQuery: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseIncompleteDetails")>]
    type ResponseIncompleteDetails =
        abstract reason: option<LiteralUnions.ContentFilterMaxOutputTokens> with get, set

    type AiCfBaaiBgeM3Input = U3<AiCfBaaiBgeM3InputQueryAndContexts, AiCfBaaiBgeM3InputEmbedding, SharedLiterals.Requests2>

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Openai_Whisper")>]
    type BaseAiCfOpenaiWhisper =
        abstract postProcessedOutputs: AiCfOpenaiWhisperOutput with get, set
        abstract inputs: AiCfOpenaiWhisperInput with get, set

    [<Import("@cloudflare/workers-types", "QueuingStrategy")>]
    type QueuingStrategy<'T> =
        abstract highWaterMark: option<float> with get, set
        abstract size: chunk: 'T -> float

    [<Import("@cloudflare/workers-types", "Ai_Cf_Aisingapore_Gemma_Sea_Lion_V4_27B_It_AsyncResponse")>]
    type AiCfAisingaporeGemmaSeaLionV427BItAsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "PromiseRejectionEvent")>]
    type PromiseRejectionEvent =
        inherit Event
        abstract reason: option<obj> with get
        abstract promise: Promise<option<obj>> with get

    [<Import("@cloudflare/workers-types", "ChatCompletionMessageFunctionToolCall")>]
    type ChatCompletionMessageFunctionToolCall =
        [<EmitProperty("function")>]
        abstract ``function``: SharedLiterals.ArgumentsName with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionLogprobs")>]
    type ChatCompletionLogprobs =
        abstract refusal: option<ResizeArray<ChatCompletionTokenLogprob>> with get, set
        abstract content: option<ResizeArray<ChatCompletionTokenLogprob>> with get, set

    [<Import("@cloudflare/workers-types", "AiTextGenerationToolInput")>]
    type AiTextGenerationToolInput =
        [<EmitProperty("function")>]
        abstract ``function``: AiTextGenerationToolLegacyInput with get, set

        [<EmitProperty("type")>]
        abstract ``type``: U2<string, AiTextGenerationToolInput.Type> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type StreamWatermarkPosition =
        | [<CompiledName("upperRight")>] UpperRight
        | [<CompiledName("upperLeft")>] UpperLeft
        | [<CompiledName("lowerLeft")>] LowerLeft
        | [<CompiledName("lowerRight")>] LowerRight
        | [<CompiledName("center")>] Center

    [<Import("@cloudflare/workers-types", "R2GetOptions")>]
    type R2GetOptions =
        abstract ssecKey: option<U2<ArrayBuffer, string>> with get, set
        abstract range: option<U4<SharedLiterals.LengthOffset, SharedLiterals.LengthOffset2, SharedLiterals.Suffix, Headers>> with get, set
        abstract onlyIf: option<U2<R2Conditional, Headers>> with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesGeographicInformation")>]
    type IncomingRequestCfPropertiesGeographicInformation =
        /// <example>
        /// "635"
        /// </example>
        abstract metroCode: option<string> with get, set
        /// <example>
        /// "TX"
        /// </example>
        abstract regionCode: option<string> with get, set
        /// <example>
        /// "Texas"
        /// </example>
        abstract region: option<string> with get, set
        /// <example>
        /// "America/Chicago"
        /// </example>
        abstract timezone: option<string> with get, set
        /// <example>
        /// "-97.74260"
        /// </example>
        abstract longitude: option<string> with get, set
        /// <example>
        /// "30.27130"
        /// </example>
        abstract latitude: option<string> with get, set
        /// <example>
        /// "78701"
        /// </example>
        abstract postalCode: option<string> with get, set
        /// <example>
        /// "Austin"
        /// </example>
        abstract city: option<string> with get, set
        /// <example>
        /// "AN"
        /// </example>
        abstract continent: option<ContinentCode> with get, set
        /// <example>
        /// "1"
        /// </example>
        abstract isEUCountry: option<string> with get, set
        /// <example>
        /// "GB"
        /// </example>
        abstract country: option<LiteralUnions.ADAEAFAda77976> with get, set

    [<Import("@cloudflare/workers-types", "ResponsePrompt")>]
    type ResponsePrompt =
        abstract version: option<string> with get, set
        abstract variables: option<System.Collections.Generic.IDictionary<string, U3<ResponseInputText, ResponseInputImage, string>>> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ReadableStreamValuesOptions")>]
    type ReadableStreamValuesOptions =
        abstract preventCancel: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "ImageTransformationOutputOptions")>]
    type ImageTransformationOutputOptions =
        abstract encoding: option<string> with get, set

    type TypedArray = U11<Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array, BigInt64Array, obj>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_AsyncResponse")>]
    type AiCfQwenQwen330BA3BFp8AsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast_JSON_Mode")>]
    type AiCfMetaLlama3370BInstructFp8FastJSONMode =
        [<EmitProperty("json_schema")>]
        abstract jsonSchema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<LiteralUnions.JsonObjectJsonSchema> with get, set

    [<Import("@cloudflare/workers-types", "IncomingRequestCfPropertiesBotManagementEnterprise")>]
    type IncomingRequestCfPropertiesBotManagementEnterprise =
        inherit IncomingRequestCfPropertiesBotManagement
        abstract botManagement: IncomingRequestCfPropertiesBotManagementEnterprise.BotManagement with get, set

    [<Import("@cloudflare/workers-types", "DurableObjectFacets")>]
    type DurableObjectFacets =
        abstract get: name: string * getStartupOptions: (unit -> U2<FacetStartupOptions<obj>, Promise<FacetStartupOptions<obj>>>) -> DurableObjectFacets.Get
        abstract abort: name: string * ?reason: obj -> unit
        abstract delete: name: string -> unit
        abstract clone: src: string * dst: string -> unit

    [<Import("@cloudflare/workers-types", "AiGatewayPatchLog")>]
    type AiGatewayPatchLog =
        abstract metadata: option<obj> with get, set
        abstract feedback: option<LiteralUnions.I1I1> with get, set
        abstract score: option<float> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Meta_Llama_3_2_11B_Vision_Instruct_Messages")>]
    type AiCfMetaLlama3211BVisionInstructMessages =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract tools: option<ResizeArray<U2<SharedLiterals.DescriptionNameParameters, SharedLiterals.FunctionType>>> with get, set
        abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
        abstract image: option<U2<AiSentenceSimilarityOutput, obj>> with get, set
        abstract messages: ResizeArray<SharedLiterals.ContentRoleToolCallId> with get, set

    [<Import("@cloudflare/workers-types", "VectorizeIndexDetails")>]
    type VectorizeIndexDetails =
        abstract vectorsCount: float with get, set
        abstract config: VectorizeIndexConfig with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract id: string with get

    [<Import("@cloudflare/workers-types", "StreamVideoHandle")>]
    type StreamVideoHandle =
        abstract captions: StreamScopedCaptions with get, set
        abstract downloads: StreamScopedDownloads with get, set
        abstract id: string with get, set
        abstract details: unit -> Promise<StreamVideo>
        abstract update: params: StreamUpdateVideoParams -> Promise<StreamVideo>
        abstract delete: unit -> Promise<unit>
        abstract generateToken: unit -> Promise<string>

    [<Import("@cloudflare/workers-types", "GatewayOptions")>]
    type GatewayOptions =
        abstract retries: option<GatewayRetries> with get, set
        abstract requestTimeoutMs: option<float> with get, set
        abstract eventId: option<string> with get, set
        abstract collectLog: option<bool> with get, set
        abstract metadata: option<obj> with get, set
        abstract skipCache: option<bool> with get, set
        abstract cacheTtl: option<float> with get, set
        abstract cacheKey: option<string> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "AiGateway")>]
    type AiGateway =
        abstract patchLog: logId: string * data: AiGatewayPatchLog -> Promise<unit>
        abstract getLog: logId: string -> Promise<AiGatewayLog>
        abstract run: data: U2<AIGatewayUniversalRequest, ResizeArray<AIGatewayUniversalRequest>> * ?options: AiGateway.Run.Options -> Promise<Response>
        abstract getUrl: ?provider: U2<LiteralUnions.AdobeFireflyAnthropicAws_7e6a892e, string> -> Promise<string>

    type IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus =
        | ``0`` = 0
        | ``1`` = 1
        | ``2`` = 2
        | ``3`` = 3
        | ``4`` = 4
        | ``5`` = 5

    type ChatCompletionContentPart = U4<ChatCompletionContentPart.Case0, ChatCompletionContentPart.Case1, ChatCompletionContentPart.Case2, ChatCompletionContentPart.Case3>
    type ResponseFormatTextConfig = U3<ResponseFormatText, ResponseFormatTextJSONSchemaConfig, ResponseFormatJSONObject>

    [<Import("@cloudflare/workers-types", "Crypto")>]
    type Crypto =
        [<EmitProperty("DigestStream")>]
        abstract digestStream: Crypto.DigestStream with get, set

        abstract subtle: SubtleCrypto with get
        abstract getRandomValues: buffer: obj -> obj
        abstract randomUUID: unit -> string

    [<Import("@cloudflare/workers-types", "UnderlyingSink")>]
    type UnderlyingSink<'W> =
        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract start: controller: WritableStreamDefaultController -> option<Promise<unit>>
        abstract write: chunk: 'W * controller: WritableStreamDefaultController -> option<Promise<unit>>
        abstract abort: ?reason: obj -> option<Promise<unit>>
        abstract close: unit -> option<Promise<unit>>

    [<Import("@cloudflare/workers-types", "ContentOptions")>]
    type ContentOptions =
        abstract html: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "ImageConversionOptions")>]
    type ImageConversionOptions =
        abstract descriptionLanguage: option<LiteralUnions.DeEnEsFrItPt> with get, set

    [<Import("@cloudflare/workers-types", "AiAutomaticSpeechRecognitionOutput")>]
    type AiAutomaticSpeechRecognitionOutput =
        abstract vtt: option<string> with get, set
        abstract words: option<ResizeArray<SharedLiterals.EndStartWord>> with get, set
        abstract text: option<string> with get, set

    type AiImageClassificationOutput = ResizeArray<SharedLiterals.LabelScore>

    [<Import("@cloudflare/workers-types", "R2Error")>]
    type R2Error =
        abstract stack: option<obj> with get
        abstract action: string with get
        abstract message: string with get
        abstract code: float with get
        abstract name: string with get

    [<Import("@cloudflare/workers-types", "DOMException")>]
    type DOMException =
        [<EmitConstructor>]
        abstract Create: ?message: string * ?name: string -> DOMException

        abstract stack: option<obj> with get
        abstract DATA_CLONE_ERR: float with get
        abstract INVALID_NODE_TYPE_ERR: float with get
        abstract TIMEOUT_ERR: float with get
        abstract QUOTA_EXCEEDED_ERR: float with get
        abstract URL_MISMATCH_ERR: float with get
        abstract ABORT_ERR: float with get
        abstract NETWORK_ERR: float with get
        abstract SECURITY_ERR: float with get
        abstract TYPE_MISMATCH_ERR: float with get
        abstract VALIDATION_ERR: float with get
        abstract INVALID_ACCESS_ERR: float with get
        abstract NAMESPACE_ERR: float with get
        abstract INVALID_MODIFICATION_ERR: float with get
        abstract SYNTAX_ERR: float with get
        abstract INVALID_STATE_ERR: float with get
        abstract INUSE_ATTRIBUTE_ERR: float with get
        abstract NOT_SUPPORTED_ERR: float with get
        abstract NOT_FOUND_ERR: float with get
        abstract NO_MODIFICATION_ALLOWED_ERR: float with get
        abstract NO_DATA_ALLOWED_ERR: float with get
        abstract INVALID_CHARACTER_ERR: float with get
        abstract WRONG_DOCUMENT_ERR: float with get
        abstract HIERARCHY_REQUEST_ERR: float with get
        abstract DOMSTRING_SIZE_ERR: float with get
        abstract INDEX_SIZE_ERR: float with get
        /// <deprecated>
        /// [MDN Reference](https://developer.mozilla.org/docs/Web/API/DOMException/code)
        /// </deprecated>
        abstract code: float with get
        abstract name: string with get
        abstract message: string with get

    [<Import("@cloudflare/workers-types", "ScheduledEvent")>]
    type ScheduledEvent =
        inherit ExtendableEvent
        abstract cron: string with get
        abstract scheduledTime: float with get
        abstract noRetry: unit -> unit

    [<Import("@cloudflare/workers-types", "CloudflareAccessIdentity")>]
    type CloudflareAccessIdentity =
        [<EmitProperty("is_gateway")>]
        abstract isGateway: option<bool> with get, set

        [<EmitProperty("is_warp")>]
        abstract isWarp: option<bool> with get, set

        abstract devicePosture: option<obj> with get, set
        abstract groups: option<ResizeArray<CloudflareAccessIdentity.Groups>> with get, set
        abstract geo: option<CloudflareAccessIdentity.Geo> with get, set
        abstract idp: option<CloudflareAccessIdentity.Idp> with get, set
        abstract amr: option<ResizeArray<string>> with get, set
        abstract ip: option<string> with get, set
        abstract iat: option<float> with get, set

        [<EmitProperty("account_id")>]
        abstract accountId: option<string> with get, set

        [<EmitProperty("user_uuid")>]
        abstract userUuid: option<string> with get, set

        abstract name: option<string> with get, set
        abstract email: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AiTextClassificationInput")>]
    type AiTextClassificationInput =
        abstract text: string with get, set

    [<Import("@cloudflare/workers-types", "Vectorize")>]
    type Vectorize =
        abstract describe: unit -> Promise<VectorizeIndexInfo>
        abstract query: vector: U3<Float32Array, Float64Array, AiSentenceSimilarityOutput> * ?options: VectorizeQueryOptions -> Promise<VectorizeMatches>
        abstract queryById: vectorId: string * ?options: VectorizeQueryOptions -> Promise<VectorizeMatches>
        abstract insert: vectors: ResizeArray<VectorizeVector> -> Promise<VectorizeAsyncMutation>
        abstract upsert: vectors: ResizeArray<VectorizeVector> -> Promise<VectorizeAsyncMutation>
        abstract deleteByIds: ids: ResizeArray<string> -> Promise<VectorizeAsyncMutation>
        abstract getByIds: ids: ResizeArray<string> -> Promise<ResizeArray<VectorizeVector>>

    [<Import("@cloudflare/workers-types", "FunctionDefinition")>]
    type FunctionDefinition =
        abstract strict: option<bool> with get, set
        abstract parameters: option<obj> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "CompoundFilter")>]
    type CompoundFilter =
        abstract filters: ResizeArray<ComparisonFilter> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: LiteralUnions.AndOr with get, set

    type AiCfAisingaporeGemmaSeaLionV427BItOutput = U4<AiCfAisingaporeGemmaSeaLionV427BItChatCompletionResponse, AiCfAisingaporeGemmaSeaLionV427BItTextCompletionResponse, AiCfAisingaporeGemmaSeaLionV427BItAsyncResponse, string>
    type ChatCompletionTool = U2<ChatCompletionFunctionTool, ChatCompletionCustomTool>

    [<Import("@cloudflare/workers-types", "WorkerStub")>]
    type WorkerStub =
        abstract getEntrypoint<'T> : ?name: string * ?options: WorkerStubEntrypointOptions -> WorkerStub.GetEntrypoint
        abstract getDurableObjectClass<'T> : ?name: string * ?options: WorkerStubEntrypointOptions -> DurableObjectClass<'T>

    [<Import("@cloudflare/workers-types", "CacheContext")>]
    type CacheContext =
        abstract purge: options: CachePurgeOptions -> Promise<CachePurgeResult>

    [<Import("@cloudflare/workers-types", "ChatCompletionContentPartRefusal")>]
    type ChatCompletionContentPartRefusal =
        abstract refusal: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "UserMessageContentPart")>]
    type UserMessageContentPart =
        abstract file: option<SharedLiterals.FileDataFileIdFilename> with get, set

        [<EmitProperty("input_audio")>]
        abstract inputAudio: option<SharedLiterals.DataFormat> with get, set

        [<EmitProperty("image_url")>]
        abstract imageUrl: option<SharedLiterals.DetailUrl> with get, set

        abstract text: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: LiteralUnions.FileImageUrlInputAudioText with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Meta_Llama_3_3_70B_Instruct_Fp8_Fast")>]
    type BaseAiCfMetaLlama3370BInstructFp8Fast =
        abstract postProcessedOutputs: AiCfMetaLlama3370BInstructFp8FastOutput with get, set
        abstract inputs: AiCfMetaLlama3370BInstructFp8FastInput with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_AsyncResponse")>]
    type AiCfBaaiBgeM3AsyncResponse =
        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "TraceItemAlarmEventInfo")>]
    type TraceItemAlarmEventInfo =
        abstract scheduledTime: Date with get

    type ResponseStreamEvent = U15<ResponseStreamEvent.Case0, ResponseStreamEvent.Case1, ResponseStreamEvent.Case2, ResponseStreamEvent.Case3, ResponseStreamEvent.Case4, ResponseStreamEvent.Case5, ResponseStreamEvent.Case6, ResponseStreamEvent.Case7, ResponseStreamEvent.Case8, ResponseStreamEvent.Case9, ResponseStreamEvent.Case10, ResponseStreamEvent.Case11, ResponseStreamEvent.Case12, ResponseStreamEvent.Case13, ResponseStreamEvent.Case14>

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Unum_Uform_Gen2_Qwen_500M")>]
    type BaseAiCfUnumUformGen2Qwen500M =
        abstract postProcessedOutputs: AiCfUnumUformGen2Qwen500MOutput with get, set
        abstract inputs: AiCfUnumUformGen2Qwen500MInput with get, set

    [<Import("@cloudflare/workers-types", "StreamWatermark")>]
    type StreamWatermark =
        abstract position: StreamWatermarkPosition with get, set
        abstract scale: float with get, set
        abstract padding: float with get, set
        abstract opacity: float with get, set
        abstract name: string with get, set
        abstract downloadedFrom: option<string> with get, set
        abstract created: string with get, set
        abstract width: float with get, set
        abstract height: float with get, set
        abstract size: float with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Meta_Llama_4_Scout_17B_16E_Instruct")>]
    type BaseAiCfMetaLlama4Scout17B16EInstruct =
        abstract postProcessedOutputs: AiCfMetaLlama4Scout17B16EInstructOutput with get, set
        abstract inputs: AiCfMetaLlama4Scout17B16EInstructInput with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryRecallResult")>]
    type AgentMemoryRecallResult =
        abstract candidates: ResizeArray<AgentMemoryScoredCandidate> with get, set
        abstract answer: string with get, set
        abstract count: float with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Ai4Bharat_Indictrans2_En_Indic_1B_Input")>]
    type AiCfAi4BharatIndictrans2EnIndic1BInput =
        [<EmitProperty("target_language")>]
        abstract targetLanguage: LiteralUnions.AsmBengAwa7cbeb380 with get, set

        abstract text: U2<ResizeArray<string>, string> with get, set

    [<Import("@cloudflare/workers-types", "MarkdownDocument")>]
    type MarkdownDocument =
        abstract blob: Blob with get, set
        abstract name: string with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunBaseOptions")>]
    type BrowserRunBaseOptions =
        abstract cacheTTL: option<float> with get, set
        abstract actionTimeout: option<float> with get, set
        abstract bestAttempt: option<bool> with get, set
        abstract waitForTimeout: option<float> with get, set
        abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
        abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
        abstract userAgent: option<string> with get, set
        abstract setJavaScriptEnabled: option<bool> with get, set
        abstract setExtraHTTPHeaders: option<obj> with get, set
        abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
        abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
        abstract allowRequestPattern: option<ResizeArray<string>> with get, set
        abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
        abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
        abstract emulateMediaType: option<string> with get, set
        abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
        abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
        abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
        abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Baai_Bge_M3_Output_EmbeddingFor_Contexts")>]
    type AiCfBaaiBgeM3OutputEmbeddingForContexts =
        abstract pooling: option<LiteralUnions.ClsMean> with get, set
        abstract shape: option<AiSentenceSimilarityOutput> with get, set
        abstract response: option<ResizeArray<AiSentenceSimilarityOutput>> with get, set

    [<Import("@cloudflare/workers-types", "FunctionMessage")>]
    type FunctionMessage =
        abstract name: string with get, set
        abstract content: string with get, set
        abstract role: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchListItemsParams")>]
    type AiSearchListItemsParams =
        [<EmitProperty("metadata_filter")>]
        abstract metadataFilter: option<string> with get, set

        abstract source: option<string> with get, set
        abstract status: option<LiteralUnions.CompletedErrorOutdated22d6dfc3> with get, set

        [<EmitProperty("sort_by")>]
        abstract sortBy: option<LiteralUnions.ModifiedAtStatus> with get, set

        abstract search: option<string> with get, set

        [<EmitProperty("per_page")>]
        abstract perPage: option<float> with get, set

        abstract page: option<float> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchItemChunksResponse")>]
    type AiSearchItemChunksResponse =
        [<EmitProperty("result_info")>]
        abstract resultInfo: SharedLiterals.CountLimitOffsetTotal with get, set

        abstract result: ResizeArray<AiSearchItemChunk> with get, set

    [<Import("@cloudflare/workers-types", "Artifacts")>]
    type Artifacts =
        abstract create: name: string * ?opts: Artifacts.Create.Opts -> Promise<ArtifactsCreateRepoResult>
        abstract get: name: string -> Promise<ArtifactsRepo>
        abstract import: params: Artifacts.Import.Params -> Promise<ArtifactsCreateRepoResult>
        abstract list: ?opts: AiSearchItemLogsParams -> Promise<ArtifactsRepoListResult>
        abstract delete: name: string -> Promise<bool>

    [<Import("@cloudflare/workers-types", "ChatCompletionToolChoiceCustom")>]
    type ChatCompletionToolChoiceCustom =
        abstract custom: SharedLiterals.Name2 with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Qwen_Qwen3_30B_A3B_Fp8_Chat_Completion_Response")>]
    type AiCfQwenQwen330BA3BFp8ChatCompletionResponse =
        [<EmitProperty("prompt_logprobs")>]
        abstract promptLogprobs: option<MainModule> with get, set

        abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
        abstract choices: option<ResizeArray<SharedLiterals.FinishReasonIndexE81dd86d>> with get, set
        abstract model: option<string> with get, set
        abstract created: option<float> with get, set
        abstract object: option<string> with get, set
        abstract id: option<string> with get, set

    [<Import("@cloudflare/workers-types", "ImageUpdateOptions")>]
    type ImageUpdateOptions =
        abstract creator: option<string> with get, set
        abstract metadata: option<obj> with get, set
        abstract requireSignedURLs: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "BrowserRunPDFOptions")>]
    type BrowserRunPDFOptions =
        abstract pdfOptions: option<SharedLiterals.DisplayHeaderFooterFooterTe436a4396> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DurableObjectJurisdiction =
        | [<CompiledName("eu")>] Eu
        | [<CompiledName("fedramp")>] Fedramp
        | [<CompiledName("fedramp-high")>] FedrampHigh

    [<Import("@cloudflare/workers-types", "EventTargetEventListenerOptions")>]
    type EventTargetEventListenerOptions =
        abstract capture: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryMemory")>]
    type AgentMemoryMemory =
        abstract updatedAt: Date with get, set
        abstract createdAt: Date with get, set
        abstract sessionId: option<string> with get, set
        abstract content: string with get, set
        abstract summary: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: AgentMemoryMemoryType with get, set

        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "RoleScopedChatInput")>]
    type RoleScopedChatInput =
        abstract name: option<string> with get, set
        abstract content: string with get, set
        abstract role: U2<LiteralUnions.AssistantSystemToolUser, RoleScopedChatInput.Role> with get, set

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Flux_Output")>]
    type AiCfDeepgramFluxOutput =
        [<EmitProperty("end_of_turn_confidence")>]
        abstract endOfTurnConfidence: option<float> with get, set

        abstract words: option<ResizeArray<AiCfDeepgramFluxOutput.Words>> with get, set
        abstract transcript: option<string> with get, set

        [<EmitProperty("audio_window_end")>]
        abstract audioWindowEnd: option<float> with get, set

        [<EmitProperty("audio_window_start")>]
        abstract audioWindowStart: option<float> with get, set

        [<EmitProperty("turn_index")>]
        abstract turnIndex: option<float> with get, set

        abstract event: option<LiteralUnions.EagerEndOfTurnEndOfTurnSta3f421e18> with get, set

        [<EmitProperty("sequence_id")>]
        abstract sequenceId: option<float> with get, set

        [<EmitProperty("request_id")>]
        abstract requestId: option<string> with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryNamespace")>]
    type AgentMemoryNamespace =
        abstract getProfile: profileName: string -> Promise<AgentMemoryProfile>
        abstract deleteProfile: profileName: string -> Promise<unit>

    [<Import("@cloudflare/workers-types", "Ai_Cf_Deepgram_Aura_2_En_Input")>]
    type AiCfDeepgramAura2EnInput =
        [<EmitProperty("bit_rate")>]
        abstract bitRate: option<float> with get, set

        [<EmitProperty("sample_rate")>]
        abstract sampleRate: option<float> with get, set

        abstract text: string with get, set
        abstract container: option<LiteralUnions.NoneOggWav> with get, set
        abstract encoding: option<LiteralUnions.AacAlawFlacLinear16Mp3MulawOpus> with get, set
        abstract speaker: option<LiteralUnions.AmaltheaAndromedaApollo7e3ce24d> with get, set

    [<Import("@cloudflare/workers-types", "ChatCompletionsStreamOptions")>]
    type ChatCompletionsStreamOptions =
        [<EmitProperty("include_obfuscation")>]
        abstract includeObfuscation: option<bool> with get, set

        [<EmitProperty("include_usage")>]
        abstract includeUsage: option<bool> with get, set

    type BodyInit = U9<ReadableStream<Uint8Array>, ArrayBuffer, obj, Blob, URLSearchParams, FormData, seq<BufferSource>, AsyncIterable<BufferSource>, string>

    [<Import("@cloudflare/workers-types", "AlreadyUploadedError")>]
    type AlreadyUploadedError =
        inherit StreamError
        abstract name: string with get, set

    /// <deprecated>
    /// Use the standalone AI Search Workers binding instead.<br/>
    /// See https://developers.cloudflare.com/ai-search/usage/workers-binding/
    /// </deprecated>
    [<Import("@cloudflare/workers-types", "AutoRagAiSearchRequestStreaming")>]
    type AutoRagAiSearchRequestStreaming =
        abstract stream: bool with get, set

        [<EmitProperty("system_prompt")>]
        abstract systemPrompt: option<string> with get, set

        [<EmitProperty("rewrite_query")>]
        abstract rewriteQuery: option<bool> with get, set

        abstract reranking: option<SharedLiterals.EnabledModel> with get, set

        [<EmitProperty("ranking_options")>]
        abstract rankingOptions: option<SharedLiterals.RankerScoreThreshold> with get, set

        [<EmitProperty("max_num_results")>]
        abstract maxNumResults: option<float> with get, set

        abstract filters: option<U2<CompoundFilter, ComparisonFilter>> with get, set
        abstract query: string with get, set

    [<Import("@cloudflare/workers-types", "Base_Ai_Cf_Qwen_Qwq_32B")>]
    type BaseAiCfQwenQwq32B =
        abstract postProcessedOutputs: AiCfQwenQwq32BOutput with get, set
        abstract inputs: AiCfQwenQwq32BInput with get, set

    [<Import("@cloudflare/workers-types", "BasicImageTransformationsGravityCoordinates")>]
    type BasicImageTransformationsGravityCoordinates =
        abstract mode: option<LiteralUnions.BoxCenterRemainder> with get, set
        abstract y: option<float> with get, set
        abstract x: option<float> with get, set

    [<Import("@cloudflare/workers-types", "EventSourceEventSourceInit")>]
    type EventSourceEventSourceInit =
        abstract fetcher: option<SharedLiterals.ConnectFetch> with get, set
        abstract withCredentials: option<bool> with get, set

    type ResponseInputContent = U2<ResponseInputText, ResponseInputImage>

    [<Import("@cloudflare/workers-types", "BaseAiTextGeneration")>]
    type BaseAiTextGeneration =
        abstract postProcessedOutputs: AiTextGenerationOutput with get, set
        abstract inputs: AiTextGenerationInput with get, set

    [<Import("@cloudflare/workers-types", "SqlStorageStatement")>]
    type SqlStorageStatement = interface end

    [<Import("@cloudflare/workers-types", "StreamVideo")>]
    type StreamVideo =
        abstract publicDetails: option<StreamPublicDetails> with get, set
        abstract clippedFromId: option<string> with get, set
        abstract liveInputId: option<string> with get, set
        abstract watermark: option<StreamWatermark> with get, set
        abstract dashPlaybackUrl: string with get, set
        abstract hlsPlaybackUrl: string with get, set
        abstract input: StreamVideoInput with get, set
        abstract duration: float with get, set
        abstract maxDurationSeconds: option<float> with get, set
        abstract maxSizeBytes: option<float> with get, set
        abstract uploadExpiry: option<string> with get, set
        abstract uploaded: option<string> with get, set
        abstract requireSignedURLs: option<bool> with get, set
        abstract allowedOrigins: ResizeArray<string> with get, set
        abstract preview: option<string> with get, set
        abstract size: float with get, set
        abstract scheduledDeletion: option<string> with get, set
        abstract modified: string with get, set
        abstract created: string with get, set
        abstract meta: obj with get, set
        abstract status: StreamVideoStatus with get, set
        abstract readyToStreamAt: option<string> with get, set
        abstract readyToStream: bool with get, set
        abstract thumbnailTimestampPct: float with get, set
        abstract thumbnail: string with get, set
        abstract creator: option<string> with get, set
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "CacheStorage")>]
    type CacheStorage =
        [<EmitProperty("default")>]
        abstract ``default``: Cache with get

        abstract ``open``: cacheName: string -> Promise<Cache>

    [<Import("@cloudflare/workers-types", "InstanceStatus")>]
    type InstanceStatus =
        abstract output: option<obj> with get, set
        abstract error: option<SharedLiterals.MessageName> with get, set
        abstract status: CloudflareWorkersModule.WorkflowInstanceStatus with get, set

    [<Import("@cloudflare/workers-types", "ResponsesInput")>]
    type ResponsesInput =
        abstract truncation: option<LiteralUnions.AutoDisabled> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract tools: option<ResizeArray<Tool>> with get, set

        [<EmitProperty("tool_choice")>]
        abstract toolChoice: option<U2<string, ToolChoiceFunction>> with get, set

        abstract text: option<ResponseTextConfig> with get, set
        abstract temperature: option<float> with get, set

        [<EmitProperty("stream_options")>]
        abstract streamOptions: option<StreamOptions> with get, set

        abstract stream: option<bool> with get, set

        [<EmitProperty("service_tier")>]
        abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

        [<EmitProperty("safety_identifier")>]
        abstract safetyIdentifier: option<string> with get, set

        abstract reasoning: option<Reasoning> with get, set

        [<EmitProperty("prompt_cache_key")>]
        abstract promptCacheKey: option<string> with get, set

        [<EmitProperty("previous_response_id")>]
        abstract previousResponseId: option<string> with get, set

        [<EmitProperty("parallel_tool_calls")>]
        abstract parallelToolCalls: option<bool> with get, set

        [<EmitProperty("max_output_tokens")>]
        abstract maxOutputTokens: option<float> with get, set

        abstract instructions: option<string> with get, set
        abstract input: option<U2<ResponseInput, string>> with get, set
        abstract include: option<ResizeArray<ResponseIncludable>> with get, set
        abstract conversation: option<U2<ResponseConversationParam, string>> with get, set
        abstract background: option<bool> with get, set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ToolChoiceOptions = | [<CompiledName("none")>] ToolChoiceOptions

    [<Import("@cloudflare/workers-types", "WebSearchResponseMetadata")>]
    type WebSearchResponseMetadata =
        abstract latencyMs: float with get, set
        abstract requestId: string with get, set
        abstract query: string with get, set

    [<Import("@cloudflare/workers-types", "WorkerStubEntrypointOptions")>]
    type WorkerStubEntrypointOptions =
        abstract limits: option<WorkerdResourceLimits> with get, set
        abstract props: option<obj> with get, set

    [<Import("@cloudflare/workers-types", "MessageBatchMetadata")>]
    type MessageBatchMetadata =
        abstract metrics: MessageBatchMetrics with get, set

    [<Import("@cloudflare/workers-types", "ReadableStreamDefaultReader")>]
    type ReadableStreamDefaultReader<'R> =
        [<EmitConstructor>]
        abstract Create: stream: ReadableStream<option<obj>> -> ReadableStreamDefaultReader<'R>

        abstract closed: Promise<unit> with get
        abstract cancel: ?reason: obj -> Promise<unit>
        abstract read: unit -> Promise<ReadableStreamReadResult<option<obj>>>
        abstract releaseLock: unit -> unit

    [<Import("@cloudflare/workers-types", "ResponseFunctionCallArgumentsDeltaEvent")>]
    type ResponseFunctionCallArgumentsDeltaEvent =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        [<EmitProperty("sequence_number")>]
        abstract sequenceNumber: float with get, set

        [<EmitProperty("output_index")>]
        abstract outputIndex: float with get, set

        [<EmitProperty("item_id")>]
        abstract itemId: string with get, set

        abstract delta: string with get, set

    [<Import("@cloudflare/workers-types", "WebSocketAcceptOptions")>]
    type WebSocketAcceptOptions =
        abstract allowHalfOpen: option<bool> with get, set

    [<Import("@cloudflare/workers-types", "AiSearchJobLogsResponse")>]
    type AiSearchJobLogsResponse =
        [<EmitProperty("result_info")>]
        abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

        abstract result: ResizeArray<AiSearchJobLog> with get, set

    [<Import("@cloudflare/workers-types", "Socket")>]
    type Socket =
        abstract secureTransport: LiteralUnions.OffOnStarttls with get
        abstract upgraded: bool with get
        abstract opened: Promise<SocketInfo> with get
        abstract closed: Promise<unit> with get
        abstract writable: WritableStream<option<obj>> with get
        abstract readable: ReadableStream<option<obj>> with get
        abstract close: unit -> Promise<unit>
        abstract startTls: ?options: TlsOptions -> Socket

    [<Import("@cloudflare/workers-types", "Element")>]
    type Element =
        abstract namespaceURI: string with get
        abstract removed: bool with get
        abstract attributes: seq<ResizeArray<string>> with get
        abstract tagName: string with get, set
        abstract getAttribute: name: string -> option<string>
        abstract hasAttribute: name: string -> bool
        abstract setAttribute: name: string * value: string -> Element
        abstract removeAttribute: name: string -> Element
        abstract before: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Element
        abstract after: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Element
        abstract prepend: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Element
        abstract append: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Element
        abstract replace: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Element
        abstract remove: unit -> Element
        abstract removeAndKeepContent: unit -> Element
        abstract setInnerContent: content: U3<ReadableStream<option<obj>>, Response, string> * ?options: ContentOptions -> Element
        abstract onEndTag: handler: (EndTag -> option<Promise<unit>>) -> unit

    [<Import("@cloudflare/workers-types", "Ai_Cf_Mistralai_Mistral_Small_3_1_24B_Instruct_Prompt")>]
    type AiCfMistralaiMistralSmall3124BInstructPrompt =
        [<EmitProperty("presence_penalty")>]
        abstract presencePenalty: option<float> with get, set

        [<EmitProperty("frequency_penalty")>]
        abstract frequencyPenalty: option<float> with get, set

        [<EmitProperty("repetition_penalty")>]
        abstract repetitionPenalty: option<float> with get, set

        abstract seed: option<float> with get, set

        [<EmitProperty("top_k")>]
        abstract topK: option<float> with get, set

        [<EmitProperty("top_p")>]
        abstract topP: option<float> with get, set

        abstract temperature: option<float> with get, set

        [<EmitProperty("max_tokens")>]
        abstract maxTokens: option<float> with get, set

        abstract stream: option<bool> with get, set
        abstract raw: option<bool> with get, set

        [<EmitProperty("guided_json")>]
        abstract guidedJson: option<obj> with get, set

        abstract prompt: string with get, set

    [<Import("@cloudflare/workers-types", "AgentMemoryProfile")>]
    type AgentMemoryProfile =
        abstract get: memoryId: string -> Promise<AgentMemoryMemory>
        abstract delete: memoryId: string -> Promise<AgentMemoryMemory>
        abstract remember: memory: AgentMemoryIncomingMemory -> Promise<AgentMemoryMemory>
        abstract ingest: messages: seq<AgentMemoryMessage> * ?options: AgentMemoryIngestOptions -> Promise<unit>
        abstract getSummary: ?options: AgentMemoryGetSummaryOptions -> Promise<AgentMemoryGetSummaryResponse>
        abstract recall: query: string * ?options: AgentMemoryRecallOptions -> Promise<AgentMemoryRecallResult>
        abstract list: ?options: AgentMemoryListMemoriesOptions -> Promise<AgentMemoryListMemoriesResult>
        abstract deleteSession: sessionId: string -> Promise<unit>

    [<Import("@cloudflare/workers-types", "ResponseFunctionToolCallOutputItem")>]
    type ResponseFunctionToolCallOutputItem =
        abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract output: U2<ResizeArray<ResponseInputContent>, string> with get, set

        [<EmitProperty("call_id")>]
        abstract callId: string with get, set

        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "ResponseFunctionToolCallItem")>]
    type ResponseFunctionToolCallItem =
        inherit ResponseFunctionToolCall
        abstract id: string with get, set

    [<Import("@cloudflare/workers-types", "AiSearchOptions")>]
    type AiSearchOptions =
        abstract cache: option<SharedLiterals.CacheThresholdEnabled> with get, set
        abstract reranking: option<SharedLiterals.EnabledMatchThresholdModel> with get, set

        [<EmitProperty("query_rewrite")>]
        abstract queryRewrite: option<SharedLiterals.EnabledModelRewritePrompt> with get, set

        abstract retrieval: option<SharedLiterals.BoostByContextAef2ce5f> with get, set
        abstract Item: key: string -> option<obj>

    [<Import("@cloudflare/workers-types", "SystemMessage")>]
    type SystemMessage =
        abstract name: option<string> with get, set
        abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set
        abstract role: string with get, set

    [<Import("@cloudflare/workers-types", "TextDecoderDecodeOptions")>]
    type TextDecoderDecodeOptions =
        abstract stream: bool with get, set

    [<Import("@cloudflare/workers-types", "EasyInputMessage")>]
    type EasyInputMessage =
        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract role: LiteralUnions.AssistantDeveloperSystemUser with get, set
        abstract content: U2<ResponseInputMessageContentList, string> with get, set

    [<Import("@cloudflare/workers-types", "UsageTags")>]
    type UsageTags =
        [<EmitProperty("total_tokens")>]
        abstract totalTokens: float with get, set

        [<EmitProperty("completion_tokens")>]
        abstract completionTokens: float with get, set

        [<EmitProperty("prompt_tokens")>]
        abstract promptTokens: float with get, set

    [<Import("@cloudflare/workers-types", "TraceItemFetchEventInfoRequest")>]
    type TraceItemFetchEventInfoRequest =
        abstract url: string with get
        abstract method: string with get
        abstract headers: obj with get
        abstract cf: option<obj> with get
        abstract getUnredacted: unit -> TraceItemFetchEventInfoRequest

    [<Import("@cloudflare/workers-types", "UserMessage")>]
    type UserMessage =
        abstract name: option<string> with get, set
        abstract content: U2<ResizeArray<UserMessageContentPart>, string> with get, set
        abstract role: string with get, set

    [<Import("@cloudflare/workers-types", "JsonWebKey")>]
    type JsonWebKey =
        abstract k: option<string> with get, set
        abstract oth: option<ResizeArray<RsaOtherPrimesInfo>> with get, set
        abstract qi: option<string> with get, set
        abstract dq: option<string> with get, set
        abstract dp: option<string> with get, set
        abstract q: option<string> with get, set
        abstract p: option<string> with get, set
        abstract e: option<string> with get, set
        abstract n: option<string> with get, set
        abstract d: option<string> with get, set
        abstract y: option<string> with get, set
        abstract x: option<string> with get, set
        abstract crv: option<string> with get, set
        abstract ext: option<bool> with get, set
        abstract alg: option<string> with get, set

        [<EmitProperty("key_ops")>]
        abstract keyOps: option<ResizeArray<string>> with get, set

        [<EmitProperty("use")>]
        abstract ``use``: option<string> with get, set

        abstract kty: string with get, set

    type ICloudflare =
        [<Erase>]
        member _.exports: CloudflareWorkersTypes.Cloudflare.Exports = JS.undefined

        [<Erase>]
        member _.env: CloudflareWorkersTypes.Cloudflare.Env = JS.undefined

    type ICloudflareWorkersModule =
        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "withEnv")>]
        static member withEnv(newEnv: option<obj>, fn: unit -> option<obj>) : option<obj> = JS.undefined

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "waitUntil")>]
        static member waitUntil(promise: Promise<option<obj>>) : unit = JS.undefined

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "withEnvAndExports")>]
        static member withEnvAndExports(newEnv: option<obj>, newExports: option<obj>, fn: unit -> option<obj>) : option<obj> = JS.undefined

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "withExports")>]
        static member withExports(newExports: option<obj>, fn: unit -> option<obj>) : option<obj> = JS.undefined

    type IWebAssembly =
        [<Import("@cloudflare/workers-types.WebAssembly", "instantiate")>]
        static member instantiate(``module``: obj, ?imports: obj) : Promise<CloudflareWorkersTypes.WebAssembly.Instance> = JS.undefined

        [<Import("@cloudflare/workers-types.WebAssembly", "validate")>]
        static member validate(bytes: obj) : bool = JS.undefined

    type ``Cloudflare:node`` =
        [<Import("@cloudflare/workers-types.cloudflare:node", "httpServerHandler")>]
        static member httpServerHandler(options: obj) : CloudflareWorkersTypes.ExportedHandler<option<obj>, option<obj>, option<obj>, option<obj>> = JS.undefined

        [<Import("@cloudflare/workers-types.cloudflare:node", "httpServerHandler")>]
        static member httpServerHandler(port: float) : CloudflareWorkersTypes.ExportedHandler<option<obj>, option<obj>, option<obj>, option<obj>> = JS.undefined

    module Ai =
        type Run =
            [<EmitProperty("request_id")>]
            abstract requestId: string with get, set

        type Models =
            abstract properties: ResizeArray<SharedLiterals.PropertyIdValue> with get, set
            abstract tags: ResizeArray<string> with get, set
            abstract task: SharedLiterals.DescriptionIdName2 with get, set
            abstract description: string with get, set
            abstract name: string with get, set
            abstract source: float with get, set
            abstract id: string with get, set

        module Models =
            type Params =
                abstract task: option<string> with get, set
                abstract source: option<float> with get, set
                abstract search: option<string> with get, set

                [<EmitProperty("per_page")>]
                abstract perPage: option<float> with get, set

                abstract page: option<float> with get, set

                [<EmitProperty("hide_experimental")>]
                abstract hideExperimental: option<bool> with get, set

                abstract author: option<string> with get, set

        module Run =
            type Inputs =
                abstract requests: ResizeArray<proptypekey<proptypekey<obj, obj>, string>> with get, set

            type Options =
                abstract signal: option<AbortSignal> with get, set
                abstract extraHeaders: option<obj> with get, set
                abstract prefix: option<string> with get, set
                abstract returnRawResponse: option<bool> with get, set
                abstract gateway: option<GatewayOptions> with get, set
                abstract tags: option<ResizeArray<string>> with get, set
                abstract websocket: option<bool> with get, set
                abstract queueRequest: option<bool> with get, set

            module Inputs =
                type Case2 =
                    abstract stream: bool with get, set

            module Options =
                type Case4 =
                    abstract signal: option<AbortSignal> with get, set
                    abstract extraHeaders: option<obj> with get, set
                    abstract prefix: option<string> with get, set
                    abstract returnRawResponse: option<bool> with get, set
                    abstract gateway: option<GatewayOptions> with get, set
                    abstract tags: option<ResizeArray<string>> with get, set
                    abstract websocket: option<bool> with get, set
                    abstract queueRequest: option<bool> with get, set

                type Case3 =
                    abstract signal: option<AbortSignal> with get, set
                    abstract extraHeaders: option<obj> with get, set
                    abstract prefix: option<string> with get, set
                    abstract returnRawResponse: option<bool> with get, set
                    abstract gateway: option<GatewayOptions> with get, set
                    abstract tags: option<ResizeArray<string>> with get, set
                    abstract websocket: option<bool> with get, set
                    abstract queueRequest: option<bool> with get, set

                type Case2 =
                    abstract signal: option<AbortSignal> with get, set
                    abstract extraHeaders: option<obj> with get, set
                    abstract prefix: option<string> with get, set
                    abstract returnRawResponse: option<bool> with get, set
                    abstract gateway: option<GatewayOptions> with get, set
                    abstract tags: option<ResizeArray<string>> with get, set
                    abstract websocket: option<bool> with get, set
                    abstract queueRequest: option<bool> with get, set

    module AiCfDeepgramFluxOutput =
        type Words =
            abstract confidence: float with get, set
            abstract word: string with get, set

    module AiCfDeepgramNova3Output =
        type Results =
            abstract sentiments: option<Results.Sentiments> with get, set
            abstract summary: option<Results.Summary> with get, set
            abstract channels: option<ResizeArray<Results.Channels>> with get, set

        module Results =
            type Channels =
                abstract alternatives: option<ResizeArray<Channels.Alternatives>> with get, set

            type Sentiments =
                abstract average: option<Sentiments.Average> with get, set
                abstract segments: option<ResizeArray<Sentiments.Segments>> with get, set

            type Summary =
                abstract short: option<string> with get, set
                abstract result: option<string> with get, set

            module Channels =
                type Alternatives =
                    abstract words: option<ResizeArray<Alternatives.Words>> with get, set
                    abstract transcript: option<string> with get, set
                    abstract confidence: option<float> with get, set

                module Alternatives =
                    type Words =
                        abstract word: option<string> with get, set
                        abstract start: option<float> with get, set

                        [<EmitProperty("end")>]
                        abstract ``end``: option<float> with get, set

                        abstract confidence: option<float> with get, set

            module Sentiments =
                type Average =
                    [<EmitProperty("sentiment_score")>]
                    abstract sentimentScore: option<float> with get, set

                    abstract sentiment: option<string> with get, set

                type Segments =
                    [<EmitProperty("sentiment_score")>]
                    abstract sentimentScore: option<float> with get, set

                    abstract sentiment: option<string> with get, set

                    [<EmitProperty("end_word")>]
                    abstract endWord: option<float> with get, set

                    [<EmitProperty("start_word")>]
                    abstract startWord: option<float> with get, set

                    abstract text: option<string> with get, set

    module AiCfGoogleGemma312BItMessages =
        type Messages =
            abstract content: option<U2<ResizeArray<SharedLiterals.ImageUrlTextType>, string>> with get, set
            abstract role: option<string> with get, set

    module AiCfMetaLlama3211BVisionInstructPrompt =
        type Image = interface end

    module AiCfMetaLlama3370BInstructFp8FastAsyncBatch =
        type Requests =
            [<EmitProperty("response_format")>]
            abstract responseFormat: option<AiCfMetaLlama3370BInstructFp8FastJSONMode2> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            [<EmitProperty("repetition_penalty")>]
            abstract repetitionPenalty: option<float> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            abstract stream: option<bool> with get, set
            abstract prompt: option<string> with get, set

            [<EmitProperty("external_reference")>]
            abstract externalReference: option<string> with get, set

    module AiCfMetaLlamaGuard38BInput =
        type ResponseFormat =
            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

        type Messages =
            abstract content: string with get, set
            abstract role: obj with get, set

    module AiCfMetaLlamaGuard38BOutput =
        type Response =
            abstract categories: option<ResizeArray<string>> with get, set
            abstract safe: option<bool> with get, set

    module AiCfOpenaiWhisperLargeV3TurboOutput =
        type Segments =
            abstract words: option<ResizeArray<SharedLiterals.EndStartWord2>> with get, set

            [<EmitProperty("no_speech_prob")>]
            abstract noSpeechProb: option<float> with get, set

            [<EmitProperty("compression_ratio")>]
            abstract compressionRatio: option<float> with get, set

            [<EmitProperty("avg_logprob")>]
            abstract avgLogprob: option<float> with get, set

            abstract temperature: option<float> with get, set
            abstract text: option<string> with get, set

            [<EmitProperty("end")>]
            abstract ``end``: option<float> with get, set

            abstract start: option<float> with get, set

        type TranscriptionInfo =
            [<EmitProperty("duration_after_vad")>]
            abstract durationAfterVad: option<float> with get, set

            abstract duration: option<float> with get, set

            [<EmitProperty("language_probability")>]
            abstract languageProbability: option<float> with get, set

            abstract language: option<string> with get, set

    module AiCfQwenQwen25Coder32BInstructMessages =
        type Messages =
            abstract content: string with get, set
            abstract role: string with get, set

    module AiGateway =
        type GetLog =
            [<EmitProperty("created_at")>]
            abstract createdAt: Date with get, set

            [<EmitProperty("response_head_complete")>]
            abstract responseHeadComplete: bool with get, set

            [<EmitProperty("response_head")>]
            abstract responseHead: option<string> with get, set

            [<EmitProperty("response_size")>]
            abstract responseSize: float with get, set

            [<EmitProperty("request_head_complete")>]
            abstract requestHeadComplete: bool with get, set

            [<EmitProperty("request_head")>]
            abstract requestHead: option<string> with get, set

            [<EmitProperty("request_size")>]
            abstract requestSize: float with get, set

            [<EmitProperty("custom_cost")>]
            abstract customCost: option<bool> with get, set

            abstract cost: option<float> with get, set
            abstract step: option<float> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("tokens_out")>]
            abstract tokensOut: option<float> with get, set

            [<EmitProperty("tokens_in")>]
            abstract tokensIn: option<float> with get, set

            abstract cached: bool with get, set
            abstract success: bool with get, set

            [<EmitProperty("response_content_type")>]
            abstract responseContentType: option<string> with get, set

            [<EmitProperty("status_code")>]
            abstract statusCode: float with get, set

            [<EmitProperty("request_content_type")>]
            abstract requestContentType: option<string> with get, set

            [<EmitProperty("request_type")>]
            abstract requestType: option<string> with get, set

            abstract duration: float with get, set
            abstract path: string with get, set

            [<EmitProperty("model_type")>]
            abstract modelType: option<string> with get, set

            abstract model: string with get, set
            abstract provider: string with get, set
            abstract id: string with get, set

        module PatchLog =
            type Data =
                abstract metadata: option<obj> with get, set
                abstract feedback: option<LiteralUnions.I1I1> with get, set
                abstract score: option<float> with get, set

        module Run =
            type Data =
                abstract query: option<obj> with get, set
                abstract headers: SharedLiterals.AuthorizationContentTypeC32cb5e3e with get, set
                abstract endpoint: string with get, set
                abstract provider: U2<LiteralUnions.AdobeFireflyAnthropicAws_7e6a892e, string> with get, set

            type Options =
                abstract signal: option<AbortSignal> with get, set
                abstract extraHeaders: option<obj> with get, set
                abstract gateway: option<UniversalGatewayOptions> with get, set

            module Options =
                type Gateway =
                    abstract retries: option<GatewayRetries> with get, set
                    abstract requestTimeoutMs: option<float> with get, set
                    abstract eventId: option<string> with get, set
                    abstract collectLog: option<bool> with get, set
                    abstract metadata: option<obj> with get, set
                    abstract skipCache: option<bool> with get, set
                    abstract cacheTtl: option<float> with get, set
                    abstract cacheKey: option<string> with get, set
                    abstract id: string with get, set

    module AiSearchInstance =
        type Search =
            abstract chunks: ResizeArray<SharedLiterals.IdItemScoreScoringDetailsTextType> with get, set

            [<EmitProperty("search_query")>]
            abstract searchQuery: string with get, set

        type ChatCompletions =
            abstract chunks: ResizeArray<SharedLiterals.IdItemScoreScoringDetailsTextType> with get, set
            abstract choices: ResizeArray<SharedLiterals.IndexMessage> with get, set
            abstract model: option<string> with get, set
            abstract object: option<string> with get, set
            abstract id: option<string> with get, set
            abstract Item: key: string -> option<obj>

        type Stats =
            abstract engine: option<SharedLiterals.R2Vectorize> with get, set

            [<EmitProperty("last_activity")>]
            abstract lastActivity: option<string> with get, set

            abstract outdated: option<float> with get, set
            abstract skipped: option<float> with get, set
            abstract error: option<float> with get, set
            abstract completed: option<float> with get, set
            abstract running: option<float> with get, set
            abstract queued: option<float> with get, set

        module ChatCompletions =
            type Params =
                [<EmitProperty("ai_search_options")>]
                abstract aiSearchOptions: option<AiSearchOptions> with get, set

                abstract stream: option<bool> with get, set
                abstract model: option<string> with get, set
                abstract messages: ResizeArray<AiSearchMessage> with get, set
                abstract Item: key: string -> option<obj>

            module Params =
                type Case2 =
                    [<EmitProperty("ai_search_options")>]
                    abstract aiSearchOptions: option<AiSearchOptions> with get, set

                    abstract stream: option<bool> with get, set
                    abstract model: option<string> with get, set
                    abstract messages: ResizeArray<AiSearchMessage> with get, set
                    abstract Item: key: string -> option<obj>

        module Update =
            type Config =
                abstract metadata: option<obj> with get, set

                [<EmitProperty("sync_interval")>]
                abstract syncInterval: option<LiteralUnions.I14400I21600I3600I43200I7200I86400> with get, set

                [<EmitProperty("namespace")>]
                abstract ``namespace``: option<string> with get, set

                [<EmitProperty("custom_metadata")>]
                abstract customMetadata: option<ResizeArray<SharedLiterals.DataTypeFieldName>> with get, set

                [<EmitProperty("cache_threshold")>]
                abstract cacheThreshold: option<LiteralUnions.AnythingGoesClose50b421b1> with get, set

                abstract cache: option<bool> with get, set

                [<EmitProperty("max_num_results")>]
                abstract maxNumResults: option<float> with get, set

                [<EmitProperty("score_threshold")>]
                abstract scoreThreshold: option<float> with get, set

                [<EmitProperty("chunk_overlap")>]
                abstract chunkOverlap: option<float> with get, set

                [<EmitProperty("chunk_size")>]
                abstract chunkSize: option<float> with get, set

                abstract chunk: option<bool> with get, set

                [<EmitProperty("retrieval_options")>]
                abstract retrievalOptions: option<SharedLiterals.BoostByKeywordMatchMode> with get, set

                [<EmitProperty("indexing_options")>]
                abstract indexingOptions: option<SharedLiterals.KeywordTokenizer> with get, set

                [<EmitProperty("fusion_method")>]
                abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

                [<EmitProperty("index_method")>]
                abstract indexMethod: option<SharedLiterals.KeywordVector> with get, set

                [<EmitProperty("hybrid_search_enabled")>]
                abstract hybridSearchEnabled: option<bool> with get, set

                [<EmitProperty("reranking_model")>]
                abstract rerankingModel: option<string> with get, set

                [<EmitProperty("rewrite_model")>]
                abstract rewriteModel: option<string> with get, set

                [<EmitProperty("ai_search_model")>]
                abstract aiSearchModel: option<string> with get, set

                [<EmitProperty("embedding_model")>]
                abstract embeddingModel: option<string> with get, set

                abstract reranking: option<bool> with get, set

                [<EmitProperty("rewrite_query")>]
                abstract rewriteQuery: option<bool> with get, set

                [<EmitProperty("ai_gateway_id")>]
                abstract aiGatewayId: option<string> with get, set

                [<EmitProperty("token_id")>]
                abstract tokenId: option<string> with get, set

                [<EmitProperty("source_params")>]
                abstract sourceParams: option<obj> with get, set

                abstract source: option<string> with get, set

                [<EmitProperty("type")>]
                abstract ``type``: option<string> with get, set

                abstract id: option<string> with get, set

    module AiSearchItem =
        type Download =
            abstract size: float with get, set
            abstract filename: string with get, set
            abstract contentType: string with get, set
            abstract body: ReadableStream<option<obj>> with get, set

        type Chunks =
            [<EmitProperty("result_info")>]
            abstract resultInfo: SharedLiterals.CountLimitOffsetTotal with get, set

            abstract result: ResizeArray<AiSearchItemChunk> with get, set

        type Logs =
            [<EmitProperty("result_info")>]
            abstract resultInfo: SharedLiterals.CountCursorPerPageTruncated with get, set

            abstract result: ResizeArray<AiSearchItemLog> with get, set

        module Chunks =
            type Params =
                abstract offset: option<float> with get, set
                abstract limit: option<float> with get, set

        module Logs =
            type Params =
                abstract cursor: option<string> with get, set
                abstract limit: option<float> with get, set

    module AiSearchItems =
        type List =
            [<EmitProperty("result_info")>]
            abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

            abstract result: ResizeArray<AiSearchItemInfo> with get, set

        module List =
            type Params =
                [<EmitProperty("metadata_filter")>]
                abstract metadataFilter: option<string> with get, set

                abstract source: option<string> with get, set
                abstract status: option<LiteralUnions.CompletedErrorOutdated22d6dfc3> with get, set

                [<EmitProperty("sort_by")>]
                abstract sortBy: option<LiteralUnions.ModifiedAtStatus> with get, set

                abstract search: option<string> with get, set

                [<EmitProperty("per_page")>]
                abstract perPage: option<float> with get, set

                abstract page: option<float> with get, set

        module Upload =
            type Options =
                abstract metadata: option<obj> with get, set

        module UploadAndPoll =
            type Options =
                abstract timeoutMs: option<float> with get, set
                abstract pollIntervalMs: option<float> with get, set
                abstract metadata: option<obj> with get, set

    module AiSearchJob =
        type Logs =
            [<EmitProperty("result_info")>]
            abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

            abstract result: ResizeArray<AiSearchJobLog> with get, set

        module Logs =
            type Params =
                [<EmitProperty("per_page")>]
                abstract perPage: option<float> with get, set

                abstract page: option<float> with get, set

    module AiSearchJobs =
        type List =
            [<EmitProperty("result_info")>]
            abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

            abstract result: ResizeArray<AiSearchJobInfo> with get, set

        module Create =
            type Params =
                abstract description: option<string> with get, set

        module List =
            type Params =
                [<EmitProperty("per_page")>]
                abstract perPage: option<float> with get, set

                abstract page: option<float> with get, set

    module AiSearchMultiChatCompletionsRequest =
        type AiSearchOptions =
            [<EmitProperty("instance_ids")>]
            abstract instanceIds: ResizeArray<string> with get, set

            abstract cache: option<SharedLiterals.CacheThresholdEnabled> with get, set
            abstract reranking: option<SharedLiterals.EnabledMatchThresholdModel> with get, set

            [<EmitProperty("query_rewrite")>]
            abstract queryRewrite: option<SharedLiterals.EnabledModelRewritePrompt> with get, set

            abstract retrieval: option<SharedLiterals.BoostByContextAef2ce5f> with get, set
            abstract Item: key: string -> option<obj>

    module AiSearchNamespace =
        type List =
            [<EmitProperty("result_info")>]
            abstract resultInfo: option<SharedLiterals.CountPagePerPageTotalCount> with get, set

            abstract result: ResizeArray<AiSearchInstanceInfo> with get, set

        type Search =
            abstract errors: option<ResizeArray<AiSearchMultiSearchError>> with get, set
            abstract chunks: ResizeArray<AiSearchMultiSearchChunk> with get, set

            [<EmitProperty("search_query")>]
            abstract searchQuery: string with get, set

        type ChatCompletions =
            abstract errors: option<ResizeArray<AiSearchMultiSearchError>> with get, set
            abstract chunks: ResizeArray<AiSearchMultiSearchChunk> with get, set
            abstract Item: key: string -> option<obj>

        module ChatCompletions =
            type Params =
                abstract stream: bool with get, set

                [<EmitProperty("ai_search_options")>]
                abstract aiSearchOptions: AiSearchMultiSearchOptions with get, set

                abstract Item: key: string -> option<obj>

            module Params =
                type AiSearchOptions =
                    [<EmitProperty("instance_ids")>]
                    abstract instanceIds: ResizeArray<string> with get, set

                    abstract cache: option<SharedLiterals.CacheThresholdEnabled> with get, set
                    abstract reranking: option<SharedLiterals.EnabledMatchThresholdModel> with get, set

                    [<EmitProperty("query_rewrite")>]
                    abstract queryRewrite: option<SharedLiterals.EnabledModelRewritePrompt> with get, set

                    abstract retrieval: option<SharedLiterals.BoostByContextAef2ce5f> with get, set
                    abstract Item: key: string -> option<obj>

                type Case2 =
                    [<EmitProperty("ai_search_options")>]
                    abstract aiSearchOptions: AiSearchMultiSearchOptions with get, set

                    abstract Item: key: string -> option<obj>

        module List =
            type Params =
                [<EmitProperty("order_by_direction")>]
                abstract orderByDirection: option<LiteralUnions.AscDesc> with get, set

                [<EmitProperty("order_by")>]
                abstract orderBy: option<string> with get, set

                abstract search: option<string> with get, set

                [<EmitProperty("per_page")>]
                abstract perPage: option<float> with get, set

                abstract page: option<float> with get, set

        module Search =
            type Chunks =
                [<EmitProperty("instance_id")>]
                abstract instanceId: string with get, set

                [<EmitProperty("scoring_details")>]
                abstract scoringDetails: option<SharedLiterals.FusionMethodKeyword6c9246d8> with get, set

                abstract item: SharedLiterals.KeyMetadataTimestamp with get, set
                abstract text: string with get, set
                abstract score: float with get, set

                [<EmitProperty("type")>]
                abstract ``type``: string with get, set

                abstract id: string with get, set

    module AiTextGenerationOutput =
        type ToolCalls = interface end

    module AiTextGenerationToolInput =
        type Type = interface end

    module Artifacts =
        module Create =
            type Opts =
                abstract setDefaultBranch: option<string> with get, set
                abstract description: option<string> with get, set
                abstract readOnly: option<bool> with get, set

        module Import =
            type Params =
                abstract target: Params.Target with get, set
                abstract source: Params.Source with get, set

            module Params =
                type Target =
                    abstract opts: option<Target.Opts> with get, set
                    abstract name: string with get, set

                type Source =
                    abstract depth: option<float> with get, set
                    abstract branch: option<string> with get, set
                    abstract url: string with get, set

                module Target =
                    type Opts =
                        abstract readOnly: option<bool> with get, set
                        abstract description: option<string> with get, set

        module List =
            type Opts =
                abstract cursor: option<string> with get, set
                abstract limit: option<float> with get, set

    module ArtifactsRepo =
        module Fork =
            type Opts =
                abstract defaultBranchOnly: option<bool> with get, set
                abstract readOnly: option<bool> with get, set
                abstract description: option<string> with get, set

    module ArtifactsRepoListResult =
        type Repos =
            abstract source: option<string> with get, set
            abstract lastPushAt: option<string> with get, set
            abstract updatedAt: string with get, set
            abstract defaultBranch: string with get, set
            abstract createdAt: string with get, set
            abstract id: string with get, set
            abstract readOnly: bool with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set

    module AutoRAG =
        type Search =
            [<EmitProperty("next_page")>]
            abstract nextPage: option<string> with get, set

            [<EmitProperty("has_more")>]
            abstract hasMore: bool with get, set

            abstract data: ResizeArray<SharedLiterals.AttributesContentFileF6de1a38> with get, set

            [<EmitProperty("search_query")>]
            abstract searchQuery: string with get, set

            abstract object: string with get, set

        type AiSearch =
            abstract response: string with get, set

            [<EmitProperty("next_page")>]
            abstract nextPage: option<string> with get, set

            [<EmitProperty("has_more")>]
            abstract hasMore: bool with get, set

            abstract data: ResizeArray<SharedLiterals.AttributesContentFileF6de1a38> with get, set

            [<EmitProperty("search_query")>]
            abstract searchQuery: string with get, set

            abstract object: string with get, set

        module AiSearch =
            type Params =
                abstract stream: bool with get, set

                [<EmitProperty("system_prompt")>]
                abstract systemPrompt: option<string> with get, set

                [<EmitProperty("rewrite_query")>]
                abstract rewriteQuery: option<bool> with get, set

                abstract reranking: option<SharedLiterals.EnabledModel> with get, set

                [<EmitProperty("ranking_options")>]
                abstract rankingOptions: option<SharedLiterals.RankerScoreThreshold> with get, set

                [<EmitProperty("max_num_results")>]
                abstract maxNumResults: option<float> with get, set

                abstract filters: option<U2<CompoundFilter, ComparisonFilter>> with get, set
                abstract query: string with get, set

            module Params =
                type Case2 =
                    [<EmitProperty("system_prompt")>]
                    abstract systemPrompt: option<string> with get, set

                    abstract stream: option<bool> with get, set

                    [<EmitProperty("rewrite_query")>]
                    abstract rewriteQuery: option<bool> with get, set

                    abstract reranking: option<SharedLiterals.EnabledModel> with get, set

                    [<EmitProperty("ranking_options")>]
                    abstract rankingOptions: option<SharedLiterals.RankerScoreThreshold> with get, set

                    [<EmitProperty("max_num_results")>]
                    abstract maxNumResults: option<float> with get, set

                    abstract filters: option<U2<CompoundFilter, ComparisonFilter>> with get, set
                    abstract query: string with get, set

    module BaseAiAutomaticSpeechRecognition =
        type PostProcessedOutputs =
            abstract vtt: option<string> with get, set
            abstract words: option<ResizeArray<SharedLiterals.EndStartWord>> with get, set
            abstract text: option<string> with get, set

        type Inputs =
            abstract audio: AiSentenceSimilarityOutput with get, set

    module BaseAiCfGoogleGemma312BIt =
        type PostProcessedOutputs =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

            abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
            abstract response: string with get, set

    module BaseAiCfGoogleGemma426BA4BIT =
        type Inputs =
            abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<WebSearchOptions> with get, set

            abstract user: option<string> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

            abstract stream: option<bool> with get, set
            abstract store: option<bool> with get, set
            abstract stop: option<U2<ResizeArray<string>, string>> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<ResponseFormat> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            abstract prediction: option<PredictionContent> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract n: option<float> with get, set
            abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<float> with get, set

            abstract logprobs: option<bool> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<obj> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            abstract audio: option<AudioParams> with get, set
            abstract model: option<string> with get, set
            abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

    module BaseAiCfMetaLlama3211BVisionInstruct =
        type PostProcessedOutputs =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

            abstract response: option<string> with get, set

    module BaseAiCfMetaLlama4Scout17B16EInstruct =
        type PostProcessedOutputs =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<SharedLiterals.FunctionIdType2>> with get, set

            abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
            abstract response: string with get, set

    module BaseAiCfMistralaiMistralSmall3124BInstruct =
        type PostProcessedOutputs =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

            abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
            abstract response: string with get, set

    module BaseAiCfMoonshotaiKimiK25 =
        type Inputs =
            abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<WebSearchOptions> with get, set

            abstract user: option<string> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

            abstract stream: option<bool> with get, set
            abstract store: option<bool> with get, set
            abstract stop: option<U2<ResizeArray<string>, string>> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<ResponseFormat> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            abstract prediction: option<PredictionContent> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract n: option<float> with get, set
            abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<float> with get, set

            abstract logprobs: option<bool> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<obj> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            abstract audio: option<AudioParams> with get, set
            abstract model: option<string> with get, set
            abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

    module BaseAiCfMoonshotaiKimiK26 =
        type Inputs =
            abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<WebSearchOptions> with get, set

            abstract user: option<string> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

            abstract stream: option<bool> with get, set
            abstract store: option<bool> with get, set
            abstract stop: option<U2<ResizeArray<string>, string>> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<ResponseFormat> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            abstract prediction: option<PredictionContent> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract n: option<float> with get, set
            abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<float> with get, set

            abstract logprobs: option<bool> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<obj> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            abstract audio: option<AudioParams> with get, set
            abstract model: option<string> with get, set
            abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

    module BaseAiCfNvidiaNemotron3120BA12B =
        type Inputs =
            abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<WebSearchOptions> with get, set

            abstract user: option<string> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

            abstract stream: option<bool> with get, set
            abstract store: option<bool> with get, set
            abstract stop: option<U2<ResizeArray<string>, string>> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<ResponseFormat> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            abstract prediction: option<PredictionContent> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract n: option<float> with get, set
            abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<float> with get, set

            abstract logprobs: option<bool> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<obj> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            abstract audio: option<AudioParams> with get, set
            abstract model: option<string> with get, set
            abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

    module BaseAiCfOpenaiGptOss20B =
        type PostProcessedOutputs =
            [<EmitProperty("system_fingerprint")>]
            abstract systemFingerprint: option<unit> with get, set

            abstract choices: option<unit> with get, set
            abstract created: option<unit> with get, set
            abstract model: option<unit> with get, set
            abstract usage: option<ResponseUsage> with get, set
            abstract truncation: option<LiteralUnions.AutoDisabled> with get, set
            abstract text: option<ResponseTextConfig> with get, set
            abstract status: option<ResponseStatus> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            [<EmitProperty("safety_identifier")>]
            abstract safetyIdentifier: option<string> with get, set

            abstract reasoning: option<Reasoning> with get, set
            abstract prompt: option<ResponsePrompt> with get, set

            [<EmitProperty("previous_response_id")>]
            abstract previousResponseId: option<string> with get, set

            [<EmitProperty("max_output_tokens")>]
            abstract maxOutputTokens: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<Tool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<U2<string, ToolChoiceFunction>> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract output: option<ResizeArray<ResponseOutputItem>> with get, set
            abstract object: option<string> with get, set
            abstract instructions: option<U2<ResizeArray<ResponseInputItem>, string>> with get, set

            [<EmitProperty("incomplete_details")>]
            abstract incompleteDetails: option<ResponseIncompleteDetails> with get, set

            abstract error: option<ResponseError> with get, set

            [<EmitProperty("output_text")>]
            abstract outputText: option<string> with get, set

            [<EmitProperty("created_at")>]
            abstract createdAt: option<float> with get, set

            abstract id: option<string> with get, set

        type Inputs =
            abstract functions: option<unit> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<unit> with get, set

            abstract store: option<unit> with get, set
            abstract seed: option<unit> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<unit> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<unit> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<unit> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<unit> with get, set

            abstract prediction: option<unit> with get, set
            abstract n: option<unit> with get, set
            abstract modalities: option<unit> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<unit> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<unit> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<unit> with get, set

            abstract logprobs: option<unit> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<unit> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<unit> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<unit> with get, set

            abstract stop: option<unit> with get, set
            abstract model: option<unit> with get, set
            abstract messages: option<unit> with get, set
            abstract audio: option<unit> with get, set
            abstract metadata: option<unit> with get, set
            abstract user: option<unit> with get, set
            abstract truncation: option<LiteralUnions.AutoDisabled> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<Tool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<U2<string, ToolChoiceFunction>> with get, set

            abstract text: option<ResponseTextConfig> with get, set
            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<StreamOptions> with get, set

            abstract stream: option<bool> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            [<EmitProperty("safety_identifier")>]
            abstract safetyIdentifier: option<string> with get, set

            abstract reasoning: option<Reasoning> with get, set

            [<EmitProperty("prompt_cache_key")>]
            abstract promptCacheKey: option<string> with get, set

            [<EmitProperty("previous_response_id")>]
            abstract previousResponseId: option<string> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            [<EmitProperty("max_output_tokens")>]
            abstract maxOutputTokens: option<float> with get, set

            abstract instructions: option<string> with get, set
            abstract input: option<U2<ResponseInput, string>> with get, set
            abstract include: option<ResizeArray<ResponseIncludable>> with get, set
            abstract conversation: option<U2<ResponseConversationParam, string>> with get, set
            abstract background: option<bool> with get, set

        module Inputs =
            type Case2 =
                [<EmitProperty("prompt_cache_key")>]
                abstract promptCacheKey: option<unit> with get, set

                abstract conversation: option<unit> with get, set
                abstract background: option<unit> with get, set
                abstract truncation: option<unit> with get, set

                [<EmitProperty("safety_identifier")>]
                abstract safetyIdentifier: option<unit> with get, set

                abstract reasoning: option<unit> with get, set

                [<EmitProperty("previous_response_id")>]
                abstract previousResponseId: option<unit> with get, set

                [<EmitProperty("max_output_tokens")>]
                abstract maxOutputTokens: option<unit> with get, set

                abstract instructions: option<unit> with get, set
                abstract include: option<unit> with get, set
                abstract text: option<unit> with get, set
                abstract input: option<unit> with get, set
                abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

                [<EmitProperty("function_call")>]
                abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

                [<EmitProperty("web_search_options")>]
                abstract webSearchOptions: option<WebSearchOptions> with get, set

                abstract user: option<string> with get, set

                [<EmitProperty("top_p")>]
                abstract topP: option<float> with get, set

                abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

                [<EmitProperty("tool_choice")>]
                abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

                abstract temperature: option<float> with get, set

                [<EmitProperty("stream_options")>]
                abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

                abstract stream: option<bool> with get, set
                abstract store: option<bool> with get, set
                abstract stop: option<U2<ResizeArray<string>, string>> with get, set

                [<EmitProperty("service_tier")>]
                abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

                abstract seed: option<float> with get, set

                [<EmitProperty("response_format")>]
                abstract responseFormat: option<ResponseFormat> with get, set

                [<EmitProperty("chat_template_kwargs")>]
                abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

                [<EmitProperty("reasoning_effort")>]
                abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

                [<EmitProperty("presence_penalty")>]
                abstract presencePenalty: option<float> with get, set

                abstract prediction: option<PredictionContent> with get, set

                [<EmitProperty("parallel_tool_calls")>]
                abstract parallelToolCalls: option<bool> with get, set

                abstract n: option<float> with get, set
                abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
                abstract metadata: option<obj> with get, set

                [<EmitProperty("max_completion_tokens")>]
                abstract maxCompletionTokens: option<float> with get, set

                [<EmitProperty("max_tokens")>]
                abstract maxTokens: option<float> with get, set

                [<EmitProperty("top_logprobs")>]
                abstract topLogprobs: option<float> with get, set

                abstract logprobs: option<bool> with get, set

                [<EmitProperty("logit_bias")>]
                abstract logitBias: option<obj> with get, set

                [<EmitProperty("frequency_penalty")>]
                abstract frequencyPenalty: option<float> with get, set

                abstract audio: option<AudioParams> with get, set
                abstract model: option<string> with get, set
                abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

        module PostProcessedOutputs =
            type Case2 =
                abstract truncation: option<unit> with get, set

                [<EmitProperty("safety_identifier")>]
                abstract safetyIdentifier: option<unit> with get, set

                abstract reasoning: option<unit> with get, set
                abstract prompt: option<unit> with get, set

                [<EmitProperty("previous_response_id")>]
                abstract previousResponseId: option<unit> with get, set

                [<EmitProperty("max_output_tokens")>]
                abstract maxOutputTokens: option<unit> with get, set

                [<EmitProperty("top_p")>]
                abstract topP: option<unit> with get, set

                [<EmitProperty("tool_choice")>]
                abstract toolChoice: option<unit> with get, set

                [<EmitProperty("parallel_tool_calls")>]
                abstract parallelToolCalls: option<unit> with get, set

                [<EmitProperty("incomplete_details")>]
                abstract incompleteDetails: option<unit> with get, set

                [<EmitProperty("output_text")>]
                abstract outputText: option<unit> with get, set

                [<EmitProperty("created_at")>]
                abstract createdAt: option<unit> with get, set

                abstract instructions: option<unit> with get, set
                abstract temperature: option<unit> with get, set
                abstract text: option<unit> with get, set
                abstract tools: option<unit> with get, set
                abstract output: option<unit> with get, set
                abstract status: option<unit> with get, set
                abstract error: option<unit> with get, set

                [<EmitProperty("service_tier")>]
                abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

                [<EmitProperty("system_fingerprint")>]
                abstract systemFingerprint: option<string> with get, set

                abstract usage: option<CompletionUsage> with get, set
                abstract choices: ResizeArray<ChatCompletionChoice> with get, set
                abstract model: string with get, set
                abstract created: float with get, set
                abstract object: string with get, set
                abstract id: string with get, set

    module BaseAiCfQwenQwen25Coder32BInstruct =
        type PostProcessedOutputs =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

            abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
            abstract response: string with get, set

    module BaseAiCfQwenQwq32B =
        type PostProcessedOutputs =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<SharedLiterals.ArgumentsName2>> with get, set

            abstract usage: option<SharedLiterals.CompletionTokensPromptB7e945e0> with get, set
            abstract response: string with get, set

    module BaseAiCfZaiOrgGlm47Flash =
        type Inputs =
            abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<U2<LiteralUnions.AutoNone, SharedLiterals.Name2>> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<WebSearchOptions> with get, set

            abstract user: option<string> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

            abstract stream: option<bool> with get, set
            abstract store: option<bool> with get, set
            abstract stop: option<U2<ResizeArray<string>, string>> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<ResponseFormat> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            abstract prediction: option<PredictionContent> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract n: option<float> with get, set
            abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<float> with get, set

            abstract logprobs: option<bool> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<obj> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            abstract audio: option<AudioParams> with get, set
            abstract model: option<string> with get, set
            abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

    module BaseAiImageClassification =
        type Inputs =
            abstract image: AiSentenceSimilarityOutput with get, set

    module BaseAiImageToText =
        type PostProcessedOutputs =
            abstract description: string with get, set

        type Inputs =
            abstract messages: option<ResizeArray<RoleScopedChatInput>> with get, set
            abstract raw: option<bool> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            [<EmitProperty("repetition_penalty")>]
            abstract repetitionPenalty: option<float> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("top_k")>]
            abstract topK: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            abstract prompt: option<string> with get, set
            abstract image: AiSentenceSimilarityOutput with get, set

    module BaseAiObjectDetection =
        type Inputs =
            abstract image: AiSentenceSimilarityOutput with get, set

    module BaseAiSentenceSimilarity =
        type Inputs =
            abstract sentences: ResizeArray<string> with get, set
            abstract source: string with get, set

    module BaseAiSummarization =
        type PostProcessedOutputs =
            abstract summary: string with get, set

        type Inputs =
            [<EmitProperty("max_length")>]
            abstract maxLength: option<float> with get, set

            [<EmitProperty("input_text")>]
            abstract inputText: string with get, set

    module BaseAiTextClassification =
        type Inputs =
            abstract text: string with get, set

    module BaseAiTextEmbeddings =
        type Inputs =
            abstract text: U2<ResizeArray<string>, string> with get, set

        type PostProcessedOutputs =
            abstract data: ResizeArray<AiSentenceSimilarityOutput> with get, set
            abstract shape: AiSentenceSimilarityOutput with get, set

    module BaseAiTextGeneration =
        type PostProcessedOutputs =
            abstract usage: option<UsageTags> with get, set

            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<PostProcessedOutputs.ToolCalls> with get, set

            abstract response: option<string> with get, set

        type Inputs =
            abstract functions: option<ResizeArray<AiTextGenerationFunctionsInput>> with get, set
            abstract tools: option<U3<ResizeArray<AiTextGenerationToolInput>, ResizeArray<AiTextGenerationToolLegacyInput>, obj>> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<AiTextGenerationResponseFormat> with get, set

            abstract messages: option<ResizeArray<RoleScopedChatInput>> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            [<EmitProperty("repetition_penalty")>]
            abstract repetitionPenalty: option<float> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("top_k")>]
            abstract topK: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            abstract stream: option<bool> with get, set
            abstract raw: option<bool> with get, set
            abstract prompt: option<string> with get, set

        module PostProcessedOutputs =
            type ToolCalls = interface end

    module BaseAiTextToImage =
        type Inputs =
            abstract seed: option<float> with get, set
            abstract guidance: option<float> with get, set
            abstract strength: option<float> with get, set

            [<EmitProperty("num_steps")>]
            abstract numSteps: option<float> with get, set

            abstract mask: option<AiSentenceSimilarityOutput> with get, set

            [<EmitProperty("image_b64")>]
            abstract imageB64: option<string> with get, set

            abstract image: option<AiSentenceSimilarityOutput> with get, set
            abstract width: option<float> with get, set
            abstract height: option<float> with get, set

            [<EmitProperty("negative_prompt")>]
            abstract negativePrompt: option<string> with get, set

            abstract prompt: string with get, set

    module BaseAiTextToSpeech =
        type Inputs =
            abstract lang: option<string> with get, set
            abstract prompt: string with get, set

    module BaseAiTranslation =
        type PostProcessedOutputs =
            [<EmitProperty("translated_text")>]
            abstract translatedText: option<string> with get, set

        type Inputs =
            [<EmitProperty("source_lang")>]
            abstract sourceLang: option<string> with get, set

            [<EmitProperty("target_lang")>]
            abstract targetLang: string with get, set

            abstract text: string with get, set

    module BasicImageTransformations =
        type Border =
            abstract width: float with get, set
            abstract color: string with get, set

        module Border =
            type Case2 =
                abstract left: float with get, set
                abstract bottom: float with get, set
                abstract right: float with get, set
                abstract top: float with get, set
                abstract color: string with get, set

    module BrowserRun =
        module QuickAction =
            type Options =
                abstract screenshotOptions: option<BrowserRunPuppeteerScreenshotOptions> with get, set
                abstract scrollPage: option<bool> with get, set
                abstract selector: option<string> with get, set
                abstract url: string with get, set
                abstract cacheTTL: option<float> with get, set
                abstract actionTimeout: option<float> with get, set
                abstract bestAttempt: option<bool> with get, set
                abstract waitForTimeout: option<float> with get, set
                abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                abstract userAgent: option<string> with get, set
                abstract setJavaScriptEnabled: option<bool> with get, set
                abstract setExtraHTTPHeaders: option<obj> with get, set
                abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                abstract emulateMediaType: option<string> with get, set
                abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

            module Options =
                type Case3 =
                    abstract pdfOptions: option<SharedLiterals.DisplayHeaderFooterFooterTe436a4396> with get, set
                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case2 =
                    abstract screenshotOptions: option<BrowserRunPuppeteerScreenshotOptions> with get, set
                    abstract scrollPage: option<bool> with get, set
                    abstract selector: option<string> with get, set
                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case11 =
                    abstract screenshotOptions: option<SharedLiterals.CaptureBeyondViewportClipF9e2df771> with get, set
                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case12 =
                    abstract screenshotOptions: option<SharedLiterals.CaptureBeyondViewportClipF9e2df771> with get, set
                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case9 =
                    abstract excludeExternalLinks: option<bool> with get, set
                    abstract visibleLinksOnly: option<bool> with get, set
                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case13 =
                    [<EmitProperty("response_format")>]
                    abstract responseFormat: option<AiTextGenerationResponseFormat> with get, set

                    abstract prompt: string with get, set

                    [<EmitProperty("custom_ai")>]
                    abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel2>> with get, set

                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case14 =
                    [<EmitProperty("response_format")>]
                    abstract responseFormat: AiTextGenerationResponseFormat with get, set

                    abstract prompt: option<string> with get, set

                    [<EmitProperty("custom_ai")>]
                    abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel2>> with get, set

                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case15 =
                    [<EmitProperty("response_format")>]
                    abstract responseFormat: option<AiTextGenerationResponseFormat> with get, set

                    abstract prompt: string with get, set

                    [<EmitProperty("custom_ai")>]
                    abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel2>> with get, set

                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case16 =
                    [<EmitProperty("response_format")>]
                    abstract responseFormat: AiTextGenerationResponseFormat with get, set

                    abstract prompt: option<string> with get, set

                    [<EmitProperty("custom_ai")>]
                    abstract customAi: option<ResizeArray<SharedLiterals.AuthorizationModel2>> with get, set

                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case8 =
                    abstract elements: ResizeArray<SharedLiterals.Selector> with get, set
                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case7 =
                    abstract elements: ResizeArray<SharedLiterals.Selector> with get, set
                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case6 =
                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case10 =
                    abstract excludeExternalLinks: option<bool> with get, set
                    abstract visibleLinksOnly: option<bool> with get, set
                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case4 =
                    abstract pdfOptions: option<SharedLiterals.DisplayHeaderFooterFooterTe436a4396> with get, set
                    abstract html: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

                type Case5 =
                    abstract url: string with get, set
                    abstract cacheTTL: option<float> with get, set
                    abstract actionTimeout: option<float> with get, set
                    abstract bestAttempt: option<bool> with get, set
                    abstract waitForTimeout: option<float> with get, set
                    abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
                    abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
                    abstract userAgent: option<string> with get, set
                    abstract setJavaScriptEnabled: option<bool> with get, set
                    abstract setExtraHTTPHeaders: option<obj> with get, set
                    abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
                    abstract allowRequestPattern: option<ResizeArray<string>> with get, set
                    abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
                    abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
                    abstract emulateMediaType: option<string> with get, set
                    abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
                    abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
                    abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
                    abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

    module BrowserRunMarkdownOptions =
        type Case2 =
            abstract html: string with get, set
            abstract cacheTTL: option<float> with get, set
            abstract actionTimeout: option<float> with get, set
            abstract bestAttempt: option<bool> with get, set
            abstract waitForTimeout: option<float> with get, set
            abstract waitForSelector: option<SharedLiterals.HiddenSelectorTimeoutVisible> with get, set
            abstract viewport: option<SharedLiterals.DeviceScaleFactorHasTouchH6969243e> with get, set
            abstract userAgent: option<string> with get, set
            abstract setJavaScriptEnabled: option<bool> with get, set
            abstract setExtraHTTPHeaders: option<obj> with get, set
            abstract allowResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
            abstract rejectResourceTypes: option<ResizeArray<BrowserRunResourceType>> with get, set
            abstract allowRequestPattern: option<ResizeArray<string>> with get, set
            abstract rejectRequestPattern: option<ResizeArray<string>> with get, set
            abstract gotoOptions: option<SharedLiterals.RefererReferrerPolicyTimeoutWaitUntil> with get, set
            abstract emulateMediaType: option<string> with get, set
            abstract cookies: option<ResizeArray<SharedLiterals.DomainExpiresHttpOnlyC6af887f>> with get, set
            abstract authenticate: option<SharedLiterals.PasswordUsername> with get, set
            abstract addStyleTag: option<ResizeArray<SharedLiterals.ContentUrl>> with get, set
            abstract addScriptTag: option<ResizeArray<SharedLiterals.ContentIdTypeUrl>> with get, set

    module BrowserRunScrapeSuccessResponse =
        type Result =
            abstract results: ResizeArray<Result.Results> with get, set
            abstract selector: string with get, set

        module Result =
            type Results =
                abstract attributes: ResizeArray<SharedLiterals.NameValue> with get, set
                abstract left: float with get, set
                abstract top: float with get, set
                abstract height: float with get, set
                abstract width: float with get, set
                abstract text: string with get, set
                abstract html: string with get, set

    module BrowserRunSnapshotSuccessResponse =
        type Result =
            abstract screenshot: string with get, set
            abstract content: string with get, set

    module CfProperties =
        type BotManagement =
            abstract ja3Hash: string with get, set
            abstract detectionIds: AiSentenceSimilarityOutput with get, set
            abstract staticResource: bool with get, set
            abstract corporateProxy: bool with get, set
            abstract verifiedBot: bool with get, set
            /// <example>
            /// 54
            /// </example>
            abstract score: float with get, set

    module ChatCompletionContentPart =
        type Case2 =
            [<EmitProperty("input_audio")>]
            abstract inputAudio: SharedLiterals.DataFormat2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Case1 =
            [<EmitProperty("image_url")>]
            abstract imageUrl: SharedLiterals.DetailUrl2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Case0 =
            abstract text: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Case3 =
            abstract file: SharedLiterals.FileDataFileIdFilename with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

    module Cloudflare =
        [<Import("@cloudflare/workers-types.Cloudflare", "GlobalProps")>]
        type GlobalProps = interface end

        [<Import("@cloudflare/workers-types.Cloudflare", "Exports")>]
        type Exports =
            abstract Item: key: string -> option<obj>

        [<Import("@cloudflare/workers-types.Cloudflare", "Env")>]
        type Env = interface end

        type GlobalProp<'K, 'Default> = U2<proptypekey<GlobalProps, 'K>, 'Default>

    module CloudflareAccessIdentity =
        type Geo =
            abstract country: string with get, set

        type Groups =
            abstract email: option<string> with get, set
            abstract name: string with get, set
            abstract id: string with get, set

        type Idp =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract id: string with get, set

    module CloudflareWorkersModule =
        type WorkflowRetentionDuration = U15<WorkflowRetentionDuration.Case0, WorkflowRetentionDuration.Case1, WorkflowRetentionDuration.Case2, WorkflowRetentionDuration.Case3, WorkflowRetentionDuration.Case4, WorkflowRetentionDuration.Case5, WorkflowRetentionDuration.Case6, WorkflowRetentionDuration.Case7, WorkflowRetentionDuration.Case8, WorkflowRetentionDuration.Case9, WorkflowRetentionDuration.Case10, WorkflowRetentionDuration.Case11, WorkflowRetentionDuration.Case12, WorkflowRetentionDuration.Case13, float>

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "DurableObject")>]
        type DurableObject<'Env, 'Props> =
            [<EmitConstructor>]
            abstract Create: ctx: Erased.Empty * env: 'Env -> DurableObject<'Env, 'Props>

            inherit Rpc.DurableObjectBranded
            abstract env: 'Env with get, set
            abstract ctx: DurableObjectState<'Props> with get, set

            [<EmitProperty("[Rpc.__DURABLE_OBJECT_BRAND]")>]
            abstract ``[rpc._DURABLEOBJECTBRAND]``: unit with get

            abstract alarm: ?alarmInfo: AlarmInvocationInfo -> option<Promise<unit>>
            abstract fetch: request: Request<option<obj>, U2<RequestInitCfProperties, obj>> -> U2<Response, Promise<Response>>
            abstract connect: socket: Socket -> option<Promise<unit>>
            abstract webSocketMessage: ws: WebSocket * message: U2<ArrayBuffer, string> -> option<Promise<unit>>
            abstract webSocketClose: ws: WebSocket * code: float * reason: string * wasClean: bool -> option<Promise<unit>>
            abstract webSocketError: ws: WebSocket * ?error: obj -> option<Promise<unit>>

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowStep")>]
        type WorkflowStep =
            abstract ``do``<'T> : name: string * callback: (WorkflowStepContext -> Promise<'T>) * ?rollbackOptions: WorkflowStep.Do.RollbackOptions -> Promise<'T>
            abstract ``do``<'T> : name: string * config: WorkflowStepConfig * callback: (WorkflowStepContext -> Promise<'T>) * ?rollbackOptions: WorkflowStep.Do.RollbackOptions -> Promise<'T>
            abstract sleep: name: string * duration: WorkflowRetentionDuration -> Promise<unit>
            abstract sleepUntil: name: string * timestamp: U2<Date, float> -> Promise<unit>
            abstract waitForEvent<'T> : name: string * options: WorkflowStep.WaitForEvent.Options -> Promise<WorkflowStep.WaitForEvent>

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type WorkflowInstanceStatus =
            | [<CompiledName("running")>] Running
            | [<CompiledName("paused")>] Paused
            | [<CompiledName("unknown")>] Unknown
            | [<CompiledName("terminated")>] Terminated
            | [<CompiledName("queued")>] Queued
            | [<CompiledName("errored")>] Errored
            | [<CompiledName("complete")>] Complete
            | [<CompiledName("waiting")>] Waiting
            | [<CompiledName("waitingForPause")>] WaitingForPause

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowEntrypoint")>]
        type WorkflowEntrypoint<'Env, 'T> =
            [<EmitConstructor>]
            abstract Create: ctx: ExecutionContext<option<obj>> * env: 'Env -> WorkflowEntrypoint<'Env, 'T>

            inherit Rpc.WorkflowEntrypointBranded
            abstract env: 'Env with get, set
            abstract ctx: ExecutionContext<option<obj>> with get, set

            [<EmitProperty("[Rpc.__WORKFLOW_ENTRYPOINT_BRAND]")>]
            abstract ``[rpc._WORKFLOWENTRYPOINTBRAND]``: unit with get

            abstract run: event: WorkflowEntrypoint.Run.Event * step: WorkflowStep -> Promise<option<obj>>

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowStepRollbackOptions")>]
        type WorkflowStepRollbackOptions =
            abstract rollbackConfig: option<WorkflowStepRollbackConfig> with get, set
            abstract rollback: (SharedLiterals.CtxErrorOutputStepName<obj> -> Promise<unit>) with get, set

        type WorkflowRollbackHandler = SharedLiterals.CtxErrorOutputStepName<obj> -> Promise<unit>

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type WorkflowDurationLabel =
            | [<CompiledName("second")>] Second
            | [<CompiledName("minute")>] Minute
            | [<CompiledName("hour")>] Hour
            | [<CompiledName("day")>] Day
            | [<CompiledName("week")>] Week
            | [<CompiledName("month")>] Month
            | [<CompiledName("year")>] Year

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type WorkflowBackoff =
            | [<CompiledName("constant")>] Constant
            | [<CompiledName("linear")>] Linear
            | [<CompiledName("exponential")>] Exponential

        type WorkflowSleepDuration = U15<WorkflowSleepDuration.Case0, WorkflowSleepDuration.Case1, WorkflowSleepDuration.Case2, WorkflowSleepDuration.Case3, WorkflowSleepDuration.Case4, WorkflowSleepDuration.Case5, WorkflowSleepDuration.Case6, WorkflowSleepDuration.Case7, WorkflowSleepDuration.Case8, WorkflowSleepDuration.Case9, WorkflowSleepDuration.Case10, WorkflowSleepDuration.Case11, WorkflowSleepDuration.Case12, WorkflowSleepDuration.Case13, float>

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkerEntrypoint")>]
        type WorkerEntrypoint<'Env, 'Props> =
            [<EmitConstructor>]
            abstract Create: ctx: ExecutionContext<option<obj>> * env: 'Env -> WorkerEntrypoint<'Env, 'Props>

            inherit Rpc.WorkerEntrypointBranded
            abstract env: 'Env with get, set
            abstract ctx: ExecutionContext<'Props> with get, set

            [<EmitProperty("[Rpc.__WORKER_ENTRYPOINT_BRAND]")>]
            abstract ``[rpc._WORKERENTRYPOINTBRAND]``: unit with get

            abstract email: message: ForwardableEmailMessage -> option<Promise<unit>>
            abstract fetch: request: Request<option<obj>, U2<RequestInitCfProperties, obj>> -> U2<Response, Promise<Response>>
            abstract connect: socket: Socket -> option<Promise<unit>>
            abstract queue: batch: MessageBatch<option<obj>> -> option<Promise<unit>>
            abstract scheduled: controller: ScheduledController -> option<Promise<unit>>
            abstract tail: events: ResizeArray<TraceItem> -> option<Promise<unit>>
            abstract tailStream: event: TailStream.TailEvent<TailStream.Onset> -> U3<TailStream.TailEvent<obj> -> option<Promise<unit>>, TailStream.TailEventHandlerObject, Promise<TailStream.TailEventHandlerType>>
            abstract test: controller: TestController -> option<Promise<unit>>
            abstract trace: traces: ResizeArray<TraceItem> -> option<Promise<unit>>

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowCronSchedule")>]
        type WorkflowCronSchedule =
            abstract scheduledTime: float with get, set
            abstract cron: string with get, set

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowEvent")>]
        type WorkflowEvent =
            abstract schedule: option<WorkflowCronSchedule> with get, set
            abstract workflowName: string with get, set
            abstract instanceId: string with get, set
            abstract timestamp: Date with get, set
            abstract payload: obj with get, set

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "RpcTarget")>]
        type RpcTarget =
            inherit Rpc.RpcTargetBranded

            [<EmitProperty("[Rpc.__RPC_TARGET_BRAND]")>]
            abstract ``[rpc._RPCTARGETBRAND]``: unit with get

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowStepConfig")>]
        type WorkflowStepConfig =
            abstract sensitive: option<string> with get, set
            abstract timeout: option<U15<WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, WorkflowStepConfig.Timeout, float>> with get, set
            abstract retries: option<SharedLiterals.BackoffDelayLimit> with get, set

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowStepRollbackConfig")>]
        type WorkflowStepRollbackConfig = interface end

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowRollbackContext")>]
        type WorkflowRollbackContext =
            /// <deprecated>
            /// Use `ctx.step.name` and `ctx.step.count` instead.
            /// </deprecated>
            abstract stepName: string with get, set
            abstract output: option<obj> with get, set
            abstract error: exn with get, set
            abstract ctx: WorkflowStepContext with get, set

        type WorkflowTimeoutDuration = U15<WorkflowTimeoutDuration.Case0, WorkflowTimeoutDuration.Case1, WorkflowTimeoutDuration.Case2, WorkflowTimeoutDuration.Case3, WorkflowTimeoutDuration.Case4, WorkflowTimeoutDuration.Case5, WorkflowTimeoutDuration.Case6, WorkflowTimeoutDuration.Case7, WorkflowTimeoutDuration.Case8, WorkflowTimeoutDuration.Case9, WorkflowTimeoutDuration.Case10, WorkflowTimeoutDuration.Case11, WorkflowTimeoutDuration.Case12, WorkflowTimeoutDuration.Case13, float>

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowStepEvent")>]
        type WorkflowStepEvent =
            abstract sensitive: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract timestamp: Date with get, set
            abstract payload: obj with get, set

        type RpcStub =
            [<EmitProperty("[__RPC_STUB_BRAND]")>]
            abstract ``[_rPCSTUBBRAND]``: obj with get, set

            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract dup: unit -> obj
            abstract ``[symbol.dispose]``: unit -> unit

        [<Import("@cloudflare/workers-types.CloudflareWorkersModule", "WorkflowStepContext")>]
        type WorkflowStepContext =
            abstract config: WorkflowStepConfig with get, set
            abstract attempt: float with get, set
            abstract step: SharedLiterals.CountName with get, set

        type WorkflowDelayDuration = U15<WorkflowDelayDuration.Case0, WorkflowDelayDuration.Case1, WorkflowDelayDuration.Case2, WorkflowDelayDuration.Case3, WorkflowDelayDuration.Case4, WorkflowDelayDuration.Case5, WorkflowDelayDuration.Case6, WorkflowDelayDuration.Case7, WorkflowDelayDuration.Case8, WorkflowDelayDuration.Case9, WorkflowDelayDuration.Case10, WorkflowDelayDuration.Case11, WorkflowDelayDuration.Case12, WorkflowDelayDuration.Case13, float>

        module WorkflowDelayDuration =
            type Case8 = string
            type Case4 = string
            type Case11 = string
            type Case7 = string
            type Case5 = string
            type Case10 = string
            type Case12 = string
            type Case9 = string
            type Case6 = string
            type Case13 = string
            type Case1 = string
            type Case3 = string
            type Case0 = string
            type Case2 = string

        module WorkflowEntrypoint =
            module Run =
                type Event =
                    abstract schedule: option<WorkflowCronSchedule> with get, set
                    abstract workflowName: string with get, set
                    abstract instanceId: string with get, set
                    abstract timestamp: Date with get, set
                    abstract payload: obj with get, set

        module WorkflowRetentionDuration =
            type Case13 = string
            type Case0 = string
            type Case1 = string
            type Case12 = string
            type Case11 = string
            type Case9 = string
            type Case8 = string
            type Case3 = string
            type Case4 = string
            type Case7 = string
            type Case5 = string
            type Case6 = string
            type Case10 = string
            type Case2 = string

        module WorkflowSleepDuration =
            type Case0 = string
            type Case12 = string
            type Case11 = string
            type Case1 = string
            type Case10 = string
            type Case2 = string
            type Case3 = string
            type Case4 = string
            type Case5 = string
            type Case9 = string
            type Case13 = string
            type Case6 = string
            type Case7 = string
            type Case8 = string

        module WorkflowStep =
            type WaitForEvent =
                abstract sensitive: option<string> with get, set

                [<EmitProperty("type")>]
                abstract ``type``: string with get, set

                abstract timestamp: Date with get, set
                abstract payload: obj with get, set

            module Do =
                type RollbackOptions =
                    abstract rollbackConfig: option<WorkflowStepRollbackConfig> with get, set
                    abstract rollback: ctx: SharedLiterals.CtxErrorOutputStepName<obj> -> Promise<unit>

            module WaitForEvent =
                type Options =
                    abstract timeout: option<U15<obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, float>> with get, set

                    [<EmitProperty("type")>]
                    abstract ``type``: string with get, set

        module WorkflowStepConfig =
            type Timeout = string

            module Timeout =
                type Case10 = string
                type Case5 = string
                type Case6 = string
                type Case9 = string
                type Case7 = string
                type Case8 = string
                type Case11 = string
                type Case4 = string
                type Case14 = string
                type Case3 = string
                type Case2 = string
                type Case13 = string
                type Case12 = string

        module WorkflowTimeoutDuration =
            type Case5 = string
            type Case6 = string
            type Case4 = string
            type Case0 = string
            type Case7 = string
            type Case8 = string
            type Case1 = string
            type Case9 = string
            type Case11 = string
            type Case12 = string
            type Case13 = string
            type Case3 = string
            type Case2 = string
            type Case10 = string

    module Crypto =
        type DigestStream =
            abstract prototype: DigestStream with get, set
            abstract Create: algorithm: U2<SubtleCryptoHashAlgorithm, string> -> DigestStream

    module D1DatabaseSession =
        type Batch =
            abstract results: ResizeArray<obj> with get, set
            abstract error: option<unit> with get, set
            abstract meta: Batch.Meta with get, set
            abstract success: bool with get, set

        module Batch =
            type Meta =
                [<EmitProperty("total_attempts")>]
                abstract totalAttempts: option<float> with get, set

                abstract timings: option<SharedLiterals.SqlDurationMs> with get, set

                [<EmitProperty("served_by_primary")>]
                abstract servedByPrimary: option<bool> with get, set

                [<EmitProperty("served_by_colo")>]
                abstract servedByColo: option<string> with get, set

                [<EmitProperty("served_by_region")>]
                abstract servedByRegion: option<string> with get, set

                abstract changes: float with get, set

                [<EmitProperty("changed_db")>]
                abstract changedDb: bool with get, set

                [<EmitProperty("last_row_id")>]
                abstract lastRowId: float with get, set

                [<EmitProperty("rows_written")>]
                abstract rowsWritten: float with get, set

                [<EmitProperty("rows_read")>]
                abstract rowsRead: float with get, set

                [<EmitProperty("size_after")>]
                abstract sizeAfter: float with get, set

                abstract duration: float with get, set
                abstract Item: key: string -> option<obj>

    module D1PreparedStatement =
        type Run =
            abstract results: ResizeArray<obj> with get, set
            abstract error: option<unit> with get, set
            abstract meta: Run.Meta with get, set
            abstract success: bool with get, set

        module Raw =
            type Options =
                abstract columnNames: bool with get, set

            module Options =
                type Case2 =
                    abstract columnNames: option<bool> with get, set

        module Run =
            type Meta =
                [<EmitProperty("total_attempts")>]
                abstract totalAttempts: option<float> with get, set

                abstract timings: option<SharedLiterals.SqlDurationMs> with get, set

                [<EmitProperty("served_by_primary")>]
                abstract servedByPrimary: option<bool> with get, set

                [<EmitProperty("served_by_colo")>]
                abstract servedByColo: option<string> with get, set

                [<EmitProperty("served_by_region")>]
                abstract servedByRegion: option<string> with get, set

                abstract changes: float with get, set

                [<EmitProperty("changed_db")>]
                abstract changedDb: bool with get, set

                [<EmitProperty("last_row_id")>]
                abstract lastRowId: float with get, set

                [<EmitProperty("rows_written")>]
                abstract rowsWritten: float with get, set

                [<EmitProperty("rows_read")>]
                abstract rowsRead: float with get, set

                [<EmitProperty("size_after")>]
                abstract sizeAfter: float with get, set

                abstract duration: float with get, set
                abstract Item: key: string -> option<obj>

    module D1Response =
        type Meta =
            [<EmitProperty("total_attempts")>]
            abstract totalAttempts: option<float> with get, set

            abstract timings: option<SharedLiterals.SqlDurationMs> with get, set

            [<EmitProperty("served_by_primary")>]
            abstract servedByPrimary: option<bool> with get, set

            [<EmitProperty("served_by_colo")>]
            abstract servedByColo: option<string> with get, set

            [<EmitProperty("served_by_region")>]
            abstract servedByRegion: option<string> with get, set

            abstract changes: float with get, set

            [<EmitProperty("changed_db")>]
            abstract changedDb: bool with get, set

            [<EmitProperty("last_row_id")>]
            abstract lastRowId: float with get, set

            [<EmitProperty("rows_written")>]
            abstract rowsWritten: float with get, set

            [<EmitProperty("rows_read")>]
            abstract rowsRead: float with get, set

            [<EmitProperty("size_after")>]
            abstract sizeAfter: float with get, set

            abstract duration: float with get, set
            abstract Item: key: string -> option<obj>

    module D1Result =
        type Meta =
            [<EmitProperty("total_attempts")>]
            abstract totalAttempts: option<float> with get, set

            abstract timings: option<SharedLiterals.SqlDurationMs> with get, set

            [<EmitProperty("served_by_primary")>]
            abstract servedByPrimary: option<bool> with get, set

            [<EmitProperty("served_by_colo")>]
            abstract servedByColo: option<string> with get, set

            [<EmitProperty("served_by_region")>]
            abstract servedByRegion: option<string> with get, set

            abstract changes: float with get, set

            [<EmitProperty("changed_db")>]
            abstract changedDb: bool with get, set

            [<EmitProperty("last_row_id")>]
            abstract lastRowId: float with get, set

            [<EmitProperty("rows_written")>]
            abstract rowsWritten: float with get, set

            [<EmitProperty("rows_read")>]
            abstract rowsRead: float with get, set

            [<EmitProperty("size_after")>]
            abstract sizeAfter: float with get, set

            abstract duration: float with get, set
            abstract Item: key: string -> option<obj>

    module DurableObjectFacets =
        type Get =
            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
            abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    module DurableObjectNamespace =
        type Get =
            abstract name: option<string> with get
            abstract id: DurableObjectId with get
            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
            abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    module EventContext =
        type Request =
            abstract tlsClientAuth: U2<IncomingRequestCfPropertiesTLSClientAuth, IncomingRequestCfPropertiesTLSClientAuthPlaceholder> with get, set
            /// <example>
            /// "635"
            /// </example>
            abstract metroCode: option<string> with get, set
            /// <example>
            /// "TX"
            /// </example>
            abstract regionCode: option<string> with get, set
            /// <example>
            /// "Texas"
            /// </example>
            abstract region: option<string> with get, set
            /// <example>
            /// "America/Chicago"
            /// </example>
            abstract timezone: option<string> with get, set
            /// <example>
            /// "-97.74260"
            /// </example>
            abstract longitude: option<string> with get, set
            /// <example>
            /// "30.27130"
            /// </example>
            abstract latitude: option<string> with get, set
            /// <example>
            /// "78701"
            /// </example>
            abstract postalCode: option<string> with get, set
            /// <example>
            /// "Austin"
            /// </example>
            abstract city: option<string> with get, set
            /// <example>
            /// "AN"
            /// </example>
            abstract continent: option<ContinentCode> with get, set
            /// <example>
            /// "1"
            /// </example>
            abstract isEUCountry: option<string> with get, set
            /// <example>
            /// "GB"
            /// </example>
            abstract country: option<LiteralUnions.ADAEAFAda77976> with get, set
            abstract hostMetadata: option<obj> with get, set
            /// <deprecated />
            abstract clientTrustScore: float with get, set
            abstract botManagement: Request.BotManagement with get, set
            abstract tlsExportedAuthenticator: option<IncomingRequestCfPropertiesExportedAuthenticatorMetadata> with get, set
            /// <example>
            /// "AEAD-AES128-GCM-SHA256"
            /// </example>
            abstract tlsCipher: string with get, set
            /// <example>
            /// "TLSv1.3"
            /// </example>
            abstract tlsVersion: string with get, set
            /// <example>
            /// "weight=192;exclusive=0;group=3;group-weight=127"
            /// </example>
            abstract requestPriority: string with get, set
            /// <example>
            /// "HTTP/2"
            /// </example>
            abstract httpProtocol: string with get, set
            /// <example>
            /// 3
            /// </example>
            abstract edgeRequestKeepAliveStatus: IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus with get, set
            /// <example>
            /// "DFW"
            /// </example>
            abstract colo: string with get, set
            /// <example>
            /// 22
            /// </example>
            abstract clientTcpRtt: option<float> with get, set
            /// <example>
            /// "gzip, deflate, br"
            /// </example>
            abstract clientAcceptEncoding: option<string> with get, set
            /// <example>
            /// "Google Cloud"
            /// </example>
            abstract asOrganization: option<string> with get, set
            /// <example>
            /// 395747
            /// </example>
            abstract asn: option<float> with get, set
            abstract Item: key: string -> option<obj>

        type Env =
            abstract ASSETS: SharedLiterals.Fetch with get, set

        module Request =
            type BotManagement =
                abstract ja3Hash: string with get, set
                abstract detectionIds: AiSentenceSimilarityOutput with get, set
                abstract staticResource: bool with get, set
                abstract corporateProxy: bool with get, set
                abstract verifiedBot: bool with get, set
                /// <example>
                /// 54
                /// </example>
                abstract score: float with get, set

    module EventPluginContext =
        type Env =
            abstract ASSETS: SharedLiterals.Fetch with get, set

    module ExportedHandler =
        type Fetch =
            abstract tlsClientAuth: U2<IncomingRequestCfPropertiesTLSClientAuth, IncomingRequestCfPropertiesTLSClientAuthPlaceholder> with get, set
            /// <example>
            /// "635"
            /// </example>
            abstract metroCode: option<string> with get, set
            /// <example>
            /// "TX"
            /// </example>
            abstract regionCode: option<string> with get, set
            /// <example>
            /// "Texas"
            /// </example>
            abstract region: option<string> with get, set
            /// <example>
            /// "America/Chicago"
            /// </example>
            abstract timezone: option<string> with get, set
            /// <example>
            /// "-97.74260"
            /// </example>
            abstract longitude: option<string> with get, set
            /// <example>
            /// "30.27130"
            /// </example>
            abstract latitude: option<string> with get, set
            /// <example>
            /// "78701"
            /// </example>
            abstract postalCode: option<string> with get, set
            /// <example>
            /// "Austin"
            /// </example>
            abstract city: option<string> with get, set
            /// <example>
            /// "AN"
            /// </example>
            abstract continent: option<ContinentCode> with get, set
            /// <example>
            /// "1"
            /// </example>
            abstract isEUCountry: option<string> with get, set
            /// <example>
            /// "GB"
            /// </example>
            abstract country: option<LiteralUnions.ADAEAFAda77976> with get, set
            abstract hostMetadata: option<obj> with get, set
            /// <deprecated />
            abstract clientTrustScore: float with get, set
            abstract botManagement: Fetch.BotManagement with get, set
            abstract tlsExportedAuthenticator: option<IncomingRequestCfPropertiesExportedAuthenticatorMetadata> with get, set
            /// <example>
            /// "AEAD-AES128-GCM-SHA256"
            /// </example>
            abstract tlsCipher: string with get, set
            /// <example>
            /// "TLSv1.3"
            /// </example>
            abstract tlsVersion: string with get, set
            /// <example>
            /// "weight=192;exclusive=0;group=3;group-weight=127"
            /// </example>
            abstract requestPriority: string with get, set
            /// <example>
            /// "HTTP/2"
            /// </example>
            abstract httpProtocol: string with get, set
            /// <example>
            /// 3
            /// </example>
            abstract edgeRequestKeepAliveStatus: IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus with get, set
            /// <example>
            /// "DFW"
            /// </example>
            abstract colo: string with get, set
            /// <example>
            /// 22
            /// </example>
            abstract clientTcpRtt: option<float> with get, set
            /// <example>
            /// "gzip, deflate, br"
            /// </example>
            abstract clientAcceptEncoding: option<string> with get, set
            /// <example>
            /// "Google Cloud"
            /// </example>
            abstract asOrganization: option<string> with get, set
            /// <example>
            /// 395747
            /// </example>
            abstract asn: option<float> with get, set
            abstract Item: key: string -> option<obj>

        module Fetch =
            type BotManagement =
                abstract ja3Hash: string with get, set
                abstract detectionIds: AiSentenceSimilarityOutput with get, set
                abstract staticResource: bool with get, set
                abstract corporateProxy: bool with get, set
                abstract verifiedBot: bool with get, set
                /// <example>
                /// 54
                /// </example>
                abstract score: float with get, set

    module HelloWorldBinding =
        type Get =
            abstract ms: option<float> with get, set
            abstract value: string with get, set

    module ImageTransformationResult =
        module Image =
            type Options =
                abstract encoding: option<string> with get, set

    module ImageTransformer =
        module Draw =
            type Options =
                abstract right: option<float> with get, set
                abstract bottom: option<float> with get, set
                abstract left: option<float> with get, set
                abstract top: option<float> with get, set
                abstract composite: option<ImageCompositeMode> with get, set
                abstract repeat: option<U2<bool, string>> with get, set
                abstract opacity: option<float> with get, set

        module Output =
            type Options =
                abstract anim: option<bool> with get, set
                abstract background: option<string> with get, set
                abstract quality: option<float> with get, set
                abstract format: LiteralUnions.ImageAvifImageGifImageJ6516ed8b with get, set

        module Transform =
            type Transform =
                abstract trim: option<U2<string, SharedLiterals.BorderBottomHeight5a030263>> with get, set
                abstract sharpen: option<float> with get, set
                abstract saturation: option<float> with get, set
                abstract rotate: option<LiteralUnions.I0I180I270I90> with get, set
                abstract gravity: option<U2<LiteralUnions.AutoBottomCenterB85bc937, SharedLiterals.ModeXY>> with get, set
                abstract segment: option<string> with get, set
                abstract gamma: option<float> with get, set
                abstract flip: option<LiteralUnions.HHvV> with get, set
                abstract fit: option<LiteralUnions.ContainCoverCrop8bc042f3> with get, set
                abstract contrast: option<float> with get, set
                abstract brightness: option<float> with get, set
                abstract border: option<U2<SharedLiterals.ColorWidth, SharedLiterals.BottomLeftRightTop2>> with get, set
                abstract blur: option<float> with get, set
                abstract background: option<string> with get, set
                abstract height: option<float> with get, set
                abstract width: option<float> with get, set

    module ImagesBinding =
        module Info =
            type Options =
                abstract encoding: option<string> with get, set

    module IncomingRequestCfProperties =
        type BotManagement =
            abstract ja3Hash: string with get, set
            abstract detectionIds: AiSentenceSimilarityOutput with get, set
            abstract staticResource: bool with get, set
            abstract corporateProxy: bool with get, set
            abstract verifiedBot: bool with get, set
            /// <example>
            /// 54
            /// </example>
            abstract score: float with get, set

    module IncomingRequestCfPropertiesBotManagementEnterprise =
        type BotManagement =
            abstract ja3Hash: string with get, set
            abstract detectionIds: AiSentenceSimilarityOutput with get, set
            abstract staticResource: bool with get, set
            abstract corporateProxy: bool with get, set
            abstract verifiedBot: bool with get, set
            /// <example>
            /// 54
            /// </example>
            abstract score: float with get, set

    module KVNamespace =
        type List =
            abstract cacheStatus: option<string> with get, set
            abstract cursor: string with get, set
            abstract keys: ResizeArray<KVNamespaceListKey<obj, obj>> with get, set

            [<EmitProperty("list_complete")>]
            abstract listComplete: bool with get, set

        module Get =
            type Options =
                abstract cacheTtl: option<float> with get, set

                [<EmitProperty("type")>]
                abstract ``type``: option<unit> with get, set

        module List =
            type Case2 =
                abstract cacheStatus: option<string> with get, set
                abstract keys: ResizeArray<KVNamespaceListKey<obj, obj>> with get, set

                [<EmitProperty("list_complete")>]
                abstract listComplete: bool with get, set

    module LoopbackForExport =
        type Invoke =
            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
            abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

        type Case2 =
            abstract Invoke: opts: SharedLiterals.Props<obj> -> DurableObjectClass<obj>
            abstract Invoke: opts: SharedLiterals.Props2 -> DurableObjectClass<obj>

        type Case3 =
            abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
            abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket
            abstract Invoke: opts: SharedLiterals.Props2 -> SharedLiterals.ConnectFetch

    module MediaTransformer =
        module Transform =
            type Transform =
                abstract height: option<float> with get, set
                abstract width: option<float> with get, set
                abstract fit: option<LiteralUnions.ContainCoverScaleDown> with get, set

    module PagesPluginFunction =
        type Env =
            abstract ASSETS: SharedLiterals.Fetch with get, set

    module R2Bucket =
        type List =
            abstract cursor: string with get, set
            abstract truncated: bool with get, set
            abstract delimitedPrefixes: ResizeArray<string> with get, set
            abstract objects: ResizeArray<R2Object> with get, set

        module Get =
            type Options =
                abstract ssecKey: option<U2<ArrayBuffer, string>> with get, set
                abstract range: option<U4<SharedLiterals.LengthOffset, SharedLiterals.LengthOffset2, SharedLiterals.Suffix, Headers>> with get, set
                abstract onlyIf: option<U2<R2Conditional, Headers>> with get, set

        module List =
            type Case2 =
                abstract truncated: bool with get, set
                abstract delimitedPrefixes: ResizeArray<string> with get, set
                abstract objects: ResizeArray<R2Object> with get, set

        module Put =
            type Options =
                abstract ssecKey: option<U2<ArrayBuffer, string>> with get, set
                abstract storageClass: option<string> with get, set
                abstract sha512: option<U3<ArrayBuffer, obj, string>> with get, set
                abstract sha384: option<U3<ArrayBuffer, obj, string>> with get, set
                abstract sha256: option<U3<ArrayBuffer, obj, string>> with get, set
                abstract sha1: option<U3<ArrayBuffer, obj, string>> with get, set
                abstract md5: option<U3<ArrayBuffer, obj, string>> with get, set
                abstract customMetadata: option<obj> with get, set
                abstract httpMetadata: option<U2<R2HTTPMetadata, Headers>> with get, set
                abstract onlyIf: option<U2<R2Conditional, Headers>> with get, set

    module ReadableStreamBYOBReader =
        type Read =
            abstract value: obj with get, set

            [<EmitProperty("done")>]
            abstract ``done``: bool with get, set

    module Request =
        type Cf =
            abstract tlsClientAuth: U2<IncomingRequestCfPropertiesTLSClientAuth, IncomingRequestCfPropertiesTLSClientAuthPlaceholder> with get, set
            /// <example>
            /// "635"
            /// </example>
            abstract metroCode: option<string> with get, set
            /// <example>
            /// "TX"
            /// </example>
            abstract regionCode: option<string> with get, set
            /// <example>
            /// "Texas"
            /// </example>
            abstract region: option<string> with get, set
            /// <example>
            /// "America/Chicago"
            /// </example>
            abstract timezone: option<string> with get, set
            /// <example>
            /// "-97.74260"
            /// </example>
            abstract longitude: option<string> with get, set
            /// <example>
            /// "30.27130"
            /// </example>
            abstract latitude: option<string> with get, set
            /// <example>
            /// "78701"
            /// </example>
            abstract postalCode: option<string> with get, set
            /// <example>
            /// "Austin"
            /// </example>
            abstract city: option<string> with get, set
            /// <example>
            /// "AN"
            /// </example>
            abstract continent: option<ContinentCode> with get, set
            /// <example>
            /// "1"
            /// </example>
            abstract isEUCountry: option<string> with get, set
            /// <example>
            /// "GB"
            /// </example>
            abstract country: option<LiteralUnions.ADAEAFAda77976> with get, set
            abstract hostMetadata: option<obj> with get, set
            /// <deprecated />
            abstract clientTrustScore: float with get, set
            abstract botManagement: Cf.BotManagement with get, set
            abstract tlsExportedAuthenticator: option<IncomingRequestCfPropertiesExportedAuthenticatorMetadata> with get, set
            /// <example>
            /// "AEAD-AES128-GCM-SHA256"
            /// </example>
            abstract tlsCipher: string with get, set
            /// <example>
            /// "TLSv1.3"
            /// </example>
            abstract tlsVersion: string with get, set
            /// <example>
            /// "weight=192;exclusive=0;group=3;group-weight=127"
            /// </example>
            abstract requestPriority: string with get, set
            /// <example>
            /// "HTTP/2"
            /// </example>
            abstract httpProtocol: string with get, set
            /// <example>
            /// 3
            /// </example>
            abstract edgeRequestKeepAliveStatus: IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus with get, set
            /// <example>
            /// "DFW"
            /// </example>
            abstract colo: string with get, set
            /// <example>
            /// 22
            /// </example>
            abstract clientTcpRtt: option<float> with get, set
            /// <example>
            /// "gzip, deflate, br"
            /// </example>
            abstract clientAcceptEncoding: option<string> with get, set
            /// <example>
            /// "Google Cloud"
            /// </example>
            abstract asOrganization: option<string> with get, set
            /// <example>
            /// 395747
            /// </example>
            abstract asn: option<float> with get, set
            abstract Item: key: string -> option<obj>

        module Cf =
            type BotManagement =
                abstract ja3Hash: string with get, set
                abstract detectionIds: AiSentenceSimilarityOutput with get, set
                abstract staticResource: bool with get, set
                abstract corporateProxy: bool with get, set
                abstract verifiedBot: bool with get, set
                /// <example>
                /// 54
                /// </example>
                abstract score: float with get, set

    module ResponseContent =
        type Case4 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

    module ResponseItem =
        type Case1 =
            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

            abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
            abstract role: LiteralUnions.DeveloperSystemUser with get, set
            abstract content: ResponseInputMessageContentList with get, set
            abstract id: string with get, set

        type Case3 =
            abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract output: U2<ResizeArray<ResponseInputContent>, string> with get, set

            [<EmitProperty("call_id")>]
            abstract callId: string with get, set

            abstract id: string with get, set

    module ResponseStreamEvent =
        type Case8 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            abstract response: Response with get, set

        type Case2 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            abstract response: Response with get, set

        type Case3 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            abstract response: Response with get, set

        type Case4 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            abstract param: option<string> with get, set
            abstract message: string with get, set
            abstract code: option<string> with get, set

        type Case13 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            abstract delta: string with get, set

            [<EmitProperty("content_index")>]
            abstract contentIndex: float with get, set

        type Case14 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            abstract refusal: string with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            [<EmitProperty("content_index")>]
            abstract contentIndex: float with get, set

        type Case12 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            [<EmitProperty("content_index")>]
            abstract contentIndex: float with get, set

        type Case11 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            abstract delta: string with get, set

            [<EmitProperty("content_index")>]
            abstract contentIndex: float with get, set

        type Case10 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            abstract item: ResponseOutputItem with get, set

        type Case5 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            abstract delta: string with get, set

        type Case1 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            abstract logprobs: ResizeArray<Logprob> with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            abstract delta: string with get, set

            [<EmitProperty("content_index")>]
            abstract contentIndex: float with get, set

        type Case6 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            abstract name: string with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            abstract arguments: string with get, set

        type Case9 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            abstract item: ResponseOutputItem with get, set

        type Case0 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            [<EmitProperty("output_index")>]
            abstract outputIndex: float with get, set

            abstract logprobs: ResizeArray<Logprob> with get, set

            [<EmitProperty("item_id")>]
            abstract itemId: string with get, set

            [<EmitProperty("content_index")>]
            abstract contentIndex: float with get, set

        type Case7 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            [<EmitProperty("sequence_number")>]
            abstract sequenceNumber: float with get, set

            abstract response: Response with get, set

    module RoleScopedChatInput =
        type Role = interface end

    module Rpc =
        type Result =
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract Item: key: string -> option<obj>

        [<Import("@cloudflare/workers-types.Rpc", "Stubify")>]
        type Stubify<'T> = interface end

        [<Import("@cloudflare/workers-types.Rpc", "WorkerEntrypointBranded")>]
        type WorkerEntrypointBranded =
            [<EmitProperty("[__WORKER_ENTRYPOINT_BRAND]")>]
            abstract ``[_wORKERENTRYPOINTBRAND]``: unit with get

        [<Import("@cloudflare/workers-types.Rpc", "Unstubify")>]
        type Unstubify<'T> = interface end

        type MethodOrProperty =
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract Item: key: string -> option<obj>

        [<Import("@cloudflare/workers-types.Rpc", "Serializable")>]
        type Serializable<'T> = interface end

        [<Import("@cloudflare/workers-types.Rpc", "Provider")>]
        type Provider<'T, 'Reserved> =
            abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>

        [<Import("@cloudflare/workers-types.Rpc", "MaybeProvider")>]
        type MaybeProvider<'T> = interface end

        [<Import("@cloudflare/workers-types.Rpc", "BaseType")>]
        type BaseType = interface end

        [<Import("@cloudflare/workers-types.Rpc", "RpcTargetBranded")>]
        type RpcTargetBranded =
            [<EmitProperty("[__RPC_TARGET_BRAND]")>]
            abstract ``[_rPCTARGETBRAND]``: unit with get

        [<Import("@cloudflare/workers-types.Rpc", "MaybeDisposable")>]
        type MaybeDisposable<'T> = interface end

        [<Import("@cloudflare/workers-types.Rpc", "StubBase")>]
        type StubBase<'T> =
            inherit Disposable

            [<EmitProperty("[__RPC_STUB_BRAND]")>]
            abstract ``[_rPCSTUBBRAND]``: 'T with get, set

            abstract dup: unit -> obj

        type Stubable = U2<RpcTargetBranded, Erased.Empty>

        [<Import("@cloudflare/workers-types.Rpc", "MaybeCallableProvider")>]
        type MaybeCallableProvider<'T> = interface end

        [<Import("@cloudflare/workers-types.Rpc", "UnstubifyAll")>]
        type UnstubifyAll<'A> =
            [<EmitProperty("[Symbol.unscopables]")>]
            abstract ``[symbol.unscopables]``: SharedLiterals._Iterator10155d5f234a with get

            abstract length: float with get, set
            abstract toString: unit -> string
            abstract toLocaleString: unit -> string
            abstract pop: unit -> option<obj>
            abstract push: [<ParamArray>] items: ResizeArray<obj> -> float
            abstract concat: [<ParamArray>] items: ResizeArray<System.Collections.Generic.IReadOnlyList<obj>> -> ResizeArray<obj>
            abstract join: ?separator: string -> string
            abstract reverse: unit -> ResizeArray<obj>
            abstract shift: unit -> option<obj>
            abstract slice: ?start: float * ?``end``: float -> ResizeArray<obj>
            abstract sort: ?compareFn: (obj -> obj -> float) -> obj
            abstract splice: start: float * ?deleteCount: float -> ResizeArray<obj>
            abstract unshift: [<ParamArray>] items: ResizeArray<obj> -> float
            abstract indexOf: searchElement: obj * ?fromIndex: float -> float
            abstract lastIndexOf: searchElement: obj * ?fromIndex: float -> float
            abstract every: predicate: (obj -> float -> ResizeArray<obj> -> bool) * ?thisArg: obj -> bool
            abstract some: predicate: (obj -> float -> ResizeArray<obj> -> option<obj>) * ?thisArg: obj -> bool
            abstract forEach: callbackfn: (obj -> float -> ResizeArray<obj> -> unit) * ?thisArg: obj -> unit
            abstract map: callbackfn: (obj -> float -> ResizeArray<obj> -> obj) * ?thisArg: obj -> ResizeArray<obj>
            abstract filter<'S> : predicate: (obj -> float -> ResizeArray<obj> -> bool) * ?thisArg: obj -> ResizeArray<'S>
            abstract reduce: callbackfn: (obj -> obj -> float -> ResizeArray<obj> -> obj) -> obj
            abstract reduceRight: callbackfn: (obj -> obj -> float -> ResizeArray<obj> -> obj) -> obj
            abstract find: predicate: (obj -> float -> ResizeArray<obj> -> bool) * ?thisArg: obj -> option<obj>
            abstract findIndex: predicate: (obj -> float -> ResizeArray<obj> -> option<obj>) * ?thisArg: obj -> float
            abstract fill: value: obj * ?start: float * ?``end``: float -> obj
            abstract copyWithin: target: float * start: float * ?``end``: float -> obj
            abstract entries: unit -> seq<float * obj>
            abstract keys: unit -> seq<float>
            abstract values: unit -> seq<obj>
            abstract includes: searchElement: obj * ?fromIndex: float -> bool
            abstract flatMap: callback: (obj -> obj -> float -> ResizeArray<obj> -> U2<obj, System.Collections.Generic.IReadOnlyList<obj>>) * ?thisArg: obj -> ResizeArray<obj>
            abstract flat: this: 'A * ?depth: obj -> ResizeArray<proptypekey<SharedLiterals.DoneRecur<'A, obj, obj, obj>, LiteralUnions.DoneRecur>>
            abstract at: index: float -> option<obj>
            abstract findLast: predicate: (obj -> float -> ResizeArray<obj> -> bool) * ?thisArg: obj -> option<obj>
            abstract findLastIndex: predicate: (obj -> float -> ResizeArray<obj> -> option<obj>) * ?thisArg: obj -> float
            abstract toReversed: unit -> ResizeArray<obj>
            abstract toSorted: ?compareFn: (obj -> obj -> float) -> ResizeArray<obj>
            abstract toSpliced: start: float * deleteCount: float * [<ParamArray>] items: ResizeArray<obj> -> ResizeArray<obj>
            abstract ``with``: index: float * value: obj -> ResizeArray<obj>
            abstract ``[symbol.iterator]``: unit -> seq<obj>

        [<Import("@cloudflare/workers-types.Rpc", "WorkflowEntrypointBranded")>]
        type WorkflowEntrypointBranded =
            [<EmitProperty("[__WORKFLOW_ENTRYPOINT_BRAND]")>]
            abstract ``[_wORKFLOWENTRYPOINTBRAND]``: unit with get

        [<Import("@cloudflare/workers-types.Rpc", "Stub")>]
        type Stub<'T> =
            [<EmitProperty("[__RPC_STUB_BRAND]")>]
            abstract ``[_rPCSTUBBRAND]``: 'T with get, set

            abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract dup: unit -> obj
            abstract ``[symbol.dispose]``: unit -> unit

        type EntrypointBranded = U3<DurableObjectBranded, WorkerEntrypointBranded, WorkflowEntrypointBranded>

        [<Import("@cloudflare/workers-types.Rpc", "DurableObjectBranded")>]
        type DurableObjectBranded =
            [<EmitProperty("[__DURABLE_OBJECT_BRAND]")>]
            abstract ``[_dURABLEOBJECTBRAND]``: unit with get

        module MaybeCallableProvider =
            type Case2 =
                abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
                abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
                abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
                abstract Item: key: string -> option<obj>

        module MethodOrProperty =
            type Case2 =
                abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
                abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
                abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
                abstract Item: key: string -> option<obj>

        module Result =
            type Case2 =
                abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
                abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
                abstract Invoke: [<ParamArray>] args: UnstubifyAll<obj> -> option<U2<obj, obj>>
                abstract Item: key: string -> option<obj>

    module RpcStub =
        type Create =
            [<EmitProperty("[__RPC_STUB_BRAND]")>]
            abstract ``[_rPCSTUBBRAND]``: obj with get, set

            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract dup: unit -> obj
            abstract ``[symbol.dispose]``: unit -> unit

    module SendEmail =
        module Send =
            type Builder =
                abstract attachments: option<ResizeArray<EmailAttachment>> with get, set
                abstract html: option<string> with get, set
                abstract text: option<string> with get, set
                abstract headers: option<obj> with get, set
                abstract bcc: option<U3<EmailAddress, ResizeArray<U2<EmailAddress, string>>, string>> with get, set
                abstract cc: option<U3<EmailAddress, ResizeArray<U2<EmailAddress, string>>, string>> with get, set
                abstract replyTo: option<U2<EmailAddress, string>> with get, set
                abstract subject: string with get, set

                [<EmitProperty("to")>]
                abstract ``to``: U3<EmailAddress, ResizeArray<U2<EmailAddress, string>>, string> with get, set

                abstract from: U2<EmailAddress, string> with get, set

    module Service =
        type Case2 =
            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
            abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    module ServiceWorkerGlobalScope =
        type Cache =
            abstract prototype: Cache with get, set
            abstract Create: unit -> Cache

        type CustomEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: CustomEvent<option<obj>> with get, set
            abstract Create: ``type``: string * ?init: CustomEventCustomEventInit -> CustomEvent<option<obj>>

        type FormData =
            abstract prototype: FormData with get, set
            abstract Create: unit -> FormData

        type ScheduledEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: ScheduledEvent with get, set
            abstract Create: ``type``: string * ?init: EventInit -> ScheduledEvent

        type CryptoKey =
            abstract prototype: CryptoKey with get, set
            abstract Create: unit -> CryptoKey

        type File =
            abstract prototype: File with get, set
            abstract Create: ?bits: ResizeArray<U4<ArrayBuffer, obj, Blob, string>> * name: string * ?options: FileOptions -> File

        type SubtleCrypto =
            abstract prototype: SubtleCrypto with get, set
            abstract Create: unit -> SubtleCrypto

        type PromiseRejectionEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: PromiseRejectionEvent with get, set
            abstract Create: ``type``: string * ?init: EventInit -> PromiseRejectionEvent

        type Blob =
            abstract prototype: Blob with get, set
            abstract Create: ?bits: ResizeArray<U4<ArrayBuffer, obj, Blob, string>> * ?options: BlobOptions -> Blob

        type TailEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: TailEvent with get, set
            abstract Create: ``type``: string * ?init: EventInit -> TailEvent

        type CacheStorage =
            abstract prototype: CacheStorage with get, set
            abstract Create: unit -> CacheStorage

        type TextDecoder =
            abstract prototype: TextDecoder with get, set
            abstract Create: ?label: string * ?options: TextDecoderConstructorOptions -> TextDecoder

        type DOMException =
            abstract DATA_CLONE_ERR: float with get
            abstract INVALID_NODE_TYPE_ERR: float with get
            abstract TIMEOUT_ERR: float with get
            abstract QUOTA_EXCEEDED_ERR: float with get
            abstract URL_MISMATCH_ERR: float with get
            abstract ABORT_ERR: float with get
            abstract NETWORK_ERR: float with get
            abstract SECURITY_ERR: float with get
            abstract TYPE_MISMATCH_ERR: float with get
            abstract VALIDATION_ERR: float with get
            abstract INVALID_ACCESS_ERR: float with get
            abstract NAMESPACE_ERR: float with get
            abstract INVALID_MODIFICATION_ERR: float with get
            abstract SYNTAX_ERR: float with get
            abstract INVALID_STATE_ERR: float with get
            abstract INUSE_ATTRIBUTE_ERR: float with get
            abstract NOT_SUPPORTED_ERR: float with get
            abstract NOT_FOUND_ERR: float with get
            abstract NO_MODIFICATION_ALLOWED_ERR: float with get
            abstract NO_DATA_ALLOWED_ERR: float with get
            abstract INVALID_CHARACTER_ERR: float with get
            abstract WRONG_DOCUMENT_ERR: float with get
            abstract HIERARCHY_REQUEST_ERR: float with get
            abstract DOMSTRING_SIZE_ERR: float with get
            abstract INDEX_SIZE_ERR: float with get
            abstract prototype: DOMException with get, set
            abstract Create: ?message: string * ?name: string -> DOMException
            abstract isError: ?error: obj -> bool

        type TextEncoder =
            abstract prototype: TextEncoder with get, set
            abstract Create: unit -> TextEncoder

        type Navigator =
            abstract prototype: Navigator with get, set
            abstract Create: unit -> Navigator

        type URLPattern =
            abstract prototype: URLPattern with get, set
            abstract Create: ?input: U2<URLPatternInit, string> * ?baseURL: U2<URLPatternOptions, string> * ?patternOptions: URLPatternOptions -> URLPattern

        type FetchEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: FetchEvent with get, set
            abstract Create: ``type``: string * ?init: EventInit -> FetchEvent

        type URL =
            abstract prototype: URL with get, set
            abstract Create: url: U2<URL, string> * ?``base``: U2<URL, string> -> URL
            abstract canParse: url: string * ?``base``: string -> bool
            abstract parse: url: string * ?``base``: string -> option<URL>
            abstract createObjectURL: object: U2<File, Blob> -> string
            abstract revokeObjectURL: objectUrl: string -> unit

        type FixedLengthStream =
            abstract prototype: FixedLengthStream with get, set
            abstract Create: expectedLength: float * ?queuingStrategy: IdentityTransformStreamQueuingStrategy -> FixedLengthStream

        type URLSearchParams =
            abstract prototype: URLSearchParams with get, set
            abstract Create: ?init: U3<seq<seq<string>>, obj, string> -> URLSearchParams

        type CloseEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: CloseEvent with get, set
            abstract Create: ``type``: string * ?initializer: CloseEventInit -> CloseEvent

        type AbortSignal =
            abstract prototype: AbortSignal with get, set
            abstract Create: unit -> AbortSignal
            abstract abort: ?reason: obj -> AbortSignal
            abstract timeout: delay: float -> AbortSignal
            abstract any: signals: ResizeArray<AbortSignal> -> AbortSignal

        type Event =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: Event with get, set
            abstract Create: ``type``: string * ?init: EventInit -> Event

        type ExtendableEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: ExtendableEvent with get, set
            abstract Create: ``type``: string * ?init: EventInit -> ExtendableEvent

        type ReadableByteStreamController =
            abstract prototype: ReadableByteStreamController with get, set
            abstract Create: unit -> ReadableByteStreamController

        type ReadableStreamDefaultController =
            abstract prototype: ReadableStreamDefaultController<option<obj>> with get, set
            abstract Create: unit -> ReadableStreamDefaultController<option<obj>>

        type Crypto =
            abstract prototype: Crypto with get, set
            abstract Create: unit -> Crypto

        type ReadableStreamBYOBRequest =
            abstract prototype: ReadableStreamBYOBRequest with get, set
            abstract Create: unit -> ReadableStreamBYOBRequest

        type EventSource =
            abstract CLOSED: float with get
            abstract OPEN: float with get
            abstract CONNECTING: float with get
            abstract prototype: EventSource with get, set
            abstract Create: url: string * ?init: EventSourceEventSourceInit -> EventSource
            abstract from: stream: ReadableStream<option<obj>> -> EventSource

        type MessagePort =
            abstract prototype: MessagePort with get, set
            abstract Create: unit -> MessagePort

        type CompressionStream =
            abstract prototype: CompressionStream with get, set
            abstract Create: format: LiteralUnions.DeflateDeflateRawGzip -> CompressionStream

        type DecompressionStream =
            abstract prototype: DecompressionStream with get, set
            abstract Create: format: LiteralUnions.DeflateDeflateRawGzip -> DecompressionStream

        type ReadableStreamDefaultReader =
            abstract prototype: ReadableStreamDefaultReader<option<obj>> with get, set
            abstract Create: stream: ReadableStream<option<obj>> -> ReadableStreamDefaultReader<option<obj>>

        type HTMLRewriter =
            abstract prototype: HTMLRewriter with get, set
            abstract Create: unit -> HTMLRewriter

        type TextEncoderStream =
            abstract prototype: TextEncoderStream with get, set
            abstract Create: unit -> TextEncoderStream

        type TextDecoderStream =
            abstract prototype: TextDecoderStream with get, set
            abstract Create: ?label: string * ?options: TextDecoderStreamTextDecoderStreamInit -> TextDecoderStream

        type Headers =
            abstract prototype: Headers with get, set
            abstract Create: ?init: HeadersInit -> Headers

        type MessageChannel =
            abstract prototype: MessageChannel with get, set
            abstract Create: unit -> MessageChannel

        type AbortController =
            abstract prototype: AbortController with get, set
            abstract Create: unit -> AbortController

        type Body =
            abstract prototype: Body with get, set
            abstract Create: unit -> Body

        type ErrorEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: ErrorEvent with get, set
            abstract Create: ``type``: string * ?init: ErrorEventErrorEventInit -> ErrorEvent

        type WebSocketRequestResponsePair =
            abstract prototype: WebSocketRequestResponsePair with get, set
            abstract Create: request: string * response: string -> WebSocketRequestResponsePair

        type TransformStreamDefaultController =
            abstract prototype: TransformStreamDefaultController<option<obj>> with get, set
            abstract Create: unit -> TransformStreamDefaultController<option<obj>>

        type MessageEvent =
            abstract BUBBLING_PHASE: float with get
            abstract AT_TARGET: float with get
            abstract CAPTURING_PHASE: float with get
            abstract NONE: float with get
            abstract prototype: MessageEvent with get, set
            abstract Create: ``type``: string * initializer: MessageEventInit -> MessageEvent

        type IdentityTransformStream =
            abstract prototype: IdentityTransformStream with get, set
            abstract Create: ?queuingStrategy: IdentityTransformStreamQueuingStrategy -> IdentityTransformStream

        type WorkerGlobalScope =
            abstract prototype: WorkerGlobalScope with get, set
            abstract Create: unit -> WorkerGlobalScope

        type ReadableStreamBYOBReader =
            abstract prototype: ReadableStreamBYOBReader with get, set
            abstract Create: stream: ReadableStream<option<obj>> -> ReadableStreamBYOBReader

        type WebSocketPair =
            abstract Create: unit -> obj

        type WritableStream =
            abstract prototype: WritableStream<option<obj>> with get, set
            abstract Create: ?underlyingSink: UnderlyingSink<option<obj>> * ?queuingStrategy: QueuingStrategy<option<obj>> -> WritableStream<option<obj>>

        type WritableStreamDefaultWriter =
            abstract prototype: WritableStreamDefaultWriter<option<obj>> with get, set
            abstract Create: stream: WritableStream<option<obj>> -> WritableStreamDefaultWriter<option<obj>>

        type TransformStream =
            abstract prototype: TransformStream<option<obj>, option<obj>> with get, set
            abstract Create: ?transformer: Transformer<obj, obj> * ?writableStrategy: QueuingStrategy<obj> * ?readableStrategy: QueuingStrategy<obj> -> TransformStream<option<obj>, option<obj>>

        type ByteLengthQueuingStrategy =
            abstract prototype: ByteLengthQueuingStrategy with get, set
            abstract Create: init: QueuingStrategyInit -> ByteLengthQueuingStrategy

        type CountQueuingStrategy =
            abstract prototype: CountQueuingStrategy with get, set
            abstract Create: init: QueuingStrategyInit -> CountQueuingStrategy

        type ReadableStream =
            abstract prototype: ReadableStream<option<obj>> with get, set
            abstract Create: underlyingSource: UnderlyingByteSource * ?strategy: QueuingStrategy<Uint8Array> -> ReadableStream<Uint8Array>
            abstract Create: ?underlyingSource: UnderlyingSource<obj> * ?strategy: QueuingStrategy<obj> -> ReadableStream<obj>

        type WritableStreamDefaultController =
            abstract prototype: WritableStreamDefaultController with get, set
            abstract Create: unit -> WritableStreamDefaultController

        module WebSocketPair =
            type Create =
                [<EmitProperty("1")>]
                abstract ``1``: WebSocket with get, set

                [<EmitProperty("0")>]
                abstract ``0``: WebSocket with get, set

    module SharedLiterals =
        type DescriptionFaviconUrlImageE54bbd2b =
            abstract faviconUrl: option<string> with get, set
            abstract imageUrl: option<string> with get, set
            abstract lastModifiedDate: option<string> with get, set
            abstract description: option<string> with get, set
            abstract title: string with get, set
            abstract url: string with get, set

        type TextType5 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type EnabledMatchThresholdModel =
            [<EmitProperty("match_threshold")>]
            abstract matchThreshold: option<float> with get, set

            abstract model: option<string> with get, set
            abstract enabled: option<bool> with get, set
            abstract Item: key: string -> option<obj>

        type FileSizeFormatHeightWidth =
            abstract height: float with get, set
            abstract width: float with get, set
            abstract fileSize: float with get, set
            abstract format: string with get, set

        type FrequencyPenaltyIgnoreEbacd687 =
            abstract messages: option<ResizeArray<RoleScopedChatInput>> with get, set
            abstract raw: option<bool> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            [<EmitProperty("repetition_penalty")>]
            abstract repetitionPenalty: option<float> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("top_k")>]
            abstract topK: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            [<EmitProperty("ignore_eos")>]
            abstract ignoreEos: option<bool> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            abstract prompt: option<string> with get, set
            abstract image: string with get, set

        type LengthOffset =
            abstract length: option<float> with get, set
            abstract offset: float with get, set

        type FunctionIdType3 =
            [<EmitProperty("function")>]
            abstract ``function``: ArgumentsName with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract id: string with get, set

        type RefererReferrerPolicyTimeoutWaitUntil =
            abstract referrerPolicy: option<string> with get, set
            abstract referer: option<string> with get, set
            abstract waitUntil: option<U2<LiteralUnions.DomcontentloadedLoadNetworD6ce7c4e, ResizeArray<BrowserRunLifecycleEvent>>> with get, set
            abstract timeout: option<float> with get, set

        type Encoding =
            abstract encoding: option<string> with get, set

        type AiSearchOptionsMessagesQuery3 =
            [<EmitProperty("ai_search_options")>]
            abstract aiSearchOptions: AiSearchMultiSearchOptions with get, set

            abstract messages: option<unit> with get, set
            abstract query: string with get, set

        type IdVariablesVersion =
            abstract version: option<string> with get, set
            abstract variables: option<System.Collections.Generic.IDictionary<string, U3<ResponseInputText, ResponseInputImage, string>>> with get, set
            abstract id: string with get, set

        type CacheStatusKeysListComplete<'Metadata, 'Key> =
            abstract cacheStatus: option<string> with get, set
            abstract keys: ResizeArray<KVNamespaceListKey<'Metadata, 'Key>> with get, set

            [<EmitProperty("list_complete")>]
            abstract listComplete: bool with get, set

        type ContentRole =
            abstract content: U2<ResizeArray<TextType>, string> with get, set
            abstract role: string with get, set

        type LatencyMsQueryRequestId =
            abstract latencyMs: float with get, set
            abstract requestId: string with get, set
            abstract query: string with get, set

        type DescriptionNameParameters2 =
            abstract parameters: option<PropertiesRequiredType7> with get, set
            abstract description: string with get, set
            abstract name: string with get, set

        type JsonSchemaType2 =
            [<EmitProperty("json_schema")>]
            abstract jsonSchema: DescriptionNameSchemaStrict with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type CLOSEDCLOSINGCONNECTING7a08c077 =
            abstract CLOSED: float with get
            abstract READY_STATE_CLOSED: float with get
            abstract CLOSING: float with get
            abstract READY_STATE_CLOSING: float with get
            abstract OPEN: float with get
            abstract READY_STATE_OPEN: float with get
            abstract CONNECTING: float with get
            abstract READY_STATE_CONNECTING: float with get
            abstract prototype: WebSocket with get, set
            abstract Create: url: string * ?protocols: U2<ResizeArray<string>, string> -> WebSocket

        type ChoicesCreatedModelSystemFingerprint =
            [<EmitProperty("system_fingerprint")>]
            abstract systemFingerprint: option<unit> with get, set

            abstract choices: option<unit> with get, set
            abstract created: option<unit> with get, set
            abstract model: option<unit> with get, set

        type KeywordVector =
            abstract keyword: option<bool> with get, set
            abstract vector: option<bool> with get, set

        type DescriptionType2 =
            abstract description: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Values =
            abstract values: option<U3<AiSentenceSimilarityOutput, Float32Array, Float64Array>> with get, set

        type LogprobsTextType =
            abstract logprobs: option<ResizeArray<Logprob>> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type DisplayHeaderFooterFooterTe436a4396 =
            abstract timeout: option<float> with get, set
            abstract outline: option<bool> with get, set
            abstract tagged: option<bool> with get, set
            abstract omitBackground: option<bool> with get, set
            abstract margin: option<BottomLeftRightTop> with get, set
            abstract preferCSSPageSize: option<bool> with get, set
            abstract height: option<Zod.ZodType> with get, set
            abstract width: option<Zod.ZodType> with get, set
            abstract format: option<LiteralUnions.A0A1A2C0f56350> with get, set
            abstract pageRanges: option<string> with get, set
            abstract landscape: option<bool> with get, set
            abstract printBackground: option<bool> with get, set
            abstract footerTemplate: option<string> with get, set
            abstract headerTemplate: option<string> with get, set
            abstract displayHeaderFooter: option<bool> with get, set
            abstract scale: option<float> with get, set

        type ActionChunkCountErrorType37c559a5 =
            abstract errorType: option<string> with get, set
            abstract processingTimeMs: option<float> with get, set
            abstract chunkCount: option<float> with get, set
            abstract fileKey: option<string> with get, set
            abstract message: string with get, set
            abstract action: string with get, set
            abstract timestamp: string with get, set

        type ConvertMaxConvertedImages =
            abstract maxConvertedImages: option<float> with get, set
            abstract convert: option<bool> with get, set

        type ArgumentsCallIdIdNameStatusType =
            abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
            abstract id: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract name: string with get, set

            [<EmitProperty("call_id")>]
            abstract callId: string with get, set

            abstract arguments: string with get, set

        type ContentNameRole2 =
            abstract name: option<string> with get, set
            abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set
            abstract role: string with get, set

        type BackoffMaxAttemptsRetryDelayMs =
            abstract backoff: option<CloudflareWorkersModule.WorkflowBackoff> with get, set
            abstract retryDelayMs: option<float> with get, set
            abstract maxAttempts: option<LiteralUnions.I1I2I3I4I5> with get, set

        type TextType2 =
            abstract text: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type TypeUrlCitation =
            [<EmitProperty("url_citation")>]
            abstract urlCitation: EndIndexStartIndexTitleUrl with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type DataEnvFunctionPath23a88d11<'Env, 'Data> =
            abstract data: 'Data with get, set
            abstract params: obj with get, set
            abstract env: DataEnvFunctionPath23a88d11.Env with get, set
            abstract functionPath: string with get, set
            abstract request: Request<option<obj>, obj> with get, set
            abstract waitUntil: promise: Promise<option<obj>> -> unit
            abstract passThroughOnException: unit -> unit
            abstract next: ?input: U2<Request<option<obj>, U2<RequestInitCfProperties, obj>>, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>

        type CronScheduledTime =
            abstract scheduledTime: float with get, set
            abstract cron: string with get, set

        type SearchContextSizeUserLocation =
            [<EmitProperty("user_location")>]
            abstract userLocation: option<WebSearchUserLocation> with get, set

            [<EmitProperty("search_context_size")>]
            abstract searchContextSize: option<AgentMemoryThinkingLevel> with get, set

        type FromToRawPrototype =
            abstract prototype: EmailMessage with get, set
            abstract Create: from: string * ``to``: string * raw: U2<ReadableStream<option<obj>>, string> -> EmailMessage

        type ContentType =
            abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Description2 =
            abstract description: string with get, set

        type DescriptionNameSchemaStrict =
            abstract strict: option<bool> with get, set
            abstract schema: option<obj> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set

        type TranslatedText =
            [<EmitProperty("translated_text")>]
            abstract translatedText: option<string> with get, set

        type CaptureBeyondViewportClipE43e4b2fb =
            abstract fromSurface: option<bool> with get, set
            abstract captureBeyondViewport: option<bool> with get, set
            abstract optimizeForSpeed: option<bool> with get, set
            abstract omitBackground: option<bool> with get, set
            abstract clip: option<HeightScaleWidthXY> with get, set
            abstract fullPage: option<bool> with get, set
            abstract quality: option<float> with get, set
            abstract encoding: option<LiteralUnions.Base64Binary> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<LiteralUnions.JpegPngWebp> with get, set

        type FormatVerbosity =
            abstract verbosity: option<LiteralUnions.HighLowMedium> with get, set
            abstract format: option<ResponseFormatTextConfig> with get, set

        type AnnotationsAudioContentB2f5d555 =
            [<EmitProperty("function_call")>]
            abstract functionCall: option<ArgumentsName> with get, set

            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<ChatCompletionMessageToolCall>> with get, set

            abstract audio: option<ChatCompletionAudio> with get, set
            abstract annotations: option<ResizeArray<ChatCompletionUrlCitation>> with get, set
            abstract refusal: option<string> with get, set
            abstract content: option<string> with get, set
            abstract role: string with get, set

        type InputName =
            abstract input: string with get, set
            abstract name: string with get, set

        type CodeName =
            abstract code: string with get, set
            abstract name: string with get, set

        type AuthorizationModel2 =
            abstract authorization: option<string> with get, set
            abstract model: string with get, set

        type EndStartWord2 =
            [<EmitProperty("end")>]
            abstract ``end``: option<float> with get, set

            abstract start: option<float> with get, set
            abstract word: option<string> with get, set

        type FunctionType =
            [<EmitProperty("function")>]
            abstract ``function``: DescriptionNameParameters with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type DoneValue2<'R> =
            abstract value: 'R with get, set

            [<EmitProperty("done")>]
            abstract ``done``: bool with get, set

        type InstanceId =
            [<EmitProperty("instance_id")>]
            abstract instanceId: string with get, set

        type BodyInitErrorJsonPrototypeRedirect =
            abstract prototype: Response with get, set
            abstract Create: ?body: U9<ReadableStream<Uint8Array>, ArrayBuffer, obj, Blob, URLSearchParams, FormData, seq<BufferSource>, AsyncIterable<BufferSource>, string> * ?init: ResponseInit -> Response
            abstract error: unit -> Response
            abstract redirect: url: string * ?status: float -> Response
            abstract json: ?any: obj * ?maybeInit: U2<ResponseInit, Response> -> Response

        type CallIdIdOutputStatusType<'T> =
            abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
            abstract id: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract output: U2<ResponseFunctionCallOutputItemList, string> with get, set

            [<EmitProperty("call_id")>]
            abstract callId: string with get, set

        type InNin =
            [<EmitProperty("$nin")>]
            abstract ``$nin``: option<ResizeArray<U3<string, float, bool>>> with get, set

            [<EmitProperty("$in")>]
            abstract ``$in``: option<ResizeArray<U3<string, float, bool>>> with get, set

        type ContentRoleStatusType<'T> =
            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

            abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set
            abstract role: LiteralUnions.DeveloperSystemUser with get, set
            abstract content: ResponseInputMessageContentList with get, set

        type SourceLangTargetLangText =
            [<EmitProperty("target_lang")>]
            abstract targetLang: string with get, set

            [<EmitProperty("source_lang")>]
            abstract sourceLang: option<string> with get, set

            abstract text: string with get, set

        type Props2 =
            abstract props: option<obj> with get, set

        type AiSearchOptionsMessagesQuery4 =
            [<EmitProperty("ai_search_options")>]
            abstract aiSearchOptions: AiSearchMultiSearchOptions with get, set

            abstract messages: ResizeArray<AiSearchMessage> with get, set
            abstract query: option<unit> with get, set

        type Props<'Props> =
            abstract props: option<'Props> with get, set

        type TextType3 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type RefusalTextType =
            abstract refusal: option<string> with get, set
            abstract text: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: LiteralUnions.RefusalText with get, set

        type TextType6 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type DimensionsVectorsCount =
            abstract dimensions: float with get, set
            abstract vectorsCount: float with get, set

        type AudioContentFunction3e5136e0 =
            [<EmitProperty("function_call")>]
            abstract functionCall: option<ArgumentsName> with get, set

            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<ChatCompletionMessageToolCall>> with get, set

            abstract audio: option<StreamDirectUploadWatermark> with get, set
            abstract name: option<string> with get, set
            abstract refusal: option<string> with get, set
            abstract content: option<U2<ResizeArray<AssistantMessageContentPart>, string>> with get, set
            abstract role: string with get, set

        type Name2 =
            abstract name: string with get, set

        type ContentContentIdDispositio8d6362d4 =
            abstract content: obj with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract filename: string with get, set
            abstract contentId: string with get, set
            abstract disposition: string with get, set

        type ContentRoleType<'T> =
            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

            abstract role: LiteralUnions.AssistantDeveloperSystemUser with get, set
            abstract content: U2<ResponseInputMessageContentList, string> with get, set

        type CacheStatusCursorKeysListComplete<'Metadata, 'Key> =
            abstract cacheStatus: option<string> with get, set
            abstract cursor: string with get, set
            abstract keys: ResizeArray<KVNamespaceListKey<'Metadata, 'Key>> with get, set

            [<EmitProperty("list_complete")>]
            abstract listComplete: bool with get, set

        type BytesLogprobToken =
            abstract bytes: option<AiSentenceSimilarityOutput> with get, set
            abstract logprob: float with get, set
            abstract token: string with get, set

        type Url2 =
            abstract url: string with get, set

        type Description =
            abstract description: string with get, set

        type DataFormatIdMimeTypeNameTokens =
            abstract data: string with get, set
            abstract tokens: float with get, set
            abstract format: string with get, set
            abstract mimeType: string with get, set
            abstract name: string with get, set
            abstract id: string with get, set

        type AudioChatTemplate59349c60 =
            abstract functions: option<ResizeArray<FunctionDefinition>> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<U2<LiteralUnions.AutoNone, Name2>> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<WebSearchOptions> with get, set

            abstract user: option<string> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<ChatCompletionTool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<ChatCompletionToolChoiceOption> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<ChatCompletionsStreamOptions> with get, set

            abstract stream: option<bool> with get, set
            abstract store: option<bool> with get, set
            abstract stop: option<U2<ResizeArray<string>, string>> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<ResponseFormat> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<ChatTemplateKwargs> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<LiteralUnions.HighLowMedium> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            abstract prediction: option<PredictionContent> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract n: option<float> with get, set
            abstract modalities: option<ResizeArray<LiteralUnions.AudioText>> with get, set
            abstract metadata: option<obj> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<float> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<float> with get, set

            abstract logprobs: option<bool> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<obj> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            abstract audio: option<AudioParams> with get, set
            abstract model: option<string> with get, set

        type CustomType2 =
            abstract custom: DescriptionFormatName with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type FinishReasonIndexFc31d0af =
            [<EmitProperty("prompt_logprobs")>]
            abstract promptLogprobs: option<MainModule> with get, set

            abstract logprobs: option<MainModule> with get, set

            [<EmitProperty("stop_reason")>]
            abstract stopReason: option<string> with get, set

            [<EmitProperty("finish_reason")>]
            abstract finishReason: string with get, set

            abstract text: string with get, set
            abstract index: float with get, set

        type NameType =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract name: string with get, set

        type EnabledModelRewritePrompt =
            [<EmitProperty("rewrite_prompt")>]
            abstract rewritePrompt: option<string> with get, set

            abstract model: option<string> with get, set
            abstract enabled: option<bool> with get, set
            abstract Item: key: string -> option<obj>

        type DocxHtmlImagePdf =
            abstract pdf: option<ImagesMetadata> with get, set
            abstract image: option<ImageConversionOptions> with get, set
            abstract docx: option<Images> with get, set
            abstract html: option<CssSelectorHostnameImages> with get, set

        type DataFormat =
            abstract format: option<LiteralUnions.Mp3Wav> with get, set
            abstract data: option<string> with get, set

        type IdItemScoreScoringDetailsTextType =
            [<EmitProperty("scoring_details")>]
            abstract scoringDetails: option<FusionMethodKeyword6c9246d8> with get, set

            abstract item: KeyMetadataTimestamp with get, set
            abstract text: string with get, set
            abstract score: float with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract id: string with get, set

        type CompletionTokensPromptB7e945e0 =
            [<EmitProperty("total_tokens")>]
            abstract totalTokens: option<float> with get, set

            [<EmitProperty("completion_tokens")>]
            abstract completionTokens: option<float> with get, set

            [<EmitProperty("prompt_tokens")>]
            abstract promptTokens: option<float> with get, set

        type CustomIdType =
            abstract custom: InputName with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract id: string with get, set

        type IdName =
            abstract name: option<string> with get
            abstract id: DurableObjectId with get

        type AudioTokensCachedTokens =
            [<EmitProperty("audio_tokens")>]
            abstract audioTokens: option<float> with get, set

            [<EmitProperty("cached_tokens")>]
            abstract cachedTokens: option<float> with get, set

        type ContentNameRole3 =
            abstract name: option<string> with get, set
            abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set
            abstract role: string with get, set

        type FinishReasonIndexE81dd86d =
            abstract logprobs: option<MainModule> with get, set

            [<EmitProperty("stop_reason")>]
            abstract stopReason: option<string> with get, set

            [<EmitProperty("finish_reason")>]
            abstract finishReason: option<string> with get, set

            abstract message: option<ContentReasoningContent068e01e9> with get, set
            abstract index: option<float> with get, set

        type DataTypeFieldName =
            [<EmitProperty("data_type")>]
            abstract dataType: LiteralUnions.BooleanDatetimeNumberText with get, set

            [<EmitProperty("field_name")>]
            abstract fieldName: string with get, set

        type CompletionTokensCompletion29ca1d44 =
            [<EmitProperty("completion_tokens_details")>]
            abstract completionTokensDetails: option<CompletionTokensDetails> with get, set

            [<EmitProperty("prompt_tokens_details")>]
            abstract promptTokensDetails: option<PromptTokensDetails> with get, set

            [<EmitProperty("total_tokens")>]
            abstract totalTokens: float with get, set

            [<EmitProperty("completion_tokens")>]
            abstract completionTokens: float with get, set

            [<EmitProperty("prompt_tokens")>]
            abstract promptTokens: float with get, set

        type CountCursorPerPageTruncated =
            abstract truncated: bool with get, set
            abstract cursor: option<string> with get, set

            [<EmitProperty("per_page")>]
            abstract perPage: float with get, set

            abstract count: float with get, set

        type GrammarType =
            abstract grammar: DefinitionSyntax with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type IndexMessage =
            abstract message: ContentRole3 with get, set
            abstract index: option<float> with get, set
            abstract Item: key: string -> option<obj>

        type AllowedToolsType =
            [<EmitProperty("allowed_tools")>]
            abstract allowedTools: ModeTools with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type ContentRefusal =
            abstract refusal: option<ResizeArray<ChatCompletionTokenLogprob>> with get, set
            abstract content: option<ResizeArray<ChatCompletionTokenLogprob>> with get, set

        type CodeDetailMessagePath =
            abstract path: option<string> with get, set
            abstract detail: option<string> with get, set
            abstract code: option<float> with get, set
            abstract message: string with get, set

        type BlobName =
            abstract blob: Blob with get, set
            abstract name: string with get, set

        type LabelScore =
            abstract label: option<string> with get, set
            abstract score: option<float> with get, set

        type Requests3 =
            abstract requests: ResizeArray<SourceLangTargetLangText> with get, set

        type ContentRoleToolCallId2 =
            [<EmitProperty("tool_call_id")>]
            abstract toolCallId: string with get, set

            abstract content: U2<ResizeArray<ChatCompletionContentPartText>, string> with get, set
            abstract role: string with get, set

        type LengthOffset2 =
            abstract length: float with get, set
            abstract offset: option<float> with get, set

        type IdMetadataNamespace =
            [<EmitProperty("namespace")>]
            abstract ``namespace``: option<string> with get, set

            abstract metadata: option<obj> with get, set
            abstract id: string with get, set

        type FunctionType2 =
            [<EmitProperty("function")>]
            abstract ``function``: AiTextGenerationToolLegacyInput with get, set

            [<EmitProperty("type")>]
            abstract ``type``: U2<string, obj> with get, set

        type ColorKeepTolerance =
            abstract keep: option<float> with get, set
            abstract tolerance: option<float> with get, set
            abstract color: option<string> with get, set

        type BackgroundConversationIncl0bba41e5<'T> =
            abstract truncation: option<LiteralUnions.AutoDisabled> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<Tool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<U2<string, ToolChoiceFunction>> with get, set

            abstract text: option<ResponseTextConfig> with get, set
            abstract temperature: option<float> with get, set

            [<EmitProperty("stream_options")>]
            abstract streamOptions: option<StreamOptions> with get, set

            abstract stream: option<bool> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            [<EmitProperty("safety_identifier")>]
            abstract safetyIdentifier: option<string> with get, set

            abstract reasoning: option<Reasoning> with get, set

            [<EmitProperty("prompt_cache_key")>]
            abstract promptCacheKey: option<string> with get, set

            [<EmitProperty("previous_response_id")>]
            abstract previousResponseId: option<string> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            [<EmitProperty("max_output_tokens")>]
            abstract maxOutputTokens: option<float> with get, set

            abstract instructions: option<string> with get, set
            abstract input: option<U2<ResponseInput, string>> with get, set
            abstract include: option<ResizeArray<ResponseIncludable>> with get, set
            abstract conversation: option<U2<ResponseConversationParam, string>> with get, set
            abstract background: option<bool> with get, set

        type CaptureBeyondViewportClipF9e2df771 =
            abstract fromSurface: option<bool> with get, set
            abstract captureBeyondViewport: option<bool> with get, set
            abstract optimizeForSpeed: option<bool> with get, set
            abstract omitBackground: option<bool> with get, set
            abstract clip: option<HeightScaleWidthXY> with get, set
            abstract fullPage: option<bool> with get, set
            abstract quality: option<float> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<LiteralUnions.JpegPngWebp> with get, set

        type DirectionField =
            abstract direction: option<LiteralUnions.AscDescExistsNotExists> with get, set
            abstract field: string with get, set

        type ArgumentsName4 =
            abstract arguments: option<obj> with get, set
            abstract name: string with get, set

        type RetriesTimeout =
            abstract timeout: option<U15<RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, RetriesTimeout.Timeout, float>> with get, set
            abstract retries: option<BackoffDelayLimit> with get, set

        type PagePerPage =
            [<EmitProperty("per_page")>]
            abstract perPage: option<float> with get, set

            abstract page: option<float> with get, set

        type DataExpiresAtIdTranscript =
            abstract transcript: string with get, set

            [<EmitProperty("expires_at")>]
            abstract expiresAt: float with get, set

            abstract data: string with get, set
            abstract id: string with get, set

        type DataPoolingShape =
            abstract pooling: option<LiteralUnions.ClsMean> with get, set
            abstract data: option<ResizeArray<AiSentenceSimilarityOutput>> with get, set
            abstract shape: option<AiSentenceSimilarityOutput> with get, set

        type ASSETS =
            abstract ASSETS: Fetch with get, set

        type ModeTools =
            abstract tools: ResizeArray<obj> with get, set
            abstract mode: LiteralUnions.AutoRequired with get, set

        type ContentReasoningContent068e01e9 =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<AiTextGenerationToolOutput>> with get, set

            [<EmitProperty("reasoning_content")>]
            abstract reasoningContent: option<string> with get, set

            abstract content: string with get, set
            abstract role: string with get, set

        type Audio =
            abstract audio: AiSentenceSimilarityOutput with get, set

        type Type5 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type BodyContentType =
            abstract contentType: option<string> with get, set
            abstract body: option<obj> with get, set

        type EndStartWord =
            [<EmitProperty("end")>]
            abstract ``end``: float with get, set

            abstract start: float with get, set
            abstract word: string with get, set

        type EnableIdPaused9f74c307 =
            abstract status: string with get, set
            abstract paused: bool with get, set

            [<EmitProperty("vectorize_name")>]
            abstract vectorizeName: string with get, set

            abstract source: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract enable: bool with get, set
            abstract id: string with get, set

        type Audio2 =
            abstract audio: string with get, set

        type DataFormat2 =
            abstract format: LiteralUnions.Mp3Wav with get, set
            abstract data: string with get, set

        type DescriptionLanguage =
            abstract descriptionLanguage: option<LiteralUnions.DeEnEsFrItPt> with get, set

        type Image =
            abstract image: AiSentenceSimilarityOutput with get, set

        type RefusalType =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract refusal: string with get, set

        type ChecksumChunksCount4f07e707 =
            abstract metadata: option<obj> with get, set

            [<EmitProperty("created_at")>]
            abstract createdAt: option<string> with get, set

            [<EmitProperty("last_seen_at")>]
            abstract lastSeenAt: option<string> with get, set

            [<EmitProperty("source_id")>]
            abstract sourceId: option<string> with get, set

            [<EmitProperty("file_size")>]
            abstract fileSize: option<float> with get, set

            [<EmitProperty("chunks_count")>]
            abstract chunksCount: option<float> with get, set

            [<EmitProperty("namespace")>]
            abstract ``namespace``: option<string> with get, set

            abstract checksum: option<string> with get, set
            abstract error: option<string> with get, set

            [<EmitProperty("next_action")>]
            abstract nextAction: option<LiteralUnions.DELETE_INDEX> with get, set

            abstract status: LiteralUnions.CompletedErrorOutdated22d6dfc3 with get, set
            abstract key: string with get, set
            abstract id: string with get, set
            abstract Item: key: string -> option<obj>

        type BodyContentType2 =
            abstract contentType: string with get, set
            abstract body: obj with get, set

        type CompletionTokensPromptB7e945e02 =
            [<EmitProperty("total_tokens")>]
            abstract totalTokens: float with get, set

            [<EmitProperty("completion_tokens")>]
            abstract completionTokens: float with get, set

            [<EmitProperty("prompt_tokens")>]
            abstract promptTokens: float with get, set

        type JsonSchemaType =
            [<EmitProperty("json_schema")>]
            abstract jsonSchema: option<obj> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type DescriptionNameParametersStrict =
            abstract strict: option<bool> with get, set
            abstract parameters: option<obj> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set

        type ColorWidth =
            abstract width: option<float> with get, set
            abstract color: option<string> with get, set

        type ModeXY =
            abstract mode: LiteralUnions.BoxCenterRemainder with get, set
            abstract y: option<float> with get, set
            abstract x: option<float> with get, set

        type PerTokenInPerTokenOut =
            [<EmitProperty("per_token_out")>]
            abstract perTokenOut: option<float> with get, set

            [<EmitProperty("per_token_in")>]
            abstract perTokenIn: option<float> with get, set

        type BottomLeftRightTop =
            abstract left: option<Zod.ZodType> with get, set
            abstract bottom: option<Zod.ZodType> with get, set
            abstract right: option<Zod.ZodType> with get, set
            abstract top: option<Zod.ZodType> with get, set

        type InstanceIdPayloadScheduleFdd336c4 =
            abstract schedule: option<CloudflareWorkersModule.WorkflowCronSchedule> with get, set
            abstract workflowName: string with get, set
            abstract instanceId: string with get, set
            abstract timestamp: Date with get, set
            abstract payload: obj with get, set

        type AudioDurationFormat2ede38df =
            abstract format: option<LiteralUnions.JpgM4aPng> with get, set
            abstract imageCount: option<float> with get, set
            abstract duration: option<string> with get, set
            abstract time: option<string> with get, set
            abstract audio: option<bool> with get, set
            abstract mode: option<LiteralUnions.AudioFrameSpritesheetVideo> with get, set

        type ContentNameRole4 =
            abstract name: option<string> with get, set
            abstract content: U2<ResizeArray<UserMessageContentPart>, string> with get, set
            abstract role: string with get, set

        type Fetch =
            abstract fetch: (U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> -> option<RequestInit<RequestInitCfProperties>> -> Promise<Response>) with get, set

        type ContentRole2 =
            abstract content: option<string> with get, set
            abstract role: LiteralUnions.AssistantDeveloperSystemToolUser with get, set

        type BorderBottomHeight5a030263 =
            abstract border: option<U2<ColorKeepTolerance, bool>> with get, set
            abstract height: option<float> with get, set
            abstract width: option<float> with get, set
            abstract right: option<float> with get, set
            abstract left: option<float> with get, set
            abstract bottom: option<float> with get, set
            abstract top: option<float> with get, set

        type IncludeObfuscation =
            [<EmitProperty("include_obfuscation")>]
            abstract includeObfuscation: option<bool> with get, set

        type ContentRoleToolCallId =
            abstract content: option<U3<ResizeArray<ImageUrlTextType>, ImageUrlTextType, string>> with get, set

            [<EmitProperty("tool_call_id")>]
            abstract toolCallId: option<string> with get, set

            abstract role: option<string> with get, set

        type LogprobTokenTopLogprobs =
            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<ResizeArray<TopLogprob>> with get, set

            abstract logprob: float with get, set
            abstract token: string with get, set

        type RankerScoreThreshold =
            [<EmitProperty("score_threshold")>]
            abstract scoreThreshold: option<float> with get, set

            abstract ranker: option<string> with get, set

        type CityCountryRegionTimezone =
            abstract timezone: option<string> with get, set
            abstract region: option<string> with get, set
            abstract country: option<string> with get, set
            abstract city: option<string> with get, set

        type CountName =
            abstract count: float with get, set
            abstract name: string with get, set

        type DefinitionSyntax =
            abstract syntax: LiteralUnions.LarkRegex with get, set
            abstract definition: string with get, set

        type CreatedAtIdMessageMessageType =
            [<EmitProperty("created_at")>]
            abstract createdAt: float with get, set

            [<EmitProperty("message_type")>]
            abstract messageType: float with get, set

            abstract message: string with get, set
            abstract id: float with get, set

        type R2Vectorize =
            abstract r2: option<MetadataSizeBytesObjectCounCad20828> with get, set
            abstract vectorize: option<DimensionsVectorsCount> with get, set

        type Type8 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type BoostByKeywordMatchMode =
            [<EmitProperty("boost_by")>]
            abstract boostBy: option<ResizeArray<DirectionField>> with get, set

            [<EmitProperty("keyword_match_mode")>]
            abstract keywordMatchMode: option<LiteralUnions.AndOr> with get, set

        type AudioChatTemplate7be84a0b =
            abstract functions: option<unit> with get, set

            [<EmitProperty("web_search_options")>]
            abstract webSearchOptions: option<unit> with get, set

            abstract store: option<unit> with get, set
            abstract seed: option<unit> with get, set

            [<EmitProperty("response_format")>]
            abstract responseFormat: option<unit> with get, set

            [<EmitProperty("chat_template_kwargs")>]
            abstract chatTemplateKwargs: option<unit> with get, set

            [<EmitProperty("reasoning_effort")>]
            abstract reasoningEffort: option<unit> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<unit> with get, set

            abstract prediction: option<unit> with get, set
            abstract n: option<unit> with get, set
            abstract modalities: option<unit> with get, set

            [<EmitProperty("max_completion_tokens")>]
            abstract maxCompletionTokens: option<unit> with get, set

            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<unit> with get, set

            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: option<unit> with get, set

            abstract logprobs: option<unit> with get, set

            [<EmitProperty("logit_bias")>]
            abstract logitBias: option<unit> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<unit> with get, set

            [<EmitProperty("function_call")>]
            abstract functionCall: option<unit> with get, set

            abstract stop: option<unit> with get, set
            abstract model: option<unit> with get, set
            abstract messages: option<unit> with get, set
            abstract audio: option<unit> with get, set
            abstract metadata: option<unit> with get, set
            abstract user: option<unit> with get, set

        type FunctionType3 =
            [<EmitProperty("function")>]
            abstract ``function``: Name2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type CodeMessage =
            abstract message: string with get, set
            abstract code: LiteralUnions.EmptyImageFileB06f83e3 with get, set

        type DetailImageUrlType =
            [<EmitProperty("image_url")>]
            abstract imageUrl: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract detail: LiteralUnions.AutoHighLow with get, set

        type BackoffDelayLimit =
            abstract backoff: option<CloudflareWorkersModule.WorkflowBackoff> with get, set
            abstract delay: U15<obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, float> with get, set
            abstract limit: float with get, set

        type PropertiesRequiredType7 =
            abstract required: ResizeArray<string> with get, set
            abstract properties: System.Collections.Generic.IDictionary<string, DescriptionType3> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: U2<string, PropertiesRequiredType7.Type> with get, set

        type ChoicesCreatedId987fa838 =
            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            [<EmitProperty("system_fingerprint")>]
            abstract systemFingerprint: option<string> with get, set

            abstract usage: option<CompletionUsage> with get, set
            abstract choices: ResizeArray<ChatCompletionChoice> with get, set
            abstract model: string with get, set
            abstract created: float with get, set
            abstract object: string with get, set
            abstract id: string with get, set

        type ContentContentIdDispositio8d6362d42 =
            abstract content: obj with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract filename: string with get, set
            abstract contentId: option<unit> with get, set
            abstract disposition: string with get, set

        type CreatedAtIdSessionId4a3adcf1 =
            abstract sessionId: option<string> with get, set
            abstract summary: string with get, set
            abstract updatedAt: Date with get, set
            abstract createdAt: Date with get, set
            abstract id: string with get, set

            [<EmitProperty("type")>]
            abstract ``type``: AgentMemoryMemoryType with get, set

        type PropertyIdValue =
            abstract value: string with get, set

            [<EmitProperty("property_id")>]
            abstract propertyId: string with get, set

        type DescriptionNameParameters =
            abstract parameters: PropertiesRequiredType6 with get, set
            abstract description: string with get, set
            abstract name: string with get, set

        type EndByteIdItemStartByteText =
            abstract item: option<KeyMetadataTimestamp> with get, set

            [<EmitProperty("end_byte")>]
            abstract endByte: float with get, set

            [<EmitProperty("start_byte")>]
            abstract startByte: float with get, set

            abstract text: string with get, set
            abstract id: string with get, set

        type ContentIdRoleStatusType =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract status: LiteralUnions.CompletedInProgressIncomplete with get, set
            abstract role: string with get, set
            abstract content: ResizeArray<U2<ResponseOutputText, ResponseOutputRefusal>> with get, set
            abstract id: string with get, set

        type Messages =
            abstract messages: ResizeArray<ChatCompletionMessageParam> with get, set

        type FiltersMaxNum29a70092 =
            [<EmitProperty("rewrite_query")>]
            abstract rewriteQuery: option<bool> with get, set

            abstract reranking: option<EnabledModel> with get, set

            [<EmitProperty("ranking_options")>]
            abstract rankingOptions: option<RankerScoreThreshold> with get, set

            [<EmitProperty("max_num_results")>]
            abstract maxNumResults: option<float> with get, set

            abstract filters: option<U2<CompoundFilter, ComparisonFilter>> with get, set
            abstract query: string with get, set

        type CustomType =
            abstract custom: Name2 with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type AcceptedPredictionTokens7902cbd7 =
            [<EmitProperty("rejected_prediction_tokens")>]
            abstract rejectedPredictionTokens: option<float> with get, set

            [<EmitProperty("accepted_prediction_tokens")>]
            abstract acceptedPredictionTokens: option<float> with get, set

            [<EmitProperty("audio_tokens")>]
            abstract audioTokens: option<float> with get, set

            [<EmitProperty("reasoning_tokens")>]
            abstract reasoningTokens: option<float> with get, set

        type ContentNameRole5 =
            abstract name: string with get, set
            abstract content: string with get, set
            abstract role: string with get, set

        type Requests2 =
            abstract requests: ResizeArray<U2<AiCfBaaiBgeM3InputQueryAndContexts1, AiCfBaaiBgeM3InputEmbedding1>> with get, set

        type StreamSystemPrompt =
            [<EmitProperty("system_prompt")>]
            abstract systemPrompt: option<string> with get, set

            abstract stream: option<bool> with get, set

        type KeywordTokenizer =
            [<EmitProperty("keyword_tokenizer")>]
            abstract keywordTokenizer: option<LiteralUnions.PorterTrigram> with get, set

        type Type6 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type FusionMethodKeyword6c9246d8 =
            [<EmitProperty("fusion_method")>]
            abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

            [<EmitProperty("reranking_score")>]
            abstract rerankingScore: option<float> with get, set

            [<EmitProperty("vector_rank")>]
            abstract vectorRank: option<float> with get, set

            [<EmitProperty("keyword_rank")>]
            abstract keywordRank: option<float> with get, set

            [<EmitProperty("vector_score")>]
            abstract vectorScore: option<float> with get, set

            [<EmitProperty("keyword_score")>]
            abstract keywordScore: option<float> with get, set

            abstract Item: key: string -> option<obj>

        type Id2 =
            abstract id: string with get, set

        type DetailImageUrlType2 =
            [<EmitProperty("image_url")>]
            abstract imageUrl: option<string> with get, set

            abstract detail: option<LiteralUnions.AutoHighLow> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Images =
            abstract images: option<EmbeddedImageConversionOptions> with get, set

        type FileDataFileIdFilename =
            abstract filename: option<string> with get, set

            [<EmitProperty("file_id")>]
            abstract fileId: option<string> with get, set

            [<EmitProperty("file_data")>]
            abstract fileData: option<string> with get, set

        type FileImageUrlInputAudioTextType =
            abstract file: option<FileDataFileIdFilename> with get, set

            [<EmitProperty("input_audio")>]
            abstract inputAudio: option<DataFormat> with get, set

            [<EmitProperty("image_url")>]
            abstract imageUrl: option<DetailUrl> with get, set

            abstract text: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: LiteralUnions.FileImageUrlInputAudioText with get, set

        type AttributesContentFileF6de1a38 =
            abstract content: ResizeArray<ChatCompletionContentPartText> with get, set
            abstract attributes: obj with get, set
            abstract score: float with get, set
            abstract filename: string with get, set

            [<EmitProperty("file_id")>]
            abstract fileId: string with get, set

        type CountPagePerPageTotalCount =
            [<EmitProperty("total_count")>]
            abstract totalCount: float with get, set

            [<EmitProperty("per_page")>]
            abstract perPage: float with get, set

            abstract page: float with get, set
            abstract count: float with get, set

        type InstanceIdMessage =
            abstract message: string with get, set

            [<EmitProperty("instance_id")>]
            abstract instanceId: string with get, set

        type PercentCompleteStatusUrl =
            abstract url: option<string> with get, set
            abstract status: StreamDownloadStatus with get, set
            abstract percentComplete: float with get, set

        type KeyTypeValue =
            abstract value: U3<bool, float, string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: LiteralUnions.EqGtGteLtLteNe with get, set

            abstract key: string with get, set

        type TextType =
            abstract text: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

        type ConvertOGImage =
            abstract convertOGImage: option<bool> with get, set

        type BottomLeftRightTop2 =
            abstract right: option<float> with get, set
            abstract left: option<float> with get, set
            abstract bottom: option<float> with get, set
            abstract top: option<float> with get, set

        type AiSearchOptionsMessagesQuery2 =
            [<EmitProperty("ai_search_options")>]
            abstract aiSearchOptions: option<AiSearchOptions> with get, set

            abstract messages: ResizeArray<AiSearchMessage> with get, set
            abstract query: option<unit> with get, set

        type IncludeObfuscationIncludeUsage =
            [<EmitProperty("include_obfuscation")>]
            abstract includeObfuscation: option<bool> with get, set

            [<EmitProperty("include_usage")>]
            abstract includeUsage: option<bool> with get, set

        type FunctionIdType2 =
            [<EmitProperty("function")>]
            abstract ``function``: option<ArgumentsName3> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

            abstract id: option<string> with get, set

        type DescriptionNameParametersStrictType =
            abstract description: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract strict: option<bool> with get, set
            abstract parameters: option<System.Collections.Generic.IDictionary<string, option<obj>>> with get, set
            abstract name: string with get, set

        type Type10 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type FiltersType =
            abstract filters: ResizeArray<ComparisonFilter> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: LiteralUnions.AndOr with get, set

        type FetchQueueRejectionhandled4a6d4926 =
            abstract rejectionhandled: PromiseRejectionEvent with get, set
            abstract unhandledrejection: PromiseRejectionEvent with get, set
            abstract queue: QueueEvent<option<obj>> with get, set
            abstract scheduled: ScheduledEvent with get, set
            abstract fetch: FetchEvent with get, set

        type BoostByContextAef2ce5f =
            [<EmitProperty("boost_by")>]
            abstract boostBy: option<ResizeArray<DirectionField>> with get, set

            [<EmitProperty("return_on_failure")>]
            abstract returnOnFailure: option<bool> with get, set

            [<EmitProperty("metadata_only")>]
            abstract metadataOnly: option<bool> with get, set

            [<EmitProperty("context_expansion")>]
            abstract contextExpansion: option<float> with get, set

            abstract filters: option<VectorizeVectorMetadataFilter> with get, set

            [<EmitProperty("max_num_results")>]
            abstract maxNumResults: option<float> with get, set

            [<EmitProperty("match_threshold")>]
            abstract matchThreshold: option<float> with get, set

            [<EmitProperty("keyword_match_mode")>]
            abstract keywordMatchMode: option<LiteralUnions.AndOr> with get, set

            [<EmitProperty("fusion_method")>]
            abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

            [<EmitProperty("retrieval_type")>]
            abstract retrievalType: option<LiteralUnions.HybridKeywordVector> with get, set

            abstract Item: key: string -> option<obj>

        type RetriesSensitiveTimeout =
            abstract sensitive: option<string> with get, set
            abstract timeout: option<U15<obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, obj, float>> with get, set
            abstract retries: option<BackoffDelayLimit> with get, set

        type DescriptionIdName2 =
            abstract description: string with get, set
            abstract name: string with get, set
            abstract id: string with get, set

        type Html =
            abstract html: string with get, set

        type TextType4 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type EqGtGteLtLteNe =
            [<EmitProperty("$gte")>]
            abstract ``$gte``: option<U3<string, float, bool>> with get, set

            [<EmitProperty("$gt")>]
            abstract ``$gt``: option<U3<string, float, bool>> with get, set

            [<EmitProperty("$lte")>]
            abstract ``$lte``: option<U3<string, float, bool>> with get, set

            [<EmitProperty("$lt")>]
            abstract ``$lt``: option<U3<string, float, bool>> with get, set

            [<EmitProperty("$ne")>]
            abstract ``$ne``: option<U3<string, float, bool>> with get, set

            [<EmitProperty("$eq")>]
            abstract ``$eq``: option<U3<string, float, bool>> with get, set

        type AiGatewayId7bdc3a35 =
            abstract metadata: option<obj> with get, set

            [<EmitProperty("sync_interval")>]
            abstract syncInterval: option<LiteralUnions.I14400I21600I3600I43200I7200I86400> with get, set

            [<EmitProperty("namespace")>]
            abstract ``namespace``: option<string> with get, set

            [<EmitProperty("custom_metadata")>]
            abstract customMetadata: option<ResizeArray<DataTypeFieldName>> with get, set

            [<EmitProperty("cache_threshold")>]
            abstract cacheThreshold: option<LiteralUnions.AnythingGoesClose50b421b1> with get, set

            abstract cache: option<bool> with get, set

            [<EmitProperty("max_num_results")>]
            abstract maxNumResults: option<float> with get, set

            [<EmitProperty("score_threshold")>]
            abstract scoreThreshold: option<float> with get, set

            [<EmitProperty("chunk_overlap")>]
            abstract chunkOverlap: option<float> with get, set

            [<EmitProperty("chunk_size")>]
            abstract chunkSize: option<float> with get, set

            abstract chunk: option<bool> with get, set

            [<EmitProperty("retrieval_options")>]
            abstract retrievalOptions: option<BoostByKeywordMatchMode> with get, set

            [<EmitProperty("indexing_options")>]
            abstract indexingOptions: option<KeywordTokenizer> with get, set

            [<EmitProperty("fusion_method")>]
            abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

            [<EmitProperty("index_method")>]
            abstract indexMethod: option<KeywordVector> with get, set

            /// <deprecated>
            /// Use index_method instead.
            /// </deprecated>
            [<EmitProperty("hybrid_search_enabled")>]
            abstract hybridSearchEnabled: option<bool> with get, set

            [<EmitProperty("reranking_model")>]
            abstract rerankingModel: option<string> with get, set

            [<EmitProperty("rewrite_model")>]
            abstract rewriteModel: option<string> with get, set

            [<EmitProperty("ai_search_model")>]
            abstract aiSearchModel: option<string> with get, set

            [<EmitProperty("embedding_model")>]
            abstract embeddingModel: option<string> with get, set

            abstract reranking: option<bool> with get, set

            [<EmitProperty("rewrite_query")>]
            abstract rewriteQuery: option<bool> with get, set

            [<EmitProperty("ai_gateway_id")>]
            abstract aiGatewayId: option<string> with get, set

            [<EmitProperty("token_id")>]
            abstract tokenId: option<string> with get, set

            [<EmitProperty("source_params")>]
            abstract sourceParams: option<obj> with get, set

            abstract source: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<U2<LiteralUnions.R2WebCrawler, string>> with get, set

            abstract id: string with get, set
            abstract Item: key: string -> option<obj>

        type EndIndexStartIndexTitleUrl =
            [<EmitProperty("end_index")>]
            abstract endIndex: float with get, set

            [<EmitProperty("start_index")>]
            abstract startIndex: float with get, set

            abstract title: string with get, set
            abstract url: string with get, set

        type CacheQueryRewriteRerankingRetrieval =
            abstract cache: option<CacheThresholdEnabled> with get, set
            abstract reranking: option<EnabledMatchThresholdModel> with get, set

            [<EmitProperty("query_rewrite")>]
            abstract queryRewrite: option<EnabledModelRewritePrompt> with get, set

            abstract retrieval: option<BoostByContextAef2ce5f> with get, set
            abstract Item: key: string -> option<obj>

        type Type7 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type InputInitPrototype<'CfHostMetadata, 'Cf> =
            abstract prototype: Request<option<obj>, U2<RequestInitCfProperties, obj>> with get, set
            abstract Create: input: U3<Request<U2<RequestInitCfProperties, obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<'Cf> -> Request<'CfHostMetadata, 'Cf>

        type DescriptionType3 =
            abstract description: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type ErrorFormatIdMimeTypeName =
            abstract error: string with get, set
            abstract format: string with get, set
            abstract mimeType: string with get, set
            abstract name: string with get, set
            abstract id: string with get, set

        type InputTokensOutputTokensTotalTokens =
            [<EmitProperty("total_tokens")>]
            abstract totalTokens: float with get, set

            [<EmitProperty("output_tokens")>]
            abstract outputTokens: float with get, set

            [<EmitProperty("input_tokens")>]
            abstract inputTokens: float with get, set

        type FunctionIdType4 =
            [<EmitProperty("function")>]
            abstract ``function``: ArgumentsName with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract id: string with get, set

        type KeyMetadataTimestamp =
            abstract metadata: option<obj> with get, set
            abstract key: string with get, set
            abstract timestamp: option<float> with get, set

        type ConversionOptionsExtraHeadersGateway =
            abstract conversionOptions: option<ConversionOptions> with get, set
            abstract extraHeaders: option<obj> with get, set
            abstract gateway: option<GatewayOptions> with get, set

        type Url =
            abstract url: option<string> with get, set

        type TotalCost =
            [<EmitProperty("total_cost")>]
            abstract totalCost: option<float> with get, set

        type ApproximateType =
            abstract approximate: CityCountryRegionTimezone with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type DimensionsMetric =
            abstract metric: VectorizeDistanceMetric with get, set
            abstract dimensions: float with get, set

        type FunctionIdType =
            [<EmitProperty("function")>]
            abstract ``function``: ArgumentsName with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract id: string with get, set

        type HiddenSelectorTimeoutVisible =
            abstract timeout: option<float> with get, set
            abstract visible: option<bool> with get, set
            abstract hidden: option<bool> with get, set
            abstract selector: string with get, set

        type Id3 =
            abstract id: string with get, set

        type EnabledModel =
            abstract model: option<string> with get, set
            abstract enabled: option<bool> with get, set

        type CacheKeyCacheTtlCollectLogE07ec9ba =
            abstract retries: option<GatewayRetries> with get, set
            abstract requestTimeoutMs: option<float> with get, set
            abstract eventId: option<string> with get, set
            abstract collectLog: option<bool> with get, set
            abstract metadata: option<obj> with get, set
            abstract skipCache: option<bool> with get, set
            abstract cacheTtl: option<float> with get, set
            abstract cacheKey: option<string> with get, set
            abstract id: string with get, set

        type Type9 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Preset =
            abstract preset: string with get, set

        type ContentUrl =
            abstract url: option<string> with get, set
            abstract content: option<string> with get, set

        type AudioDtype =
            abstract dtype: option<LiteralUnions.Float32Float64Uint8> with get, set
            abstract audio: BodyContentType2 with get, set

        type CodeName2 =
            abstract code: string with get, set
            abstract name: string with get, set

        type AuthorizationContentTypeC32cb5e3e =
            [<EmitProperty("Content-Type")>]
            abstract contentType: option<string> with get, set

            [<EmitProperty("Authorization")>]
            abstract authorization: option<string> with get, set

            [<EmitProperty("cf-aig-collect-log")>]
            abstract cfAigCollectLog: option<U2<string, bool>> with get, set

            [<EmitProperty("cf-aig-backoff")>]
            abstract cfAigBackoff: option<string> with get, set

            [<EmitProperty("cf-aig-retry-delay")>]
            abstract cfAigRetryDelay: option<U2<string, float>> with get, set

            [<EmitProperty("cf-aig-max-attempts")>]
            abstract cfAigMaxAttempts: option<U2<string, float>> with get, set

            [<EmitProperty("cf-aig-request-timeout")>]
            abstract cfAigRequestTimeout: option<U2<string, float>> with get, set

            [<EmitProperty("cf-aig-event-id")>]
            abstract cfAigEventId: option<string> with get, set

            [<EmitProperty("cf-aig-cache-key")>]
            abstract cfAigCacheKey: option<string> with get, set

            [<EmitProperty("cf-aig-skip-cache")>]
            abstract cfAigSkipCache: option<U2<string, bool>> with get, set

            [<EmitProperty("cf-aig-cache-ttl")>]
            abstract cfAigCacheTtl: option<U2<string, float>> with get, set

            [<EmitProperty("cf-aig-custom-cost")>]
            abstract cfAigCustomCost: option<U3<PerTokenInPerTokenOut, TotalCost, string>> with get, set

            [<EmitProperty("cf-aig-metadata")>]
            abstract cfAigMetadata: option<U2<obj, string>> with get, set

        type ResponseToolCallsUsage =
            [<EmitProperty("tool_calls")>]
            abstract toolCalls: option<ResizeArray<ArgumentsName2>> with get, set

            abstract usage: option<CompletionTokensPromptB7e945e0> with get, set
            abstract response: string with get, set

        type PropertiesRequiredType6 =
            abstract properties: System.Collections.Generic.IDictionary<string, DescriptionType2> with get, set
            abstract required: option<ResizeArray<string>> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type CreatedAtErrorFea3b468 =
            abstract truncation: option<unit> with get, set

            [<EmitProperty("safety_identifier")>]
            abstract safetyIdentifier: option<unit> with get, set

            abstract reasoning: option<unit> with get, set
            abstract prompt: option<unit> with get, set

            [<EmitProperty("previous_response_id")>]
            abstract previousResponseId: option<unit> with get, set

            [<EmitProperty("max_output_tokens")>]
            abstract maxOutputTokens: option<unit> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<unit> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<unit> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<unit> with get, set

            [<EmitProperty("incomplete_details")>]
            abstract incompleteDetails: option<unit> with get, set

            [<EmitProperty("output_text")>]
            abstract outputText: option<unit> with get, set

            [<EmitProperty("created_at")>]
            abstract createdAt: option<unit> with get, set

            abstract instructions: option<unit> with get, set
            abstract temperature: option<unit> with get, set
            abstract text: option<unit> with get, set
            abstract tools: option<unit> with get, set
            abstract output: option<unit> with get, set
            abstract status: option<unit> with get, set
            abstract error: option<unit> with get, set

        type ImageUrlTextType =
            [<EmitProperty("image_url")>]
            abstract imageUrl: option<Url> with get, set

            abstract text: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

        type CacheThresholdEnabled =
            [<EmitProperty("cache_threshold")>]
            abstract cacheThreshold: option<LiteralUnions.AnythingGoesClose50b421b1> with get, set

            abstract enabled: option<bool> with get, set

        type AttemptConfigStep =
            abstract config: CloudflareWorkersModule.WorkflowStepConfig with get, set
            abstract attempt: float with get, set
            abstract step: CountName with get, set

        type CountLimitOffsetTotal =
            abstract offset: float with get, set
            abstract limit: float with get, set
            abstract total: float with get, set
            abstract count: float with get, set

        type Score =
            abstract score: float with get, set

        type ClearThinkingEnableThinking =
            [<EmitProperty("clear_thinking")>]
            abstract clearThinking: option<bool> with get, set

            [<EmitProperty("enable_thinking")>]
            abstract enableThinking: option<bool> with get, set

        type PoolingText =
            abstract pooling: option<LiteralUnions.ClsMean> with get, set
            abstract text: U2<ResizeArray<string>, string> with get, set

        type Id =
            abstract id: string with get, set

        type Type4 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type ArgumentsName =
            abstract arguments: string with get, set
            abstract name: string with get, set

        type DeviceScaleFactorHasTouchH6969243e =
            abstract hasTouch: option<bool> with get, set
            abstract isLandscape: option<bool> with get, set
            abstract isMobile: option<bool> with get, set
            abstract deviceScaleFactor: option<float> with get, set
            abstract height: float with get, set
            abstract width: float with get, set

        type AttributesDiagnosticChannel27417ae8 =
            abstract attributes: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set

            [<EmitProperty("return")>]
            abstract ``return``: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set

            abstract log: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set

            [<EmitProperty("exception")>]
            abstract ``exception``: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set

            abstract diagnosticChannel: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set
            abstract spanClose: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set
            abstract spanOpen: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set
            abstract outcome: (option<TailStream.TailEvent<obj> -> option<Promise<unit>>>) with get, set

        type DetailUrl =
            abstract detail: option<LiteralUnions.AutoHighLow> with get, set
            abstract url: option<string> with get, set

        type ImagesMetadata =
            abstract metadata: option<bool> with get, set
            abstract images: option<EmbeddedImageConversionOptions> with get, set

        type LogprobToken =
            abstract logprob: option<float> with get, set
            abstract token: option<string> with get, set

        type BytesLogprobTokenTopLogprobs =
            [<EmitProperty("top_logprobs")>]
            abstract topLogprobs: ResizeArray<ChatCompletionTopLogprob> with get, set

            abstract bytes: option<AiSentenceSimilarityOutput> with get, set
            abstract logprob: float with get, set
            abstract token: string with get, set

        type FunctionType4 =
            [<EmitProperty("function")>]
            abstract ``function``: FunctionDefinition with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

        type Suffix =
            abstract suffix: float with get, set

        type AudioDtype2 =
            abstract dtype: option<LiteralUnions.Float32Float64Uint8> with get, set
            abstract audio: string with get, set

        type BackgroundConversationIncl18a26c75 =
            [<EmitProperty("prompt_cache_key")>]
            abstract promptCacheKey: option<unit> with get, set

            abstract conversation: option<unit> with get, set
            abstract background: option<unit> with get, set
            abstract truncation: option<unit> with get, set

            [<EmitProperty("safety_identifier")>]
            abstract safetyIdentifier: option<unit> with get, set

            abstract reasoning: option<unit> with get, set

            [<EmitProperty("previous_response_id")>]
            abstract previousResponseId: option<unit> with get, set

            [<EmitProperty("max_output_tokens")>]
            abstract maxOutputTokens: option<unit> with get, set

            abstract instructions: option<unit> with get, set
            abstract include: option<unit> with get, set
            abstract text: option<unit> with get, set
            abstract input: option<unit> with get, set

        type Port =
            abstract port: float with get, set

        type SqlDurationMs =
            [<EmitProperty("sql_duration_ms")>]
            abstract sqlDurationMs: float with get, set

        type ContentIdTypeUrl =
            abstract id: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<string> with get, set

            abstract url: option<string> with get, set
            abstract content: option<string> with get, set

        type CreatedAtError772d78a5 =
            abstract usage: option<ResponseUsage> with get, set
            abstract truncation: option<LiteralUnions.AutoDisabled> with get, set
            abstract text: option<ResponseTextConfig> with get, set
            abstract status: option<ResponseStatus> with get, set

            [<EmitProperty("service_tier")>]
            abstract serviceTier: option<LiteralUnions.AutoDefaultFlexPriorityScale> with get, set

            [<EmitProperty("safety_identifier")>]
            abstract safetyIdentifier: option<string> with get, set

            abstract reasoning: option<Reasoning> with get, set
            abstract prompt: option<ResponsePrompt> with get, set

            [<EmitProperty("previous_response_id")>]
            abstract previousResponseId: option<string> with get, set

            [<EmitProperty("max_output_tokens")>]
            abstract maxOutputTokens: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract tools: option<ResizeArray<Tool>> with get, set

            [<EmitProperty("tool_choice")>]
            abstract toolChoice: option<U2<string, ToolChoiceFunction>> with get, set

            abstract temperature: option<float> with get, set

            [<EmitProperty("parallel_tool_calls")>]
            abstract parallelToolCalls: option<bool> with get, set

            abstract output: option<ResizeArray<ResponseOutputItem>> with get, set
            abstract object: option<string> with get, set
            abstract instructions: option<U2<ResizeArray<ResponseInputItem>, string>> with get, set

            [<EmitProperty("incomplete_details")>]
            abstract incompleteDetails: option<ResponseIncompleteDetails> with get, set

            abstract error: option<ResponseError> with get, set

            [<EmitProperty("output_text")>]
            abstract outputText: option<string> with get, set

            [<EmitProperty("created_at")>]
            abstract createdAt: option<float> with get, set

            abstract id: option<string> with get, set

        type Reason =
            abstract reason: option<LiteralUnions.ContentFilterMaxOutputTokens> with get, set

        type DetailUrl2 =
            abstract detail: option<LiteralUnions.AutoHighLow> with get, set
            abstract url: string with get, set

        type MetadataSizeBytesObjectCounCad20828 =
            abstract objectCount: float with get, set
            abstract metadataSizeBytes: float with get, set
            abstract payloadSizeBytes: float with get, set

        type TextType8 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type StatusTitle =
            abstract title: string with get, set
            abstract status: float with get, set

        type ContentNameRole =
            abstract name: option<string> with get, set
            abstract content: string with get, set
            abstract role: U2<LiteralUnions.AssistantSystemToolUser, obj> with get, set

        type ArgumentsName2 =
            abstract name: option<string> with get, set
            abstract arguments: option<obj> with get, set

        type CtxErrorOutputStepName<'T> =
            abstract stepName: string with get, set
            abstract output: option<'T> with get, set
            abstract error: exn with get, set
            abstract ctx: CloudflareWorkersModule.WorkflowStepContext with get, set

        type DescriptionNameParameters3 =
            abstract parameters: option<PropertiesRequiredType7> with get, set
            abstract description: string with get, set
            abstract name: string with get, set

        type ContentRole3 =
            abstract content: option<string> with get, set
            abstract role: LiteralUnions.AssistantDeveloperSystemToolUser with get, set
            abstract Item: key: string -> option<obj>

        type DescriptionNameSchemaStrictType =
            abstract strict: option<bool> with get, set
            abstract description: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract schema: System.Collections.Generic.IDictionary<string, option<obj>> with get, set
            abstract name: string with get, set

        type EffortGenerateSummarySummary =
            abstract summary: option<LiteralUnions.AutoConciseDetailed> with get, set

            [<EmitProperty("generate_summary")>]
            abstract generateSummary: option<LiteralUnions.AutoConciseDetailed> with get, set

            abstract effort: option<LiteralUnions.HighLowMediumMinimal> with get, set

        type CreatedDownloadedFromHeighFcac8e09 =
            abstract position: StreamWatermarkPosition with get, set
            abstract scale: float with get, set
            abstract padding: float with get, set
            abstract opacity: float with get, set
            abstract name: string with get, set
            abstract downloadedFrom: option<string> with get, set
            abstract created: string with get, set
            abstract width: float with get, set
            abstract height: float with get, set
            abstract size: float with get, set
            abstract id: string with get, set

        type CssSelectorHostnameImages =
            abstract cssSelector: option<string> with get, set
            abstract hostname: option<string> with get, set
            abstract images: option<CssSelectorHostnameImages.Images> with get, set

        type DomainExpiresHttpOnlyC6af887f =
            abstract partitionKey: option<string> with get, set
            abstract sourcePort: option<float> with get, set
            abstract sourceScheme: option<LiteralUnions.NonSecureSecureUnset> with get, set
            abstract sameParty: option<bool> with get, set
            abstract priority: option<LiteralUnions.HighLowMedium> with get, set
            abstract expires: option<float> with get, set
            abstract sameSite: option<LiteralUnions.LaxNoneStrict> with get, set
            abstract httpOnly: option<bool> with get, set
            abstract secure: option<bool> with get, set
            abstract path: option<string> with get, set
            abstract domain: option<string> with get, set
            abstract url: option<string> with get, set
            abstract value: string with get, set
            abstract name: string with get, set

        type Format =
            abstract format: string with get, set

        type FrequencyPenaltyImageFb6819ce =
            [<EmitProperty("max_tokens")>]
            abstract maxTokens: option<float> with get, set

            abstract image: U2<AiSentenceSimilarityOutput, obj> with get, set

            [<EmitProperty("presence_penalty")>]
            abstract presencePenalty: option<float> with get, set

            [<EmitProperty("frequency_penalty")>]
            abstract frequencyPenalty: option<float> with get, set

            [<EmitProperty("repetition_penalty")>]
            abstract repetitionPenalty: option<float> with get, set

            abstract seed: option<float> with get, set

            [<EmitProperty("top_k")>]
            abstract topK: option<float> with get, set

            [<EmitProperty("top_p")>]
            abstract topP: option<float> with get, set

            abstract raw: option<bool> with get, set
            abstract prompt: option<string> with get, set

        type Text =
            abstract text: option<string> with get, set

        type HeightScaleWidthXY =
            abstract scale: option<float> with get, set
            abstract height: float with get, set
            abstract width: float with get, set
            abstract y: float with get, set
            abstract x: float with get, set

        type AiSearchOptionsMessagesQuery =
            [<EmitProperty("ai_search_options")>]
            abstract aiSearchOptions: option<AiSearchOptions> with get, set

            abstract messages: option<unit> with get, set
            abstract query: string with get, set

        type InstanceIds =
            [<EmitProperty("instance_ids")>]
            abstract instanceIds: ResizeArray<string> with get, set

        type FinishReasonIndexLogprobsMessage =
            abstract logprobs: option<ChatCompletionLogprobs> with get, set

            [<EmitProperty("finish_reason")>]
            abstract finishReason: LiteralUnions.ContentFilterFunction33a35c6e with get, set

            abstract message: ChatCompletionResponseMessage with get, set
            abstract index: float with get, set

        type TextType7 =
            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract text: string with get, set

        type AiGatewayId59da3456 =
            abstract metadata: option<obj> with get, set

            [<EmitProperty("sync_interval")>]
            abstract syncInterval: option<LiteralUnions.I14400I21600I3600I43200I7200I86400> with get, set

            [<EmitProperty("custom_metadata")>]
            abstract customMetadata: option<ResizeArray<DataTypeFieldName>> with get, set

            [<EmitProperty("cache_threshold")>]
            abstract cacheThreshold: option<LiteralUnions.AnythingGoesClose50b421b1> with get, set

            abstract cache: option<bool> with get, set

            [<EmitProperty("max_num_results")>]
            abstract maxNumResults: option<float> with get, set

            [<EmitProperty("score_threshold")>]
            abstract scoreThreshold: option<float> with get, set

            [<EmitProperty("chunk_overlap")>]
            abstract chunkOverlap: option<float> with get, set

            [<EmitProperty("chunk_size")>]
            abstract chunkSize: option<float> with get, set

            abstract chunk: option<bool> with get, set

            [<EmitProperty("retrieval_options")>]
            abstract retrievalOptions: option<BoostByKeywordMatchMode> with get, set

            [<EmitProperty("indexing_options")>]
            abstract indexingOptions: option<KeywordTokenizer> with get, set

            [<EmitProperty("fusion_method")>]
            abstract fusionMethod: option<LiteralUnions.MaxRrf> with get, set

            [<EmitProperty("index_method")>]
            abstract indexMethod: option<KeywordVector> with get, set

            /// <deprecated>
            /// Use index_method instead.
            /// </deprecated>
            [<EmitProperty("hybrid_search_enabled")>]
            abstract hybridSearchEnabled: option<bool> with get, set

            [<EmitProperty("reranking_model")>]
            abstract rerankingModel: option<string> with get, set

            [<EmitProperty("rewrite_model")>]
            abstract rewriteModel: option<string> with get, set

            [<EmitProperty("ai_search_model")>]
            abstract aiSearchModel: option<string> with get, set

            [<EmitProperty("embedding_model")>]
            abstract embeddingModel: option<string> with get, set

            abstract reranking: option<bool> with get, set

            [<EmitProperty("rewrite_query")>]
            abstract rewriteQuery: option<bool> with get, set

            [<EmitProperty("ai_gateway_id")>]
            abstract aiGatewayId: option<string> with get, set

            [<EmitProperty("token_id")>]
            abstract tokenId: option<string> with get, set

            [<EmitProperty("modified_at")>]
            abstract modifiedAt: option<string> with get, set

            [<EmitProperty("created_at")>]
            abstract createdAt: option<string> with get, set

            [<EmitProperty("namespace")>]
            abstract ``namespace``: option<string> with get, set

            abstract status: option<string> with get, set
            abstract paused: option<bool> with get, set

            [<EmitProperty("source_params")>]
            abstract sourceParams: option<obj> with get, set

            abstract source: option<string> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: option<U2<LiteralUnions.R2WebCrawler, string>> with get, set

            abstract id: string with get, set
            abstract Item: key: string -> option<obj>

        type FormatVoice =
            abstract format: LiteralUnions.AacFlacMp3OpusPcm16Wav with get, set
            abstract voice: U2<StreamDirectUploadWatermark, string> with get, set

        type ContentEncryptedContentD8f7b914 =
            abstract status: option<LiteralUnions.CompletedInProgressIncomplete> with get, set

            [<EmitProperty("encrypted_content")>]
            abstract encryptedContent: option<string> with get, set

            abstract content: option<ResizeArray<ResponseReasoningContentItem>> with get, set

            [<EmitProperty("type")>]
            abstract ``type``: string with get, set

            abstract summary: ResizeArray<ResponseReasoningSummaryItem> with get, set
            abstract id: string with get, set

        type DescriptionEndReasonB5dfa358 =
            [<EmitProperty("end_reason")>]
            abstract endReason: option<string> with get, set

            [<EmitProperty("ended_at")>]
            abstract endedAt: option<string> with get, set

            [<EmitProperty("started_at")>]
            abstract startedAt: option<string> with get, set

            [<EmitProperty("last_seen_at")>]
            abstract lastSeenAt: option<string> with get, set

            abstract description: option<string> with get, set
            abstract source: LiteralUnions.ScheduleUser with get, set
            abstract id: string with get, set

        type ArgumentsName3 =
            abstract arguments: option<obj> with get, set
            abstract name: option<string> with get, set

        type IdScore =
            abstract score: option<float> with get, set
            abstract id: option<float> with get, set

        type Requests =
            abstract requests: ResizeArray<PoolingText> with get, set

        type DescriptionFormatName =
            abstract format: option<ChatCompletionCustomToolFormat> with get, set
            abstract description: option<string> with get, set
            abstract name: string with get, set

        module AiSearchOptionsMessagesQuery3 =
            type AiSearchOptions =
                [<EmitProperty("instance_ids")>]
                abstract instanceIds: ResizeArray<string> with get, set

                abstract cache: option<CacheThresholdEnabled> with get, set
                abstract reranking: option<EnabledMatchThresholdModel> with get, set

                [<EmitProperty("query_rewrite")>]
                abstract queryRewrite: option<EnabledModelRewritePrompt> with get, set

                abstract retrieval: option<BoostByContextAef2ce5f> with get, set
                abstract Item: key: string -> option<obj>

        module AiSearchOptionsMessagesQuery4 =
            type AiSearchOptions =
                [<EmitProperty("instance_ids")>]
                abstract instanceIds: ResizeArray<string> with get, set

                abstract cache: option<CacheThresholdEnabled> with get, set
                abstract reranking: option<EnabledMatchThresholdModel> with get, set

                [<EmitProperty("query_rewrite")>]
                abstract queryRewrite: option<EnabledModelRewritePrompt> with get, set

                abstract retrieval: option<BoostByContextAef2ce5f> with get, set
                abstract Item: key: string -> option<obj>

        module CssSelectorHostnameImages =
            type Images =
                abstract convertOGImage: option<bool> with get, set
                abstract maxConvertedImages: option<float> with get, set
                abstract convert: option<bool> with get, set
                abstract descriptionLanguage: option<LiteralUnions.DeEnEsFrItPt> with get, set

        module DataEnvFunctionPath23a88d11 =
            type Env =
                abstract ASSETS: Fetch with get, set

        module Images =
            type Images =
                abstract maxConvertedImages: option<float> with get, set
                abstract convert: option<bool> with get, set
                abstract descriptionLanguage: option<LiteralUnions.DeEnEsFrItPt> with get, set

        module ImagesMetadata =
            type Images =
                abstract maxConvertedImages: option<float> with get, set
                abstract convert: option<bool> with get, set
                abstract descriptionLanguage: option<LiteralUnions.DeEnEsFrItPt> with get, set

        module InputInitPrototype =
            module Create =
                type Input =
                    abstract tlsClientAuth: U2<IncomingRequestCfPropertiesTLSClientAuth, IncomingRequestCfPropertiesTLSClientAuthPlaceholder> with get, set
                    /// <example>
                    /// "635"
                    /// </example>
                    abstract metroCode: option<string> with get, set
                    /// <example>
                    /// "TX"
                    /// </example>
                    abstract regionCode: option<string> with get, set
                    /// <example>
                    /// "Texas"
                    /// </example>
                    abstract region: option<string> with get, set
                    /// <example>
                    /// "America/Chicago"
                    /// </example>
                    abstract timezone: option<string> with get, set
                    /// <example>
                    /// "-97.74260"
                    /// </example>
                    abstract longitude: option<string> with get, set
                    /// <example>
                    /// "30.27130"
                    /// </example>
                    abstract latitude: option<string> with get, set
                    /// <example>
                    /// "78701"
                    /// </example>
                    abstract postalCode: option<string> with get, set
                    /// <example>
                    /// "Austin"
                    /// </example>
                    abstract city: option<string> with get, set
                    /// <example>
                    /// "AN"
                    /// </example>
                    abstract continent: option<ContinentCode> with get, set
                    /// <example>
                    /// "1"
                    /// </example>
                    abstract isEUCountry: option<string> with get, set
                    /// <example>
                    /// "GB"
                    /// </example>
                    abstract country: option<LiteralUnions.ADAEAFAda77976> with get, set
                    abstract hostMetadata: option<obj> with get, set
                    /// <deprecated />
                    abstract clientTrustScore: float with get, set
                    abstract botManagement: Input.BotManagement with get, set
                    abstract tlsExportedAuthenticator: option<IncomingRequestCfPropertiesExportedAuthenticatorMetadata> with get, set
                    /// <example>
                    /// "AEAD-AES128-GCM-SHA256"
                    /// </example>
                    abstract tlsCipher: string with get, set
                    /// <example>
                    /// "TLSv1.3"
                    /// </example>
                    abstract tlsVersion: string with get, set
                    /// <example>
                    /// "weight=192;exclusive=0;group=3;group-weight=127"
                    /// </example>
                    abstract requestPriority: string with get, set
                    /// <example>
                    /// "HTTP/2"
                    /// </example>
                    abstract httpProtocol: string with get, set
                    /// <example>
                    /// 3
                    /// </example>
                    abstract edgeRequestKeepAliveStatus: IncomingRequestCfPropertiesEdgeRequestKeepAliveStatus with get, set
                    /// <example>
                    /// "DFW"
                    /// </example>
                    abstract colo: string with get, set
                    /// <example>
                    /// 22
                    /// </example>
                    abstract clientTcpRtt: option<float> with get, set
                    /// <example>
                    /// "gzip, deflate, br"
                    /// </example>
                    abstract clientAcceptEncoding: option<string> with get, set
                    /// <example>
                    /// "Google Cloud"
                    /// </example>
                    abstract asOrganization: option<string> with get, set
                    /// <example>
                    /// 395747
                    /// </example>
                    abstract asn: option<float> with get, set
                    abstract Item: key: string -> option<obj>

                module Input =
                    type BotManagement =
                        abstract ja3Hash: string with get, set
                        abstract detectionIds: AiSentenceSimilarityOutput with get, set
                        abstract staticResource: bool with get, set
                        abstract corporateProxy: bool with get, set
                        abstract verifiedBot: bool with get, set
                        /// <example>
                        /// 54
                        /// </example>
                        abstract score: float with get, set

        module PropertiesRequiredType7 =
            type Type = interface end

        module RetriesTimeout =
            type Timeout = string

            module Timeout =
                type Case11 = string
                type Case14 = string
                type Case13 = string
                type Case12 = string
                type Case10 = string
                type Case3 = string
                type Case9 = string
                type Case8 = string
                type Case7 = string
                type Case2 = string
                type Case6 = string
                type Case5 = string
                type Case4 = string

    module SqlStorage =
        type Statement =
            abstract prototype: SqlStorageStatement with get, set
            abstract Create: unit -> SqlStorageStatement

        type Cursor =
            abstract prototype: SqlStorageCursor<option<obj>> with get, set
            abstract Create: unit -> SqlStorageCursor<obj>

    module SqlStorageCursor =
        type Next =
            abstract value: obj with get, set

            [<EmitProperty("done")>]
            abstract ``done``: option<bool> with get, set

        module Next =
            type Case2 =
                abstract value: option<unit> with get, set

                [<EmitProperty("done")>]
                abstract ``done``: bool with get, set

    module StreamBinding =
        type CreateDirectUpload =
            abstract scheduledDeletion: option<string> with get, set
            abstract watermark: option<StreamWatermark> with get, set
            abstract id: string with get, set
            abstract uploadURL: string with get, set

        module CreateDirectUpload =
            type Params =
                abstract watermark: option<StreamDirectUploadWatermark> with get, set
                abstract scheduledDeletion: option<string> with get, set
                abstract thumbnailTimestampPct: option<float> with get, set
                abstract requireSignedURLs: option<bool> with get, set
                abstract allowedOrigins: option<ResizeArray<string>> with get, set
                abstract meta: option<obj> with get, set
                abstract creator: option<string> with get, set
                abstract expiry: option<string> with get, set
                abstract maxDurationSeconds: float with get, set

        module Upload =
            type Params =
                abstract watermarkId: option<string> with get, set
                abstract thumbnailTimestampPct: option<float> with get, set
                abstract scheduledDeletion: option<string> with get, set
                abstract requireSignedURLs: option<bool> with get, set
                abstract meta: option<obj> with get, set
                abstract creator: option<string> with get, set
                abstract allowedOrigins: option<ResizeArray<string>> with get, set

    module StreamScopedCaptions =
        type Upload =
            abstract status: option<StreamDownloadStatus> with get, set
            abstract language: string with get, set
            abstract label: string with get, set
            abstract generated: option<bool> with get, set

    module StreamScopedDownloads =
        type Generate =
            [<EmitProperty("default")>]
            abstract ``default``: option<StreamDownload> with get, set

            abstract audio: option<StreamDownload> with get, set

    module StreamVideo =
        type Input =
            abstract height: float with get, set
            abstract width: float with get, set

        type Status =
            abstract errorReasonText: string with get, set
            abstract errorReasonCode: string with get, set
            abstract pctComplete: option<string> with get, set
            abstract step: option<string> with get, set
            abstract state: string with get, set

        type PublicDetails =
            abstract logo: option<string> with get, set

            [<EmitProperty("channel_link")>]
            abstract channelLink: option<string> with get, set

            [<EmitProperty("share_link")>]
            abstract shareLink: option<string> with get, set

            abstract title: option<string> with get, set

    module StreamVideoHandle =
        module Update =
            type Params =
                abstract thumbnailTimestampPct: option<float> with get, set
                abstract scheduledDeletion: option<string> with get, set
                abstract requireSignedURLs: option<bool> with get, set
                abstract meta: option<obj> with get, set
                abstract maxDurationSeconds: option<float> with get, set
                abstract creator: option<string> with get, set
                abstract allowedOrigins: option<ResizeArray<string>> with get, set

    module StreamVideos =
        module List =
            type Params =
                abstract afterComp: option<StreamPaginationComparison> with get, set
                abstract after: option<string> with get, set
                abstract beforeComp: option<StreamPaginationComparison> with get, set
                abstract before: option<string> with get, set
                abstract limit: option<float> with get, set

    module StreamWatermarks =
        module Generate =
            type Params =
                abstract position: option<StreamWatermarkPosition> with get, set
                abstract scale: option<float> with get, set
                abstract padding: option<float> with get, set
                abstract opacity: option<float> with get, set
                abstract name: option<string> with get, set

    module TailStream =
        [<Import("@cloudflare/workers-types.TailStream", "HibernatableWebSocketEventInfoMessage")>]
        type HibernatableWebSocketEventInfoMessage =
            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "TracePreviewInfo")>]
        type TracePreviewInfo =
            abstract name: string with get
            abstract slug: string with get
            abstract id: string with get

        [<Import("@cloudflare/workers-types.TailStream", "SpanClose")>]
        type SpanClose =
            abstract outcome: EventOutcome with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "ScriptVersion")>]
        type ScriptVersion =
            abstract message: option<string> with get
            abstract tag: option<string> with get
            abstract id: string with get

        [<Import("@cloudflare/workers-types.TailStream", "ScheduledEventInfo")>]
        type ScheduledEventInfo =
            abstract cron: string with get
            abstract scheduledTime: Date with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "DroppedEventsDiagnostic")>]
        type DroppedEventsDiagnostic =
            abstract count: float with get
            abstract diagnosticsType: string with get

        [<Import("@cloudflare/workers-types.TailStream", "FetchResponseInfo")>]
        type FetchResponseInfo =
            abstract statusCode: float with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type EventOutcome =
            | [<CompiledName("exception")>] Exception
            | [<CompiledName("ok")>] Ok
            | [<CompiledName("canceled")>] Canceled
            | [<CompiledName("unknown")>] Unknown
            | [<CompiledName("killSwitch")>] KillSwitch
            | [<CompiledName("daemonDown")>] DaemonDown
            | [<CompiledName("exceededCpu")>] ExceededCpu
            | [<CompiledName("exceededMemory")>] ExceededMemory
            | [<CompiledName("loadShed")>] LoadShed
            | [<CompiledName("responseStreamDisconnected")>] ResponseStreamDisconnected
            | [<CompiledName("scriptNotFound")>] ScriptNotFound
            | [<CompiledName("internalError")>] InternalError
            | [<CompiledName("exceededWallTime")>] ExceededWallTime

        type EventType = U10<Onset, Outcome, SpanOpen, SpanClose, DiagnosticChannelEvent, Exception, Log, StreamDiagnostic, Return, Attributes>

        [<Import("@cloudflare/workers-types.TailStream", "JsRpcEventInfo")>]
        type JsRpcEventInfo =
            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "DiagnosticChannelEvent")>]
        type DiagnosticChannelEvent =
            abstract message: option<obj> with get
            abstract channel: string with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Exception")>]
        type Exception =
            abstract stack: option<string> with get
            abstract message: string with get
            abstract name: string with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Log")>]
        type Log =
            abstract message: obj with get
            abstract level: LiteralUnions.DebugErrorInfoLogWarn with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "AlarmEventInfo")>]
        type AlarmEventInfo =
            abstract scheduledTime: Date with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "QueueEventInfo")>]
        type QueueEventInfo =
            abstract batchSize: float with get
            abstract queueName: string with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        type TailEventHandlerType = U2<TailEvent<obj> -> option<Promise<unit>>, TailEventHandlerObject>

        [<Import("@cloudflare/workers-types.TailStream", "TailEventHandlerObject")>]
        type TailEventHandlerObject =
            abstract attributes: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set

            [<EmitProperty("return")>]
            abstract ``return``: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set

            abstract log: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set

            [<EmitProperty("exception")>]
            abstract ``exception``: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set

            abstract diagnosticChannel: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set
            abstract spanClose: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set
            abstract spanOpen: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set
            abstract outcome: (option<TailEvent<obj> -> option<Promise<unit>>>) with get, set

        [<Import("@cloudflare/workers-types.TailStream", "Return")>]
        type Return =
            abstract info: option<FetchResponseInfo> with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "CustomEventInfo")>]
        type CustomEventInfo =
            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "StreamDiagnostic")>]
        type StreamDiagnostic =
            abstract diagnostic: DroppedEventsDiagnostic with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "HibernatableWebSocketEventInfo")>]
        type HibernatableWebSocketEventInfo =
            abstract info: U3<HibernatableWebSocketEventInfoClose, HibernatableWebSocketEventInfoError, HibernatableWebSocketEventInfoMessage> with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Attribute")>]
        type Attribute =
            abstract value: U6<ResizeArray<string>, ResizeArray<bool>, AiSentenceSimilarityOutput, string, bool, float> with get
            abstract name: string with get

        [<Import("@cloudflare/workers-types.TailStream", "HibernatableWebSocketEventInfoError")>]
        type HibernatableWebSocketEventInfoError =
            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "TailEvent")>]
        type TailEvent<'Event> =
            abstract event: 'Event with get
            abstract sequence: float with get
            abstract timestamp: Date with get
            abstract spanContext: SpanContext with get
            abstract invocationId: string with get

        [<Import("@cloudflare/workers-types.TailStream", "TraceEventInfo")>]
        type TraceEventInfo =
            abstract traces: ResizeArray<option<string>> with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "EmailEventInfo")>]
        type EmailEventInfo =
            abstract rawSize: float with get
            abstract rcptTo: string with get
            abstract mailFrom: string with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Onset")>]
        type Onset =
            abstract info: U10<FetchEventInfo, ConnectEventInfo, JsRpcEventInfo, ScheduledEventInfo, AlarmEventInfo, QueueEventInfo, EmailEventInfo, TraceEventInfo, HibernatableWebSocketEventInfo, CustomEventInfo> with get
            abstract preview: option<TracePreviewInfo> with get
            abstract scriptVersion: option<ScriptVersion> with get
            abstract scriptTags: option<ResizeArray<string>> with get
            abstract scriptName: option<string> with get
            abstract executionModel: string with get
            abstract entrypoint: option<string> with get
            abstract dispatchNamespace: option<string> with get
            abstract spanId: string with get
            abstract attributes: ResizeArray<Attribute> with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "SpanOpen")>]
        type SpanOpen =
            abstract info: option<U3<FetchEventInfo, JsRpcEventInfo, Attributes>> with get
            abstract spanId: string with get
            abstract name: string with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Outcome")>]
        type Outcome =
            abstract wallTime: float with get
            abstract cpuTime: float with get
            abstract outcome: EventOutcome with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "SpanContext")>]
        type SpanContext =
            abstract traceFlags: option<float> with get
            abstract spanId: option<string> with get
            abstract traceId: string with get

        [<Import("@cloudflare/workers-types.TailStream", "ConnectEventInfo")>]
        type ConnectEventInfo =
            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Attributes")>]
        type Attributes =
            abstract info: ResizeArray<Attribute> with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "HibernatableWebSocketEventInfoClose")>]
        type HibernatableWebSocketEventInfoClose =
            abstract wasClean: bool with get
            abstract code: float with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        [<Import("@cloudflare/workers-types.TailStream", "Header")>]
        type Header =
            abstract value: string with get
            abstract name: string with get

        [<Import("@cloudflare/workers-types.TailStream", "FetchEventInfo")>]
        type FetchEventInfo =
            abstract headers: ResizeArray<Header> with get
            abstract cfJson: option<obj> with get
            abstract url: string with get
            abstract method: string with get

            [<EmitProperty("type")>]
            abstract ``type``: string with get

        type TailEventHandler = TailEvent<obj> -> option<Promise<unit>>

    module ToMarkdownService =
        type Supported =
            abstract extension: string with get, set
            abstract mimeType: string with get, set

    module Tracing =
        type Span =
            abstract prototype: Span with get, set
            abstract Create: unit -> Span

    module VectorizeMatches =
        type Matches =
            abstract score: float with get, set

            [<EmitProperty("namespace")>]
            abstract ``namespace``: option<string> with get, set

            abstract metadata: option<obj> with get, set
            abstract id: string with get, set
            abstract values: option<U3<AiSentenceSimilarityOutput, Float32Array, Float64Array>> with get, set

    module WebAssembly =
        [<Import("@cloudflare/workers-types.WebAssembly", "Module")>]
        type Module =
            abstract customSections: ``module``: Module * sectionName: string -> ResizeArray<ArrayBuffer>
            abstract exports: ``module``: Module -> ResizeArray<ModuleExportDescriptor>
            abstract imports: ``module``: Module -> ResizeArray<ModuleImportDescriptor>

        [<Import("@cloudflare/workers-types.WebAssembly", "GlobalDescriptor")>]
        type GlobalDescriptor =
            [<EmitProperty("mutable")>]
            abstract ``mutable``: option<bool> with get, set

            abstract value: ValueType with get, set

        [<Import("@cloudflare/workers-types.WebAssembly", "Instance")>]
        type Instance =
            [<EmitConstructor>]
            abstract Create: ``module``: Module * ?imports: Imports -> Instance

            abstract exports: Exports with get

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type ValueType =
            | [<CompiledName("anyfunc")>] Anyfunc
            | [<CompiledName("externref")>] Externref
            | [<CompiledName("f32")>] F32
            | [<CompiledName("f64")>] F64
            | [<CompiledName("i32")>] I32
            | [<CompiledName("i64")>] I64
            | [<CompiledName("v128")>] V128

        [<Import("@cloudflare/workers-types.WebAssembly", "Memory")>]
        type Memory =
            [<EmitConstructor>]
            abstract Create: descriptor: MemoryDescriptor -> Memory

            abstract buffer: ArrayBuffer with get
            abstract grow: delta: float -> float

        [<Import("@cloudflare/workers-types.WebAssembly", "CompileError")>]
        type CompileError =
            interface
                [<EmitConstructor>]
                abstract Create: ?message: string -> CompileError
            end

        [<Import("@cloudflare/workers-types.WebAssembly", "ModuleImportDescriptor")>]
        type ModuleImportDescriptor =
            abstract name: string with get, set

            [<EmitProperty("module")>]
            abstract ``module``: string with get, set

            abstract kind: ImportExportKind with get, set

        [<Import("@cloudflare/workers-types.WebAssembly", "Table")>]
        type Table =
            [<EmitConstructor>]
            abstract Create: descriptor: TableDescriptor * ?value: obj -> Table

            abstract length: float with get
            abstract get: index: float -> option<obj>
            abstract grow: delta: float * ?value: obj -> float
            abstract set: index: float * ?value: obj -> unit

        type ImportValue = U5<obj, Global, Memory, Table, float>

        [<Import("@cloudflare/workers-types.WebAssembly", "Exports")>]
        type Exports = interface end

        [<Import("@cloudflare/workers-types.WebAssembly", "Imports")>]
        type Imports = interface end

        [<Import("@cloudflare/workers-types.WebAssembly", "ModuleExportDescriptor")>]
        type ModuleExportDescriptor =
            abstract name: string with get, set
            abstract kind: ImportExportKind with get, set

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type ImportExportKind =
            | [<CompiledName("function")>] Function
            | [<CompiledName("global")>] Global
            | [<CompiledName("memory")>] Memory
            | [<CompiledName("table")>] Table

        [<Import("@cloudflare/workers-types.WebAssembly", "MemoryDescriptor")>]
        type MemoryDescriptor =
            abstract shared: option<bool> with get, set
            abstract maximum: option<float> with get, set
            abstract initial: float with get, set

        [<Import("@cloudflare/workers-types.WebAssembly", "ModuleImports")>]
        type ModuleImports = interface end

        [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
        type TableKind =
            | [<CompiledName("anyfunc")>] Anyfunc
            | [<CompiledName("externref")>] Externref

        [<Import("@cloudflare/workers-types.WebAssembly", "Global")>]
        type Global =
            [<EmitConstructor>]
            abstract Create: descriptor: GlobalDescriptor * ?value: obj -> Global

            abstract value: option<obj> with get, set
            abstract valueOf: unit -> option<obj>

        [<Import("@cloudflare/workers-types.WebAssembly", "RuntimeError")>]
        type RuntimeError =
            interface
                [<EmitConstructor>]
                abstract Create: ?message: string -> RuntimeError
            end

        [<Import("@cloudflare/workers-types.WebAssembly", "TableDescriptor")>]
        type TableDescriptor =
            abstract maximum: option<float> with get, set
            abstract initial: float with get, set
            abstract element: TableKind with get, set

        type ExportValue = U4<obj, Global, Memory, Table>

    module WebSearch =
        type Search =
            abstract metadata: WebSearchResponseMetadata with get, set
            abstract items: ResizeArray<WebSearchResult> with get, set

        module Search =
            type Options =
                abstract limit: option<float> with get, set
                abstract query: string with get, set

    module WorkerGlobalScope =
        type EventTarget =
            abstract prototype: EventTarget<option<obj>> with get, set
            abstract Create: unit -> EventTarget<obj>

    module WorkerStub =
        type GetEntrypoint =
            abstract Invoke: [<ParamArray>] args: Rpc.UnstubifyAll<obj> -> option<U2<obj, obj>>
            abstract ``then``: ?onfulfilled: (obj -> U2<obj, Promise<obj>>) * ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract catch: ?onrejected: (option<obj> -> U2<obj, Promise<obj>>) -> Promise<U2<obj, obj>>
            abstract Item: key: string -> option<obj>
            abstract fetch: input: U3<Request<option<obj>, U2<RequestInitCfProperties, obj>>, URL, string> * ?init: RequestInit<U2<RequestInitCfProperties, obj>> -> Promise<Response>
            abstract connect: address: U2<SocketAddress, string> * ?options: SocketOptions -> Socket

    module WorkflowInstance =
        module SendEvent =
            type typepayload =
                abstract payload: option<obj> with get, set

                [<EmitProperty("type")>]
                abstract ``type``: string with get, set

    module WorkflowInstanceCreateOptions =
        type Retention =
            abstract errorRetention: option<WorkflowRetentionDuration> with get, set
            abstract successRetention: option<WorkflowRetentionDuration> with get, set

    module WorkflowInstanceRestartOptions =
        type From =
            [<EmitProperty("type")>]
            abstract ``type``: option<LiteralUnions.DoSleepWaitForEvent> with get, set

            abstract count: option<float> with get, set
            abstract name: string with get, set

    module WorkflowRetentionDuration =
        type Case5 = string
        type Case6 = string
        type Case7 = string
        type Case0 = string
        type Case4 = string
        type Case3 = string
        type Case2 = string
        type Case8 = string
        type Case9 = string
        type Case10 = string
        type Case11 = string
        type Case1 = string
        type Case12 = string
        type Case13 = string

    module XOR =
        type Case2 =
            abstract Item: key: string -> option<obj>

    module ``Cloudflare:node`` =
        [<Import("@cloudflare/workers-types.cloudflare:node", "NodeStyleServer")>]
        type NodeStyleServer =
            abstract listen: [<ParamArray>] args: ResizeArray<option<obj>> -> obj
            abstract address: unit -> NodeStyleServer.Address

        type HttpServerHandler =
            abstract Invoke: port: float -> ExportedHandler<option<obj>, option<obj>, option<obj>, option<obj>>
            abstract Invoke: options: SharedLiterals.Port -> ExportedHandler<option<obj>, option<obj>, option<obj>, option<obj>>
            abstract Invoke: server: NodeStyleServer -> ExportedHandler<option<obj>, option<obj>, option<obj>, option<obj>>

        module NodeStyleServer =
            type Address =
                abstract port: option<float> with get, set

    module ``Cloudflare:pipelines`` =
        [<Import("@cloudflare/workers-types.cloudflare:pipelines", "PipelineBatchMetadata")>]
        type PipelineBatchMetadata =
            abstract pipelineName: string with get, set
            abstract pipelineId: string with get, set

        [<Import("@cloudflare/workers-types.cloudflare:pipelines", "PipelineTransformationEntrypoint")>]
        type PipelineTransformationEntrypoint<'Env, 'I, 'O> =
            [<EmitConstructor>]
            abstract Create: ctx: ExecutionContext<option<obj>> * env: 'Env -> PipelineTransformationEntrypoint<'Env, 'I, 'O>

            abstract ctx: ExecutionContext<option<obj>> with get, set
            abstract env: 'Env with get, set
            abstract run: records: ResizeArray<'I> * metadata: PipelineBatchMetadata -> Promise<ResizeArray<'O>>

        [<Import("@cloudflare/workers-types.cloudflare:pipelines", "Pipeline")>]
        type Pipeline<'T> =
            abstract send: records: ResizeArray<'T> -> Promise<unit>

        [<Import("@cloudflare/workers-types.cloudflare:pipelines", "PipelineRecord")>]
        type PipelineRecord = interface end

        module PipelineTransformationEntrypoint =
            module Run =
                type Metadata =
                    abstract pipelineName: string with get, set
                    abstract pipelineId: string with get, set

    module ``Cloudflare:workflows`` =
        [<Import("@cloudflare/workers-types.cloudflare:workflows", "NonRetryableError")>]
        type NonRetryableError =
            interface
                [<EmitConstructor>]
                abstract Create: message: string * ?name: string -> NonRetryableError
            end
