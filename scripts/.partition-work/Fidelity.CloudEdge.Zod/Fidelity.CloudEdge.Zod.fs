namespace rec Fidelity.CloudEdge

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Xantham.FableCore.Extensions

type Disposable =
    abstract ``[symbol.dispose]``: unit -> unit

module LiteralUnions =
    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Base64Base64urlCidrv443a9850b =
        | [<CompiledName("email")>] Email
        | [<CompiledName("includes")>] Includes
        | [<CompiledName("base64")>] Base64
        | [<CompiledName("emoji")>] Emoji
        | [<CompiledName("url")>] Url
        | [<CompiledName("uuid")>] Uuid
        | [<CompiledName("guid")>] Guid
        | [<CompiledName("nanoid")>] Nanoid
        | [<CompiledName("cuid")>] Cuid
        | [<CompiledName("cuid2")>] Cuid2
        | [<CompiledName("ulid")>] Ulid
        | [<CompiledName("xid")>] Xid
        | [<CompiledName("ksuid")>] Ksuid
        | [<CompiledName("datetime")>] Datetime
        | [<CompiledName("date")>] Date
        | [<CompiledName("time")>] Time
        | [<CompiledName("duration")>] Duration
        | [<CompiledName("ipv4")>] Ipv4
        | [<CompiledName("ipv6")>] Ipv6
        | [<CompiledName("cidrv4")>] Cidrv4
        | [<CompiledName("cidrv6")>] Cidrv6
        | [<CompiledName("base64url")>] Base64url
        | [<CompiledName("json_string")>] JsonString
        | [<CompiledName("e164")>] E164
        | [<CompiledName("lowercase")>] Lowercase
        | [<CompiledName("uppercase")>] Uppercase
        | [<CompiledName("regex")>] Regex
        | [<CompiledName("jwt")>] Jwt
        | [<CompiledName("starts_with")>] StartsWith
        | [<CompiledName("ends_with")>] EndsWith

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArrayBooleanIntegerEbaf6c12 =
        | [<CompiledName("boolean")>] Boolean
        | [<CompiledName("null")>] Null
        | [<CompiledName("number")>] Number
        | [<CompiledName("integer")>] Integer
        | [<CompiledName("string")>] String
        | [<CompiledName("array")>] Array
        | [<CompiledName("object")>] Object

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type HighLowMediumHighMediumLow =
        | [<CompiledName("low")>] Low
        | [<CompiledName("medium-low")>] MediumLow
        | [<CompiledName("medium-high")>] MediumHigh
        | [<CompiledName("high")>] High

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoNoneRequired =
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("required")>] Required
        | [<CompiledName("none")>] None

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AgentToolChannelChatCcd57fcc =
        | [<CompiledName("message")>] Message
        | [<CompiledName("email")>] Email
        | [<CompiledName("state")>] State
        | [<CompiledName("rpc")>] Rpc
        | [<CompiledName("chat")>] Chat
        | [<CompiledName("transcript")>] Transcript
        | [<CompiledName("fiber")>] Fiber
        | [<CompiledName("agentTool")>] AgentTool
        | [<CompiledName("schedule")>] Schedule
        | [<CompiledName("lifecycle")>] Lifecycle
        | [<CompiledName("workflow")>] Workflow
        | [<CompiledName("mcp")>] Mcp
        | [<CompiledName("channel")>] Channel

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AF_AN_AS_EU_NA_OC_SA =
        | AF
        | AN
        | AS
        | EU
        | NA
        | OC
        | SA

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AtleastoneMany =
        | [<CompiledName("many")>] Many
        | [<CompiledName("atleastone")>] Atleastone

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type XY =
        | [<CompiledName("x")>] X
        | [<CompiledName("y")>] Y

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AmaltheaAndromedaApollo7e3ce24d =
        | [<CompiledName("amalthea")>] Amalthea
        | [<CompiledName("andromeda")>] Andromeda
        | [<CompiledName("apollo")>] Apollo
        | [<CompiledName("arcas")>] Arcas
        | [<CompiledName("aries")>] Aries
        | [<CompiledName("asteria")>] Asteria
        | [<CompiledName("athena")>] Athena
        | [<CompiledName("atlas")>] Atlas
        | [<CompiledName("aurora")>] Aurora
        | [<CompiledName("callista")>] Callista
        | [<CompiledName("cora")>] Cora
        | [<CompiledName("cordelia")>] Cordelia
        | [<CompiledName("delia")>] Delia
        | [<CompiledName("draco")>] Draco
        | [<CompiledName("electra")>] Electra
        | [<CompiledName("harmonia")>] Harmonia
        | [<CompiledName("helena")>] Helena
        | [<CompiledName("hera")>] Hera
        | [<CompiledName("hermes")>] Hermes
        | [<CompiledName("hyperion")>] Hyperion
        | [<CompiledName("iris")>] Iris
        | [<CompiledName("janus")>] Janus
        | [<CompiledName("juno")>] Juno
        | [<CompiledName("jupiter")>] Jupiter
        | [<CompiledName("luna")>] Luna
        | [<CompiledName("mars")>] Mars
        | [<CompiledName("minerva")>] Minerva
        | [<CompiledName("neptune")>] Neptune
        | [<CompiledName("odysseus")>] Odysseus
        | [<CompiledName("ophelia")>] Ophelia
        | [<CompiledName("orion")>] Orion
        | [<CompiledName("orpheus")>] Orpheus
        | [<CompiledName("pandora")>] Pandora
        | [<CompiledName("phoebe")>] Phoebe
        | [<CompiledName("pluto")>] Pluto
        | [<CompiledName("saturn")>] Saturn
        | [<CompiledName("thalia")>] Thalia
        | [<CompiledName("theia")>] Theia
        | [<CompiledName("vesta")>] Vesta
        | [<CompiledName("zeus")>] Zeus

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LongMediumShort =
        | [<CompiledName("medium")>] Medium
        | [<CompiledName("short")>] Short
        | [<CompiledName("long")>] Long

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletedErrorOutdated22d6dfc3 =
        | [<CompiledName("queued")>] Queued
        | [<CompiledName("running")>] Running
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("error")>] Error
        | [<CompiledName("skipped")>] Skipped
        | [<CompiledName("outdated")>] Outdated

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContainCoverCrop8bc042f3 =
        | [<CompiledName("scale-down")>] ScaleDown
        | [<CompiledName("contain")>] Contain
        | [<CompiledName("pad")>] Pad
        | [<CompiledName("squeeze")>] Squeeze
        | [<CompiledName("cover")>] Cover
        | [<CompiledName("crop")>] Crop

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DoneRecur =
        | [<CompiledName("done")>] Done
        | [<CompiledName("recur")>] Recur

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ReadWrite =
        | [<CompiledName("read")>] Read
        | [<CompiledName("write")>] Write

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArrayBigintBoolean607d1ffa =
        | [<CompiledName("string")>] String
        | [<CompiledName("number")>] Number
        | [<CompiledName("bigint")>] Bigint
        | [<CompiledName("boolean")>] Boolean
        | [<CompiledName("symbol")>] Symbol
        | [<CompiledName("undefined")>] Undefined
        | [<CompiledName("object")>] Object
        | [<CompiledName("function")>] Function
        | [<CompiledName("unknown")>] Unknown
        | [<CompiledName("integer")>] Integer
        | [<CompiledName("array")>] Array
        | [<CompiledName("null")>] Null
        | [<CompiledName("map")>] Map
        | [<CompiledName("set")>] Set
        | [<CompiledName("date")>] Date
        | [<CompiledName("never")>] Never
        | [<CompiledName("void")>] Void
        | [<CompiledName("nan")>] Nan
        | [<CompiledName("promise")>] Promise
        | [<CompiledName("float")>] Float

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type RpcSseStreamableHttp =
        | [<CompiledName("sse")>] Sse
        | [<CompiledName("streamable-http")>] StreamableHttp
        | [<CompiledName("rpc")>] Rpc

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ProactiveReactive =
        | [<CompiledName("proactive")>] Proactive
        | [<CompiledName("reactive")>] Reactive

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AssistantUser =
        | [<CompiledName("user")>] User
        | [<CompiledName("assistant")>] Assistant

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AfrApacApacNe83359f17 =
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

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Float32Float64Uint8 =
        | [<CompiledName("uint8")>] Uint8
        | [<CompiledName("float32")>] Float32
        | [<CompiledName("float64")>] Float64

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AngusArcasAsteriaB111950c =
        | [<CompiledName("angus")>] Angus
        | [<CompiledName("asteria")>] Asteria
        | [<CompiledName("arcas")>] Arcas
        | [<CompiledName("orion")>] Orion
        | [<CompiledName("orpheus")>] Orpheus
        | [<CompiledName("athena")>] Athena
        | [<CompiledName("luna")>] Luna
        | [<CompiledName("zeus")>] Zeus
        | [<CompiledName("perseus")>] Perseus
        | [<CompiledName("helios")>] Helios
        | [<CompiledName("hera")>] Hera
        | [<CompiledName("stella")>] Stella

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArrayBigintDateNumberSetString =
        | [<CompiledName("array")>] Array
        | [<CompiledName("string")>] String
        | [<CompiledName("number")>] Number
        | [<CompiledName("set")>] Set
        | [<CompiledName("date")>] Date
        | [<CompiledName("bigint")>] Bigint

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ErrorFollowManual =
        | [<CompiledName("error")>] Error
        | [<CompiledName("follow")>] Follow
        | [<CompiledName("manual")>] Manual

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContainCoverScaleDown =
        | [<CompiledName("contain")>] Contain
        | [<CompiledName("cover")>] Cover
        | [<CompiledName("scale-down")>] ScaleDown

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompleteErroredPaused4918918f =
        | [<CompiledName("running")>] Running
        | [<CompiledName("paused")>] Paused
        | [<CompiledName("unknown")>] Unknown
        | [<CompiledName("terminated")>] Terminated
        | [<CompiledName("queued")>] Queued
        | [<CompiledName("errored")>] Errored
        | [<CompiledName("complete")>] Complete
        | [<CompiledName("waiting")>] Waiting
        | [<CompiledName("waitingForPause")>] WaitingForPause

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoHighLow =
        | [<CompiledName("low")>] Low
        | [<CompiledName("high")>] High
        | [<CompiledName("auto")>] Auto

    type I14400I21600I3600I43200I7200I86400 =
        | ``3600`` = 3600
        | ``7200`` = 7200
        | ``14400`` = 14400
        | ``21600`` = 21600
        | ``43200`` = 43200
        | ``86400`` = 86400

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArrayBigintBooleanDddb3709 =
        | [<CompiledName("string")>] String
        | [<CompiledName("number")>] Number
        | [<CompiledName("bigint")>] Bigint
        | [<CompiledName("boolean")>] Boolean
        | [<CompiledName("symbol")>] Symbol
        | [<CompiledName("undefined")>] Undefined
        | [<CompiledName("object")>] Object
        | [<CompiledName("function")>] Function
        | [<CompiledName("array")>] Array
        | [<CompiledName("null")>] Null
        | [<CompiledName("map")>] Map
        | [<CompiledName("set")>] Set
        | [<CompiledName("record")>] Record
        | [<CompiledName("date")>] Date
        | [<CompiledName("int")>] Int
        | [<CompiledName("file")>] File
        | [<CompiledName("never")>] Never
        | [<CompiledName("void")>] Void
        | [<CompiledName("tuple")>] Tuple
        | [<CompiledName("nonoptional")>] Nonoptional
        | [<CompiledName("nan")>] Nan

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AlvaroAquilaCarina8376d988 =
        | [<CompiledName("sirio")>] Sirio
        | [<CompiledName("nestor")>] Nestor
        | [<CompiledName("carina")>] Carina
        | [<CompiledName("celeste")>] Celeste
        | [<CompiledName("alvaro")>] Alvaro
        | [<CompiledName("diana")>] Diana
        | [<CompiledName("aquila")>] Aquila
        | [<CompiledName("selena")>] Selena
        | [<CompiledName("estrella")>] Estrella
        | [<CompiledName("javier")>] Javier

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BoxCenterRemainder =
        | [<CompiledName("remainder")>] Remainder
        | [<CompiledName("box-center")>] BoxCenter

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BypassNormalizePassthrough =
        | [<CompiledName("normalize")>] Normalize
        | [<CompiledName("passthrough")>] Passthrough
        | [<CompiledName("bypass")>] Bypass

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type V1V2V3V4V5V6V7V8 =
        | [<CompiledName("v6")>] V6
        | [<CompiledName("v4")>] V4
        | [<CompiledName("v1")>] V1
        | [<CompiledName("v2")>] V2
        | [<CompiledName("v3")>] V3
        | [<CompiledName("v5")>] V5
        | [<CompiledName("v7")>] V7
        | [<CompiledName("v8")>] V8

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BooleanDatetimeNumberText =
        | [<CompiledName("text")>] Text
        | [<CompiledName("number")>] Number
        | [<CompiledName("boolean")>] Boolean
        | [<CompiledName("datetime")>] Datetime

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompleteErrorPendingRunning =
        | [<CompiledName("pending")>] Pending
        | [<CompiledName("running")>] Running
        | [<CompiledName("complete")>] Complete
        | [<CompiledName("error")>] Error

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MergeResetSet =
        | [<CompiledName("set")>] Set
        | [<CompiledName("merge")>] Merge
        | [<CompiledName("reset")>] Reset

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ``01`` =
        | [<CompiledName("1")>] ``1``
        | [<CompiledName("0")>] ``0``

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AssistantDeveloperSystemUser =
        | [<CompiledName("user")>] User
        | [<CompiledName("assistant")>] Assistant
        | [<CompiledName("system")>] System
        | [<CompiledName("developer")>] Developer

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DayHourMinuteMonthSecondWeekYear =
        | [<CompiledName("second")>] Second
        | [<CompiledName("minute")>] Minute
        | [<CompiledName("hour")>] Hour
        | [<CompiledName("day")>] Day
        | [<CompiledName("week")>] Week
        | [<CompiledName("month")>] Month
        | [<CompiledName("year")>] Year

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AllServersNoneThisServer =
        | [<CompiledName("none")>] None
        | [<CompiledName("thisServer")>] ThisServer
        | [<CompiledName("allServers")>] AllServers

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletedInProgressIncomplete =
        | [<CompiledName("in_progress")>] InProgress
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("incomplete")>] Incomplete

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BytesJsonTextV8 =
        | [<CompiledName("json")>] Json
        | [<CompiledName("text")>] Text
        | [<CompiledName("v8")>] V8
        | [<CompiledName("bytes")>] Bytes

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Base64Binary =
        | [<CompiledName("binary")>] Binary
        | [<CompiledName("base64")>] Base64

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContentFilterMaxOutputTokens =
        | [<CompiledName("max_output_tokens")>] MaxOutputTokens
        | [<CompiledName("content_filter")>] ContentFilter

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MapRecord =
        | [<CompiledName("map")>] Map
        | [<CompiledName("record")>] Record

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type PassthroughStrictStrip =
        | [<CompiledName("strict")>] Strict
        | [<CompiledName("passthrough")>] Passthrough
        | [<CompiledName("strip")>] Strip

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ALREADYEXISTSFORK99eced21 =
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

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AuthenticatingConnectedConF3c65ddc =
        | [<CompiledName("failed")>] Failed
        | [<CompiledName("authenticating")>] Authenticating
        | [<CompiledName("connecting")>] Connecting
        | [<CompiledName("connected")>] Connected
        | [<CompiledName("discovering")>] Discovering
        | [<CompiledName("ready")>] Ready

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AUTHORIZED_REDIRECT =
        | AUTHORIZED
        | REDIRECT

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AppliedErrorExecutingPendingReverted =
        | [<CompiledName("executing")>] Executing
        | [<CompiledName("applied")>] Applied
        | [<CompiledName("pending")>] Pending
        | [<CompiledName("reverted")>] Reverted
        | [<CompiledName("error")>] Error

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DateDateTimeEmailUri =
        | [<CompiledName("email")>] Email
        | [<CompiledName("uri")>] Uri
        | [<CompiledName("date")>] Date
        | [<CompiledName("date-time")>] DateTime

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type HybridKeywordVector =
        | [<CompiledName("vector")>] Vector
        | [<CompiledName("keyword")>] Keyword
        | [<CompiledName("hybrid")>] Hybrid

    type I0I1I2I3I4I5 =
        | ``0`` = 0
        | ``1`` = 1
        | ``2`` = 2
        | ``3`` = 3
        | ``4`` = 4
        | ``5`` = 5

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type InlineRef =
        | [<CompiledName("ref")>] Ref
        | [<CompiledName("inline")>] Inline

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ADAEAFAda77976 =
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
        | T1

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AnyfuncExternrefF32F64I32I64V128 =
        | [<CompiledName("anyfunc")>] Anyfunc
        | [<CompiledName("externref")>] Externref
        | [<CompiledName("f32")>] F32
        | [<CompiledName("f64")>] F64
        | [<CompiledName("i32")>] I32
        | [<CompiledName("i64")>] I64
        | [<CompiledName("v128")>] V128

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CancelledCompletedFailedAee391f7 =
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("failed")>] Failed
        | [<CompiledName("working")>] Working
        | [<CompiledName("input_required")>] InputRequired
        | [<CompiledName("cancelled")>] Cancelled

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ConstantExponentialLinear =
        | [<CompiledName("constant")>] Constant
        | [<CompiledName("linear")>] Linear
        | [<CompiledName("exponential")>] Exponential

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AssistantDeveloperSystemToolUser =
        | [<CompiledName("system")>] System
        | [<CompiledName("developer")>] Developer
        | [<CompiledName("user")>] User
        | [<CompiledName("assistant")>] Assistant
        | [<CompiledName("tool")>] Tool

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AudioText =
        | [<CompiledName("text")>] Text
        | [<CompiledName("audio")>] Audio

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DeveloperSystemUser =
        | [<CompiledName("user")>] User
        | [<CompiledName("system")>] System
        | [<CompiledName("developer")>] Developer

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FetchQueueRejectionhandled4a6d4926 =
        | [<CompiledName("fetch")>] Fetch
        | [<CompiledName("queue")>] Queue
        | [<CompiledName("scheduled")>] Scheduled
        | [<CompiledName("unhandledrejection")>] Unhandledrejection
        | [<CompiledName("rejectionhandled")>] Rejectionhandled

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AnyThrow =
        | [<CompiledName("throw")>] Throw
        | [<CompiledName("any")>] Any

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DynamicOneShotReuse =
        | [<CompiledName("one-shot")>] OneShot
        | [<CompiledName("reuse")>] Reuse
        | [<CompiledName("dynamic")>] Dynamic

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type NarrateReact =
        | [<CompiledName("react")>] React
        | [<CompiledName("narrate")>] Narrate

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type EuFedrampFedrampHigh =
        | [<CompiledName("eu")>] Eu
        | [<CompiledName("fedramp")>] Fedramp
        | [<CompiledName("fedramp-high")>] FedrampHigh

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoRequired =
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("required")>] Required

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LosslessLossyOff =
        | [<CompiledName("lossy")>] Lossy
        | [<CompiledName("lossless")>] Lossless
        | [<CompiledName("off")>] Off

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoConciseDetailed =
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("concise")>] Concise
        | [<CompiledName("detailed")>] Detailed

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContentEncodingContentMediaCe2498ed =
        | [<CompiledName("contentEncoding")>] ContentEncoding
        | [<CompiledName("contentMediaType")>] ContentMediaType
        | [<CompiledName("contentSchema")>] ContentSchema
        | [<CompiledName("maxLength")>] MaxLength
        | [<CompiledName("minLength")>] MinLength
        | [<CompiledName("pattern")>] Pattern

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type HighLowMedium =
        | [<CompiledName("low")>] Low
        | [<CompiledName("medium")>] Medium
        | [<CompiledName("high")>] High
        | [<CompiledName("Low")>] Low2
        | [<CompiledName("Medium")>] Medium2
        | [<CompiledName("High")>] High2

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletedErrorRejectedRolledBack =
        | [<CompiledName("error")>] Error
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("rejected")>] Rejected
        | [<CompiledName("rolled_back")>] RolledBack

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AudioDefault =
        | [<CompiledName("default")>] Default
        | [<CompiledName("audio")>] Audio

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FinishGiveUp =
        | [<CompiledName("finish")>] Finish
        | [<CompiledName("give_up")>] GiveUp

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type EqGtGteLtLteNe =
        | [<CompiledName("$eq")>] Eq
        | [<CompiledName("$ne")>] Ne
        | [<CompiledName("$lt")>] Lt
        | [<CompiledName("$lte")>] Lte
        | [<CompiledName("$gt")>] Gt
        | [<CompiledName("$gte")>] Gte
        | [<CompiledName("eq")>] Eq2
        | [<CompiledName("ne")>] Ne2
        | [<CompiledName("gt")>] Gt2
        | [<CompiledName("gte")>] Gte2
        | [<CompiledName("lt")>] Lt2
        | [<CompiledName("lte")>] Lte2

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedCompletedErrorRunningStarting =
        | [<CompiledName("error")>] Error
        | [<CompiledName("running")>] Running
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("starting")>] Starting
        | [<CompiledName("aborted")>] Aborted

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletedErrorPausedEe36275e =
        | [<CompiledName("error")>] Error
        | [<CompiledName("running")>] Running
        | [<CompiledName("paused")>] Paused
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("rejected")>] Rejected
        | [<CompiledName("rolled_back")>] RolledBack

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ``FAILEDFAILED:certificate haA37cab5c`` =
        | FAILED
        | SUCCESS
        | [<CompiledName("FAILED:self signed certificate")>] ``FAILED:self signed certificate``
        | [<CompiledName("FAILED:unable to verify the first certificate")>] ``FAILED:unable to verify the first certificate``
        | [<CompiledName("FAILED:certificate is not yet valid")>] ``FAILED:certificate is not yet valid``
        | [<CompiledName("FAILED:certificate has expired")>] ``FAILED:certificate has expired``

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LarkRegex =
        | [<CompiledName("lark")>] Lark
        | [<CompiledName("regex")>] Regex

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoRpcSseStreamableHttp =
        | [<CompiledName("rpc")>] Rpc
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("sse")>] Sse
        | [<CompiledName("streamable-http")>] StreamableHttp

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CanceledDaemonDownExceededE9a45b4b =
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

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedErrorInterrupted =
        | [<CompiledName("error")>] Error
        | [<CompiledName("interrupted")>] Interrupted
        | [<CompiledName("aborted")>] Aborted

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type HighLowMediumMinimal =
        | [<CompiledName("low")>] Low
        | [<CompiledName("high")>] High
        | [<CompiledName("medium")>] Medium
        | [<CompiledName("minimal")>] Minimal

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CancelledCompletedFailed90b9f981 =
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("failed")>] Failed
        | [<CompiledName("queued")>] Queued
        | [<CompiledName("cancelled")>] Cancelled
        | [<CompiledName("in_progress")>] InProgress
        | [<CompiledName("incomplete")>] Incomplete

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DoSleepWaitForEvent =
        | [<CompiledName("do")>] Do
        | [<CompiledName("sleep")>] Sleep
        | [<CompiledName("waitForEvent")>] WaitForEvent

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AssistantSystemUser =
        | [<CompiledName("system")>] System
        | [<CompiledName("user")>] User
        | [<CompiledName("assistant")>] Assistant

    type I0I180I270I360I90 =
        | ``0`` = 0
        | ``90`` = 90
        | ``180`` = 180
        | ``270`` = 270
        | ``360`` = 360

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AnyfuncExternref =
        | [<CompiledName("anyfunc")>] Anyfunc
        | [<CompiledName("externref")>] Externref

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type NFC_NFD_NFKC_NFKD =
        | NFC
        | NFD
        | NFKC
        | NFKD

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type EqGtGteLtLte =
        | [<CompiledName("eq")>] Eq
        | [<CompiledName("gt")>] Gt
        | [<CompiledName("gte")>] Gte
        | [<CompiledName("lt")>] Lt
        | [<CompiledName("lte")>] Lte

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AdobeFireflyAnthropicAws_7e6a892e =
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

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MethodSnippet =
        | [<CompiledName("method")>] Method
        | [<CompiledName("snippet")>] Snippet

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ADAEAF3a029520 =
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

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContentCrawlJson1ffaf9cf =
        | [<CompiledName("content")>] Content
        | [<CompiledName("screenshot")>] Screenshot
        | [<CompiledName("pdf")>] Pdf
        | [<CompiledName("markdown")>] Markdown
        | [<CompiledName("snapshot")>] Snapshot
        | [<CompiledName("scrape")>] Scrape
        | [<CompiledName("json")>] Json
        | [<CompiledName("links")>] Links
        | [<CompiledName("crawl")>] Crawl

    type I0I180I270I90 =
        | ``0`` = 0
        | ``90`` = 90
        | ``180`` = 180
        | ``270`` = 270

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FunctionGlobalMemoryTable =
        | [<CompiledName("function")>] Function
        | [<CompiledName("global")>] Global
        | [<CompiledName("memory")>] Memory
        | [<CompiledName("table")>] Table

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ImageAvifImageGifImageJ6516ed8b =
        | [<CompiledName("image/jpeg")>] ImageJpeg
        | [<CompiledName("image/png")>] ImagePng
        | [<CompiledName("image/gif")>] ImageGif
        | [<CompiledName("image/webp")>] ImageWebp
        | [<CompiledName("image/avif")>] ImageAvif
        | [<CompiledName("rgb")>] Rgb
        | [<CompiledName("rgba")>] Rgba

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type NoneOggWav =
        | [<CompiledName("none")>] None
        | [<CompiledName("wav")>] Wav
        | [<CompiledName("ogg")>] Ogg

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ClsMean =
        | [<CompiledName("mean")>] Mean
        | [<CompiledName("cls")>] Cls

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type NonSecureSecureUnset =
        | Unset
        | NonSecure
        | Secure

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ``FAILEDFAILED:certificate ha4e9ecc73`` =
        | FAILED
        | SUCCESS
        | NONE
        | [<CompiledName("FAILED:self signed certificate")>] ``FAILED:self signed certificate``
        | [<CompiledName("FAILED:unable to verify the first certificate")>] ``FAILED:unable to verify the first certificate``
        | [<CompiledName("FAILED:certificate is not yet valid")>] ``FAILED:certificate is not yet valid``
        | [<CompiledName("FAILED:certificate has expired")>] ``FAILED:certificate has expired``

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AacAlawFlacLinear16Mp3MulawOpus =
        | [<CompiledName("linear16")>] Linear16
        | [<CompiledName("flac")>] Flac
        | [<CompiledName("mulaw")>] Mulaw
        | [<CompiledName("alaw")>] Alaw
        | [<CompiledName("mp3")>] Mp3
        | [<CompiledName("opus")>] Opus
        | [<CompiledName("aac")>] Aac

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DefaultError =
        | [<CompiledName("default")>] Default
        | [<CompiledName("error")>] Error

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletedFailed =
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("failed")>] Failed

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DELETE_INDEX =
        | INDEX
        | DELETE

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoDisabled =
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("disabled")>] Disabled

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type JpegPngWebp =
        | [<CompiledName("png")>] Png
        | [<CompiledName("webp")>] Webp
        | [<CompiledName("jpeg")>] Jpeg

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ForbiddenOptionalRequired =
        | [<CompiledName("optional")>] Optional
        | [<CompiledName("required")>] Required
        | [<CompiledName("forbidden")>] Forbidden

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ES256ES384ES512Af1463e1 =
        | HS256
        | HS384
        | HS512
        | RS256
        | RS384
        | RS512
        | ES256
        | ES384
        | ES512
        | PS256
        | PS384
        | PS512
        | EdDSA

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MaxRrf =
        | [<CompiledName("max")>] Max
        | [<CompiledName("rrf")>] Rrf

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AllIndexedNone =
        | [<CompiledName("all")>] All
        | [<CompiledName("none")>] None
        | [<CompiledName("indexed")>] Indexed

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoDefaultFlexPriorityScale =
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("default")>] Default
        | [<CompiledName("flex")>] Flex
        | [<CompiledName("scale")>] Scale
        | [<CompiledName("priority")>] Priority

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Draft04Draft07Draft202005379767 =
        | [<CompiledName("draft-04")>] Draft04
        | [<CompiledName("draft-07")>] Draft07
        | [<CompiledName("draft-2020-12")>] Draft202012
        | [<CompiledName("openapi-3.0")>] Openapi30

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LaxNoneStrict =
        | Strict
        | Lax
        | None

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ScheduleUser =
        | [<CompiledName("user")>] User
        | [<CompiledName("schedule")>] Schedule

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type EagerEndOfTurnEndOfTurnSta3f421e18 =
        | Update
        | StartOfTurn
        | EagerEndOfTurn
        | TurnResumed
        | EndOfTurn

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type OauthOidc =
        | [<CompiledName("oauth")>] Oauth
        | [<CompiledName("oidc")>] Oidc

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContainCoverCrop0093f896 =
        | [<CompiledName("scale-down")>] ScaleDown
        | [<CompiledName("scale-up")>] ScaleUp
        | [<CompiledName("contain")>] Contain
        | [<CompiledName("cover")>] Cover
        | [<CompiledName("crop")>] Crop
        | [<CompiledName("pad")>] Pad
        | [<CompiledName("squeeze")>] Squeeze

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DELETE_GET_PATCH_POST_PUT =
        | GET
        | POST
        | PUT
        | PATCH
        | DELETE

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoNone =
        | [<CompiledName("none")>] None
        | [<CompiledName("auto")>] Auto

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletionsExperimentalExtA1abd3ff =
        | [<CompiledName("experimental")>] Experimental
        | [<CompiledName("logging")>] Logging
        | [<CompiledName("completions")>] Completions
        | [<CompiledName("prompts")>] Prompts
        | [<CompiledName("resources")>] Resources
        | [<CompiledName("tools")>] Tools
        | [<CompiledName("tasks")>] Tasks
        | [<CompiledName("extensions")>] Extensions

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type NoCacheNoStore =
        | [<CompiledName("no-store")>] NoStore
        | [<CompiledName("no-cache")>] NoCache

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FirstPrimaryFirstUnconstrained =
        | [<CompiledName("first-primary")>] FirstPrimary
        | [<CompiledName("first-unconstrained")>] FirstUnconstrained

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type _DURABLE1bc60cc4 =
        | __DURABLE_OBJECT_BRAND
        | [<CompiledName("fetch")>] Fetch
        | [<CompiledName("queue")>] Queue
        | [<CompiledName("alarm")>] Alarm
        | [<CompiledName("connect")>] Connect
        | [<CompiledName("state")>] State
        | [<CompiledName("schedule")>] Schedule
        | [<CompiledName("mcp")>] Mcp
        | [<CompiledName("destroy")>] Destroy
        | [<CompiledName("retry")>] Retry
        | [<CompiledName("name")>] Name
        | _keepAliveRefs
        | [<CompiledName("initialState")>] InitialState
        | [<CompiledName("sessionAffinity")>] SessionAffinity
        | [<CompiledName("observability")>] Observability
        | [<CompiledName("sql")>] Sql
        | [<CompiledName("setState")>] SetState
        | [<CompiledName("setConnectionReadonly")>] SetConnectionReadonly
        | [<CompiledName("isConnectionReadonly")>] IsConnectionReadonly
        | [<CompiledName("_unsafe_getConnectionFlag")>] _unsafeGetConnectionFlag
        | [<CompiledName("_unsafe_setConnectionFlag")>] _unsafeSetConnectionFlag
        | [<CompiledName("shouldConnectionBeReadonly")>] ShouldConnectionBeReadonly
        | [<CompiledName("shouldSendProtocolMessages")>] ShouldSendProtocolMessages
        | [<CompiledName("isConnectionProtocolEnabled")>] IsConnectionProtocolEnabled
        | [<CompiledName("validateStateChange")>] ValidateStateChange
        | [<CompiledName("onStateChanged")>] OnStateChanged
        | [<CompiledName("onStateUpdate")>] OnStateUpdate
        | _onEmail
        | [<CompiledName("replyToEmail")>] ReplyToEmail
        | [<CompiledName("sendEmail")>] SendEmail
        | [<CompiledName("onError")>] OnError
        | [<CompiledName("render")>] Render
        | [<CompiledName("dequeue")>] Dequeue
        | [<CompiledName("dequeueAll")>] DequeueAll
        | [<CompiledName("dequeueAllByCallback")>] DequeueAllByCallback
        | [<CompiledName("getQueue")>] GetQueue
        | [<CompiledName("getQueues")>] GetQueues
        | [<CompiledName("_cf_scheduleForFacet")>] _cfScheduleForFacet
        | [<CompiledName("_cf_scheduleEveryForFacet")>] _cfScheduleEveryForFacet
        | [<CompiledName("_cf_cancelScheduleForFacet")>] _cfCancelScheduleForFacet
        | [<CompiledName("_cf_cleanupFacetPrefix")>] _cfCleanupFacetPrefix
        | [<CompiledName("_cf_getScheduleForFacet")>] _cfGetScheduleForFacet
        | [<CompiledName("_cf_listSchedulesForFacet")>] _cfListSchedulesForFacet
        | [<CompiledName("_cf_acquireFacetKeepAlive")>] _cfAcquireFacetKeepAlive
        | [<CompiledName("_cf_releaseFacetKeepAlive")>] _cfReleaseFacetKeepAlive
        | [<CompiledName("_cf_registerFacetRun")>] _cfRegisterFacetRun
        | [<CompiledName("_cf_unregisterFacetRun")>] _cfUnregisterFacetRun
        | [<CompiledName("scheduleEvery")>] ScheduleEvery
        | [<CompiledName("getSchedule")>] GetSchedule
        | [<CompiledName("getScheduleById")>] GetScheduleById
        | [<CompiledName("getSchedules")>] GetSchedules
        | [<CompiledName("listSchedules")>] ListSchedules
        | [<CompiledName("cancelSchedule")>] CancelSchedule
        | [<CompiledName("keepAlive")>] KeepAlive
        | [<CompiledName("keepAliveWhile")>] KeepAliveWhile
        | [<CompiledName("inspectFiber")>] InspectFiber
        | [<CompiledName("inspectFiberByKey")>] InspectFiberByKey
        | [<CompiledName("listFibers")>] ListFibers
        | [<CompiledName("cancelFiber")>] CancelFiber
        | [<CompiledName("cancelFiberByKey")>] CancelFiberByKey
        | [<CompiledName("resolveFiber")>] ResolveFiber
        | [<CompiledName("deleteFibers")>] DeleteFibers
        | [<CompiledName("runFiber")>] RunFiber
        | [<CompiledName("startFiber")>] StartFiber
        | [<CompiledName("stash")>] Stash
        | [<CompiledName("onFiberRecovered")>] OnFiberRecovered
        | _onAlarmHousekeeping
        | [<CompiledName("_cf_checkRunFibersForFacet")>] _cfCheckRunFibersForFacet
        | [<CompiledName("_cf_dispatchScheduledCallback")>] _cfDispatchScheduledCallback
        | [<CompiledName("_cf_invokeAgentPath")>] _cfInvokeAgentPath
        | [<CompiledName("_cf_destroyDescendantFacet")>] _cfDestroyDescendantFacet
        | [<CompiledName("onAlarm")>] OnAlarm
        | [<CompiledName("broadcast")>] Broadcast
        | [<CompiledName("getConnection")>] GetConnection
        | [<CompiledName("getConnections")>] GetConnections
        | [<CompiledName("_cf_broadcastToSubAgent")>] _cfBroadcastToSubAgent
        | [<CompiledName("_cf_subAgentConnectionMetas")>] _cfSubAgentConnectionMetas
        | [<CompiledName("_cf_sendToSubAgentConnection")>] _cfSendToSubAgentConnection
        | [<CompiledName("_cf_closeSubAgentConnection")>] _cfCloseSubAgentConnection
        | [<CompiledName("_cf_setSubAgentConnectionState")>] _cfSetSubAgentConnectionState
        | [<CompiledName("_cf_handleSubAgentWebSocketConnect")>] _cfHandleSubAgentWebSocketConnect
        | [<CompiledName("_cf_handleSubAgentWebSocketMessage")>] _cfHandleSubAgentWebSocketMessage
        | [<CompiledName("_cf_handleSubAgentWebSocketClose")>] _cfHandleSubAgentWebSocketClose
        | [<CompiledName("onBeforeSubAgent")>] OnBeforeSubAgent
        | [<CompiledName("_cf_invokeSubAgent")>] _cfInvokeSubAgent
        | [<CompiledName("_cf_invokeSubAgentPath")>] _cfInvokeSubAgentPath
        | [<CompiledName("_cf_initAsFacet")>] _cfInitAsFacet
        | [<CompiledName("parentPath")>] ParentPath
        | [<CompiledName("selfPath")>] SelfPath
        | [<CompiledName("parentAgent")>] ParentAgent
        | [<CompiledName("subAgent")>] SubAgent
        | [<CompiledName("maxConcurrentAgentTools")>] MaxConcurrentAgentTools
        | [<CompiledName("onAgentToolStart")>] OnAgentToolStart
        | [<CompiledName("onAgentToolFinish")>] OnAgentToolFinish
        | [<CompiledName("onProgress")>] OnProgress
        | [<CompiledName("reportProgress")>] ReportProgress
        | [<CompiledName("runAgentTool")>] RunAgentTool
        | [<CompiledName("cancelAgentTool")>] CancelAgentTool
        | _cfDetachedReconcileTick
        | [<CompiledName("hasAgentToolRun")>] HasAgentToolRun
        | [<CompiledName("clearAgentToolRuns")>] ClearAgentToolRuns
        | [<CompiledName("abortSubAgent")>] AbortSubAgent
        | [<CompiledName("deleteSubAgent")>] DeleteSubAgent
        | [<CompiledName("hasSubAgent")>] HasSubAgent
        | [<CompiledName("listSubAgents")>] ListSubAgents
        | [<CompiledName("_cf_scheduleDestroy")>] _cfScheduleDestroy
        | [<CompiledName("getCallableMethods")>] GetCallableMethods
        | [<CompiledName("runWorkflow")>] RunWorkflow
        | [<CompiledName("sendWorkflowEvent")>] SendWorkflowEvent
        | [<CompiledName("approveWorkflow")>] ApproveWorkflow
        | [<CompiledName("rejectWorkflow")>] RejectWorkflow
        | [<CompiledName("terminateWorkflow")>] TerminateWorkflow
        | [<CompiledName("pauseWorkflow")>] PauseWorkflow
        | [<CompiledName("resumeWorkflow")>] ResumeWorkflow
        | [<CompiledName("restartWorkflow")>] RestartWorkflow
        | [<CompiledName("getWorkflowStatus")>] GetWorkflowStatus
        | [<CompiledName("getWorkflow")>] GetWorkflow
        | [<CompiledName("getWorkflows")>] GetWorkflows
        | [<CompiledName("deleteWorkflow")>] DeleteWorkflow
        | [<CompiledName("deleteWorkflows")>] DeleteWorkflows
        | [<CompiledName("migrateWorkflowBinding")>] MigrateWorkflowBinding
        | [<CompiledName("onWorkflowCallback")>] OnWorkflowCallback
        | [<CompiledName("onWorkflowProgress")>] OnWorkflowProgress
        | [<CompiledName("onWorkflowComplete")>] OnWorkflowComplete
        | [<CompiledName("onWorkflowError")>] OnWorkflowError
        | [<CompiledName("onWorkflowEvent")>] OnWorkflowEvent
        | [<CompiledName("_workflow_handleCallback")>] _workflowHandleCallback
        | [<CompiledName("_workflow_broadcast")>] _workflowBroadcast
        | [<CompiledName("_workflow_updateState")>] _workflowUpdateState
        | [<CompiledName("addMcpServer")>] AddMcpServer
        | [<CompiledName("removeMcpServer")>] RemoveMcpServer
        | [<CompiledName("getMcpServers")>] GetMcpServers
        | [<CompiledName("createMcpOAuthProvider")>] CreateMcpOAuthProvider
        | [<CompiledName("webSocketMessage")>] WebSocketMessage
        | [<CompiledName("webSocketClose")>] WebSocketClose
        | [<CompiledName("webSocketError")>] WebSocketError
        | [<CompiledName("__unsafe_ensureInitialized")>] _UnsafeEnsureInitialized
        | [<CompiledName("setName")>] SetName
        | _initAndFetch
        | [<CompiledName("getConnectionTags")>] GetConnectionTags
        | [<CompiledName("onStart")>] OnStart
        | [<CompiledName("onConnect")>] OnConnect
        | [<CompiledName("onMessage")>] OnMessage
        | [<CompiledName("onClose")>] OnClose
        | [<CompiledName("onRequest")>] OnRequest
        | [<CompiledName("onException")>] OnException

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MapSet =
        | [<CompiledName("map")>] Map
        | [<CompiledName("set")>] Set

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BudgetExceededInspectFail534c0c95 =
        | [<CompiledName("no-progress")>] NoProgress
        | [<CompiledName("window-exceeded")>] WindowExceeded
        | [<CompiledName("not-tailable")>] NotTailable
        | [<CompiledName("inspect-timeout")>] InspectTimeout
        | [<CompiledName("inspect-failed")>] InspectFailed
        | [<CompiledName("recovery-deadline")>] RecoveryDeadline
        | [<CompiledName("budget-exceeded")>] BudgetExceeded

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AndOr =
        | [<CompiledName("and")>] And
        | [<CompiledName("or")>] Or

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompletedErrorPaused180ca67a =
        | [<CompiledName("error")>] Error
        | [<CompiledName("paused")>] Paused
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("rejected")>] Rejected
        | [<CompiledName("rolled_back")>] RolledBack

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AnchorCommentDefsF9675ce0 =
        | [<CompiledName("default")>] Default
        | [<CompiledName("description")>] Description
        | [<CompiledName("title")>] Title
        | [<CompiledName("type")>] Type
        | [<CompiledName("$anchor")>] Anchor
        | [<CompiledName("$comment")>] Comment
        | [<CompiledName("$defs")>] Defs
        | [<CompiledName("$dynamicAnchor")>] DynamicAnchor
        | [<CompiledName("$dynamicRef")>] DynamicRef
        | [<CompiledName("$id")>] Id
        | [<CompiledName("$ref")>] Ref
        | [<CompiledName("$schema")>] Schema
        | [<CompiledName("$vocabulary")>] Vocabulary
        | [<CompiledName("allOf")>] AllOf
        | [<CompiledName("anyOf")>] AnyOf
        | [<CompiledName("const")>] Const
        | [<CompiledName("definitions")>] Definitions
        | [<CompiledName("deprecated")>] Deprecated
        | [<CompiledName("else")>] Else
        | [<CompiledName("enum")>] Enum
        | [<CompiledName("examples")>] Examples
        | [<CompiledName("format")>] Format
        | [<CompiledName("if")>] If
        | [<CompiledName("not")>] Not
        | [<CompiledName("oneOf")>] OneOf
        | [<CompiledName("readOnly")>] ReadOnly
        | [<CompiledName("then")>] Then
        | [<CompiledName("writeOnly")>] WriteOnly

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FalseTrue =
        | [<CompiledName("true")>] True
        | [<CompiledName("false")>] False

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AnyArrayBigint45c8abf9 =
        | [<CompiledName("string")>] String
        | [<CompiledName("number")>] Number
        | [<CompiledName("int")>] Int
        | [<CompiledName("boolean")>] Boolean
        | [<CompiledName("bigint")>] Bigint
        | [<CompiledName("symbol")>] Symbol
        | [<CompiledName("null")>] Null
        | [<CompiledName("undefined")>] Undefined
        | [<CompiledName("void")>] Void
        | [<CompiledName("never")>] Never
        | [<CompiledName("any")>] Any
        | [<CompiledName("unknown")>] Unknown
        | [<CompiledName("date")>] Date
        | [<CompiledName("object")>] Object
        | [<CompiledName("record")>] Record
        | [<CompiledName("file")>] File
        | [<CompiledName("array")>] Array
        | [<CompiledName("tuple")>] Tuple
        | [<CompiledName("union")>] Union
        | [<CompiledName("intersection")>] Intersection
        | [<CompiledName("map")>] Map
        | [<CompiledName("set")>] Set
        | [<CompiledName("enum")>] Enum
        | [<CompiledName("literal")>] Literal
        | [<CompiledName("nullable")>] Nullable
        | [<CompiledName("optional")>] Optional
        | [<CompiledName("nonoptional")>] Nonoptional
        | [<CompiledName("success")>] Success
        | [<CompiledName("transform")>] Transform
        | [<CompiledName("default")>] Default
        | [<CompiledName("prefault")>] Prefault
        | [<CompiledName("catch")>] Catch
        | [<CompiledName("nan")>] Nan
        | [<CompiledName("pipe")>] Pipe
        | [<CompiledName("readonly")>] Readonly
        | [<CompiledName("template_literal")>] TemplateLiteral
        | [<CompiledName("promise")>] Promise
        | [<CompiledName("lazy")>] Lazy
        | [<CompiledName("function")>] Function
        | [<CompiledName("custom")>] Custom

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CompleteErrorEventProgress =
        | [<CompiledName("error")>] Error
        | [<CompiledName("event")>] Event
        | [<CompiledName("complete")>] Complete
        | [<CompiledName("progress")>] Progress

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContinueRetry =
        | [<CompiledName("retry")>] Retry
        | [<CompiledName("continue")>] Continue

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ContentFilterFunction33a35c6e =
        | [<CompiledName("stop")>] Stop
        | [<CompiledName("length")>] Length
        | [<CompiledName("tool_calls")>] ToolCalls
        | [<CompiledName("content_filter")>] ContentFilter
        | [<CompiledName("function_call")>] FunctionCall

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type PromptsResourceTemplatesRe68162d96 =
        | [<CompiledName("prompts")>] Prompts
        | [<CompiledName("resources")>] Resources
        | [<CompiledName("tools")>] Tools
        | [<CompiledName("resourceTemplates")>] ResourceTemplates

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DomcontentloadedLoadNetworD6ce7c4e =
        | [<CompiledName("load")>] Load
        | [<CompiledName("domcontentloaded")>] Domcontentloaded
        | [<CompiledName("networkidle0")>] Networkidle0
        | [<CompiledName("networkidle2")>] Networkidle2

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type JurisdictionLocationHintPr56258b17 =
        | [<CompiledName("jurisdiction")>] Jurisdiction
        | [<CompiledName("locationHint")>] LocationHint
        | [<CompiledName("props")>] Props
        | [<CompiledName("routingRetry")>] RoutingRetry

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LogReexecute =
        | [<CompiledName("log")>] Log
        | [<CompiledName("reexecute")>] Reexecute

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ExclusiveMaximumExclusiveMi1c2c1d7a =
        | [<CompiledName("exclusiveMaximum")>] ExclusiveMaximum
        | [<CompiledName("exclusiveMinimum")>] ExclusiveMinimum
        | [<CompiledName("maximum")>] Maximum
        | [<CompiledName("minimum")>] Minimum
        | [<CompiledName("multipleOf")>] MultipleOf

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type IntegerNumber =
        | [<CompiledName("number")>] Number
        | [<CompiledName("integer")>] Integer

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type RefThrow =
        | [<CompiledName("ref")>] Ref
        | [<CompiledName("throw")>] Throw

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type SseStreamableHttp =
        | [<CompiledName("sse")>] Sse
        | [<CompiledName("streamable-http")>] StreamableHttp

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AnythingGoesClose50b421b1 =
        | [<CompiledName("super_strict_match")>] SuperStrictMatch
        | [<CompiledName("close_enough")>] CloseEnough
        | [<CompiledName("flexible_friend")>] FlexibleFriend
        | [<CompiledName("anything_goes")>] AnythingGoes

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AcceptCancelDecline =
        | [<CompiledName("cancel")>] Cancel
        | [<CompiledName("accept")>] Accept
        | [<CompiledName("decline")>] Decline

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AdditionalPropertiesDepende98e30ece =
        | [<CompiledName("required")>] Required
        | [<CompiledName("properties")>] Properties
        | [<CompiledName("additionalProperties")>] AdditionalProperties
        | [<CompiledName("dependencies")>] Dependencies
        | [<CompiledName("dependentRequired")>] DependentRequired
        | [<CompiledName("dependentSchemas")>] DependentSchemas
        | [<CompiledName("maxProperties")>] MaxProperties
        | [<CompiledName("minProperties")>] MinProperties
        | [<CompiledName("patternProperties")>] PatternProperties
        | [<CompiledName("propertyNames")>] PropertyNames
        | [<CompiledName("unevaluatedProperties")>] UnevaluatedProperties

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type MessageInputImageImageUrC4a97e9e =
        | [<CompiledName("message.input_image.image_url")>] MessageInputImageImageUrl
        | [<CompiledName("message.output_text.logprobs")>] MessageOutputTextLogprobs

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CosineDotProductEuclidean =
        | [<CompiledName("euclidean")>] Euclidean
        | [<CompiledName("cosine")>] Cosine
        | [<CompiledName("dot-product")>] DotProduct

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type HttpHttps =
        | [<CompiledName("http")>] Http
        | [<CompiledName("https")>] Https

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedCompletedError103eaa86 =
        | [<CompiledName("running")>] Running
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("error")>] Error
        | [<CompiledName("aborted")>] Aborted
        | [<CompiledName("interrupted")>] Interrupted

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type OffOnStarttls =
        | [<CompiledName("on")>] On
        | [<CompiledName("off")>] Off
        | [<CompiledName("starttls")>] Starttls

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AscDescExistsNotExists =
        | [<CompiledName("asc")>] Asc
        | [<CompiledName("desc")>] Desc
        | [<CompiledName("exists")>] Exists
        | [<CompiledName("not_exists")>] NotExists

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Base64Base64urlCidrD39639d7 =
        | [<CompiledName("email")>] Email
        | [<CompiledName("base64")>] Base64
        | [<CompiledName("emoji")>] Emoji
        | [<CompiledName("url")>] Url
        | [<CompiledName("uuid")>] Uuid
        | [<CompiledName("nanoid")>] Nanoid
        | [<CompiledName("cuid")>] Cuid
        | [<CompiledName("cuid2")>] Cuid2
        | [<CompiledName("ulid")>] Ulid
        | [<CompiledName("datetime")>] Datetime
        | [<CompiledName("date")>] Date
        | [<CompiledName("time")>] Time
        | [<CompiledName("duration")>] Duration
        | [<CompiledName("base64url")>] Base64url
        | [<CompiledName("regex")>] Regex
        | [<CompiledName("jwt")>] Jwt
        | [<CompiledName("ip")>] Ip
        | [<CompiledName("cidr")>] Cidr

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DurableWorkRecoverySchedu9703c103 =
        | [<CompiledName("transcript-hydration")>] TranscriptHydration
        | [<CompiledName("scheduled-task-reconcile")>] ScheduledTaskReconcile
        | [<CompiledName("durable-work-recovery")>] DurableWorkRecovery

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ``Http:JsonSchemaOrgDraftA6a2ca44`` =
        | [<CompiledName("https://json-schema.org/draft/2020-12/schema")>] ``Https:JsonSchemaOrgDraft202012Schema``
        | [<CompiledName("http://json-schema.org/draft-07/schema#")>] ``Http:JsonSchemaOrgDraft07Schema#``
        | [<CompiledName("http://json-schema.org/draft-04/schema#")>] ``Http:JsonSchemaOrgDraft04Schema#``

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedCompletedError9ee90e87 =
        | [<CompiledName("error")>] Error
        | [<CompiledName("running")>] Running
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("interrupted")>] Interrupted
        | [<CompiledName("starting")>] Starting
        | [<CompiledName("aborted")>] Aborted

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CorsNoCorsSameOrigin =
        | [<CompiledName("cors")>] Cors
        | [<CompiledName("no-cors")>] NoCors
        | [<CompiledName("same-origin")>] SameOrigin

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ClientCloseTerminateSession =
        | [<CompiledName("terminate-session")>] TerminateSession
        | [<CompiledName("client-close")>] ClientClose

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type LooseStrict =
        | [<CompiledName("strict")>] Strict
        | [<CompiledName("loose")>] Loose

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DeflateDeflateRawGzip =
        | [<CompiledName("gzip")>] Gzip
        | [<CompiledName("deflate")>] Deflate
        | [<CompiledName("deflate-raw")>] DeflateRaw

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ActiveExpiredRevoked =
        | [<CompiledName("active")>] Active
        | [<CompiledName("expired")>] Expired
        | [<CompiledName("revoked")>] Revoked

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AssistantSystemToolUser =
        | [<CompiledName("user")>] User
        | [<CompiledName("assistant")>] Assistant
        | [<CompiledName("system")>] System
        | [<CompiledName("tool")>] Tool

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ConnectorMethodSnippet =
        | [<CompiledName("connector")>] Connector
        | [<CompiledName("method")>] Method
        | [<CompiledName("snippet")>] Snippet

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ErrorRunning =
        | [<CompiledName("running")>] Running
        | [<CompiledName("error")>] Error

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ConvertPassthrough =
        | [<CompiledName("passthrough")>] Passthrough
        | [<CompiledName("convert")>] Convert

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type WsWss =
        | [<CompiledName("ws")>] Ws
        | [<CompiledName("wss")>] Wss

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CopyrightKeepNone =
        | [<CompiledName("keep")>] Keep
        | [<CompiledName("copyright")>] Copyright
        | [<CompiledName("none")>] None

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AacFlacMp3OpusPcm16Wav =
        | [<CompiledName("wav")>] Wav
        | [<CompiledName("aac")>] Aac
        | [<CompiledName("mp3")>] Mp3
        | [<CompiledName("flac")>] Flac
        | [<CompiledName("opus")>] Opus
        | [<CompiledName("pcm16")>] Pcm16

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ErrorInprogressReady =
        | [<CompiledName("error")>] Error
        | [<CompiledName("ready")>] Ready
        | [<CompiledName("inprogress")>] Inprogress

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type BackwardForward =
        | [<CompiledName("forward")>] Forward
        | [<CompiledName("backward")>] Backward

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AlertCriticalDebugC0186d25 =
        | [<CompiledName("error")>] Error
        | [<CompiledName("debug")>] Debug
        | [<CompiledName("info")>] Info
        | [<CompiledName("notice")>] Notice
        | [<CompiledName("warning")>] Warning
        | [<CompiledName("critical")>] Critical
        | [<CompiledName("alert")>] Alert
        | [<CompiledName("emergency")>] Emergency

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type JsonObjectJsonSchema =
        | [<CompiledName("json_object")>] JsonObject
        | [<CompiledName("json_schema")>] JsonSchema

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type InNin =
        | [<CompiledName("$in")>] In
        | [<CompiledName("$nin")>] Nin

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FileImageUrlInputAudioText =
        | [<CompiledName("text")>] Text
        | [<CompiledName("image_url")>] ImageUrl
        | [<CompiledName("input_audio")>] InputAudio
        | [<CompiledName("file")>] File

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AsmBengAwa7cbeb380 =
        | [<CompiledName("asm_Beng")>] AsmBeng
        | [<CompiledName("awa_Deva")>] AwaDeva
        | [<CompiledName("ben_Beng")>] BenBeng
        | [<CompiledName("bho_Deva")>] BhoDeva
        | [<CompiledName("brx_Deva")>] BrxDeva
        | [<CompiledName("doi_Deva")>] DoiDeva
        | [<CompiledName("eng_Latn")>] EngLatn
        | [<CompiledName("gom_Deva")>] GomDeva
        | [<CompiledName("gon_Deva")>] GonDeva
        | [<CompiledName("guj_Gujr")>] GujGujr
        | [<CompiledName("hin_Deva")>] HinDeva
        | [<CompiledName("hne_Deva")>] HneDeva
        | [<CompiledName("kan_Knda")>] KanKnda
        | [<CompiledName("kas_Arab")>] KasArab
        | [<CompiledName("kas_Deva")>] KasDeva
        | [<CompiledName("kha_Latn")>] KhaLatn
        | [<CompiledName("lus_Latn")>] LusLatn
        | [<CompiledName("mag_Deva")>] MagDeva
        | [<CompiledName("mai_Deva")>] MaiDeva
        | [<CompiledName("mal_Mlym")>] MalMlym
        | [<CompiledName("mar_Deva")>] MarDeva
        | [<CompiledName("mni_Beng")>] MniBeng
        | [<CompiledName("mni_Mtei")>] MniMtei
        | [<CompiledName("npi_Deva")>] NpiDeva
        | [<CompiledName("ory_Orya")>] OryOrya
        | [<CompiledName("pan_Guru")>] PanGuru
        | [<CompiledName("san_Deva")>] SanDeva
        | [<CompiledName("sat_Olck")>] SatOlck
        | [<CompiledName("snd_Arab")>] SndArab
        | [<CompiledName("snd_Deva")>] SndDeva
        | [<CompiledName("tam_Taml")>] TamTaml
        | [<CompiledName("tel_Telu")>] TelTelu
        | [<CompiledName("urd_Arab")>] UrdArab
        | [<CompiledName("unr_Deva")>] UnrDeva

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type JpgM4aPng =
        | [<CompiledName("jpg")>] Jpg
        | [<CompiledName("png")>] Png
        | [<CompiledName("m4a")>] M4a

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ApplicationGraphqlApplicat548c1c07 =
        | [<CompiledName("application/json")>] ApplicationJson
        | [<CompiledName("application/xml")>] ApplicationXml
        | [<CompiledName("application/x-www-form-urlencoded")>] ApplicationXWwwFormUrlencoded
        | [<CompiledName("application/javascript")>] ApplicationJavascript
        | [<CompiledName("application/pdf")>] ApplicationPdf
        | [<CompiledName("application/zip")>] ApplicationZip
        | [<CompiledName("application/vnd.ms-excel")>] ApplicationVndMsExcel
        | [<CompiledName("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")>] ApplicationVndOpenxmlformatsOfficedocumentSpreadsheetmlSheet
        | [<CompiledName("application/msword")>] ApplicationMsword
        | [<CompiledName("application/vnd.openxmlformats-officedocument.wordprocessingml.document")>] ApplicationVndOpenxmlformatsOfficedocumentWordprocessingmlDocument
        | [<CompiledName("application/vnd.ms-powerpoint")>] ApplicationVndMsPowerpoint
        | [<CompiledName("application/vnd.openxmlformats-officedocument.presentationml.presentation")>] ApplicationVndOpenxmlformatsOfficedocumentPresentationmlPresentation
        | [<CompiledName("application/octet-stream")>] ApplicationOctetStream
        | [<CompiledName("application/graphql")>] ApplicationGraphql
        | [<CompiledName("text/html")>] TextHtml
        | [<CompiledName("text/plain")>] TextPlain
        | [<CompiledName("text/css")>] TextCss
        | [<CompiledName("text/javascript")>] TextJavascript
        | [<CompiledName("text/csv")>] TextCsv
        | [<CompiledName("image/png")>] ImagePng
        | [<CompiledName("image/jpeg")>] ImageJpeg
        | [<CompiledName("image/gif")>] ImageGif
        | [<CompiledName("image/svg+xml")>] ImageSvgXml
        | [<CompiledName("image/webp")>] ImageWebp
        | [<CompiledName("audio/mpeg")>] AudioMpeg
        | [<CompiledName("audio/ogg")>] AudioOgg
        | [<CompiledName("audio/wav")>] AudioWav
        | [<CompiledName("audio/webm")>] AudioWebm
        | [<CompiledName("video/mp4")>] VideoMp4
        | [<CompiledName("video/webm")>] VideoWebm
        | [<CompiledName("video/ogg")>] VideoOgg
        | [<CompiledName("font/woff")>] FontWoff
        | [<CompiledName("font/woff2")>] FontWoff2
        | [<CompiledName("font/ttf")>] FontTtf
        | [<CompiledName("font/otf")>] FontOtf
        | [<CompiledName("multipart/form-data")>] MultipartFormData

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type OptionalRequired =
        | [<CompiledName("optional")>] Optional
        | [<CompiledName("required")>] Required

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CenterLowerLeftLowerRightC8ab9f4a =
        | [<CompiledName("upperRight")>] UpperRight
        | [<CompiledName("upperLeft")>] UpperLeft
        | [<CompiledName("lowerLeft")>] LowerLeft
        | [<CompiledName("lowerRight")>] LowerRight
        | [<CompiledName("center")>] Center

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DebugErrorInfoLogWarn =
        | [<CompiledName("debug")>] Debug
        | [<CompiledName("error")>] Error
        | [<CompiledName("info")>] Info
        | [<CompiledName("log")>] Log
        | [<CompiledName("warn")>] Warn

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ExtendedStrict =
        | [<CompiledName("extended")>] Extended
        | [<CompiledName("strict")>] Strict

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type HHvV =
        | [<CompiledName("h")>] H
        | [<CompiledName("v")>] V
        | [<CompiledName("hv")>] Hv

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type EmptyImageFileB06f83e3 =
        | [<CompiledName("server_error")>] ServerError
        | [<CompiledName("rate_limit_exceeded")>] RateLimitExceeded
        | [<CompiledName("invalid_prompt")>] InvalidPrompt
        | [<CompiledName("vector_store_timeout")>] VectorStoreTimeout
        | [<CompiledName("invalid_image")>] InvalidImage
        | [<CompiledName("invalid_image_format")>] InvalidImageFormat
        | [<CompiledName("invalid_base64_image")>] InvalidBase64Image
        | [<CompiledName("invalid_image_url")>] InvalidImageUrl
        | [<CompiledName("image_too_large")>] ImageTooLarge
        | [<CompiledName("image_too_small")>] ImageTooSmall
        | [<CompiledName("image_parse_error")>] ImageParseError
        | [<CompiledName("image_content_policy_violation")>] ImageContentPolicyViolation
        | [<CompiledName("invalid_image_mode")>] InvalidImageMode
        | [<CompiledName("image_file_too_large")>] ImageFileTooLarge
        | [<CompiledName("unsupported_image_media_type")>] UnsupportedImageMediaType
        | [<CompiledName("empty_image_file")>] EmptyImageFile
        | [<CompiledName("failed_to_download_image")>] FailedToDownloadImage
        | [<CompiledName("image_file_not_found")>] ImageFileNotFound

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ExpiredInvalidMalformedE1b15311 =
        | [<CompiledName("missing_headers")>] MissingHeaders
        | [<CompiledName("expired")>] Expired
        | [<CompiledName("invalid")>] Invalid
        | [<CompiledName("malformed_timestamp")>] MalformedTimestamp

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AllClientDiscoveryTokensVerifier =
        | [<CompiledName("all")>] All
        | [<CompiledName("client")>] Client
        | [<CompiledName("tokens")>] Tokens
        | [<CompiledName("verifier")>] Verifier
        | [<CompiledName("discovery")>] Discovery

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutomaticManual =
        | [<CompiledName("automatic")>] Automatic
        | [<CompiledName("manual")>] Manual

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type RefusalText =
        | [<CompiledName("text")>] Text
        | [<CompiledName("refusal")>] Refusal

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type InputOutput =
        | [<CompiledName("input")>] Input
        | [<CompiledName("output")>] Output

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type FinanceGeneralMedical =
        | [<CompiledName("general")>] General
        | [<CompiledName("medical")>] Medical
        | [<CompiledName("finance")>] Finance

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedDirtyValid =
        | [<CompiledName("aborted")>] Aborted
        | [<CompiledName("dirty")>] Dirty
        | [<CompiledName("valid")>] Valid

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CombinedIgnorePipe =
        | [<CompiledName("pipe")>] Pipe
        | [<CompiledName("ignore")>] Ignore
        | [<CompiledName("combined")>] Combined

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ClientServer =
        | [<CompiledName("server")>] Server
        | [<CompiledName("client")>] Client

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AutoBottomCenterB85bc937 =
        | [<CompiledName("face")>] Face
        | [<CompiledName("left")>] Left
        | [<CompiledName("right")>] Right
        | [<CompiledName("top")>] Top
        | [<CompiledName("bottom")>] Bottom
        | [<CompiledName("center")>] Center
        | [<CompiledName("auto")>] Auto
        | [<CompiledName("entropy")>] Entropy

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type R2WebCrawler =
        | [<CompiledName("r2")>] R2
        | [<CompiledName("web-crawler")>] WebCrawler

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type IncludeOmitSameOrigin =
        | [<CompiledName("include")>] Include
        | [<CompiledName("omit")>] Omit
        | [<CompiledName("same-origin")>] SameOrigin

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type EventFactInstructionTask =
        | [<CompiledName("event")>] Event
        | [<CompiledName("task")>] Task
        | [<CompiledName("fact")>] Fact
        | [<CompiledName("instruction")>] Instruction

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CronDelayedIntervalScheduled =
        | [<CompiledName("scheduled")>] Scheduled
        | [<CompiledName("delayed")>] Delayed
        | [<CompiledName("cron")>] Cron
        | [<CompiledName("interval")>] Interval

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type Mp3Wav =
        | [<CompiledName("wav")>] Wav
        | [<CompiledName("mp3")>] Mp3

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AmrNbAmrWbFlac807e0cb2 =
        | [<CompiledName("linear16")>] Linear16
        | [<CompiledName("flac")>] Flac
        | [<CompiledName("mulaw")>] Mulaw
        | [<CompiledName("amr-nb")>] AmrNb
        | [<CompiledName("amr-wb")>] AmrWb
        | [<CompiledName("opus")>] Opus
        | [<CompiledName("speex")>] Speex
        | [<CompiledName("g729")>] G729

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArraybufferBlob =
        | [<CompiledName("blob")>] Blob
        | [<CompiledName("arraybuffer")>] Arraybuffer

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedCompletedError945d03ea =
        | [<CompiledName("pending")>] Pending
        | [<CompiledName("error")>] Error
        | [<CompiledName("running")>] Running
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("interrupted")>] Interrupted
        | [<CompiledName("aborted")>] Aborted

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type A0A1A2C0f56350 =
        | [<CompiledName("letter")>] Letter
        | [<CompiledName("legal")>] Legal
        | [<CompiledName("tabloid")>] Tabloid
        | [<CompiledName("ledger")>] Ledger
        | [<CompiledName("a0")>] A0
        | [<CompiledName("a1")>] A1
        | [<CompiledName("a2")>] A2
        | [<CompiledName("a3")>] A3
        | [<CompiledName("a4")>] A4
        | [<CompiledName("a5")>] A5
        | [<CompiledName("a6")>] A6

    type I1I2I3I4I5 =
        | ``1`` = 1
        | ``2`` = 2
        | ``3`` = 3
        | ``4`` = 4
        | ``5`` = 5

    type I1I1 =
        | ``-1`` = -1
        | ``1`` = 1

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ParsePersistRecovery9adb37d3 =
        | [<CompiledName("parse")>] Parse
        | [<CompiledName("persist")>] Persist
        | [<CompiledName("turn")>] Turn
        | [<CompiledName("stream")>] Stream
        | [<CompiledName("recovery")>] Recovery
        | [<CompiledName("transcript")>] Transcript

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ``Action:ledger:conflictActio1876ca5e`` =
        | [<CompiledName("connect")>] Connect
        | [<CompiledName("rpc")>] Rpc
        | [<CompiledName("state:update")>] ``State:update``
        | [<CompiledName("rpc:error")>] ``Rpc:error``
        | [<CompiledName("message:request")>] ``Message:request``
        | [<CompiledName("message:response")>] ``Message:response``
        | [<CompiledName("message:clear")>] ``Message:clear``
        | [<CompiledName("message:cancel")>] ``Message:cancel``
        | [<CompiledName("message:error")>] ``Message:error``
        | [<CompiledName("tool:result")>] ``Tool:result``
        | [<CompiledName("tool:approval")>] ``Tool:approval``
        | [<CompiledName("schedule:create")>] ``Schedule:create``
        | [<CompiledName("schedule:execute")>] ``Schedule:execute``
        | [<CompiledName("schedule:cancel")>] ``Schedule:cancel``
        | [<CompiledName("schedule:retry")>] ``Schedule:retry``
        | [<CompiledName("schedule:error")>] ``Schedule:error``
        | [<CompiledName("schedule:duplicate_warning")>] ``Schedule:duplicateWarning``
        | [<CompiledName("alarm:memory_limit_reset")>] ``Alarm:memoryLimitReset``
        | [<CompiledName("queue:create")>] ``Queue:create``
        | [<CompiledName("queue:retry")>] ``Queue:retry``
        | [<CompiledName("queue:error")>] ``Queue:error``
        | [<CompiledName("submission:create")>] ``Submission:create``
        | [<CompiledName("submission:status")>] ``Submission:status``
        | [<CompiledName("submission:error")>] ``Submission:error``
        | [<CompiledName("action:ledger:replayed")>] ``Action:ledger:replayed``
        | [<CompiledName("action:ledger:pending")>] ``Action:ledger:pending``
        | [<CompiledName("action:ledger:conflict")>] ``Action:ledger:conflict``
        | [<CompiledName("action:ledger:serialize_failed")>] ``Action:ledger:serializeFailed``
        | [<CompiledName("action:ledger:settled")>] ``Action:ledger:settled``
        | [<CompiledName("action:ledger:reclaimed")>] ``Action:ledger:reclaimed``
        | [<CompiledName("action:ledger:swept")>] ``Action:ledger:swept``
        | [<CompiledName("action:pause:created")>] ``Action:pause:created``
        | [<CompiledName("action:pause:approved")>] ``Action:pause:approved``
        | [<CompiledName("action:pause:rejected")>] ``Action:pause:rejected``
        | [<CompiledName("action:pause:swept")>] ``Action:pause:swept``
        | [<CompiledName("action:reply-attached")>] ``Action:replyAttached``
        | [<CompiledName("channel:resolved")>] ``Channel:resolved``
        | [<CompiledName("channel:delivered")>] ``Channel:delivered``
        | [<CompiledName("notice:delivered")>] ``Notice:delivered``
        | [<CompiledName("notice:failed")>] ``Notice:failed``
        | [<CompiledName("fiber:run:started")>] ``Fiber:run:started``
        | [<CompiledName("fiber:run:completed")>] ``Fiber:run:completed``
        | [<CompiledName("fiber:run:failed")>] ``Fiber:run:failed``
        | [<CompiledName("fiber:run:interrupted")>] ``Fiber:run:interrupted``
        | [<CompiledName("fiber:recovery:detected")>] ``Fiber:recovery:detected``
        | [<CompiledName("fiber:recovery:attempt")>] ``Fiber:recovery:attempt``
        | [<CompiledName("fiber:recovery:handled")>] ``Fiber:recovery:handled``
        | [<CompiledName("fiber:recovery:skipped")>] ``Fiber:recovery:skipped``
        | [<CompiledName("fiber:recovery:failed")>] ``Fiber:recovery:failed``
        | [<CompiledName("chat:request:failed")>] ``Chat:request:failed``
        | [<CompiledName("chat:turn:start")>] ``Chat:turn:start``
        | [<CompiledName("chat:turn:finish")>] ``Chat:turn:finish``
        | [<CompiledName("chat:recovery:detected")>] ``Chat:recovery:detected``
        | [<CompiledName("chat:recovery:scheduled")>] ``Chat:recovery:scheduled``
        | [<CompiledName("chat:recovery:attempt")>] ``Chat:recovery:attempt``
        | [<CompiledName("chat:recovery:completed")>] ``Chat:recovery:completed``
        | [<CompiledName("chat:recovery:skipped")>] ``Chat:recovery:skipped``
        | [<CompiledName("chat:recovery:exhausted")>] ``Chat:recovery:exhausted``
        | [<CompiledName("chat:recovery:failed")>] ``Chat:recovery:failed``
        | [<CompiledName("chat:transcript:repaired")>] ``Chat:transcript:repaired``
        | [<CompiledName("chat:onstart:degraded")>] ``Chat:onstart:degraded``
        | [<CompiledName("chat:hydration:windowed")>] ``Chat:hydration:windowed``
        | [<CompiledName("chat:media:evicted")>] ``Chat:media:evicted``
        | [<CompiledName("chat:stream:stalled")>] ``Chat:stream:stalled``
        | [<CompiledName("chat:context:compacted")>] ``Chat:context:compacted``
        | [<CompiledName("agent_tool:recovery:begin")>] ``AgentTool:recovery:begin``
        | [<CompiledName("agent_tool:recovery:row")>] ``AgentTool:recovery:row``
        | [<CompiledName("agent_tool:recovery:deadline")>] ``AgentTool:recovery:deadline``
        | [<CompiledName("agent_tool:recovery:reattach")>] ``AgentTool:recovery:reattach``
        | [<CompiledName("agent_tool:recovery:complete")>] ``AgentTool:recovery:complete``
        | [<CompiledName("agent_tool:recovery:failed")>] ``AgentTool:recovery:failed``
        | [<CompiledName("agent_tool:detached:delivery_failed")>] ``AgentTool:detached:deliveryFailed``
        | [<CompiledName("agent_tool:detached:live_count_warning")>] ``AgentTool:detached:liveCountWarning``
        | [<CompiledName("destroy")>] Destroy
        | [<CompiledName("disconnect")>] Disconnect
        | [<CompiledName("email:receive")>] ``Email:receive``
        | [<CompiledName("email:reply")>] ``Email:reply``
        | [<CompiledName("email:send")>] ``Email:send``
        | [<CompiledName("workflow:start")>] ``Workflow:start``
        | [<CompiledName("workflow:event")>] ``Workflow:event``
        | [<CompiledName("workflow:approved")>] ``Workflow:approved``
        | [<CompiledName("workflow:rejected")>] ``Workflow:rejected``
        | [<CompiledName("workflow:terminated")>] ``Workflow:terminated``
        | [<CompiledName("workflow:paused")>] ``Workflow:paused``
        | [<CompiledName("workflow:resumed")>] ``Workflow:resumed``
        | [<CompiledName("workflow:restarted")>] ``Workflow:restarted``
        | [<CompiledName("mcp:client:preconnect")>] ``Mcp:client:preconnect``
        | [<CompiledName("mcp:client:connect")>] ``Mcp:client:connect``
        | [<CompiledName("mcp:client:authorize")>] ``Mcp:client:authorize``
        | [<CompiledName("mcp:client:discover")>] ``Mcp:client:discover``
        | [<CompiledName("mcp:client:close")>] ``Mcp:client:close``

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DevtoolsTab =
        | [<CompiledName("tab")>] Tab
        | [<CompiledName("devtools")>] Devtools

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CustomMetadataHttpMetadata =
        | [<CompiledName("httpMetadata")>] HttpMetadata
        | [<CompiledName("customMetadata")>] CustomMetadata

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type CspviolationreportDocument_f832a72d =
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

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AllClientTokensVerifier =
        | [<CompiledName("all")>] All
        | [<CompiledName("client")>] Client
        | [<CompiledName("tokens")>] Tokens
        | [<CompiledName("verifier")>] Verifier

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AbortedCompletedErrorInterrupted =
        | [<CompiledName("error")>] Error
        | [<CompiledName("completed")>] Completed
        | [<CompiledName("interrupted")>] Interrupted
        | [<CompiledName("aborted")>] Aborted

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ModifiedAtStatus =
        | [<CompiledName("status")>] Status
        | [<CompiledName("modified_at")>] ModifiedAt

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DeEnEsFrItPt =
        | [<CompiledName("en")>] En
        | [<CompiledName("es")>] Es
        | [<CompiledName("fr")>] Fr
        | [<CompiledName("it")>] It
        | [<CompiledName("pt")>] Pt
        | [<CompiledName("de")>] De

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ClientSecretBasicDc89cc1f =
        | [<CompiledName("none")>] None
        | [<CompiledName("client_secret_basic")>] ClientSecretBasic
        | [<CompiledName("client_secret_post")>] ClientSecretPost

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AscDesc =
        | [<CompiledName("asc")>] Asc
        | [<CompiledName("desc")>] Desc

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AtopInLighterOutOverXor =
        | [<CompiledName("out")>] Out
        | [<CompiledName("in")>] In
        | [<CompiledName("over")>] Over
        | [<CompiledName("atop")>] Atop
        | [<CompiledName("xor")>] Xor
        | [<CompiledName("lighter")>] Lighter

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AvifBaselineJpegJpeg47a90ada =
        | [<CompiledName("avif")>] Avif
        | [<CompiledName("webp")>] Webp
        | [<CompiledName("json")>] Json
        | [<CompiledName("jpeg")>] Jpeg
        | [<CompiledName("png")>] Png
        | [<CompiledName("baseline-jpeg")>] BaselineJpeg
        | [<CompiledName("png-force")>] PngForce
        | [<CompiledName("svg")>] Svg

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AdditionalItemsContainsIteE596ef31 =
        | [<CompiledName("additionalItems")>] AdditionalItems
        | [<CompiledName("contains")>] Contains
        | [<CompiledName("items")>] Items
        | [<CompiledName("maxContains")>] MaxContains
        | [<CompiledName("maxItems")>] MaxItems
        | [<CompiledName("minContains")>] MinContains
        | [<CompiledName("minItems")>] MinItems
        | [<CompiledName("prefixItems")>] PrefixItems
        | [<CompiledName("unevaluatedItems")>] UnevaluatedItems
        | [<CompiledName("uniqueItems")>] UniqueItems

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type IgnorePipe =
        | [<CompiledName("pipe")>] Pipe
        | [<CompiledName("ignore")>] Ignore

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type DarkLight =
        | [<CompiledName("light")>] Light
        | [<CompiledName("dark")>] Dark

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type AudioFrameSpritesheetVideo =
        | [<CompiledName("video")>] Video
        | [<CompiledName("spritesheet")>] Spritesheet
        | [<CompiledName("frame")>] Frame
        | [<CompiledName("audio")>] Audio

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type GenerateInterpolate =
        | [<CompiledName("interpolate")>] Interpolate
        | [<CompiledName("generate")>] Generate

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type PorterTrigram =
        | [<CompiledName("porter")>] Porter
        | [<CompiledName("trigram")>] Trigram

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ``7bit8bitBase64C35a02b4`` =
        | [<CompiledName("7bit")>] ``7bit``
        | [<CompiledName("8bit")>] ``8bit``
        | [<CompiledName("base64")>] Base64
        | [<CompiledName("binary")>] Binary
        | [<CompiledName("ietf-token")>] IetfToken
        | [<CompiledName("quoted-printable")>] QuotedPrintable
        | [<CompiledName("x-token")>] XToken

    [<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
    type ArrayBigintDate86b2e2b7 =
        | [<CompiledName("number")>] Number
        | [<CompiledName("int")>] Int
        | [<CompiledName("bigint")>] Bigint
        | [<CompiledName("date")>] Date
        | [<CompiledName("string")>] String
        | [<CompiledName("array")>] Array
        | [<CompiledName("set")>] Set
        | [<CompiledName("file")>] File

module SharedLiterals =
    type OnresumptiontokenRelatedReqF6244fa9 =
        abstract resumptionToken: option<string> with get, set
        abstract relatedRequestId: option<obj> with get, set
        abstract onresumptiontoken: token: string -> unit

    type _metaAnnotationsA03c34942 =
        [<EmitProperty("type")>]
        abstract ``type``: Zod.ZodType with get, set

        abstract title: Zod.ZodType with get, set
        abstract name: Zod.ZodType with get, set
        abstract icons: Zod.ZodType with get, set
        abstract _meta: Zod.ZodType with get, set
        abstract annotations: Zod.ZodType with get, set
        abstract size: Zod.ZodType with get, set
        abstract mimeType: Zod.ZodType with get, set
        abstract description: Zod.ZodType with get, set
        abstract uri: Zod.ZodType with get, set

    type IdempotencyKeyRequestIdSubmissionId =
        abstract idempotencyKey: option<string> with get, set
        abstract requestId: option<string> with get, set
        abstract submissionId: string with get, set

    type PropertiesRequiredType3 =
        abstract required: option<ResizeArray<string>> with get, set
        abstract properties: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType71 =
        abstract timestamp: float with get, set
        abstract payload: obj with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type DescriptionStreaming =
        abstract streaming: option<bool> with get, set
        abstract description: option<string> with get, set

    type ThatThatLocalesF8ee7b5e =
        abstract Invoke: that: string -> float
        abstract Invoke: that: string * ?locales: U2<ResizeArray<string>, string> * ?options: Erased.Intl -> float
        abstract Invoke: that: string * ?locales: Erased.Intl * ?options: Erased.Intl -> float

    type Code4 =
        abstract code: string with get, set

    type JsonrpcMethodParams =
        abstract params: option<_meta2> with get, set
        abstract jsonrpc: string with get, set
        abstract method: string with get, set

    type _metaAnnotationsTextType2 =
        abstract _meta: Zod.ZodType with get, set
        abstract annotations: Zod.ZodType with get, set
        abstract text: Zod.ZodType with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Zod.ZodType with get, set

    type InOut2 = interface end

    type CompletionsExperimentalExtA1abd3ff =
        abstract extensions: option<obj> with get, set
        abstract tasks: option<CancelListRequests2> with get, set
        abstract tools: option<ListChanged> with get, set
        abstract resources: option<ListChangedSubscribe> with get, set
        abstract prompts: option<ListChanged> with get, set
        abstract completions: option<Erased.Empty> with get, set
        abstract logging: option<Erased.Empty> with get, set
        abstract experimental: option<obj> with get, set

    type _metaAnnotationsTextType =
        abstract _meta: option<obj> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract text: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ErrorSnapshotStatus =
        abstract snapshot: option<obj> with get, set
        abstract error: option<obj> with get, set
        abstract status: string with get, set

    type ElapsedMsErrorFiberIdA169a092 =
        abstract managed: option<bool> with get, set
        abstract elapsedMs: option<float> with get, set
        abstract error: string with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type MatcherRegexp =
        abstract Invoke: regexp: U2<obj, string> -> option<obj>
        abstract Invoke: matcher: SymbolMatch -> option<obj>

    type AgentNamePayloadTimestampType47 =
        abstract timestamp: float with get, set
        abstract payload: AdmissionContinuationDurat4f4f0774 with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type KindRunIdSummary =
        abstract summary: string with get, set
        abstract runId: string with get, set
        abstract kind: string with get, set

    type KindPathRootBindingVersion =
        abstract path: ResizeArray<Erased.Empty> with get, set
        abstract rootBinding: string with get, set
        abstract version: int with get, set
        abstract kind: string with get, set

    type ExecutionId3 =
        abstract executionId: string with get, set

    type AgentNamePayloadTimestampType75 =
        abstract timestamp: float with get, set
        abstract payload: FromSubjectTo with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType46 =
        abstract timestamp: float with get, set
        abstract payload: AdmissionContinuationGener9021d3af with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AnchorCommentDefs0800bb6e6<'T> =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e6<'T>, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e6<'T>, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e6<'T>, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e6<'T>, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e6<'T>, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<'T>, System.Collections.Generic.IReadOnlyList<'T>>> with get, set
        abstract enum: option<U2<ResizeArray<'T>, System.Collections.Generic.IReadOnlyList<'T>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e6<'T>, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<obj> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e6<'T>, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e6<'T>, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<AnchorCommentDefs0800bb6e6.Const> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e6<'T>, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e6<'T>, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e6<'T>, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e6<'T>, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type CapabilityErrorStateUrl =
        abstract capability: option<string> with get, set
        abstract error: option<string> with get, set
        abstract state: option<string> with get, set
        abstract url: option<string> with get, set

    type RequestIdStatusSubmissionId =
        abstract status: string with get, set
        abstract requestId: option<string> with get, set
        abstract submissionId: string with get, set

    type AnchorCommentDefs0800bb6e2 =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e2, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e2, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>, System.Collections.Generic.IReadOnlyList<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>>> with get, set
        abstract enum: option<U2<ResizeArray<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>, System.Collections.Generic.IReadOnlyList<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e2, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e2, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e2, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e2, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type ElapsedMsFiberIdFiberNameManaged =
        abstract managed: option<bool> with get, set
        abstract elapsedMs: option<float> with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type PromptsResourcesServersTools =
        abstract resources: ResizeArray<obj> with get, set
        abstract prompts: ResizeArray<obj> with get, set
        abstract tools: ResizeArray<obj> with get, set
        abstract servers: System.Collections.Generic.IDictionary<string, Erased.Empty> with get, set

    type State =
        abstract state: string with get, set

    type DoneRecur<'A, 'InnerArr, 'Depth, 'Arr> =
        abstract recur: U2<proptypekey<DoneRecur2<'InnerArr, 'Depth, 'Arr>, LiteralUnions.DoneRecur>, 'Arr> with get, set

        [<EmitProperty("done")>]
        abstract ``done``: 'A with get, set

    type ListChanged =
        abstract listChanged: option<bool> with get, set

    type AgentNamePayloadTimestampType78 =
        abstract timestamp: float with get, set
        abstract payload: Error with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaAnnotationsResourceType2<'T> =
        abstract _meta: Zod.ZodType with get, set
        abstract annotations: Zod.ZodType with get, set
        abstract resource: Zod.ZodType with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Zod.ZodType with get, set

    type _metaAnnotationsE7edce492 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme2>> with get, set
        abstract _meta: option<System.Collections.Generic.IDictionary<string, option<obj>>> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract mimeType: option<string> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract uriTemplate: string with get, set

    type RequestIdTimeoutMs =
        abstract timeoutMs: float with get, set
        abstract requestId: string with get, set

    type AgentNamePayloadTimestampType66 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsRunCount with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ActionInputHashKey =
        abstract inputHash: string with get, set
        abstract key: string with get, set
        abstract action: string with get, set

    type ErrorOutputStatus =
        abstract output: option<obj> with get, set
        abstract error: option<MessageName> with get, set
        abstract status: obj with get, set

    type _metaMessageModeRequestedSchemaTask =
        abstract mode: option<string> with get, set
        abstract task: option<Ttl> with get, set
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a> with get, set
        abstract requestedSchema: PropertiesRequiredType with get, set
        abstract message: string with get, set

    type ToolCallIdToolName =
        abstract toolName: string with get, set
        abstract toolCallId: string with get, set

    type TimeoutWaitUntil =
        abstract timeout: option<float> with get, set
        abstract waitUntil: option<obj> with get, set

    type AgentNamePayloadTimestampType23 =
        abstract timestamp: float with get, set
        abstract payload: ErrorMethod with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AnchorCommentDefs0800bb6e4<'T> =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e4<'T>, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e4<'T>, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e4<'T>, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e4<'T>, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e4<'T>, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<'T>, System.Collections.Generic.IReadOnlyList<'T>>> with get, set
        abstract enum: option<U2<ResizeArray<'T>, System.Collections.Generic.IReadOnlyList<'T>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e4<'T>, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<obj> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e4<'T>, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e4<'T>, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<AnchorCommentDefs0800bb6e4.Const> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e4<'T>, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e4<'T>, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e4<'T>, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e4<'T>, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type DoneRecur2<'InnerArr, 'Depth, 'Arr> =
        abstract recur: U2<proptypekey<DoneRecur2<'InnerArr, 'Depth, 'Arr>, LiteralUnions.DoneRecur>, 'Arr> with get, set

        [<EmitProperty("done")>]
        abstract ``done``: 'InnerArr with get, set

    type AgentNamePayloadTimestampType59 =
        abstract timestamp: float with get, set
        abstract payload: BytesExternalizedBytesMessagesParts with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType45 =
        abstract timestamp: float with get, set
        abstract payload: obj with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AuthProviderFetchReconnect1e9d2f71 =
        abstract sessionId: option<string> with get, set
        abstract reconnectionOptions: option<obj> with get, set
        abstract requestInit: option<obj> with get, set
        abstract authProvider: option<obj> with get, set
        abstract fetch: url: U2<obj, string> * ?init: obj -> Promise<obj>

    type AgentNamePayloadTimestampType17 =
        abstract timestamp: float with get, set
        abstract payload: ActionInputHashKey with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type DeserializeAttachmentIdSerCc99579c<'TState, 'T> =
        abstract server: string with get, set
        abstract tags: System.Collections.Generic.IReadOnlyList<string> with get, set
        abstract state: option<obj> with get, set
        abstract uri: option<string> with get, set
        abstract id: string with get, set
        abstract setState: ?state: U2<'TState, obj -> 'TState> -> obj
        abstract serializeAttachment: attachment: 'T -> unit
        abstract deserializeAttachment: unit -> option<'T>

    type ReasonSnapshotStatus2 =
        abstract snapshot: option<obj> with get, set
        abstract reason: option<string> with get, set
        abstract status: string with get, set

    type AgentNamePayloadTimestampType33 =
        abstract timestamp: float with get, set
        abstract payload: ChannelError with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AttemptReasonRequestIdShortened =
        abstract attempt: option<float> with get, set
        abstract requestId: option<string> with get, set
        abstract shortened: bool with get, set
        abstract reason: LiteralUnions.ProactiveReactive with get, set

    type SymbolToStringTag =
        [<EmitProperty("[Symbol.toStringTag]")>]
        abstract ``[symbol.toStringTag]``: string with get

    type ReasonWorkflowId =
        abstract reason: option<string> with get, set
        abstract workflowId: string with get, set

    type _metaAnnotationsE7edce49 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme>> with get, set
        abstract _meta: option<obj> with get, set
        abstract annotations: option<AudienceLastModifiedPriority> with get, set
        abstract mimeType: option<string> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract uriTemplate: string with get, set

    type _meta2 =
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a> with get, set

    type DescriptionNameRequired =
        abstract required: option<bool> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set

    type ElicitationSampling =
        abstract elicitation: option<Create> with get, set
        abstract sampling: option<CreateMessage> with get, set

    type CtxEnv<'T> =
        abstract Create: ctx: Erased.Empty * env: unit -> 'T

    type CallbackCountType =
        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract count: float with get, set
        abstract callback: string with get, set

    type ChannelKindTurnEnded =
        abstract turnEnded: bool with get, set
        abstract kind: string with get, set
        abstract channel: string with get, set

    type RequestId =
        abstract requestId: string with get, set

    type AgentNamePayloadTimestampType15 =
        abstract timestamp: float with get, set
        abstract payload: RequestIdStatusSubmissionId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type TaskId2 =
        abstract taskId: Zod.ZodType with get, set

    type Source =
        abstract source: option<string> with get, set

    type AgentNamePayloadTimestampType35 =
        abstract timestamp: float with get, set
        abstract payload: FiberIdFiberNameManaged with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type MessagePercentStatusStep =
        abstract percent: option<float> with get, set
        abstract message: option<string> with get, set
        abstract status: option<LiteralUnions.CompleteErrorPendingRunning> with get, set
        abstract step: option<string> with get, set
        abstract Item: key: string -> option<obj>

    type FiberIdFiberNameManaged =
        abstract managed: option<bool> with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type ElapsedMsFiberIdFiberName029ca7c8 =
        abstract managed: option<bool> with get, set
        abstract elapsedMs: option<float> with get, set
        abstract reason: string with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type IoModelcontextprotocolRelaF70aae9a3 =
        [<EmitProperty("io.modelcontextprotocol/related-task")>]
        abstract ``io.modelcontextprotocol/relatedTask``: option<TaskId4> with get, set

        abstract progressToken: option<U2<string, float>> with get, set
        abstract Item: x: string -> option<obj>

    type ApprovedToolCallId =
        abstract approved: bool with get, set
        abstract toolCallId: string with get, set

    type _errors =
        abstract _errors: ResizeArray<string> with get, set

    type SymbolSplit =
        abstract ``[symbol.split]``: string: string * ?limit: float -> ResizeArray<string>

    type FormForm =
        abstract Invoke: form: LiteralUnions.NFC_NFD_NFKC_NFKD -> string
        abstract Invoke: ?form: string -> string

    type AgentNamePayloadTimestampType57 =
        abstract timestamp: float with get, set
        abstract payload: ErrorStep with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType29 =
        abstract timestamp: float with get, set
        abstract payload: ActionAttachmentType with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ActionKey =
        abstract key: string with get, set
        abstract action: string with get, set

    type NormalizedInputsRemovedTool58fba2ad =
        abstract toolCallIds: option<ResizeArray<string>> with get, set
        abstract normalizedInputs: float with get, set
        abstract removedToolCalls: float with get, set
        abstract requestId: option<string> with get, set

    type IdJsonrpcResult =
        abstract result: _meta2 with get, set
        abstract id: Zod.ZodType with get, set
        abstract jsonrpc: string with get, set

    type AnchorCommentDefs0800bb6e10 =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e10, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e10, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e10, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e10, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e10, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<bool>, System.Collections.Generic.IReadOnlyList<bool>>> with get, set
        abstract enum: option<U2<ResizeArray<bool>, System.Collections.Generic.IReadOnlyList<bool>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e10, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<bool> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e10, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e10, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<bool> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e10, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e10, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e10, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e10, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type SizeSize =
        abstract Invoke: size: float -> string
        abstract Invoke: size: string -> string

    type AuthUrlClientIdServerId =
        abstract clientId: option<string> with get, set
        abstract authUrl: string with get, set
        abstract serverId: string with get, set

    type AgentNamePayloadTimestampType70 =
        abstract timestamp: float with get, set
        abstract payload: LiveCountThreshold with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type BudgetBytesHydratedMessagesD0eeb8cc =
        abstract hydratedMessages: float with get, set
        abstract budgetBytes: float with get, set
        abstract totalContentBytes: float with get, set

    type CodeDataMessage =
        abstract data: option<obj> with get, set
        abstract message: string with get, set
        abstract code: float with get, set

    type AgentNamePayloadTimestampType20 =
        abstract timestamp: float with get, set
        abstract payload: ActionKey with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType9 =
        abstract timestamp: float with get, set
        abstract payload: ErrorLimitSealedStrikes with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ElicitationExperimentalExtFf9b7be32 =
        abstract extensions: option<obj> with get, set
        abstract tasks: option<CancelListRequests> with get, set
        abstract roots: option<ListChanged> with get, set
        abstract elicitation: option<FormUrl> with get, set
        abstract sampling: option<ContextTools> with get, set
        abstract experimental: option<obj> with get, set

    type Tools =
        abstract tools: option<Call> with get, set

    type AgentNamePayloadTimestampType56 =
        abstract timestamp: float with get, set
        abstract payload: obj with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ErrorRequestIdSubmissionId =
        abstract error: string with get, set
        abstract requestId: option<string> with get, set
        abstract submissionId: string with get, set

    type DeviceScaleFactorHeightWidth =
        abstract deviceScaleFactor: option<float> with get, set
        abstract height: float with get, set
        abstract width: float with get, set

    type AgentNamePayloadTimestampType84 =
        abstract timestamp: float with get, set
        abstract payload: WorkflowIdWorkflowName with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AudienceLastModifiedPriority3 =
        abstract lastModified: Zod.ZodType with get, set
        abstract priority: Zod.ZodType with get, set
        abstract audience: Zod.ZodType with get, set

    type CreateMessage =
        abstract createMessage: option<Erased.Empty> with get, set

    type SymbolSearch =
        abstract ``[symbol.search]``: string: string -> float

    type ErrorLimitSealedStrikes =
        abstract error: string with get, set
        abstract sealed: bool with get, set
        abstract limit: float with get, set
        abstract strikes: float with get, set

    type ApplyDefaults =
        abstract applyDefaults: option<bool> with get, set

    type ExecutionId4 =
        abstract executionId: string with get, set

    type ContextTools =
        abstract tools: option<Erased.Empty> with get, set
        abstract context: option<Erased.Empty> with get, set

    type AgentNamePayloadTimestampType4 =
        abstract timestamp: float with get, set
        abstract payload: CallbackId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType58 =
        abstract timestamp: float with get, set
        abstract payload: BudgetBytesHydratedMessagesD0eeb8cc with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type FiberIdFiberNameManagedRecoveryReason =
        abstract recoveryReason: string with get, set
        abstract managed: option<bool> with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type AuthorizationModel =
        abstract authorization: string with get, set
        abstract model: string with get, set

    type SymbolReplace =
        abstract ``[symbol.replace]``: string: string * replaceValue: string -> string

    type ErrorIdJsonrpc =
        abstract id: option<U2<string, float>> with get, set
        abstract error: CodeDataMessage with get, set
        abstract jsonrpc: string with get, set

    type ElicitationExperimentalExtFf9b7be3 =
        abstract extensions: option<obj> with get, set
        abstract tasks: option<CancelListRequests> with get, set
        abstract roots: option<ListChanged> with get, set
        abstract elicitation: option<FormUrl> with get, set
        abstract sampling: option<ContextTools> with get, set
        abstract experimental: option<obj> with get, set

    type AgentNamePayloadTimestampType16 =
        abstract timestamp: float with get, set
        abstract payload: ErrorRequestIdSubmissionId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaElicitationId20f972a8 =
        abstract task: option<Ttl> with get, set
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a> with get, set
        abstract url: string with get, set
        abstract elicitationId: string with get, set
        abstract message: string with get, set
        abstract mode: string with get, set

    type AgentNamePayloadTimestampType32 =
        abstract timestamp: float with get, set
        abstract payload: ChannelInformModelKind with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ChannelInformModelKind =
        abstract informModel: bool with get, set
        abstract kind: string with get, set
        abstract channel: string with get, set

    type AgentNamePayloadTimestampType2 =
        abstract timestamp: float with get, set
        abstract payload: ApprovedToolCallId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaAnnotationsDataMimeTypeType2 =
        abstract _meta: option<obj> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract mimeType: string with get, set
        abstract data: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type NameValue =
        abstract value: string with get, set
        abstract name: string with get, set

    type AgentNamePayloadTimestampType22 =
        abstract timestamp: float with get, set
        abstract payload: ActionAgeMsInputHashKey with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AdmissionContinuationDurat4f4f0774 =
        abstract error: option<string> with get, set
        abstract durationMs: float with get, set
        abstract status: string with get, set
        abstract generation: option<float> with get, set
        abstract continuation: option<bool> with get, set
        abstract admission: string with get, set
        abstract trigger: string with get, set
        abstract requestId: string with get, set

    type AnchorCommentDefsE87d3677 =
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract minItems: option<float> with get, set
        abstract minContains: option<float> with get, set
        abstract maxItems: option<float> with get, set
        abstract maxContains: option<float> with get, set
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract writeOnly: option<bool> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        abstract readOnly: option<bool> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e2, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e2, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        abstract format: option<string> with get, set
        abstract examples: option<U2<ResizeArray<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>, System.Collections.Generic.IReadOnlyList<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>>> with get, set
        abstract enum: option<U2<ResizeArray<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>, System.Collections.Generic.IReadOnlyList<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e2, bool>> with get, set

        abstract deprecated: option<bool> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e2, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e2, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e2, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e2, bool>>>> with get, set

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

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set
        abstract description: option<string> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<U33<unit -> seq<string>, ResizeArray<string> -> string, option<float> -> option<float> -> string, string -> option<float> -> float, string -> option<float> -> float, string -> option<float> -> bool, float -> option<string>, float -> string, float -> float, ThatThatLocalesF8ee7b5e, MatcherRegexp, SearchValueReplaceValueSeaF6f0a0aa, RegexpSearcher, SeparatorLimitSplitterLimit, float -> option<float> -> string, LocalesLocales, float -> option<float> -> string, float -> option<float>, string -> option<float> -> bool, FormForm, float -> string, string -> option<float> -> bool, string -> string, string -> string, SizeSize, string -> string, float -> option<string> -> string, obj -> seq<obj>, SearchValueReplaceValueSea30d3f247, unit -> bool, unit -> string, string, float>> with get, set

    type Timeout =
        abstract timeout: option<float> with get, set

    type AtDataFractionMessageMilestonePhase =
        abstract data: option<obj> with get, set
        abstract at: float with get, set
        abstract milestone: option<string> with get, set
        abstract phase: option<string> with get, set
        abstract message: option<string> with get, set
        abstract fraction: option<float> with get, set

    type _metaToolResult2 =
        abstract toolResult: Zod.ZodType with get, set
        abstract _meta: Zod.ZodType with get, set

    type ModeNames =
        abstract mode: option<LiteralUnions.NarrateReact> with get, set
        abstract names: ResizeArray<string> with get, set

    type Create =
        abstract create: option<Erased.Empty> with get, set

    type DoneValue =
        abstract value: option<unit> with get, set

        [<EmitProperty("done")>]
        abstract ``done``: bool with get, set

    type AdmissionContinuationGener9021d3af =
        abstract generation: option<float> with get, set
        abstract continuation: option<bool> with get, set
        abstract admission: string with get, set
        abstract trigger: string with get, set
        abstract requestId: string with get, set

    type AgentNamePayloadTimestampType3 =
        abstract timestamp: float with get, set
        abstract payload: CallbackId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AutoRefreshDebounceMsOnChanged =
        abstract debounceMs: option<float> with get, set
        abstract autoRefresh: option<bool> with get, set
        abstract onChanged: ?error: exn * ?items: ResizeArray<obj> -> unit

    type SymbolIteratorSymbolUnF8943d18<'T, 'Array, 'U, 'S, 'This, 'D, 'A> =
        [<EmitProperty("[Symbol.unscopables]")>]
        abstract ``[symbol.unscopables]``: _Iterator10155d5f234a with get

        abstract length: float with get, set
        abstract toString: unit -> string
        abstract toLocaleString: unit -> string
        abstract pop: unit -> option<obj>
        abstract push: [<ParamArray>] items: ResizeArray<'T> -> float
        abstract concat: [<ParamArray>] items: ResizeArray<System.Collections.Generic.IReadOnlyList<obj>> -> ResizeArray<'T>
        abstract join: ?separator: string -> string
        abstract reverse: unit -> ResizeArray<'T>
        abstract shift: unit -> option<obj>
        abstract slice: ?start: float * ?``end``: float -> ResizeArray<'T>
        abstract sort: ?compareFn: (obj -> obj -> float) -> 'Array
        abstract splice: start: float * ?deleteCount: float -> ResizeArray<'T>
        abstract unshift: [<ParamArray>] items: ResizeArray<'T> -> float
        abstract indexOf: searchElement: obj * ?fromIndex: float -> float
        abstract lastIndexOf: searchElement: obj * ?fromIndex: float -> float
        abstract every: predicate: (obj -> float -> ResizeArray<'T> -> bool) * ?thisArg: obj -> bool
        abstract some: predicate: (obj -> float -> ResizeArray<'T> -> option<obj>) * ?thisArg: obj -> bool
        abstract forEach: callbackfn: (obj -> float -> ResizeArray<'T> -> unit) * ?thisArg: obj -> unit
        abstract map: callbackfn: (obj -> float -> ResizeArray<'T> -> 'U) * ?thisArg: obj -> ResizeArray<'U>
        abstract filter<'S> : predicate: (obj -> float -> ResizeArray<'T> -> bool) * ?thisArg: obj -> ResizeArray<'S>
        abstract reduce: callbackfn: (obj -> obj -> float -> ResizeArray<'T> -> obj) -> obj
        abstract reduceRight: callbackfn: (obj -> obj -> float -> ResizeArray<'T> -> obj) -> obj
        abstract find: predicate: (obj -> float -> ResizeArray<'T> -> bool) * ?thisArg: obj -> option<'S>
        abstract findIndex: predicate: (obj -> float -> ResizeArray<'T> -> option<obj>) * ?thisArg: obj -> float
        abstract fill: value: obj * ?start: float * ?``end``: float -> 'Array
        abstract copyWithin: target: float * start: float * ?``end``: float -> 'Array
        abstract entries: unit -> seq<float * obj>
        abstract keys: unit -> seq<float>
        abstract values: unit -> seq<obj>
        abstract includes: searchElement: obj * ?fromIndex: float -> bool
        abstract flatMap: callback: ('This -> obj -> float -> ResizeArray<'T> -> U2<'U, System.Collections.Generic.IReadOnlyList<'U>>) * ?thisArg: 'This -> ResizeArray<'U>
        abstract flat: this: 'A * ?depth: 'D -> ResizeArray<proptypekey<DoneRecur<'A, obj, obj, obj>, LiteralUnions.DoneRecur>>
        abstract at: index: float -> option<obj>
        abstract findLast: predicate: (obj -> float -> ResizeArray<'T> -> bool) * ?thisArg: obj -> option<'S>
        abstract findLastIndex: predicate: (obj -> float -> ResizeArray<'T> -> option<obj>) * ?thisArg: obj -> float
        abstract toReversed: unit -> ResizeArray<'T>
        abstract toSorted: ?compareFn: (obj -> obj -> float) -> ResizeArray<'T>
        abstract toSpliced: start: float * deleteCount: float * [<ParamArray>] items: ResizeArray<'T> -> ResizeArray<'T>
        abstract ``with``: index: float * value: obj -> ResizeArray<'T>
        abstract ``[symbol.iterator]``: unit -> seq<obj>

    type AtDataNameSequence =
        abstract data: option<obj> with get, set
        abstract at: float with get, set
        abstract sequence: float with get, set
        abstract name: string with get, set

    type AgentNamePayloadTimestampType42 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsFiberIdFiberName029ca7c8 with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaContentIsErrorStructuredContent3 =
        abstract isError: Zod.ZodType with get, set
        abstract structuredContent: Zod.ZodType with get, set
        abstract content: Zod.ZodType with get, set
        abstract _meta: Zod.ZodType with get, set

    type SearchValueReplaceValueSeaF6f0a0aa =
        abstract Invoke: searchValue: U2<obj, string> * replaceValue: string -> string
        abstract Invoke: searchValue: U2<obj, string> * replacer: (string -> ResizeArray<option<obj>> -> string) -> string
        abstract Invoke: searchValue: SymbolReplace * replaceValue: string -> string
        abstract Invoke: searchValue: SymbolReplace2 * replacer: (string -> ResizeArray<option<obj>> -> string) -> string

    type ConnectionId =
        abstract connectionId: string with get, set

    type BindingKindNameVersion =
        abstract name: string with get, set
        abstract binding: string with get, set
        abstract version: int with get, set
        abstract kind: string with get, set

    type MetadataSnapshotStatus =
        abstract metadata: option<obj> with get, set
        abstract snapshot: option<obj> with get, set
        abstract status: string with get, set

    type AttemptsCallbackErrorId =
        abstract attempts: float with get, set
        abstract error: string with get, set
        abstract id: string with get, set
        abstract callback: string with get, set

    type AttemptIncidentIdMaxAttempE654a98e =
        abstract recoveryKind: LiteralUnions.ContinueRetry with get, set
        abstract maxAttempts: float with get, set
        abstract attempt: float with get, set
        abstract requestId: string with get, set
        abstract incidentId: string with get, set

    type _metaAnnotationsE54c02692 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme2>> with get, set
        abstract _meta: option<System.Collections.Generic.IDictionary<string, option<obj>>> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract size: option<float> with get, set
        abstract mimeType: option<string> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract uri: string with get, set

    type AuthErrorAuthSuccessServerId2 =
        abstract authError: string with get, set
        abstract authSuccess: bool with get, set
        abstract serverId: option<string> with get, set

    type AgentNamePayloadTimestampType80 =
        abstract timestamp: float with get, set
        abstract payload: ReasonWorkflowId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type RelatedRequestId =
        abstract relatedRequestId: option<obj> with get, set

    type _secureRoutedForward1f3e6318 =
        abstract _secureRouted: option<bool> with get, set
        abstract rawSize: float with get, set
        abstract headers: obj with get, set

        [<EmitProperty("to")>]
        abstract ``to``: string with get, set

        abstract from: string with get, set
        abstract getRaw: unit -> Promise<Uint8Array>
        abstract setReject: reason: string -> unit
        abstract forward: rcptTo: string * ?headers: obj -> Promise<obj>
        abstract reply: options: FromRawTo -> Promise<obj>

    type AgentNamePayloadTimestampType50 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttempE654a98e with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AttemptCallbackIdMaxAttempts =
        abstract maxAttempts: float with get, set
        abstract attempt: float with get, set
        abstract id: string with get, set
        abstract callback: string with get, set

    type AgentNamePayloadTimestampType =
        abstract timestamp: float with get, set
        abstract payload: obj with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ElapsedMsFiberIdFiberNameDffcba4f =
        abstract recoveryReason: string with get, set
        abstract managed: option<bool> with get, set
        abstract elapsedMs: option<float> with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type BodyKindRunId =
        abstract body: string with get, set
        abstract runId: string with get, set
        abstract kind: string with get, set

    type AgentNamePayloadTimestampType77 =
        abstract timestamp: float with get, set
        abstract payload: WorkflowIdWorkflowName with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType11 =
        abstract timestamp: float with get, set
        abstract payload: AttemptCallbackIdMaxAttempts with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type Type3 =
        [<EmitProperty("type")>]
        abstract ``type``: Type3.Type with get, set

    type WorkflowIdWorkflowName =
        abstract workflowName: option<string> with get, set
        abstract workflowId: string with get, set

    type _metaBlobMimeTypeUri2 =
        abstract blob: Zod.ZodType with get, set
        abstract _meta: Zod.ZodType with get, set
        abstract mimeType: Zod.ZodType with get, set
        abstract uri: Zod.ZodType with get, set

    type ActionExecutionId =
        abstract executionId: string with get, set
        abstract action: string with get, set

    type _metaDescriptionMessages =
        abstract description: option<string> with get, set
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a3> with get, set
        abstract messages: ResizeArray<ContentRole4> with get, set
        abstract Item: x: string -> option<obj>

    type AgentNamePayloadTimestampType6 =
        abstract timestamp: float with get, set
        abstract payload: AttemptCallbackIdMaxAttempts with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentTypeBudgetMsRunId =
        abstract budgetMs: float with get, set
        abstract agentType: string with get, set
        abstract runId: string with get, set

    type InOut6 =
        [<EmitProperty("in")>]
        abstract ``in``: obj with get, set

        abstract out: obj with get, set

    type _metaActionContent =
        abstract content: option<obj> with get, set
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a> with get, set
        abstract action: LiteralUnions.AcceptCancelDecline with get, set

    type AgentNamePayloadTimestampType69 =
        abstract timestamp: float with get, set
        abstract payload: CallbackErrorKindRunIdStatus with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AuthProviderEventSourceInitE02b5685 =
        abstract requestInit: option<obj> with get, set
        abstract eventSourceInit: option<Erased.Eventsource> with get, set
        abstract authProvider: option<obj> with get, set
        abstract fetch: url: U2<obj, string> * ?init: obj -> Promise<obj>

    type FormUrl =
        abstract url: option<Erased.Empty> with get, set
        abstract form: option<ApplyDefaults> with get, set

    type MimeTypeSizesSrcTheme2 =
        abstract theme: option<LiteralUnions.DarkLight> with get, set
        abstract sizes: option<ResizeArray<string>> with get, set
        abstract mimeType: option<string> with get, set
        abstract src: string with get, set

    type CancelListRequests =
        abstract requests: option<ElicitationSampling> with get, set
        abstract cancel: option<Erased.Empty> with get, set
        abstract list: option<Erased.Empty> with get, set

    type AgentNamePayloadTimestampType25 =
        abstract timestamp: float with get, set
        abstract payload: ActionExecutionIdToolCallId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType34 =
        abstract timestamp: float with get, set
        abstract payload: obj with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaAnnotationsDataMimeTypeType3 =
        abstract _meta: Zod.ZodType with get, set
        abstract annotations: Zod.ZodType with get, set
        abstract mimeType: Zod.ZodType with get, set
        abstract data: Zod.ZodType with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Zod.ZodType with get, set

    type AgentTypeDisplayInputPreviC703c0a7 =
        abstract display: option<Erased.Empty> with get, set
        abstract order: float with get, set
        abstract inputPreview: option<obj> with get, set
        abstract agentType: string with get, set
        abstract runId: string with get, set
        abstract kind: string with get, set

    type DescriptionExecuteInputSchC4e21aff =
        abstract replay: option<LiteralUnions.LogReexecute> with get, set
        abstract requiresApproval: option<bool> with get, set
        abstract outputSchema: option<Erased.JsonSchema> with get, set
        abstract inputSchema: option<Erased.JsonSchema> with get, set
        abstract description: option<string> with get, set
        abstract execute: ?args: obj * ?ctx: Erased.Empty -> option<U2<Promise<option<obj>>, obj>>
        abstract revert: ?args: obj * ?result: obj * ?ctx: Erased.Empty -> option<Promise<unit>>

    type _metaContents =
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a3> with get, set
        abstract contents: ResizeArray<U2<_metaMimeTypeTextUri, _metaBlobMimeTypeUri>> with get, set
        abstract Item: x: string -> option<obj>

    type AgentNamePayloadTimestampType49 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttempE654a98e with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type MimeTypeSizesSrcTheme =
        abstract theme: option<LiteralUnions.DarkLight> with get, set
        abstract sizes: option<ResizeArray<string>> with get, set
        abstract mimeType: option<string> with get, set
        abstract src: string with get, set

    type ChannelError =
        abstract error: string with get, set
        abstract channel: string with get, set

    type AuthErrorAuthSuccessServerId =
        abstract authError: option<unit> with get, set
        abstract authSuccess: bool with get, set
        abstract serverId: string with get, set

    type ActionAttachmentType =
        abstract attachmentType: string with get, set
        abstract action: option<string> with get, set

    type RunCountTotalTimeoutMs =
        abstract totalTimeoutMs: option<float> with get, set
        abstract runCount: float with get, set

    type IconName =
        abstract icon: option<string> with get, set
        abstract name: option<string> with get, set

    type MethodStreaming =
        abstract streaming: option<bool> with get, set
        abstract method: string with get, set

    type IoModelcontextprotocolRelaF70aae9a2 =
        [<EmitProperty("io.modelcontextprotocol/related-task")>]
        abstract ``io.modelcontextprotocol/relatedTask``: Zod.ZodType with get, set

        abstract progressToken: Zod.ZodType with get, set

    type ServerId =
        abstract serverId: string with get, set

    type BccCcFrom4e00bdc5 =
        abstract html: option<string> with get, set
        abstract text: option<string> with get, set
        abstract headers: option<obj> with get, set
        abstract bcc: option<U2<ResizeArray<string>, string>> with get, set
        abstract cc: option<U2<ResizeArray<string>, string>> with get, set
        abstract replyTo: option<U2<EmailName, string>> with get, set
        abstract subject: string with get, set

        [<EmitProperty("to")>]
        abstract ``to``: U2<ResizeArray<string>, string> with get, set

        abstract from: U2<EmailName, string> with get, set

    type DescriptionNameRequired2 =
        abstract required: option<bool> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set

    type AudienceLastModifiedPriority =
        abstract lastModified: option<string> with get, set
        abstract priority: option<float> with get, set
        abstract audience: option<ResizeArray<obj>> with get, set

    type AgentNamePayloadTimestampType85 =
        abstract timestamp: float with get, set
        abstract payload: WorkflowIdWorkflowName with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaAnnotations08cb50cb =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme>> with get, set
        abstract _meta: option<obj> with get, set
        abstract execution: option<TaskSupport> with get, set
        abstract annotations: option<DestructiveHintIdempotentHi8e2ddc36> with get, set
        abstract outputSchema: option<PropertiesRequiredType3> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract inputSchema: PropertiesRequiredType3 with get, set

    type SymbolIteratorSymbolUn2cf8c030<'T, 'U, 'S, 'This, 'D, 'A> =
        [<EmitProperty("[Symbol.unscopables]")>]
        abstract ``[symbol.unscopables]``: _Iterator1015F2ef9c1e with get

        abstract length: float with get
        abstract toString: unit -> string
        abstract toLocaleString: unit -> string
        abstract concat: [<ParamArray>] items: ResizeArray<System.Collections.Generic.IReadOnlyList<obj>> -> ResizeArray<obj>
        abstract join: ?separator: string -> string
        abstract slice: ?start: float * ?``end``: float -> ResizeArray<obj>
        abstract indexOf: searchElement: obj * ?fromIndex: float -> float
        abstract lastIndexOf: searchElement: obj * ?fromIndex: float -> float
        abstract every: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> bool) * ?thisArg: obj -> bool
        abstract some: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> option<obj>) * ?thisArg: obj -> bool
        abstract forEach: callbackfn: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> unit) * ?thisArg: obj -> unit
        abstract map: callbackfn: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> 'U) * ?thisArg: obj -> ResizeArray<'U>
        abstract filter: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> bool) * ?thisArg: obj -> ResizeArray<'S>
        abstract reduce: callbackfn: (obj -> obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> obj) -> obj
        abstract reduceRight: callbackfn: (obj -> obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> obj) -> obj
        abstract find: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> bool) * ?thisArg: obj -> option<'S>
        abstract findIndex: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> option<obj>) * ?thisArg: obj -> float
        abstract entries: unit -> seq<float * obj>
        abstract keys: unit -> seq<float>
        abstract values: unit -> seq<obj>
        abstract includes: searchElement: obj * ?fromIndex: float -> bool
        abstract flatMap: callback: ('This -> obj -> float -> ResizeArray<obj> -> U2<'U, System.Collections.Generic.IReadOnlyList<'U>>) * ?thisArg: 'This -> ResizeArray<'U>
        abstract flat: this: 'A * ?depth: 'D -> ResizeArray<proptypekey<DoneRecur<'A, obj, obj, obj>, LiteralUnions.DoneRecur>>
        abstract at: index: float -> option<obj>
        abstract findLast: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> bool) * ?thisArg: obj -> option<'S>
        abstract findLastIndex: predicate: (obj -> float -> System.Collections.Generic.IReadOnlyList<'T> -> option<obj>) * ?thisArg: obj -> float
        abstract toReversed: unit -> ResizeArray<obj>
        abstract toSorted: ?compareFn: (obj -> obj -> float) -> ResizeArray<obj>
        abstract toSpliced: start: float * deleteCount: float * [<ParamArray>] items: ResizeArray<obj> -> ResizeArray<obj>
        abstract ``with``: index: float * value: obj -> ResizeArray<obj>
        abstract ``[symbol.iterator]``: unit -> seq<obj>

    type WebSocketConnectionTimeout_ca64dcf12 =
        abstract debug: option<bool> with get, set
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

        abstract shouldReconnectOnClose: event: Erased.Empty -> bool
        abstract debugLogger: [<ParamArray>] args: ResizeArray<option<obj>> -> unit

    type MethodParams =
        abstract params: option<_meta2> with get, set
        abstract method: string with get, set

    type AssistantUser =
        abstract assistant: string with get, set
        abstract user: string with get, set

    type AuthUrlClientIdState =
        abstract clientId: option<string> with get, set
        abstract authUrl: string with get, set
        abstract state: string with get, set

    type AgentNamePayloadTimestampType62 =
        abstract timestamp: float with get, set
        abstract payload: RunCountTotalTimeoutMs with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ErrorState =
        abstract error: string with get, set
        abstract state: string with get, set

    type _metaArgumentsE5be7b5f2 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme2>> with get, set
        abstract _meta: option<System.Collections.Generic.IDictionary<string, option<obj>>> with get, set
        abstract arguments: option<ResizeArray<DescriptionNameRequired2>> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set

    type _metaAnnotationsE54c0269 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme>> with get, set
        abstract _meta: option<obj> with get, set
        abstract annotations: option<AudienceLastModifiedPriority> with get, set
        abstract size: option<float> with get, set
        abstract mimeType: option<string> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set
        abstract uri: string with get, set

    type CallbackErrorKindRunIdStatus =
        abstract error: string with get, set
        abstract callback: option<string> with get, set
        abstract status: string with get, set
        abstract kind: LiteralUnions.FinishGiveUp with get, set
        abstract runId: string with get, set

    type AnchorCommentDefs0800bb6e8 =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e8, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e8, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e8, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e8, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e8, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<obj, System.Collections.Generic.IReadOnlyList<float>>> with get, set
        abstract enum: option<U2<obj, System.Collections.Generic.IReadOnlyList<float>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e8, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<float> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e8, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e8, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<float> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e8, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e8, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e8, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e8, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type ElapsedMsRunCount =
        abstract elapsedMs: option<float> with get, set
        abstract runCount: float with get, set

    type DescriptionIconsName444a27cf2 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme>> with get, set
        abstract description: option<string> with get, set
        abstract websiteUrl: option<string> with get, set
        abstract name: string with get, set
        abstract version: string with get, set

    type DarkLight =
        abstract dark: string with get, set
        abstract light: string with get, set

    type BytesExternalizedBytesMessagesParts =
        abstract externalizedBytes: float with get, set
        abstract bytes: float with get, set
        abstract parts: float with get, set
        abstract messages: float with get, set

    type AgentNamePayloadTimestampType73 =
        abstract timestamp: float with get, set
        abstract payload: CodeConnectionIdReason with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type InOut =
        [<EmitProperty("in")>]
        abstract ``in``: obj with get, set

        abstract out: obj with get, set

    type _metaToolResult =
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a3> with get, set
        abstract toolResult: option<obj> with get, set
        abstract Item: x: string -> option<obj>

    type AnchorCommentDefs0800bb6e3<'Value, 'SchemaType> =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<'SchemaType> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<Erased.JsonSchemaTyped> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<Erased.JsonSchemaTyped>, System.Collections.Generic.IReadOnlyList<Erased.JsonSchemaTyped>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<Erased.JsonSchemaTyped> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<Erased.JsonSchemaTyped> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<'Value>, System.Collections.Generic.IReadOnlyList<'Value>>> with get, set
        abstract enum: option<U2<ResizeArray<'Value>, System.Collections.Generic.IReadOnlyList<'Value>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<Erased.JsonSchemaTyped> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        /// <deprecated>
        /// `dependencies` has been split into two keywords:<br/>
        /// `dependentSchemas` and `dependentRequired`.
        /// </deprecated>
        abstract dependencies: option<obj> with get, set
        /// <deprecated>
        /// `definitions` has been renamed to `$defs`.
        /// </deprecated>
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<'Value> with get, set

        abstract contentSchema: option<Erased.JsonSchemaTyped> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<Erased.JsonSchemaTyped> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<'Value> with get, set

        abstract anyOf: option<U2<ResizeArray<Erased.JsonSchemaTyped>, System.Collections.Generic.IReadOnlyList<Erased.JsonSchemaTyped>>> with get, set
        abstract allOf: option<U2<ResizeArray<Erased.JsonSchemaTyped>, System.Collections.Generic.IReadOnlyList<Erased.JsonSchemaTyped>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        /// <deprecated>
        /// `additionalItems` has been deprecated in favor of `prefixItems`<br/>
        /// paired with `items`.
        /// </deprecated>
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type ErrorPhaseStateTransportUrl =
        abstract phase: option<LiteralUnions.ClientCloseTerminateSession> with get, set
        abstract error: option<string> with get, set
        abstract state: string with get, set
        abstract transport: option<string> with get, set
        abstract url: string with get, set

    type TaskSupport2 =
        abstract taskSupport: option<LiteralUnions.ForbiddenOptionalRequired> with get, set

    type PendingSettled =
        abstract pending: float with get, set
        abstract settled: float with get, set

    type CallbackId =
        abstract id: string with get, set
        abstract callback: string with get, set

    type SchemaType =
        abstract schema: option<obj> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType44 =
        abstract timestamp: float with get, set
        abstract payload: ErrorMessagesPersistedRequestIdStage with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType83 =
        abstract timestamp: float with get, set
        abstract payload: WorkflowIdWorkflowName with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ErrorServerIdValid =
        abstract error: option<string> with get, set
        abstract serverId: option<string> with get, set
        abstract valid: bool with get, set

    type _metaAnnotationsA03c3494 =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme2>> with get, set
        abstract _meta: option<System.Collections.Generic.IDictionary<string, option<obj>>> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract size: option<float> with get, set
        abstract mimeType: option<string> with get, set
        abstract description: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

        abstract name: string with get, set
        abstract uri: string with get, set

    type AgentNamePayloadTimestampType60 =
        abstract timestamp: float with get, set
        abstract payload: RequestIdTimeoutMs with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type Send2 =
        abstract send: message: U2<obj, BccCcFrom4e00bdc5> -> Promise<obj>

    type AgentNamePayloadTimestampType82 =
        abstract timestamp: float with get, set
        abstract payload: WorkflowIdWorkflowName with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType52 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttemp048494fb with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType8 =
        abstract timestamp: float with get, set
        abstract payload: CallbackCountType with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type TaskId3 =
        abstract taskId: string with get, set

    type AnnotationsDescriptorsInstBcba505e =
        abstract annotations: option<obj> with get, set
        abstract descriptors: Erased.Empty with get, set
        abstract instructions: option<string> with get, set
        abstract name: string with get, set

    type ErrorStep =
        abstract error: string with get, set
        abstract step: LiteralUnions.DurableWorkRecoverySchedu9703c103 with get, set

    type DestructiveHintIdempotentHi8e2ddc362 =
        abstract openWorldHint: option<bool> with get, set
        abstract idempotentHint: option<bool> with get, set
        abstract destructiveHint: option<bool> with get, set
        abstract readOnlyHint: option<bool> with get, set
        abstract title: option<string> with get, set

    type AnchorCommentDefs0800bb6e7 =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e7, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e7, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e7, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e7, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e7, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<unit>, System.Collections.Generic.IReadOnlyList<unit>>> with get, set
        abstract enum: option<U2<ResizeArray<unit>, System.Collections.Generic.IReadOnlyList<unit>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e7, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<unit> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e7, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e7, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<unit> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e7, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e7, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e7, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e7, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type CompletedAtCreatedAtError05b518ea =
        abstract completedAt: option<Date> with get, set
        abstract updatedAt: Date with get, set
        abstract createdAt: Date with get, set
        abstract error: option<MessageName> with get, set
        abstract metadata: option<obj> with get, set
        abstract status: obj with get, set
        abstract workflowName: string with get, set
        abstract workflowId: string with get, set
        abstract id: string with get, set

    type EndStart =
        [<EmitProperty("end")>]
        abstract ``end``: option<Date> with get, set

        abstract start: option<Date> with get, set

    type DescriptionIconsName444a27cf =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme>> with get, set
        abstract description: option<string> with get, set
        abstract websiteUrl: option<string> with get, set
        abstract name: string with get, set
        abstract version: string with get, set

    type ErrorStateTransportUrl =
        abstract error: option<string> with get, set
        abstract state: string with get, set
        abstract transport: string with get, set
        abstract url: string with get, set

    type SearchValueReplaceValueSea30d3f247 =
        abstract Invoke: searchValue: U2<obj, string> * replaceValue: string -> string
        abstract Invoke: searchValue: U2<obj, string> * replacer: (string -> ResizeArray<option<obj>> -> string) -> string

    type SymbolReplace2 =
        abstract ``[symbol.replace]``: string: string * replacer: (string -> ResizeArray<option<obj>> -> string) -> string

    type AgentNamePayloadTimestampType5 =
        abstract timestamp: float with get, set
        abstract payload: CallbackId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ReplayRequiresApproval =
        abstract replay: option<LiteralUnions.LogReexecute> with get, set
        abstract requiresApproval: option<bool> with get, set

    type AgentNamePayloadTimestampType41 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsFiberIdFiberNameFc0abbde with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType81 =
        abstract timestamp: float with get, set
        abstract payload: ReasonWorkflowId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType64 =
        abstract timestamp: float with get, set
        abstract payload: AgentTypeElapsedMsRunId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType68 =
        abstract timestamp: float with get, set
        abstract payload: Error with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaContentIsErrorStructuredContent =
        abstract isError: option<bool> with get, set
        abstract structuredContent: option<obj> with get, set
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a3> with get, set
        abstract content: ResizeArray<U5<_metaAnnotationsTextType, _metaAnnotationsDataMimeTypeType, _metaAnnotationsDataMimeTypeType2, _metaAnnotationsResourceType, _metaAnnotationsA03c3494>> with get, set
        abstract Item: x: string -> option<obj>

    type Args =
        abstract Create: [<ParamArray>] args: ResizeArray<option<obj>> -> option<obj>

    type Request =
        abstract request: obj with get, set

    type AgentNamePayloadTimestampType21 =
        abstract timestamp: float with get, set
        abstract payload: ActionInputHashKey with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type EmailName =
        abstract name: option<string> with get, set
        abstract email: string with get, set

    type AgentNamePayloadTimestampType90 =
        abstract timestamp: float with get, set
        abstract payload: CapabilityErrorStateUrl with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentTypeElapsedMsRunId =
        abstract elapsedMs: option<float> with get, set
        abstract agentType: string with get, set
        abstract runId: string with get, set

    type AgentNamePayloadTimestampType91 =
        abstract timestamp: float with get, set
        abstract payload: ErrorPhaseStateTransportUrl with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ClassNameName =
        abstract name: string with get, set
        abstract className: string with get, set

    type AgentNamePayloadTimestampType72 =
        abstract timestamp: float with get, set
        abstract payload: ConnectionId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type CallTool =
        abstract callTool: method: string * ?args: obj -> Promise<option<obj>>

    type AgentNamePayloadTimestampType61 =
        abstract timestamp: float with get, set
        abstract payload: AttemptReasonRequestIdShortened with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type TaskId =
        abstract taskId: string with get, set

    type FieldErrorsFormErrors<'U> =
        abstract fieldErrors: obj with get, set
        abstract formErrors: ResizeArray<'U> with get, set

    type CodeContinueInputInstMessagePath =
        abstract continue: option<proptypekey<obj, string>> with get, set
        abstract inst: option<proptypekey<obj, string>> with get, set
        abstract path: option<proptypekey<obj, string>> with get, set
        abstract message: option<proptypekey<obj, string>> with get, set
        abstract input: proptypekey<obj, string> with get, set
        abstract code: option<proptypekey<CodeContinueInputInstMessagePath.Code, string>> with get, set

    type AgentNamePayloadTimestampType7 =
        abstract timestamp: float with get, set
        abstract payload: AttemptsCallbackErrorId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _Iterator10155d5f234a =
        [<EmitProperty("__@unscopables@1017")>]
        abstract ``_@unscopables@1017``: option<bool> with get, set

        [<EmitProperty("__@iterator@1015")>]
        abstract ``_@iterator@1015``: option<bool> with get, set

        [<EmitProperty("with")>]
        abstract ``with``: option<bool> with get, set

        abstract toSpliced: option<bool> with get, set
        abstract toSorted: option<bool> with get, set
        abstract toReversed: option<bool> with get, set
        abstract findLastIndex: option<bool> with get, set
        abstract findLast: option<bool> with get, set
        abstract at: option<bool> with get, set
        abstract flat: option<bool> with get, set
        abstract flatMap: option<bool> with get, set
        abstract includes: option<bool> with get, set
        abstract values: option<bool> with get, set
        abstract keys: option<bool> with get, set
        abstract entries: option<bool> with get, set
        abstract copyWithin: option<bool> with get, set
        abstract fill: option<bool> with get, set
        abstract findIndex: option<bool> with get, set
        abstract find: option<bool> with get, set
        abstract reduceRight: option<bool> with get, set
        abstract reduce: option<bool> with get, set
        abstract filter: option<bool> with get, set
        abstract map: option<bool> with get, set
        abstract forEach: option<bool> with get, set
        abstract some: option<bool> with get, set
        abstract every: option<bool> with get, set
        abstract lastIndexOf: option<bool> with get, set
        abstract indexOf: option<bool> with get, set
        abstract unshift: option<bool> with get, set
        abstract splice: option<bool> with get, set
        abstract sort: option<bool> with get, set
        abstract slice: option<bool> with get, set
        abstract shift: option<bool> with get, set
        abstract reverse: option<bool> with get, set
        abstract join: option<bool> with get, set
        abstract concat: option<bool> with get, set
        abstract push: option<bool> with get, set
        abstract pop: option<bool> with get, set
        abstract toLocaleString: option<bool> with get, set
        abstract toString: option<bool> with get, set
        abstract length: option<bool> with get, set

    type AgentNamePayloadTimestampType79 =
        abstract timestamp: float with get, set
        abstract payload: EventTypeWorkflowId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AutoRefreshDebounceMsOnChanged2<'T> =
        abstract onChanged: obj with get, set
        abstract debounceMs: option<float> with get, set
        abstract autoRefresh: option<bool> with get, set

    type AgentNamePayloadTimestampType30 =
        abstract timestamp: float with get, set
        abstract payload: ChannelKindRequestId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type TaskId4 =
        abstract taskId: string with get, set

    type AgentNamePayloadTimestampType39 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsFiberIdFiberNameDffcba4f with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AttributesHeightHtml7c0a12ab =
        abstract attributes: ResizeArray<NameValue> with get, set
        abstract left: float with get, set
        abstract top: float with get, set
        abstract height: float with get, set
        abstract width: float with get, set
        abstract text: string with get, set
        abstract html: string with get, set

    type AgentNamePayloadTimestampType88 =
        abstract timestamp: float with get, set
        abstract payload: AuthUrlClientIdServerId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type PollIntervalTtl =
        abstract pollInterval: option<float> with get, set
        abstract ttl: option<float> with get, set

    type AgentTypeElapsedMsReasonRunIdStatus =
        abstract elapsedMs: option<float> with get, set
        abstract reason: option<string> with get, set
        abstract status: string with get, set
        abstract agentType: string with get, set
        abstract runId: string with get, set

    type AgentNamePayloadTimestampType38 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsFiberIdFiberNameDffcba4f with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type FromSubjectTo2 =
        abstract subject: string with get, set

        [<EmitProperty("to")>]
        abstract ``to``: U2<ResizeArray<string>, string> with get, set

        abstract from: string with get, set

    type _metaAnnotationsDataMimeTypeType4 =
        abstract _meta: Zod.ZodType with get, set
        abstract annotations: Zod.ZodType with get, set
        abstract mimeType: Zod.ZodType with get, set
        abstract data: Zod.ZodType with get, set

        [<EmitProperty("type")>]
        abstract ``type``: Zod.ZodType with get, set

    type IoModelcontextprotocolRelaF70aae9a =
        [<EmitProperty("io.modelcontextprotocol/related-task")>]
        abstract ``io.modelcontextprotocol/relatedTask``: option<TaskId> with get, set

        abstract progressToken: option<U2<string, float>> with get, set

    type AgentNamePayloadTimestampType43 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsErrorFiberIdFiberNameReason with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaAnnotationsDataMimeTypeType =
        abstract _meta: option<obj> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract mimeType: string with get, set
        abstract data: string with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ElapsedMsErrorFiberIdFiberNameReason =
        abstract reason: option<string> with get, set
        abstract elapsedMs: option<float> with get, set
        abstract error: string with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type HeadersType =
        [<EmitProperty("type")>]
        abstract ``type``: option<Erased.Empty> with get, set

        abstract headers: option<obj> with get, set

    type ConnectFetch =
        abstract fetch: input: U3<obj, obj, string> * ?init: obj -> Promise<obj>
        abstract connect: address: U2<obj, string> * ?options: obj -> obj

    type ClassNameName2 =
        abstract name: string with get, set
        abstract className: string with get, set

    type Swept =
        abstract swept: float with get, set

    type AgentNamePayloadTimestampType12 =
        abstract timestamp: float with get, set
        abstract payload: MethodStreaming with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type InOut5 = interface end

    type AgentNamePayloadTimestampType48 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttempE654a98e with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ContentRole4 =
        abstract content: U5<_metaAnnotationsTextType, _metaAnnotationsDataMimeTypeType, _metaAnnotationsDataMimeTypeType2, _metaAnnotationsResourceType, _metaAnnotationsA03c3494> with get, set
        abstract role: obj with get, set

    type _metaMimeTypeTextUri =
        abstract _meta: option<obj> with get, set
        abstract mimeType: option<string> with get, set
        abstract text: string with get, set
        abstract uri: string with get, set

    type AgentNamePayloadTimestampType24 =
        abstract timestamp: float with get, set
        abstract payload: PendingSettled with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AudienceLastModifiedPriority2 =
        abstract lastModified: option<string> with get, set
        abstract priority: option<float> with get, set
        abstract audience: option<ResizeArray<obj>> with get, set

    type AgentNamePayloadTimestampType31 =
        abstract timestamp: float with get, set
        abstract payload: ChannelKindTurnEnded with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType63 =
        abstract timestamp: float with get, set
        abstract payload: AgentTypeElapsedMsReasonRunIdStatus with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type FromSubjectTo =
        abstract subject: option<string> with get, set

        [<EmitProperty("to")>]
        abstract ``to``: string with get, set

        abstract from: string with get, set

    type ActionAgeMsInputHashKey =
        abstract ageMs: float with get, set
        abstract inputHash: string with get, set
        abstract key: string with get, set
        abstract action: string with get, set

    type AgentNamePayloadTimestampType13 =
        abstract timestamp: float with get, set
        abstract payload: AttemptsCallbackErrorId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ErrorMessagesPersistedRequestIdStage =
        abstract error: string with get, set
        abstract messagesPersisted: option<bool> with get, set
        abstract stage: LiteralUnions.ParsePersistRecovery9adb37d3 with get, set
        abstract requestId: option<string> with get, set

    type SymbolMatch =
        abstract ``[symbol.match]``: string: string -> option<obj>

    type AgentNamePayloadTimestampType87 =
        abstract timestamp: float with get, set
        abstract payload: ErrorStateTransportUrl with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type MimeTypeSizesSrcTheme3 =
        abstract theme: Zod.ZodType with get, set
        abstract sizes: Zod.ZodType with get, set
        abstract mimeType: Zod.ZodType with get, set
        abstract src: Zod.ZodType with get, set

    type _metaAnnotationsResourceType =
        abstract _meta: option<obj> with get, set
        abstract annotations: option<AudienceLastModifiedPriority2> with get, set
        abstract resource: U2<_metaMimeTypeTextUri, _metaBlobMimeTypeUri> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType76 =
        abstract timestamp: float with get, set
        abstract payload: FromSubjectTo2 with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ChannelKindRequestId =
        abstract requestId: option<string> with get, set
        abstract kind: string with get, set
        abstract channel: string with get, set

    type Ttl =
        abstract ttl: option<float> with get, set

    type _metaBlobMimeTypeUri =
        abstract _meta: option<obj> with get, set
        abstract mimeType: option<string> with get, set
        abstract blob: string with get, set
        abstract uri: string with get, set

    type AgentNamePayloadTimestampType19 =
        abstract timestamp: float with get, set
        abstract payload: ActionInputHashKey with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type LocalesLocales =
        abstract Invoke: ?locales: U2<ResizeArray<string>, string> -> string
        abstract Invoke: ?locales: Erased.Intl -> string

    type CodeConnectionIdReason =
        abstract reason: string with get, set
        abstract code: float with get, set
        abstract connectionId: string with get, set

    type RegexpSearcher =
        abstract Invoke: regexp: U2<obj, string> -> float
        abstract Invoke: searcher: SymbolSearch -> float

    type AgentNamePayloadTimestampType37 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsErrorFiberIdA169a092 with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaMimeTypeTextUri2 =
        abstract text: Zod.ZodType with get, set
        abstract _meta: Zod.ZodType with get, set
        abstract mimeType: Zod.ZodType with get, set
        abstract uri: Zod.ZodType with get, set

    type TaskSupport =
        abstract taskSupport: option<LiteralUnions.ForbiddenOptionalRequired> with get, set

    type AnchorCommentDefs0800bb6e9 =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e9, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e9, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e9, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e9, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e9, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<obj, System.Collections.Generic.IReadOnlyList<float>>> with get, set
        abstract enum: option<U2<obj, System.Collections.Generic.IReadOnlyList<float>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e9, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<float> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e9, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e9, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<float> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e9, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e9, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e9, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e9, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type ErrorMethod =
        abstract error: string with get, set
        abstract method: string with get, set

    type AgentNamePayloadTimestampType10 =
        abstract timestamp: float with get, set
        abstract payload: CallbackId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType26 =
        abstract timestamp: float with get, set
        abstract payload: ActionExecutionId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type MessageName =
        abstract message: string with get, set
        abstract name: string with get, set

    type AgentNamePayloadTimestampType18 =
        abstract timestamp: float with get, set
        abstract payload: ActionInputHashKey with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type DestructiveHintIdempotentHi8e2ddc36 =
        abstract openWorldHint: option<bool> with get, set
        abstract idempotentHint: option<bool> with get, set
        abstract destructiveHint: option<bool> with get, set
        abstract readOnlyHint: option<bool> with get, set
        abstract title: option<string> with get, set

    type IdJsonrpcMethodParams =
        abstract params: option<_meta2> with get, set
        abstract id: Zod.ZodType with get, set
        abstract jsonrpc: string with get, set
        abstract method: string with get, set

    type AgentNamePayloadTimestampType53 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttemp048494fb2 with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AttemptIncidentIdMaxAttemp048494fb =
        abstract reason: option<string> with get, set
        abstract recoveryKind: LiteralUnions.ContinueRetry with get, set
        abstract maxAttempts: float with get, set
        abstract attempt: float with get, set
        abstract requestId: string with get, set
        abstract incidentId: string with get, set

    type PropertiesRequiredType =
        abstract required: option<ResizeArray<string>> with get, set
        abstract properties: obj with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type PasswordUsername =
        abstract password: string with get, set
        abstract username: string with get, set

    type ReasonSnapshotStatus =
        abstract snapshot: option<obj> with get, set
        abstract reason: option<string> with get, set
        abstract status: string with get, set

    type _metaArgumentsE5be7b5f =
        abstract title: option<string> with get, set
        abstract icons: option<ResizeArray<MimeTypeSizesSrcTheme>> with get, set
        abstract _meta: option<obj> with get, set
        abstract arguments: option<ResizeArray<DescriptionNameRequired>> with get, set
        abstract description: option<string> with get, set
        abstract name: string with get, set

    type CancelListRequests2 =
        abstract requests: option<Tools> with get, set
        abstract cancel: option<Erased.Empty> with get, set
        abstract list: option<Erased.Empty> with get, set

    type _meta =
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a> with get, set

    type ChildStillRunningErrorKind669e1342 =
        abstract childStillRunning: option<bool> with get, set
        abstract reason: option<Erased.Empty> with get, set
        abstract error: string with get, set
        abstract runId: string with get, set
        abstract kind: string with get, set

    type FromRawTo =
        abstract raw: string with get, set

        [<EmitProperty("to")>]
        abstract ``to``: string with get, set

        abstract from: string with get, set

    type AnchorCommentDefs0800bb6e5 =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<string> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e5, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e5, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e5, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e5, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e5, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract enum: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e5, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<string> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e5, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e5, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<string> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e5, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e5, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e5, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e5, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type Call =
        abstract call: option<Erased.Empty> with get, set

    type AgentNamePayloadTimestampType74 =
        abstract timestamp: float with get, set
        abstract payload: FromSubjectTo with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ElapsedMsFiberIdFiberNameFc0abbde =
        abstract managed: option<bool> with get, set
        abstract elapsedMs: option<float> with get, set
        abstract status: option<string> with get, set
        abstract fiberName: string with get, set
        abstract fiberId: string with get, set

    type _secureRoutedAgentIdAgentName =
        abstract _secureRouted: option<bool> with get, set
        abstract agentId: string with get, set
        abstract agentName: string with get, set

    type AgentNamePayloadTimestampType36 =
        abstract timestamp: float with get, set
        abstract payload: ElapsedMsFiberIdFiberNameManaged with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AgentNamePayloadTimestampType40 =
        abstract timestamp: float with get, set
        abstract payload: FiberIdFiberNameManagedRecoveryReason with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _metaCapabilities64dfe903 =
        abstract _meta: option<IoModelcontextprotocolRelaF70aae9a> with get, set
        abstract clientInfo: DescriptionIconsName444a27cf with get, set
        abstract capabilities: ElicitationExperimentalExtFf9b7be32 with get, set
        abstract protocolVersion: string with get, set

    type AgentNamePayloadTimestampType86 =
        abstract timestamp: float with get, set
        abstract payload: ServerId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type AnchorCommentDefs0800bb6e =
        abstract writeOnly: option<bool> with get, set
        abstract uniqueItems: option<bool> with get, set
        abstract unevaluatedProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract unevaluatedItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: option<Erased.JsonSchemaTyped> with get, set

        abstract title: option<string> with get, set

        [<EmitProperty("then")>]
        abstract ``then``: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract required: option<U2<ResizeArray<string>, System.Collections.Generic.IReadOnlyList<string>>> with get, set
        abstract readOnly: option<bool> with get, set
        abstract propertyNames: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract properties: option<obj> with get, set
        abstract prefixItems: option<U4<AnchorCommentDefs0800bb6e, ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>, bool>> with get, set
        abstract patternProperties: option<obj> with get, set
        abstract pattern: option<string> with get, set
        abstract oneOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>>> with get, set

        [<EmitProperty("not")>]
        abstract ``not``: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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
        abstract items: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("if")>]
        abstract ``if``: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract format: option<string> with get, set
        abstract exclusiveMinimum: option<float> with get, set
        abstract exclusiveMaximum: option<float> with get, set
        abstract examples: option<U2<ResizeArray<option<obj>>, System.Collections.Generic.IReadOnlyList<option<obj>>>> with get, set
        abstract enum: option<U2<ResizeArray<option<obj>>, System.Collections.Generic.IReadOnlyList<option<obj>>>> with get, set

        [<EmitProperty("else")>]
        abstract ``else``: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        abstract description: option<string> with get, set
        abstract deprecated: option<bool> with get, set
        abstract dependentSchemas: option<obj> with get, set
        abstract dependentRequired: option<obj> with get, set
        abstract dependencies: option<obj> with get, set
        abstract definitions: option<obj> with get, set

        [<EmitProperty("default")>]
        abstract ``default``: option<obj> with get, set

        abstract contentSchema: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract contentMediaType: option<string> with get, set
        abstract contentEncoding: option<LiteralUnions.``7bit8bitBase64C35a02b4``> with get, set
        abstract contains: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

        [<EmitProperty("const")>]
        abstract ``const``: option<obj> with get, set

        abstract anyOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>>> with get, set
        abstract allOf: option<U2<ResizeArray<U2<AnchorCommentDefs0800bb6e, bool>>, System.Collections.Generic.IReadOnlyList<U2<AnchorCommentDefs0800bb6e, bool>>>> with get, set
        abstract additionalProperties: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set
        abstract additionalItems: option<U2<AnchorCommentDefs0800bb6e, bool>> with get, set

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

    type EventTypeWorkflowId =
        abstract eventType: option<string> with get, set
        abstract workflowId: string with get, set

    type SeparatorLimitSplitterLimit =
        abstract Invoke: separator: U2<obj, string> * ?limit: float -> ResizeArray<string>
        abstract Invoke: splitter: SymbolSplit * ?limit: float -> ResizeArray<string>

    type AgentNamePayloadTimestampType51 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttempE654a98e with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type Error =
        abstract error: string with get, set

    type MethodParams3 =
        abstract params: obj with get, set
        abstract method: string with get, set

    type AgentNamePayloadTimestampType54 =
        abstract timestamp: float with get, set
        abstract payload: AttemptIncidentIdMaxAttemp048494fb with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type LiveCountThreshold =
        abstract threshold: float with get, set
        abstract liveCount: float with get, set

    type _metaContentIsErrorStructuredContent2<'T> =
        abstract isError: Zod.ZodType with get, set
        abstract structuredContent: Zod.ZodType with get, set
        abstract content: Zod.ZodType with get, set
        abstract _meta: Zod.ZodType with get, set

    type CapabilitiesJsonSchemaValid8be6632a =
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
        abstract listChanged: option<obj> with get, set
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
        abstract jsonSchemaValidator: option<obj> with get, set
        abstract capabilities: option<obj> with get, set

    type AgentNamePayloadTimestampType65 =
        abstract timestamp: float with get, set
        abstract payload: AgentTypeBudgetMsRunId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type _Iterator1015F2ef9c1e =
        [<EmitProperty("__@unscopables@1017")>]
        abstract ``_@unscopables@1017``: option<bool> with get, set

        [<EmitProperty("__@iterator@1015")>]
        abstract ``_@iterator@1015``: option<bool> with get, set

        [<EmitProperty("with")>]
        abstract ``with``: option<bool> with get, set

        abstract toSpliced: option<bool> with get, set
        abstract toSorted: option<bool> with get, set
        abstract toReversed: option<bool> with get, set
        abstract findLastIndex: option<bool> with get, set
        abstract findLast: option<bool> with get, set
        abstract at: option<bool> with get, set
        abstract flat: option<bool> with get, set
        abstract flatMap: option<bool> with get, set
        abstract includes: option<bool> with get, set
        abstract values: option<bool> with get, set
        abstract keys: option<bool> with get, set
        abstract entries: option<bool> with get, set
        abstract findIndex: option<bool> with get, set
        abstract find: option<bool> with get, set
        abstract reduceRight: option<bool> with get, set
        abstract reduce: option<bool> with get, set
        abstract filter: option<bool> with get, set
        abstract map: option<bool> with get, set
        abstract forEach: option<bool> with get, set
        abstract some: option<bool> with get, set
        abstract every: option<bool> with get, set
        abstract lastIndexOf: option<bool> with get, set
        abstract indexOf: option<bool> with get, set
        abstract slice: option<bool> with get, set
        abstract join: option<bool> with get, set
        abstract concat: option<bool> with get, set
        abstract toLocaleString: option<bool> with get, set
        abstract toString: option<bool> with get, set
        abstract length: option<bool> with get, set

    type AgentNamePayloadTimestampType89 =
        abstract timestamp: float with get, set
        abstract payload: ToolCallIdToolName with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type TimeoutMs =
        abstract timeoutMs: option<float> with get, set

    type ListChangedSubscribe =
        abstract listChanged: option<bool> with get, set
        abstract subscribe: option<bool> with get, set

    type DebouncedNotificationMethodsA0e265e1 =
        abstract maxTaskQueueSize: option<float> with get, set
        abstract defaultTaskPollInterval: option<float> with get, set
        abstract taskMessageQueue: option<obj> with get, set
        abstract taskStore: option<obj> with get, set
        abstract debouncedNotificationMethods: option<ResizeArray<string>> with get, set
        abstract enforceStrictCapabilities: option<bool> with get, set

    type ErrorKindRunId =
        abstract error: string with get, set
        abstract runId: string with get, set
        abstract kind: string with get, set

    type AgentNamePayloadTimestampType67 =
        abstract timestamp: float with get, set
        abstract payload: RequestId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type Selector =
        abstract selector: string with get, set

    type AgentNamePayloadTimestampType14 =
        abstract timestamp: float with get, set
        abstract payload: IdempotencyKeyRequestIdSubmissionId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type MessageProgressTotal =
        abstract message: option<string> with get, set
        abstract total: option<float> with get, set
        abstract progress: float with get, set

    type AgentNamePayloadTimestampType28 =
        abstract timestamp: float with get, set
        abstract payload: Swept with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type ActionExecutionIdToolCallId =
        abstract toolCallId: string with get, set
        abstract executionId: string with get, set
        abstract action: string with get, set

    type AttemptIncidentIdMaxAttemp048494fb2 =
        abstract reason: string with get, set
        abstract recoveryKind: LiteralUnions.ContinueRetry with get, set
        abstract maxAttempts: float with get, set
        abstract attempt: float with get, set
        abstract requestId: string with get, set
        abstract incidentId: string with get, set

    type AgentNamePayloadTimestampType55 =
        abstract timestamp: float with get, set
        abstract payload: NormalizedInputsRemovedTool58fba2ad with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type MethodParams2 =
        abstract params: option<_meta2> with get, set
        abstract method: string with get, set

    type AgentNamePayloadTimestampType27 =
        abstract timestamp: float with get, set
        abstract payload: ActionExecutionId with get, set
        abstract name: option<string> with get, set
        abstract agent: option<string> with get, set

        [<EmitProperty("type")>]
        abstract ``type``: string with get, set

    type KindReasonRunId =
        abstract reason: option<string> with get, set
        abstract runId: string with get, set
        abstract kind: string with get, set

    type PromptsResourcesTools =
        abstract resources: option<AutoRefreshDebounceMsOnChanged> with get, set
        abstract prompts: option<AutoRefreshDebounceMsOnChanged> with get, set
        abstract tools: option<AutoRefreshDebounceMsOnChanged> with get, set

    module AgentTypeDisplayInputPreviC703c0a7 =
        type Display =
            abstract icon: option<string> with get, set
            abstract name: option<string> with get, set
            abstract Item: key: string -> option<obj>

    module AnchorCommentDefs0800bb6e4 =
        type Const = interface end

    module AnchorCommentDefs0800bb6e6 =
        type Const = interface end

    module CodeContinueInputInstMessagePath =
        type Code =
            abstract continue: option<bool> with get
            abstract inst: option<U2<Zod.ZodType, Zod.ZodType>> with get
            abstract path: option<proptypekey<obj, string>> with get, set
            abstract message: option<proptypekey<obj, string>> with get, set
            abstract input: option<proptypekey<obj, string>> with get, set
            abstract code: option<proptypekey<obj, string>> with get, set
            abstract Item: key: string -> option<obj>

    module Type3 =
        type Type = string

// ─────────────────────────────────────────────────────────────────────────────
// HAND-SHAPED OVERLAY (recipe: [[entry]] zod, policy = opaque-handle).
// Replaces the generated Zod machinery module at emission — this FRAGMENT is
// appended at namespace level inside the Zod unit file; it must not declare a
// namespace of its own. Developer-owned (the Farscape Overlay discipline):
// regeneration never touches it.
//
// Policy: zod is OPAQUE. F# code builds schemas through the builders below and
// passes them around as `ZodType` handles; none of zod's ~13k-line generic
// machinery (ZodObject<'Shape,'Config>, internals, v3/v4 twins) is emitted.
// The MCP SDK's zod-compat sniffs the runtime `_zod` marker and routes these
// v4-classic values down its v4 arm, so the one handle covers the SDK's
// AnySchema / AnyObjectSchema / ZodRawShapeCompat union.
// ─────────────────────────────────────────────────────────────────────────────
module Zod =

    open Fable.Core

    /// Opaque handle to a zod schema value (v4 classic, runtime import "zod").
    [<AllowNullLiteral>]
    type ZodType =
        /// z.string().describe("...") — descriptions flow into the JSON Schema
        /// the MCP SDK advertises for a tool; keep them on the handle.
        abstract describe: description: string -> ZodType
        /// Postfix optionality: z.string().optional()
        abstract optional: unit -> ZodType
        abstract nullable: unit -> ZodType
        /// `default` is reserved in F#; emit the raw member call.
        [<Emit("$0.default($1)")>]
        abstract withDefault: value: obj -> ZodType

    /// Phantom arity aliases: generated references that survive substitution as
    /// generic applications (`Zod.ZodType<Shape, Mode>` from `ZodObject<S, $strip>`
    /// instantiations) collapse onto the same handle. The parameters carry no
    /// meaning — the schema is opaque by policy.
    /// Unused typars are illegal on abbreviations (FS0035); the arity variants
    /// are interfaces EXTENDING the handle so generic applications still unify
    /// with `ZodType` member positions where needed.
    [<AllowNullLiteral>]
    type ZodType<'A> =
        inherit ZodType
    [<AllowNullLiteral>]
    type ZodType<'A, 'B> =
        inherit ZodType
    [<AllowNullLiteral>]
    type ZodType<'A, 'B, 'C> =
        inherit ZodType

    /// ZodRawShapeCompat = Record<string, AnySchema> — the tool-authoring shape
    /// (`registerTool` inputSchema/outputSchema). MUST be a plain JS object:
    /// the SDK enumerates it with Object.keys/values. Fable's Dictionary is a
    /// class instance, not a POJO, so the honest encoding is `obj` built with
    /// the `shape` helper below (createObj yields a true POJO).
    type ZodRawShape = obj

    /// Build a raw shape for registerTool: `Zod.shape [ "city", Zod.string() ]`.
    let inline shape (fields: (string * ZodType) list) : ZodRawShape =
        JsInterop.createObj (fields |> List.map (fun (k, v) -> k, box v))

    /// The builder set (recipe `builders` key). Selective named imports from the
    /// zod package root — the same specifier the MCP SDK imports at runtime, so
    /// the Workers bundle already carries it.
    [<Import("string", "zod")>]
    let string () : ZodType = jsNative

    [<Import("number", "zod")>]
    let number () : ZodType = jsNative

    [<Import("boolean", "zod")>]
    let boolean () : ZodType = jsNative

    [<Import("object", "zod")>]
    let object (shape: ZodRawShape) : ZodType = jsNative

    [<Import("literal", "zod")>]
    let literal (value: obj) : ZodType = jsNative

    /// z.enum([...]) — `enum` is reserved in F#.
    [<Import("enum", "zod")>]
    let enumOf (values: string array) : ZodType = jsNative

    [<Import("array", "zod")>]
    let array (element: ZodType) : ZodType = jsNative

    [<Import("optional", "zod")>]
    let optional (inner: ZodType) : ZodType = jsNative

    [<Import("union", "zod")>]
    let union (options: ZodType array) : ZodType = jsNative

    [<Import("discriminatedUnion", "zod")>]
    let discriminatedUnion (discriminator: string) (options: ZodType array) : ZodType = jsNative
