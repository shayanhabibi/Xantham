module DefaultIntersectionConsumer

open DefaultIntersectionLab

let createDirect (handler: Connection<obj> -> unit) : Agent = Agent.Create handler

let createImported (handler: Connection<obj> -> unit) : ImportedAgent = ImportedAgent.Create handler
