module GroupedDomAliasesConsumer

open GroupedDomAliases.WorkerAugmentationLab

let roundTripCache (request: Request) : Request.Cache =
    (GroupedDomAliasesLab.Exports.roundTrip request).cache

let cloudflareMetadata (request: Request) : string = request.cf
