module Xantham.Generator.RunGate.ExportProvenance

open TypeOnlyExportLab

let run check =
    let payload: Message = Payload.Create "seed"
    let client: Client = Exports.visible payload
    let result: Payload = client.send (Payload.Create "sent")
    check "a value-exported factory returns the type-only class instance" (result.text = "seed:sent")
