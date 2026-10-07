open System.IO.Compression

let arguments = fsi.CommandLineArgs |> Array.skip 1
ZipFile.ExtractToDirectory(arguments[0], arguments[1])
