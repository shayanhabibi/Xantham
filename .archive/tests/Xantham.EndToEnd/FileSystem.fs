module FileSystem

open EasyBuild.FileSystemProvider
open Fake.IO

let [<Literal>] fixtureNameSigil = "_FIXTURE_NAME_"

let [<Literal>] private repoRoot = __SOURCE_DIRECTORY__ + "/../.."
let [<Literal>] private xanthamFableSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Fable"
let [<Literal>] private xanthamFableCoreSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Fable.Core"
let [<Literal>] private xanthamDecoderSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Decoder"
let [<Literal>] private xanthamGeneratorSource = __SOURCE_DIRECTORY__ + "/../../src/Xantham.Generator"

let [<Literal>] private virtualTestDirectory =
    "
    driver/
        Program.fs
        Driver.fsproj
    fixtures/
        " + fixtureNameSigil + "/
            package.json
    output/
        " + fixtureNameSigil + ".json
        " + fixtureNameSigil + ".fs
    verify/
        " + fixtureNameSigil + ".wrapped.fs
        Verify." + fixtureNameSigil + ".fsproj
"

module Source =
    module Fable =
        type Core = AbsoluteFileSystem<xanthamFableCoreSource>
    type Fable = AbsoluteFileSystem<xanthamFableSource>
    type Decoder = AbsoluteFileSystem<xanthamDecoderSource>
    type Generator = AbsoluteFileSystem<xanthamGeneratorSource>
    
type This = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
type Repo = AbsoluteFileSystem<repoRoot>
type VirtualThis = VirtualFileSystem<__SOURCE_DIRECTORY__, virtualTestDirectory>

module VirtualThis =
    let setupDirectories =
        lazy
        [
            VirtualThis.driver.``.``
            VirtualThis.fixtures.``.``
            VirtualThis.output.``.``
            VirtualThis.verify.``.``
        ]
        |> List.iter Directory.ensure
