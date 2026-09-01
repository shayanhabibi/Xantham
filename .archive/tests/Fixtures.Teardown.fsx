#r "nuget: Fake.JavaScript.Npm"
#r "nuget: EasyBuild.FileSystemProvider"

(*
Antonym to `Fixtures.Setup.fsx`. Removes all folders created by the fixtures.
*)

open EasyBuild.FileSystemProvider
open Fake.IO

module FileSystem =
    [<Literal>]
    let private repoRoot = __SOURCE_DIRECTORY__ + "/.."
    type This = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
    type Repo = AbsoluteFileSystem<repoRoot>
    type VirtualThis = VirtualFileSystem<This.``.``, "
        fixtures/
            _FIXTURE_NAME_/
                node_modules/
                    _FIXTURE_NAME_/
                        dist/
                            index.d.ts
                package.json
    ">

Directory.delete FileSystem.VirtualThis.fixtures.``.``