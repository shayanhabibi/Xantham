"""Repeatable F# export signature probes against the consumer's built dependencies.

Run from any directory: rtk python <this file> [case substring].
Build src/Xantham.Fable.Core.TS first. No network or dependency changes.
"""
from pathlib import Path
import json
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[4]
DEPS = [ROOT / 'src/Xantham.Fable.Core/bin/Debug/net8.0/Xantham.Fable.Core.dll',
        ROOT / 'src/Xantham.Fable.Core.TS/bin/Debug/net8.0/Xantham.Fable.Core.TS.dll',
        Path.home() / '.nuget/packages/fable.core/5.2.0/lib/netstandard2.0/Fable.Core.dll']
HEADER = '\n'.join(f'#r @"{p}"' for p in DEPS) + '\nopen System\nopen Fable.Core\nopen Fable.Core.JS\nopen Fable.Core.TS.Dom\n'

# expected diagnostic None means both declarations and explicit calls must compile.
CASES = [
 ('legal-parameters', None, 'type E =\n    static member f(x: string) = x\n    static member f(x: float) = x\nlet a: string = E.f "x"\nlet b: float = E.f 1.0'),
 ('return-only', 'FS0438', 'type E =\n    static member f(x: string): string = x\n    static member f(x: string): float = 1.0'),
 ('generic-alias', 'FS0438', "type A<'T> = 'T option array\ntype B<'T> = A<'T>\ntype E =\n    static member f(x: B<string>) = ()\n    static member f(x: string option array) = ()"),
 ('alpha-generics', 'FS0438', "type E =\n    static member f<'A>(x: 'A) = x\n    static member f<'B>(x: 'B) = x"),
 ('generic-arity', None, "type E =\n    static member f<'A>(x: 'A) = x\n    static member f<'A,'B>(x: 'A) = x\nlet a = E.f<string> \"x\"\nlet b = E.f<string,int> \"x\""),
 ('constraints-only', 'FS0438', "type E =\n    static member f<'A when 'A :> IDisposable>(x: 'A) = ()\n    static member f<'B when 'B :> IComparable>(x: 'B) = ()"),
 ('optional-option', 'FS0438', 'type E =\n    static member f(?x: string) = ()\n    static member f(x: string option) = ()'),
 ('rest-array', 'FS0438', 'type E =\n    static member f([<ParamArray>] x: string array) = ()\n    static member f(x: string array) = ()'),
 ('nullary-unit', None, 'type E =\n    static member f() = ()\nlet a = E.f()'),
 ('nullary-explicit-unit', None, 'type E =\n    static member f() = ()\n    static member f(x: unit) = ()\nlet a = E.f()\nlet b = E.f(x = ())'),
 ('optional-omission', 'FS0041', 'type E =\n    static member f(?x: string) = ()\n    static member f(?x: float) = ()\nlet a = E.f()'),
 ('empty-rest', 'FS0041', 'type E =\n    static member f([<ParamArray>] x: string array) = ()\n    static member f([<ParamArray>] x: float array) = ()\nlet a = E.f()'),
 ('named-optional', None, 'type E =\n    static member f(?text: string) = ()\n    static member f(?number: float) = ()\nlet a = E.f(text = "x")\nlet b = E.f(number = 1.0)'),
 ('property-method', 'FS0434', 'type E =\n    static member f: string = "x"\n    static member f(x: float) = x'),
 ('getter-method', 'FS0438', 'type E =\n    static member f: string = "x"\n    static member get_f(): string = "y"'),
 ('setter-method', 'FS0438', 'type E =\n    static member f with get(): string = "x" and set(value: string) = ()\n    static member set_f(value: string) = ()'),
 ('measure-erasure', 'FS0438', '[<Measure>] type A\n[<Measure>] type B\ntype E =\n    static member f(x: float<A>) = ()\n    static member f(x: float<B>) = ()'),
 ('string-measure-erasure', 'FS0438', '[<Measure>] type A\n[<Measure>] type B\ntype E =\n    static member f(x: string<A>) = ()\n    static member f(x: string<B>) = ()'),
 ('option-function-alias', 'FS0438', "type A<'T> = ('T -> string) option\ntype E =\n    static member f(x: A<float>) = ()\n    static member f(x: (float -> string) option) = ()"),
 ('delegate-abbreviation', 'FS0438', 'type A = Func<string,float>\ntype E =\n    static member f(x: A) = ()\n    static member f(x: Func<string,float>) = ()'),
 ('delegate-nominal', None, 'type A = delegate of string -> float\ntype B = delegate of string -> float\ntype E =\n    static member f(x: A) = ()\n    static member f(x: B) = ()\nlet a = E.f(A(fun _ -> 1.0))\nlet b = E.f(B(fun _ -> 1.0))'),
 ('tuple-parameter', None, 'type E =\n    static member f(x: (string * float)) = ()\n    static member f(x: string, y: float) = ()\nlet a = E.f(("x", 1.0))\nlet b = E.f("x", 1.0)'),
]

def main():
    for dependency in DEPS:
        if not dependency.exists():
            raise SystemExit(f'Missing consumer dependency: {dependency}')
    results = []
    with tempfile.TemporaryDirectory(prefix='xantham-export-probes-') as temp:
        for name, expected, source in CASES:
            if len(sys.argv) > 1 and sys.argv[1] not in name:
                continue
            path = Path(temp) / (name + '.fsx')
            path.write_text(HEADER + source + '\n', encoding='utf-8')
            run = subprocess.run(['rtk', 'proxy', 'dotnet', 'fsi', '--exec', str(path)], capture_output=True, text=True)
            output = run.stdout + run.stderr
            ok = run.returncode == 0 if expected is None else run.returncode != 0 and expected in output
            results.append({'case': name, 'expected': expected or 'compiles', 'passed': ok, 'exit': run.returncode,
                            'diagnostics': [line.strip() for line in output.splitlines() if 'error FS' in line]})
            print(json.dumps(results[-1]), flush=True)
    (Path(__file__).parent / 'results.json').write_text(json.dumps(results, indent=2) + '\n', encoding='utf-8')
    return 0 if results and all(r['passed'] for r in results) else 1

if __name__ == '__main__':
    sys.exit(main())
