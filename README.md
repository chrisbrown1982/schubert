# schubert

    .
    ├── Main.idr             entry point; pick an example to render to MIDI
    ├── schubert-lib.ipkg    library package definition (src/)
    ├── examples-lib.ipkg    example melodies package definition (examples/)
    ├── src/                 Scales.idr, Duration.idr, Melody.idr, Midi.idr
    ├── examples/            example melodies (e.g. Twinkle.idr)
    └── output/              generated MIDI output (default write location)

## Building

Idris resolves imports out of a single source directory, so `src/` and
`examples/` are each built as their own package first (both packages happen
to share the same `build/ttc` output when built from this directory, so
`Main.idr` can import from either):

    idris2 --build schubert-lib.ipkg
    idris2 --build examples-lib.ipkg
    IDRIS2_PATH=build/ttc idris2 -o schubert Main.idr

## Running

    ./build/exec/schubert <example>

With no argument, or an unrecognised one, it lists the available examples
and writes nothing. Each example is written to `output/<example>.mid`.
