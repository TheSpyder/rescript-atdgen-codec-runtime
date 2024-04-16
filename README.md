# rescript-atdgen-codec-runtime

`rescript-atdgen-codec-runtime` is a ReScript runtime for
[atdgen](https://github.com/ahrefs/atd), a code-generation tool for JSON handling. It is based on the direct json types of ReScript.

## Installation

```
yarn add rescript-atdgen-codec-runtime
```

This package doesn't take care of running `atdgen` to derive code from type definitions. For that, you need to [`rescript-atdgen-generator`](https://github.com/TheSpyder/rescript-atdgen-generator) to generate the `.ml` and `.mli` files from `.atd` sources.

## A note about ReScript 11
`atd` is an OCaml tool, and generates OCaml files. ReScript still supports compiling OCaml, but the `atd` generated code does not support uncurried mode. The ahrefs team is open to a contribution that generates ReScript instead, if someone is up for the task. For more information and to add your voice to the discussion please follow the github issue (TODO).

## Usage

Add `rescript-atdgen-generator` to the `bs-dependencies` of `bsconfig.json`.

To write atd type definitions, please have a look at the [atd documentation](https://atd.readthedocs.io/en/latest/).

## Simple example

Reason code to query and deserialize the response of a REST API using the `meetup.atd` file in `example/src`. It requires `bs-fetch`.

(TODO: rewrite in ReScript with either rescript-webapi or rescript-fetch)

```
let get = (url, decode) =>
  Js.Promise.(
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(~method_=Get, ()),
    )
    |> then_(Fetch.Response.json)
    |> then_(json => json |> decode |> resolve)
  );

let v: Meetup_t.events =
  get(
    "http://localhost:8000/events",
    Atdgen_codec_runtime.Decode.decode(Meetup_bs.read_events),
  );
```

## Full example

The [example](example) directory contains a full example of a simple cli to read and write data in a JSON file.

For a complete introduction from atdgen installation to json manipulation, please refer to [Getting started with atdgen and melange](https://tech.ahrefs.com/getting-started-with-atdgen-and-bucklescript-1f3a14004081). The article talks about melange a lot, which isn't relevant, but it does still serve as a good introduction to `atd` syntax.

## License and Credits

All code is licensed as MIT. See [LICENSE](LICENSE).

This project was forked from [@ahrefs/bs-atdgen-codec-runtime](https://github.com/ahrefs/bs-atdgen-codec-runtime). That project has now migrated to melange, I made a copy of the commit I forked from [here](https://github.com/thespyder/bs-atdgen-codec-runtime).
