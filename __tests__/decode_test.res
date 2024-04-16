open Jest

let run_decode_test = (~name, ~read, ~data, ~expected) => {
  open Expect
  let decode = Atdgen_codec_runtime.Decode.decode(read)
  let data' = decode(data)
  test(name, () => expect(data') |> toEqual(expected))
}

@ocaml.doc("
  atdgen doesn't ever encode a null, but it's quite possible when decoding external data.

  both nullable and optional field null values need to decode to `None`.
 ")
let () = describe("JSON decoding tests", () => {
  run_decode_test(
    ~name="nullable field decoding a null",
    ~read=Test_bs.read_optional_field,
    ~expected={with_default: 9, no_default: None, no_default_nullable: None},
    ~data=%raw(`{ no_default_nullable: null }`),
  )
  run_decode_test(
    ~name="optional field decoding a null",
    ~read=Test_bs.read_optional_field,
    ~expected={with_default: 9, no_default: None, no_default_nullable: None},
    ~data=%raw(`{ no_default: null}`),
  )
})