open Jest
open Expect

let wrap_exn = exp =>
  try {
    let _ = exp()
    "not called"
  } catch {
  | Atdgen_codec_runtime.Decode.DecodeError(str) => str
  }

let () = describe("exceptions", () => {
  test("unit", () => {
    let j = Js.Json.parseExn(`{}`)
    expect(wrap_exn(() => Atdgen_codec_runtime.Decode.unit(j)))->toBe("Expected null, was {}")
  })

  test("option_as_constr", () => {
    let j = Js.Json.parseExn(`{}`)
    expect(
      wrap_exn(
        () =>
          {
            open Atdgen_codec_runtime.Decode
            option_as_constr(int)
          }(j),
      ),
    )->toBe(`Both attempts to parse the value "{}" failed. Errors:
1) Expected string, was {}
2) Expected array, was {}`)
  })

  test("enum", () => {
    let j = Js.Json.parseExn(`{}`)
    expect(
      wrap_exn(
        () =>
          {
            open Atdgen_codec_runtime.Decode
            enum(list{})
          }(j),
      ),
    )->toBe(`Both attempts to parse the value "{}" failed. Errors:
1) Expected string, was {}
2) Expected array, was {}`)
  })

  test("missing field in record", () => {
    let j = Js.Json.parseExn(`{"o": 44}`)
    expect(wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_ro, j)))->toBe(
      "Expected field 'c'",
    )
  })

  test("optional field with default: wrong type throws exception", () => {
    let j = Js.Json.parseExn(`{"with_default": "not right"}`)
    expect(
      wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_optional_field, j)),
    )->toBe(`with_default: Expected int, was "not right"`)
  })

  test("optional field: wrong type throws exception", () => {
    let j = Js.Json.parseExn(`{"no_default": "not right"}`)
    expect(
      wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_optional_field, j)),
    )->toBe(`no_default: Expected int, was "not right"`)
  })

  test("error in variant", () => {
    let j = Js.Json.parseExn(`["A", "not right"]`)
    expect(
      wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_v, j)),
    )->toBe(`A: Expected int, was "not right"`)
  })

  test("deeply nested error (array element fails)", () => {
    let j = Js.Json.parseExn(`["A", [[1, "not right"], "Bool"]]`)
    expect(
      wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_deeply_nested, j)),
    )->toBe(`A.0.1: Expected int, was "not right"`)
  })

  test("deeply nested error (tuple element fails)", () => {
    let j = Js.Json.parseExn(`["A", [[1, 2], "Boolean"]]`)
    expect(
      wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_deeply_nested, j)),
    )->toBe(`A.1.Boolean: unknown constructor "Boolean"`)
  })

  test("deeply nested error (rec_list element fails deep enough)", () => {
    let j = Js.Json.parseExn(`["A", [[1, 2], ["List", ["Bool", "Fail"]]]]`)
    expect(
      wrap_exn(() => Atdgen_codec_runtime.Decode.decode(Test_bs.read_deeply_nested, j)),
    )->toBe(`A.1.List.1.Fail: unknown constructor "Fail"`)
  })
})
