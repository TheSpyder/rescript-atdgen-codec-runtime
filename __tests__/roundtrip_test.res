@@ocaml.text(" Test that encoding + decoding a value equals to the original value. ")

open Jest

let run_test = (~name, ~write, ~read, ~data) => {
  open Expect
  let encode = Atdgen_codec_runtime.Encode.encode(write)
  let decode = Atdgen_codec_runtime.Decode.decode(read)
  let json = encode(data)
  let data' = decode(json)
  test(name, () => expect(data)->toEqual(data'))
}

let () = describe("roundtrip tests", () => {
  run_test(
    ~name="record",
    ~write=Test_bs.write_r,
    ~read=Test_bs.read_r,
    ~data={Test_t.a: 1, b: "string"},
  )
  run_test(
    ~name="record optional absent",
    ~write=Test_bs.write_ro,
    ~read=Test_bs.read_ro,
    ~data={Test_t.c: "s", o: None},
  )
  run_test(
    ~name="record optional present",
    ~write=Test_bs.write_ro,
    ~read=Test_bs.read_ro,
    ~data={Test_t.c: "s", o: Some(3L)},
  )
  run_test(
    ~name="variant list",
    ~write=Test_bs.write_vl,
    ~read=Test_bs.read_vl,
    ~data=list{Test_t.A(1), B("s")},
  )
  run_test(
    ~name="variant poly list",
    ~write=Test_bs.write_vpl,
    ~read=Test_bs.read_vpl,
    ~data=list{#A(1), #B("s")},
  )
  run_test(~name="tuple", ~write=Test_bs.write_t, ~read=Test_bs.read_t, ~data=(1, "s", 1.1))
  run_test(~name="int nullable absent", ~write=Test_bs.write_n, ~read=Test_bs.read_n, ~data=None)
  run_test(
    ~name="int nullable present",
    ~write=Test_bs.write_n,
    ~read=Test_bs.read_n,
    ~data=Some(1),
  )
  run_test(~name="int64", ~write=Test_bs.write_myInt, ~read=Test_bs.read_myInt, ~data=3L)
  run_test(
    ~name="recurse",
    ~write=Test_bs.write_recurse,
    ~read=Test_bs.read_recurse,
    ~data={Test_t.recurse_items: list{{recurse_items: list{}}}},
  )
  run_test(
    ~name="mutual recurse",
    ~write=Test_bs.write_mutual_recurse1,
    ~read=Test_bs.read_mutual_recurse1,
    ~data={
      let rec mutual_recurse1 = {Test_t.mutual_recurse2: mutual_recurse2}
      and mutual_recurse2 = list{{Test_t.mutual_recurse1: list{}}}
      mutual_recurse1
    },
  )
  run_test(
    ~name="rec list",
    ~write=Test_bs.write_rec_list,
    ~read=Test_bs.read_rec_list,
    ~data=#List(list{#Bool, #Bool, #List(list{#Bool}), #List(list{})}),
  )
  run_test(
    ~name="adapter variant 1",
    ~write=Test_bs.write_adapted,
    ~read=Test_bs.read_adapted,
    ~data={
      open Test_t
      #A({thing: "thing", other_thing: false})
    },
  )
  run_test(
    ~name="adapter variant 2",
    ~write=Test_bs.write_adapted,
    ~read=Test_bs.read_adapted,
    ~data={
      open Test_t
      #B({thing: 1})
    },
  )
  run_test(
    ~name="adapter kind field - variant 1",
    ~write=Test_bs.write_adapted_kind,
    ~read=Test_bs.read_adapted_kind,
    ~data={
      open Test_t
      #A({thing: "thing", other_thing: false})
    },
  )
  run_test(
    ~name="adapter kind field - variant 2",
    ~write=Test_bs.write_adapted_kind,
    ~read=Test_bs.read_adapted_kind,
    ~data={
      open Test_t
      #B({thing: 1})
    },
  )
  run_test(
    ~name="int array",
    ~write=Test_bs.write_an_array,
    ~read=Test_bs.read_an_array,
    ~data=[1, 2, 3, 4, 5],
  )
  run_test(
    ~name="record with optional fields",
    ~write=Test_bs.write_optional_field,
    ~read=Test_bs.read_optional_field,
    ~data={with_default: 1, no_default: None, no_default_nullable: Some(11)},
  )
  run_test(
    ~name="adapter scalar",
    ~write=Test_bs.write_adapted_scalar,
    ~read=Test_bs.read_adapted_scalar,
    ~data=#A(1),
  )
  run_test(
    ~name="adapter scalar - variant 2",
    ~write=Test_bs.write_adapted_scalar,
    ~read=Test_bs.read_adapted_scalar,
    ~data=#B("thing"),
  )
  run_test(
    ~name="adapter list",
    ~write=Test_bs.write_adapted_list,
    ~read=Test_bs.read_adapted_list,
    ~data=#A(list{1}),
  )
})
