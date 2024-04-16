exception DecodeError(string)
exception DecodeErrorPath(list<string>, string)

type t<'a> = Js.Json.t => 'a

let make = f => f

let decode' = (f, json) => f(json)

let decode = (f, json) =>
  try f(json) catch {
  | DecodeErrorPath(path, msg) =>
    let path = String.concat(".", path)
    raise(DecodeError(`${path}: ${msg}`))
  }

let with_segment = (segment, f, json) =>
  try f(json) catch {
  | DecodeError(msg) => raise(DecodeErrorPath(list{segment}, msg))
  | DecodeErrorPath(path, msg) => raise(DecodeErrorPath(list{segment, ...path}, msg))
  }

let string = s => switch (Js.Json.decodeString(s)) {
  | Some(str) => str
  | None => raise(DecodeError(`Expected string, was ${s->Js.Json.stringify}`))
}

let optional = (f, json) => {
  try (Some(json->f)) catch {
    | DecodeError(_) => None
  }
}

let char: (Js.Json.t => char) = s => switch (string(s)) {
  | s when (Js.String.length(s) === 1) => String.get(s, 0)
  | s => raise(DecodeError(`Expected character, was ${s}`))
}

let float = f => switch (Js.Json.decodeNumber(f)) {
  | Some(number) => number
  | None => raise(DecodeError(`Expected float, was ${f->Js.Json.stringify}`))
}

let int: (Js.Json.t => int) = i => switch (Js.Json.decodeNumber(i)) {
  | Some(number) when (%raw("number === parseInt(number)")) => Obj.magic(number)
  | _ => raise(DecodeError(`Expected int, was ${i->Js.Json.stringify}`))
}

let bool = b => switch (Js.Json.decodeBoolean(b)) {
  | Some(bool) => bool
  | None => raise(DecodeError(`Expected bool, was ${b->Js.Json.stringify}`))
}

let unit = j =>
  if Js.Json.test(j, Null) {
    ()
  } else {
    raise(DecodeError("Expected null, was " ++ Js.Json.stringify(j)))
  }

let int32 = j => Int32.of_string(string(j))

let int64 = j => Int64.of_string(string(j))

let array = (decode, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    let target = Belt.Array.makeUninitializedUnsafe(length)
    for i in 0 to length - 1 {
      let value = try with_segment(string_of_int(i), decode, Array.unsafe_get(source, i)) catch {
      | DecodeError(msg) =>
        raise(DecodeError(msg ++ ("\n\tin array at index " ++ string_of_int(i))))
      }

      Array.unsafe_set(target, i, value)
    }
    target
  } else {
    raise(DecodeError("Expected array, was " ++ Js.Json.stringify(json)))
  }

let list = (decode, json) => json |> array(decode) |> Array.to_list

let pair = (decodeA, decodeB, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 2 {
      try (
        with_segment("0", decodeA, Array.unsafe_get(source, 0)),
        with_segment("1", decodeB, Array.unsafe_get(source, 1)),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin pair/tuple2"))
      }
    } else {
      raise(DecodeError(`Expected array of length 2, was array of length ${length->Belt.Int.toString}`))
    }
  } else {
    raise(DecodeError("Expected array, was " ++ Js.Json.stringify(json)))
  }

let tuple2 = pair

let tuple3 = (decodeA, decodeB, decodeC, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 3 {
      try (
        with_segment("0", decodeA, Array.unsafe_get(source, 0)),
        with_segment("1", decodeB, Array.unsafe_get(source, 1)),
        with_segment("2", decodeC, Array.unsafe_get(source, 2)),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple3"))
      }
    } else {
      raise(DecodeError(`Expected array of length 3, was array of length ${length->Belt.Int.toString}`))
    }
  } else {
    raise(DecodeError("Expected array, was " ++ Js.Json.stringify(json)))
  }

let tuple4 = (decodeA, decodeB, decodeC, decodeD, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 4 {
      try (
        with_segment("1", decodeA, Array.unsafe_get(source, 0)),
        with_segment("2", decodeB, Array.unsafe_get(source, 1)),
        with_segment("3", decodeC, Array.unsafe_get(source, 2)),
        with_segment("4", decodeD, Array.unsafe_get(source, 3)),
      ) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple4"))
      }
    } else {
      raise(DecodeError(`Expected array of length 4, was array of length ${length->Belt.Int.toString}`))
    }
  } else {
    raise(DecodeError("Expected array, was " ++ Js.Json.stringify(json)))
  }

let dict = (decode, json) =>
  if Js.Json.test(json, Object) {
    let source: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let keys = Js.Dict.keys(source)
    let l = Js.Array.length(keys)
    let target = Js.Dict.empty()
    for i in 0 to l - 1 {
      let key = Array.unsafe_get(keys, i)
      let value = try with_segment(key, decode, Js.Dict.unsafeGet(source, key)) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin dict"))
      }

      Js.Dict.set(target, key, value)
    }
    target
  } else {
    raise(DecodeError("Expected object, was " ++ Js.Json.stringify(json)))
  }

let field = (key, decode, json) =>
  if Js.Json.test(json, Object) {
    let dict: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch Js.Dict.get(dict, key) {
    | Some(value) =>
      try with_segment(key, decode, value) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ ("\n\tat field '" ++ (key ++ "'"))))
      }
    | None => raise(DecodeError(`Expected field '${key}'`))
    }
  } else {
    raise(DecodeError("Expected object, was " ++ Js.Json.stringify(json)))
  }

let obj_array = (f, json) => dict(f, json) |> Js.Dict.entries

let obj_list = (f, json) => obj_array(f, json) |> Array.to_list

let nullable = (decode, json) =>
  if Js.Json.test(json, Null) {
    None
  } else {
    Some(decode(json))
  }
  
let map = (f, decoder) => (json) => f(decoder(json))

/* Unlike Json_decode.field, this returns None if key is not found */
let fieldOptional = (key, decode, json) =>
  if Js.Json.test(json, Object) {
    let dict: Js.Dict.t<Js.Json.t> = Obj.magic((json: Js.Json.t))
    switch Js.Dict.get(dict, key) {
    | None => None
    /* treat fields with null values as missing fields (atdgen's default) */
    | Some(value) if Js.Json.test(value, Null) => None
    | Some(value) =>
      try Some(with_segment(key, decode, value)) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ ("\n\tat field '" ++ (key ++ "'"))))
      }
    }
  } else {
    raise(DecodeError("Expected object, was " ++ Js.Json.stringify(json)))
  }

let fieldDefault = (s, default, f) =>
  fieldOptional(s, f) |> map(x =>
    switch x {
    | None => default
    | Some(s) => s
    }
  )

let tuple1 = (f, x) =>
  if Js.Array.isArray(x) {
    let source: array<Js.Json.t> = Obj.magic((x: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 1 {
      try with_segment("0", f, Array.unsafe_get(source, 0)) catch {
      | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple1"))
      }
    } else {
      raise(DecodeError(`Expected array of length 1, was array of length ${length->Belt.Int.toString}`))
    }
  } else {
    raise(DecodeError("Expected array, was " ++ Js.Json.stringify(x)))
  }

let either = (l, r, json) =>
  try l(json) catch {
    | DecodeError(e1) =>
      try r(json) catch {
        | DecodeError(e2) =>
          raise(DecodeError(
            `Both attempts to parse the value "${json->Js.Json.stringify}" failed. Errors:\n` ++
            `1) ${e1}\n` ++
            `2) ${e2}`
          ))
      }
  }

let enum = (l, json) => {
  let constr0 = j => {
    let s = string(j)
    #Constr0(s)
  }

  let constr = j => {
    let p = pair(string, x => x, j)
    #Constr(p)
  }

  switch either(constr0, constr, json) {
  | #Constr0(s) =>
    with_segment(
      s,
      () =>
        switch List.assoc(s, l) {
        | exception Not_found => raise(DecodeError(`unknown constructor "${s}"`))
        | #Single(a) => a
        | #Decode(_) => raise(DecodeError(`constructor "${s}" expects arguments`))
        },
      (),
    )
  | #Constr(s, args) =>
    with_segment(
      s,
      () =>
        switch List.assoc(s, l) {
        | exception Not_found => raise(DecodeError(`unknown constructor "${s}"`))
        | #Single(_) => raise(DecodeError(`constructor "${s}" doesn't expect arguments`))
        | #Decode(d) => decode'(d, args)
        },
      (),
    )
  }
}

let option_as_constr = f =>
  either(
    x =>
      if string(x) == "None" {
        None
      } else {
        raise(DecodeError("Expected None, was " ++ Js.Json.stringify(x)))
      },
    x =>
      switch pair(string, f, x) {
      | ("Some", v) => Some(v)
      | _ => raise(DecodeError("Expected Some _, was " ++ Js.Json.stringify(x)))
      },
  )

let adapter = (normalize: Js.Json.t => Js.Json.t, reader: t<'a>, json) => reader(normalize(json))
