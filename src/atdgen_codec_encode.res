type t<'a> = 'a => Js.Json.t

let make = f => f

let encode = (f, x) => f(x)

let unit = () => Js.Json.null

let string = s => Js.Json.string(s)

let float = f => Js.Json.number(f)

let int = i => Js.Json.number(Belt.Int.toFloat(i))

let bool = b => Js.Json.boolean(b)

let char = c => Js.Json.string(String.make(1, c))

let array = (encoder, a) => a->Belt.Array.map(encoder)->Js.Json.array

let int32 = s => string(Int32.to_string(s))

let int64 = s => string(Int64.to_string(s))

type spec<'a, 'b> = {name: string, data: 'a, encode: t<'b>}

type field_spec<'a> =
  | Optional(spec<option<'a>, 'a>, option<'a>)
  | Required(spec<'a, 'a>, option<'a>)

type rec field = F(field_spec<'a>): field

let list = (f, l) => l |> Belt.List.toArray |> array(f)

let field = (~default=?, encode, ~name, data) => F(Required({name, data, encode}, default))

let field_o = (~default=?, encode, ~name, data) => F(Optional({name, data, encode}, default))

let obj = fields => List.fold_left((acc, F(f)) =>
    switch f {
    | Required({name, data, encode}, None) => list{(name, encode(data)), ...acc}
    | Required({name, data, encode}, Some(default)) =>
      if default == data {
        acc
      } else {
        list{(name, encode(data)), ...acc}
      }
    | Optional({name, data, encode}, default) =>
      switch (data, default) {
      | (None, _) => acc
      | (Some(s), Some(default)) =>
        if s == default {
          acc
        } else {
          list{(name, encode(s)), ...acc}
        }
      | (Some(s), None) => list{(name, encode(s)), ...acc}
      }
    }
  , list{}, fields)->Js.Dict.fromList->Js.Json.object_

let tuple1 = (f, a) => ([f(a)])->Js.Json.array
let tuple2 = (fa, fb, (a, b)) => ([fa(a), fb(b)])->Js.Json.array
let tuple3 = (fa, fb, fc, (a, b, c)) => ([fa(a), fb(b), fc(c)])->Js.Json.array
let tuple4 = (fa, fb, fc, fd, (a, b, c, d)) => ([fa(a), fb(b), fc(c), fd(d)])->Js.Json.array

let contramap = (f, g, b) => g(f(b))

let nullable = (f, v) => switch(v) {
  | None => Js.Json.null
  | Some(value) => f(value)
}

let constr0 = string

let constr1 = (s, f, x) => tuple2(string, f, (s, x))

let option_as_constr = (f, x) =>
  switch x {
  | None => string("None")
  | Some(s) => tuple2(string, f, ("Some", s))
  }

let adapter = (restore: Js.Json.t => Js.Json.t, writer: t<'a>, x) => {
  let encoded = writer(x)
  restore(encoded)
}
