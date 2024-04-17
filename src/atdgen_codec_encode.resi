type t<'a> = 'a => Js.Json.t

let make: ('a => Js.Json.t) => t<'a>

let encode: (t<'a>, 'a) => Js.Json.t

let unit: t<unit>

let string: t<string>

let float: t<float>

let int: t<int>

let bool: t<bool>

let char: t<char>

let list: t<'a> => t<list<'a>>

let array: t<'a> => t<array<'a>>

let int32: t<int32>

let int64: t<int64>

type field

let field: (~default: 'a=?, t<'a>, ~name: string, 'a) => field

let field_o: (~default: 'a=?, t<'a>, ~name: string, option<'a>) => field

let obj: list<field> => Js.Json.t

let tuple1: t<'a> => t<'a>

let tuple2: (t<'a>, t<'b>) => t<('a, 'b)>

let tuple3: (t<'a>, t<'b>, t<'c>) => t<('a, 'b, 'c)>

let tuple4: (t<'a>, t<'b>, t<'c>, t<'d>) => t<('a, 'b, 'c, 'd)>

let constr0: string => Js.Json.t

let constr1: (string, t<'a>, 'a) => Js.Json.t

let contramap: ('b => 'a, t<'a>) => t<'b>

let nullable: t<'a> => t<option<'a>>

let option_as_constr: t<'a> => t<option<'a>>

let adapter: (Js.Json.t => Js.Json.t, t<'a>) => t<'a>
