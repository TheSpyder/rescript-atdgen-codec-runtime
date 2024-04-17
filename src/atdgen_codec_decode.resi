type t<'a> = Js.Json.t => 'a

let make: (Js.Json.t => 'a) => t<'a>

let decode: (t<'a>, Js.Json.t) => 'a

let unit: t<unit>

let bool: t<bool>

let int: t<int>

let float: t<float>

let char: t<char>

let string: t<string>

let int32: t<int32>

let int64: t<int64>

let optional: t<'a> => t<option<'a>>

let list: t<'a> => t<list<'a>>

let array: t<'a> => t<array<'a>>

let obj_list: t<'a> => t<list<(string, 'a)>>

let obj_array: t<'a> => t<array<(string, 'a)>>

/* a field that should be present */
let field: (string, t<'a>) => t<'a>

/* a field that turns into a an optional value when absent */
let fieldDefault: (string, 'a, t<'a>) => t<'a>

/* a field that returns None when is absent */
let fieldOptional: (string, t<'a>) => t<option<'a>>

let map: ('a => 'b, t<'a>) => t<'b>

let tuple1: t<'a> => t<'a>

let tuple2: (t<'a>, t<'b>) => t<('a, 'b)>

let tuple3: (t<'a>, t<'b>, t<'c>) => t<('a, 'b, 'c)>

let tuple4: (t<'a>, t<'b>, t<'c>, t<'d>) => t<('a, 'b, 'c, 'd)>

let enum: list<(string, [#Single('a) | #Decode(t<'a>)])> => t<'a>

let nullable: t<'a> => t<option<'a>>

let option_as_constr: t<'a> => t<option<'a>>

let adapter: (Js.Json.t => Js.Json.t, t<'a>) => t<'a>
