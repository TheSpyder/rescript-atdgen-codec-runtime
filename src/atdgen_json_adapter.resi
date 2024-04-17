module type S = {
  @ocaml.doc(" Convert a real json tree into an atd-compliant form. ")
  let normalize: Js.Json.t => Js.Json.t

  @ocaml.doc(" Convert an atd-compliant json tree into a real json tree. ")
  let restore: Js.Json.t => Js.Json.t
}

module Type_field: {
  module type Param = {
    let type_field_name: string
  }

  @ocaml.doc(" Default parameters, using [type_field_name = \"type\"]. ")
  module Default_param: Param

  @ocaml.doc(" Default adapter assuming a [\"type\"] field. ")
  include S

  @ocaml.doc(" Functor, allowing the use of a custom parameter:
    {[
    module Kind_field = Type_field.Make (struct let type_field_name = \"kind\" end)
    ]}
  ")
  module Make: (Param: Param) => S
}
