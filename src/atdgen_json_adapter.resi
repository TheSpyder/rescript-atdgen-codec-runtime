module type S = {
  /** Convert a real json tree into an atd-compliant form. */
  let normalize: Js.Json.t => Js.Json.t

  /** Convert an atd-compliant json tree into a real json tree. */
  let restore: Js.Json.t => Js.Json.t
}

module Type_field: {
  module type Param = {
    let type_field_name: string
  }

  /** Default parameters, using [type_field_name = "type"]. */
  module Default_param: Param

  /** Default adapter assuming a ["type"] field. */
  include S

  /** Functor, allowing the use of a custom parameter:
    {[
    module Kind_field = Type_field.Make (struct let type_field_name = "kind" end)
    ]}
  */
  module Make: (Param: Param) => S
}
