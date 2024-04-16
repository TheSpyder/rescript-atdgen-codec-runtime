module type S = {
  let normalize: Js.Json.t => Js.Json.t

  let restore: Js.Json.t => Js.Json.t
}

module Type_field = {
  module type Param = {
    let type_field_name: string
  }

  module Make = (Param: Param): S => {
    open Param

    let normalize = json =>
      switch json |> Js.Json.classify {
      | JSONObject(obj) =>
        switch Js.Dict.get(obj, type_field_name) {
        | Some(type_) =>
          // this makes an array with the type field name as the first element
          let normalized: Js.Json.t = Obj.magic((type_, json))
          normalized
        | None => json
        }
      | _ => json
      }

    let restore = json =>
      switch json |> Js.Json.classify {
      // pulls the type out of the array and returns the object with the type set
      | JSONArray([v, o]) if Js.typeof(v) == "string" =>
        switch o |> Js.Json.classify {
        | JSONObject(obj) =>
          Js.Dict.set(obj, type_field_name, v)
          Js.Json.object_(obj)
        | _ => json
        }
      | _ => json
      }
  }

  module Default_param: Param = {
    let type_field_name = "type"
  }

  include Make(Default_param)
}
