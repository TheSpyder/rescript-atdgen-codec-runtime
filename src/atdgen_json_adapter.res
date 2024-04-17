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

    let normalize = (json: Js.Json.t) =>
      switch json |> Js.Json.classify {
      | JSONObject(obj) =>
        switch Js.Dict.get(obj, type_field_name) {
        | Some(type_) =>
          let normalized: Js.Json.t = Obj.magic((type_, json))
          normalized
        | None => json
        }
      | _ => json
      }

    let restore = json =>
      switch json |> Js.Json.classify {
      | JSONArray([v, o]) if Js.typeof(v) == "string" =>
        switch o |> Js.Json.classify {
        | JSONObject(obj) =>
          Js.Dict.set(obj, type_field_name, v)
          Json_encode.jsonDict(obj)
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
