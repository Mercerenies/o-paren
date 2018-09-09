
module LocalMap = Map.Make(String)

let built_in_map = LocalMap.empty

let construct_env () = new Env.env built_in_map
