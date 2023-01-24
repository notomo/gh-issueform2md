let enumerate f xs =
  let indicies = List.init (List.length xs) (fun x -> x) in
  xs |> List.combine indicies |> List.map f

let all_errors xs =
  xs |> List.filter Result.is_error |> List.map Result.get_error

let all_somes xs = xs |> List.filter Option.is_some |> List.map Option.get
let rec repeat x n = if n <= 0 then [] else x :: repeat x (n - 1)
