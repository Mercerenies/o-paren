
let drop_string n str =
  String.sub str n (String.length str - n)

let first f (a, b) = (f a, b)

let second f (a, b) = (a, f b)

let safe_get str n =
  try
    Some (String.get str n)
  with Invalid_argument _ -> None

let cons a b = a :: b
