
let split_utf8 str =
  str
  |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun lst s -> s::lst) []
  |> List.rev


let split_utf16be str =
  str
  |> Uuseg_string.fold_utf_16be `Grapheme_cluster (fun lst s -> s::lst) []
  |> List.rev
