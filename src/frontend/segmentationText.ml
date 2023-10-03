
let split_utf8 str = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun lst s -> s::lst) [] str

