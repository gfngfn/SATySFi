(* signature for Mcdparser *)
  open Types

  type state
  type tree_and_state

  val mcdparser : (token list) -> tree
