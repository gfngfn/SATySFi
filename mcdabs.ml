
type macro_name = string
type id_name = string
type var_name = string
type letter = string

type abstract_tree = EndOfAbsBlock
                   | AbsBlock of abstract_tree * abstract_tree
                   | ContentOf of var_name | Output of letter
                   | Pop of var_name * var_name * abstract_tree * abstract_tree
                   | Macro of macro_name * (var_name list) * abstract_tree * abstract_tree
                   | Apply of macro_name * id_name * (abstract_tree list)
                   | Invalid
                   | Separated of abstract_tree * abstract_tree

let rec concrete_to_abstract (conctr : tree list) =
	match conctr with
	  NonTerminal(Total, blk :: [END_OF_INPUT]) -> concrete_to_abstract(blk)
	| NonTerminal(Block, []) -> EndOfAbsBlock
	| NonTerminal(Block, stc :: blk) -> AbsBlock(concrete_to_abstract(stc), concrete_to_abstract(blk))
	| NonTerminal(Group)