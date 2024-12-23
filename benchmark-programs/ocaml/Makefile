build:
	opam exec -- ocamlopt -o factorial_accumulator factorial_accumulator.ml
	opam exec -- ocamlopt -o fibonacci_recursive fibonacci_recursive.ml
	opam exec -- ocamlopt -o sum_range sum_range.ml
	opam exec -- ocamlopt -o iterate_increment iterate_increment.ml
	opam exec -- ocamlopt -o match_options match_options.ml
	opam exec -- ocamlopt -o lookup_tree lookup_tree.ml
	opam exec -- ocamlopt -o erase_unused erase_unused.ml

clean:
	rm -f *.{cmi,cmx,o}
	rm -f factorial_accumulator
	rm -f fibonacci_recursive
	rm -f sum_range
	rm -f iterate_increment
	rm -f match_options
	rm -f lookup_tree
	rm -f erase_unused
