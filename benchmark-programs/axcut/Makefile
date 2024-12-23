build:
	nasm -f elf64 factorial_accumulator.x86.asm
	gcc -o factorial_accumulator main.c factorial_accumulator.x86.o
	nasm -f elf64 fibonacci_recursive.x86.asm
	gcc -o fibonacci_recursive main.c fibonacci_recursive.x86.o
	nasm -f elf64 sum_range.x86.asm
	gcc -o sum_range main.c sum_range.x86.o
	nasm -f elf64 iterate_increment.x86.asm
	gcc -o iterate_increment main.c iterate_increment.x86.o
	nasm -f elf64 match_options.x86.asm
	gcc -o match_options main.c match_options.x86.o
	nasm -f elf64 lookup_tree.x86.asm
	gcc -o lookup_tree main.c lookup_tree.x86.o
	nasm -f elf64 erase_unused.x86.asm
	gcc -o erase_unused main.c erase_unused.x86.o

clean:
	rm -f *.o
	rm -f factorial_accumulator
	rm -f fibonacci_recursive
	rm -f sum_range
	rm -f iterate_increment
	rm -f match_options
	rm -f lookup_tree
	rm -f erase_unused
