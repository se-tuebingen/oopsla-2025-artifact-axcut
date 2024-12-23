LANGS = axcut mlton ocaml koka koka_opt rust rust_opt
PROGRAMS = factorial_accumulator fibonacci_recursive sum_range iterate_increment match_options lookup_tree erase_unused

build_%:
	@make -C $* build

clean: $(foreach LANG,$(LANGS),clean_$(LANG))

clean_%:
	@make -C $* clean

test: $(foreach LANG,$(LANGS),test_$(LANG))
	@echo "Tests successful!"

test_%: build_%
	@for PROG in $(PROGRAMS); do\
			if ! ./$*/$${PROG} 10 | diff - expected/$${PROG}.txt; then exit 1; fi;\
	done

bench: $(foreach LANG,$(LANGS),bench_$(LANG))

bench_axcut: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_axcut)
bench_mlton: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_mlton)
bench_ocaml: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_ocaml)
bench_koka: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_koka)
bench_koka_opt: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_koka_opt)
bench_rust: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_rust)
bench_rust_opt: $(foreach PROG,$(PROGRAMS),bench_$(PROG)_rust_opt)

bench_factorial_accumulator_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/factorial_accumulator.csv \
    './$*/factorial_accumulator 1000000' \
    './$*/factorial_accumulator 2000000' \
    './$*/factorial_accumulator 3000000' \
    './$*/factorial_accumulator 4000000' \
    './$*/factorial_accumulator 5000000' \
    './$*/factorial_accumulator 6000000' \
    './$*/factorial_accumulator 7000000' \
    './$*/factorial_accumulator 8000000' \
    './$*/factorial_accumulator 9000000' \
    './$*/factorial_accumulator 10000000'

bench_fibonacci_recursive_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/fibonacci_recursive.csv \
    './$*/fibonacci_recursive 31' \
    './$*/fibonacci_recursive 32' \
    './$*/fibonacci_recursive 33' \
    './$*/fibonacci_recursive 34' \
    './$*/fibonacci_recursive 35' \
    './$*/fibonacci_recursive 36' \
    './$*/fibonacci_recursive 37' \
    './$*/fibonacci_recursive 38' \
    './$*/fibonacci_recursive 39' \
    './$*/fibonacci_recursive 40'

bench_sum_range_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/sum_range.csv \
    './$*/sum_range 1000000' \
    './$*/sum_range 2000000' \
    './$*/sum_range 3000000' \
    './$*/sum_range 4000000' \
    './$*/sum_range 5000000' \
    './$*/sum_range 6000000' \
    './$*/sum_range 7000000' \
    './$*/sum_range 8000000' \
    './$*/sum_range 9000000' \
    './$*/sum_range 10000000'

bench_iterate_increment_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/iterate_increment.csv \
    './$*/iterate_increment 10000000' \
    './$*/iterate_increment 20000000' \
    './$*/iterate_increment 30000000' \
    './$*/iterate_increment 40000000' \
    './$*/iterate_increment 50000000' \
    './$*/iterate_increment 60000000' \
    './$*/iterate_increment 70000000' \
    './$*/iterate_increment 80000000' \
    './$*/iterate_increment 90000000' \
    './$*/iterate_increment 100000000'

bench_match_options_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/match_options.csv \
    './$*/match_options 1000000' \
    './$*/match_options 2000000' \
    './$*/match_options 3000000' \
    './$*/match_options 4000000' \
    './$*/match_options 5000000' \
    './$*/match_options 6000000' \
    './$*/match_options 7000000' \
    './$*/match_options 8000000' \
    './$*/match_options 9000000' \
    './$*/match_options 10000000'

bench_lookup_tree_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/lookup_tree.csv \
    './$*/lookup_tree 1000000' \
    './$*/lookup_tree 2000000' \
    './$*/lookup_tree 3000000' \
    './$*/lookup_tree 4000000' \
    './$*/lookup_tree 5000000' \
    './$*/lookup_tree 6000000' \
    './$*/lookup_tree 7000000' \
    './$*/lookup_tree 8000000' \
    './$*/lookup_tree 9000000' \
    './$*/lookup_tree 10000000'

bench_erase_unused_%: build_%
	ulimit -s 2097152 && hyperfine -w 10 --export-csv $*/erase_unused.csv \
    './$*/erase_unused 1000' \
    './$*/erase_unused 2000' \
    './$*/erase_unused 3000' \
    './$*/erase_unused 4000' \
    './$*/erase_unused 5000' \
    './$*/erase_unused 6000' \
    './$*/erase_unused 7000' \
    './$*/erase_unused 8000' \
    './$*/erase_unused 9000' \
    './$*/erase_unused 10000'

show:
	@rm -f results-last.md
	@echo benchmark > results-last-mean.csv
	@for PROG in $(PROGRAMS); do\
		echo $${PROG} >> results-last-mean.csv;\
	done
	@for LANG in $(LANGS); do\
		echo $${LANG} > results-$${LANG}-mean.csv;\
		sed -n '1p' $${LANG}/erase_unused.csv > results-$${LANG}.csv;\
		for PROG in $(PROGRAMS); do\
			sed -n '$$p' $${LANG}/$${PROG}.csv | tee -a results-$${LANG}.csv | cut -d',' -f2 >> results-$${LANG}-mean.csv;\
		done;\
		paste -d',' results-last-mean.csv results-$${LANG}-mean.csv > tempfile && mv tempfile results-last-mean.csv;\
		echo "### $${LANG}" | tee -a results-last.md;\
		PYTHONWARNINGS="ignore" csvlook --max-columns 4 results-$${LANG}.csv | tee -a results-last.md;\
		echo | tee -a results-last.md;\
		rm -f results-$${LANG}.csv results-$${LANG}-mean.csv;\
	done
	@echo "### mean" | tee -a results-last.md
	@PYTHONWARNINGS="ignore" csvlook results-last-mean.csv | tee -a results-last.md
	@rm -f results-last-mean.csv
