all: ao_scheme_builtin.h ao_scheme_const.h test/ao_scheme_test

clean:
	+cd make-const && make clean
	+cd test && make clean
	rm -f ao_scheme_const.h ao_scheme_builtin.h

ao_scheme_const.h: ao_scheme_const.lisp make-const/ao_scheme_make_const
	make-const/ao_scheme_make_const -o $@ ao_scheme_const.lisp

ao_scheme_builtin.h: ao_scheme_make_builtin ao_scheme_builtin.txt
	nickle ao_scheme_make_builtin ao_scheme_builtin.txt > $@

make-const/ao_scheme_make_const: FRC
	+cd make-const && make ao_scheme_make_const

test/ao_scheme_test: FRC ao_scheme_const.h ao_scheme_builtin.h
	+cd test && make ao_scheme_test

FRC:
