all: ao_scheme_builtin.h make-const/ao_scheme_make_const test/ao-scheme tiny-test/ao-scheme-tiny

clean:
	+cd make-const && make clean
	+cd test && make clean
	+cd tiny-test && make clean
	rm -f ao_scheme_builtin.h

ao_scheme_builtin.h: ao_scheme_make_builtin ao_scheme_builtin.txt
	nickle ao_scheme_make_builtin ao_scheme_builtin.txt > $@

make-const/ao_scheme_make_const: FRC ao_scheme_builtin.h
	+cd make-const && make ao_scheme_make_const

test/ao-scheme: FRC ao_scheme_builtin.h make-const/ao_scheme_make_const
	+cd test && make

tiny-test/ao-scheme-tiny: FRC ao_scheme_builtin.h make-const/ao_scheme_make_const
	+cd tiny-test && make

install: all
	+cd test && make install
	+cd tiny-test && make install

FRC:
