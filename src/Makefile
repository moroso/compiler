# At the moment, all that exists is the regexp library.

all: regexp regexp-tests lexer-tests docs

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

lexer-tests: libregexp-*.rlib

%-tests: %.rs
	rustc -L. --test $< -o $@

lib%-*.rlib: %.rs
	rustc -L. -O $<

.PHONY: clean
clean:
	rm -rf *~ doc *-tests *.rlib
