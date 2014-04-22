all: regexp-tests lexer-tests parser-tests docs

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

lexer-tests: libregexp-*.rlib
parser-tests: liblexer-*.rlib
liblexer-*.rlib: libregexp-*.rlib

%-tests: %.rs
	rustc -L. --test $< -o $@

lib%-*.rlib: %.rs
	rustc -L. -O $<

.PHONY: clean
clean:
	rm -rf *~ doc *-tests *.rlib
