# At the moment, all that exists is the regexp library.

all: regexp-tests docs

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

%-tests: %.rs
	rustc --test $< -o $@

.PHONY: clean
clean:
	rm -rf *~ doc *-tests
