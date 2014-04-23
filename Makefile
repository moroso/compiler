all: mc

test: mc-tests
	./mc-tests

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

MC_FILES := src/main.rs src/regexp.rs src/lexer.rs

mc: $(MC_FILES)
	rustc $< -o $@

mc-tests: $(MC_FILES)
	rustc --test $< -o $@

.PHONY: all test docs clean
clean:
	rm -rf *~ doc mc mc-tests
