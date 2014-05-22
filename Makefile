
MC_FILES := src/main.rs \
            src/lexer.rs \
            src/parser.rs \
            src/span.rs \
            src/typechecker.rs \
            src/package.rs \
            src/resolver.rs \
            src/util.rs \
            src/values.rs \
            src/ast/mod.rs \
            src/ast/visit.rs \
            src/ast/defmap.rs \
			src/target/ccross.rs \
            src/ir/mod.rs \
            src/ir/ast_to_intermediate.rs \
            src/ir/liveness.rs \
            src/ir/constant_fold.rs \
			src/intermediate_tests.rs \

mc: $(MC_FILES)
	rustc $< -o $@

mc-tests: $(MC_FILES)
	rustc --test $< -o $@ -g

ir-tests: $(MC_FILES)
	rustc $< --cfg ir_tests -o $@

all: mc mc-tests ir-tests

test: mc-tests
	./mc-tests

test-ir: ir-tests
	./ir-tests

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

%.c: %.mc mc
	./mc --target c < $< > $@
	cat $@

test/%: test/%.c
	gcc $< -o $@

.PHONY: all test docs clean
clean:
	rm -rf *~ doc mc mc-tests ir-tests
