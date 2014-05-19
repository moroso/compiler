all: mc mc2c mc-tests ir_tests

test: mc-tests
	./mc-tests

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

LIB_FILES := src/lexer.rs \
             src/parser.rs \
             src/span.rs \
             src/typechecker.rs \
             src/resolver.rs \
             src/values.rs \
             src/ast/mod.rs \
             src/ast/visit.rs \
             src/ast/defmap.rs \
	           src/intermediate/mod.rs \
	           src/intermediate/ast_to_intermediate.rs \
	           src/intermediate/liveness.rs \
	           src/intermediate/constant_fold.rs \

MC_FILES := src/main.rs \
            $(LIB_FILES)

MC2C_FILES := src/mc2c.rs \
              $(LIB_FILES)

IR_TEST_FILES := src/intermediate_tests.rs \
              $(LIB_FILES)

%.c: %.mc mc2c
	./mc2c < $< > $@
	cat $@

test/%: test/%.c
	gcc $< -o $@

mc: $(MC_FILES)
	rustc $< -o $@

mc2c: $(MC2C_FILES)
	rustc $< -o $@

ir_tests: $(IR_TEST_FILES)
	rustc $< -o $@

mc-tests: $(MC_FILES)
	rustc --test $< -o $@ -g

.PHONY: all test docs clean
clean:
	rm -rf *~ doc mc mc-tests mc2c
