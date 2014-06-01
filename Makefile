
MC_FILES := main.rs \
            lexer.rs \
            parser.rs \
            span.rs \
            typechecker.rs \
            package.rs \
            resolver.rs \
            util.rs \
            values.rs \
            ast/mod.rs \
            ast/visit.rs \
            ast/defmap.rs \
			target/ccross.rs \
            ir/mod.rs \
            ir/ast_to_intermediate.rs \
            ir/liveness.rs \
            ir/constant_fold.rs \
			intermediate_tests.rs \

TEST_FILES := test_enums.mc \

mc: $(addprefix src/,$(MC_FILES))
	rustc $< -o $@

mc-tests: $(addprefix src/,$(MC_FILES))
	rustc --test $< -o $@ -g

ir-tests: $(addprefix src/,$(MC_FILES))
	rustc $< --cfg ir_tests -o $@

all: mc mc-tests ir-tests

run-tests: mc-tests
	./mc-tests

run-ir-tests: ir-tests
	./ir-tests

run-all-tests: run-tests run-ir-tests test

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

test: test/c test/c-bin

test/c: $(addprefix test/,$(patsubst %.mc,c/%.c,$(TEST_FILES)))

test/c-bin: $(addprefix test/,$(patsubst %.mc,c-bin/%,$(TEST_FILES)))

test/c/%.c: test/%.mc mc
	mkdir -p $(dir $@)
	./mc --target c < $< > $@ 2>$(addsuffix .log,$@) || (cat $@; cat $(addsuffix .log,$@); false)

test/c-bin/%: test/c/%.c
	cat $<
	mkdir -p $(dir $@)
	gcc $< -o $@

all-c-tests: test/c-bin/test_casts test/c-bin/test_param_types test/c-bin/test_recursive_types test/c-bin/test
	for a in $^; do $$a; done

.PHONY: all docs clean run-tests run-ir-tests run-all-tests
clean:
	rm -rf *~ doc mc mc-tests ir-tests test/c test/c-bin
