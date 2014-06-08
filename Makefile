RUST_FLAGS ?=

MC_FILES := \
	main.rs \
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
	ir/ssa.rs \
	ir/util.rs \
	intermediate_tests.rs \

TEST_FILES := \
	test.mc \
	test_casts.mc \
	test_indexing.mc \
	test_param_types.mc \
	test_pointer_arith.mc \
	test_recursive_types.mc \
	test_array.mc \
	test_globals.mc

mc: $(addprefix src/,$(MC_FILES))
	rustc $(RUST_FLAGS) $< -o $@ -g

mc-tests: $(addprefix src/,$(MC_FILES))
	rustc $(RUST_FLAGS) --test $< -o $@ -g

ir-tests: $(addprefix src/,$(MC_FILES))
	rustc $(RUST_FLAGS) $< --cfg ir_tests -o $@

all: mc mc-tests ir-tests

run-tests: mc-tests
	./mc-tests

run-ir-tests: ir-tests
	./ir-tests

check: run-tests run-ir-tests test

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

test: test/c test/c-bin

test/c: $(addprefix test/,$(patsubst %.mc,c/%.c,$(TEST_FILES)))

test/c-bin: $(addprefix test/,$(patsubst %.mc,c-bin/%,$(TEST_FILES)))

test/c/%.c: test/%.mc mc
	mkdir -p $(dir $@)
	./mc --target c < $< > $@ 2>$(addsuffix .log,$@) || (cat $@; cat $(addsuffix .log,$@); rm $@; false)

test/c-bin/%: test/c/%.c
	mkdir -p $(dir $@)
	gcc $< -o $@ || (cat $<; false)

.PHONY: all docs clean run-tests run-ir-tests check
clean:
	rm -rf *~ doc mc mc-tests ir-tests test/c test/c-bin
