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
	ir/intermediate_to_c.rs \
	intermediate_tests.rs \

TEST_FILES := $(patsubst test/%,%,$(wildcard test/test_*.mb))

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

test: test/c test/c-bin test/c-results

test/c: $(addprefix test/,$(patsubst %.mb,c/%.c,$(TEST_FILES)))

test/c-bin: $(addprefix test/,$(patsubst %.mb,c-bin/%,$(TEST_FILES)))

test/c-results: $(addprefix test/,$(patsubst %.mb,c-results/%.txt,$(TEST_FILES)))

test/c/%.c: test/%.mb mc
	@mkdir -p $(dir $@)
	@./mc --target c < $< > $@ 2>$(addsuffix .log,$@) || (cat $@; cat $(addsuffix .log,$@); rm $@; false)

test/c-bin/%: test/c/%.c test/%.txt
	@echo Running $(patsubst test/c/%.c,%,$<)...
	@mkdir -p $(dir $@)
	@gcc $< -o $@ || (cat $<; false)

test/c-results/%.txt: test/c-bin/%
	@mkdir -p $(dir $@)
	@./$< > $@
	@diff $@ $(patsubst test/c-results/%.txt,test/%.txt,$@)

.PHONY: all docs clean run-tests run-ir-tests check
clean:
	rm -rf *~ doc mc mc-tests ir-tests test/c test/c-bin test/c-results/*.txt
