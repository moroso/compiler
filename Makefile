RUST_FLAGS ?=

MC_FILES := \
	main.rs \
	package.rs \
	span.rs \
	typechecker.rs \
	values.rs \
	intrinsics/mod.rs \
	intrinsics/size_of.rs \
	ir/ast_to_intermediate.rs \
	ir/constant_fold.rs \
	ir/liveness.rs \
	ir/mod.rs \
	ir/ssa.rs \
	ir/util.rs \
	mas/ast.rs \
	mas/lexer.rs \
	mas/mod.rs \
	mas/parser.rs \
	mc/ast/defmap.rs \
	mc/ast/mod.rs \
	mc/ast/visit.rs \
	mc/mod.rs \
	mc/lexer.rs \
	mc/parser.rs \
	mc/prelude.mb \
	mc/resolver.rs \
	mc/session.rs \
	target/ccross.rs \
	target/ir.rs \
	target/mod.rs \
	util/lexer.rs \
	util/mod.rs \

TEST_FILES := $(patsubst test/%,%,$(wildcard test/test_*.mb))

mc: $(addprefix src/,$(MC_FILES))
	rustc $(RUST_FLAGS) $< --cfg mc -o $@ -g

mas: $(addprefix src/,$(MC_FILES))
	rustc $(RUST_FLAGS) $< --cfg mas -o $@

unittest: $(addprefix src/,$(MC_FILES))
	rustc $(RUST_FLAGS) --test $< -o $@ -g

all: mc mas unittest

run-tests: unittest
	./unittest

check: run-tests test

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

test: test/c test/c-bin test/c-results

test/c: $(addprefix test/,$(patsubst %.mb,c/%.c,$(TEST_FILES)))

test/c-bin: $(addprefix test/,$(patsubst %.mb,c-bin/%,$(TEST_FILES)))

test/c-results: $(addprefix test/,$(patsubst %.mb,c-results/%.txt,$(TEST_FILES)))

test/c/%.c: test/%.mb mc
	@mkdir -p $(dir $@)
	@./mc --target c $< > $@ 2>$(addsuffix .log,$@) || (cat $@; cat $(addsuffix .log,$@); rm $@; false)

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
	rm -rf *~ doc mc mas unittest test/c test/c-bin test/c-results/*.txt
