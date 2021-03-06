TARGET ?= debug

CPU_SIM ?= ../cpu/sim/cpu_sim/cpu_sim

SIM_DURATION ?= 5

MC_FILES := \
	lib.rs \
	package.rs \
	span.rs \
	typechecker.rs \
	values.rs \
	codegen/combine.rs \
	codegen/ir_to_asm.rs \
	codegen/mod.rs \
	codegen/register_color.rs \
	intrinsics/mod.rs \
	intrinsics/size_of.rs \
	ir/ast_to_intermediate.rs \
	ir/conflicts.rs \
	ir/constant_fold.rs \
	ir/dead_code.rs \
	ir/inliner.rs \
	ir/liveness.rs \
	ir/mod.rs \
	ir/multiply_optimizer.rs \
	ir/ssa.rs \
	ir/util.rs \
	mas/ast.rs \
	mas/encoder.rs \
	mas/labels.rs \
	mas/lexer.rs \
	mas/mod.rs \
	mas/parser.rs \
	mas/scheduler.rs \
	mas/util.rs \
	mc/ast/defmap.rs \
	mc/ast/pathmap.rs \
	mc/ast/macros.rs \
	mc/ast/mod.rs \
	mc/ast/mut_visitor.rs \
	mc/ast/visitor.rs \
	mc/mod.rs \
	mc/deps.rs \
	mc/lexer.rs \
	mc/parser.rs \
	mc/prelude.mb \
	mc/std.mb \
	mc/resolver.rs \
	mc/session.rs \
	target/asm.rs \
	target/ccross.rs \
	target/debug_info.rs \
	target/ir.rs \
	target/mod.rs \
	target/util.rs \
	util/lexer.rs \
	util/graph.rs \
	util/mod.rs \

LIBS := \
	prelude.ma \
	prelude_bsld.ma \
	prelude.mb

ASM_ONLY_TESTS = test_inline_asm_basic.mb

TEST_FILES := $(patsubst test/%,%,$(wildcard test/test_*.mb))

IR_TEST_FILES := $(TEST_FILES)

ASM_TEST_FILES := $(TEST_FILES) $(patsubst test/%,%,$(wildcard test/asm_test_*.mb))

mbc mas: $(addprefix src/,$(MC_FILES)) $(addprefix lib/,$(LIBS))
ifeq ($(TARGET),debug)
	cargo build
else
	cargo build --release
endif
	ln -sf target/$(TARGET)/mbc .
	ln -sf target/$(TARGET)/mas .

unittest: $(addprefix src/,$(MC_FILES))
	cargo test

all: mc mas unittest

run-tests: unittest
	./unittest 2>/dev/null

check: run-tests test

docs: doc/regexp/index.html

doc/%/index.html: %.rs
	rustdoc $<

test: test/c test/c-bin test/c-results test/ir-c-results test/asm-results

test/c: $(addprefix test/,$(patsubst %.mb,c/%.c,$(TEST_FILES)))
test/ir-c: $(addprefix test/,$(patsubst %.mb,ir-c/%.c,$(IR_TEST_FILES)))

test/c-bin: $(addprefix test/,$(patsubst %.mb,c-bin/%,$(TEST_FILES)))
test/ir-c-bin: $(addprefix test/,$(patsubst %.mb,ir-c-bin/%,$(IR_TEST_FILES)))
test/asm-bin: $(addprefix test/,$(patsubst %.mb,asm-bin/%.bin,$(ASM_TEST_FILES)))

test/c-results: $(addprefix test/,$(patsubst %.mb,c-results/%.txt,$(TEST_FILES)))
test/ir-c-results: $(addprefix test/,$(patsubst %.mb,ir-c-results/%.txt,$(IR_TEST_FILES)))
test/asm-results: $(addprefix test/,$(patsubst %.mb,asm-results/%.txt,$(ASM_TEST_FILES)))

test/c/%.c: test/%.mb mbc
	@mkdir -p $(dir $@)
	@echo -e " MBC\t$<"
	@./mbc --target c $< -o $@ 2>$(addsuffix .log,$@) || (cat $@; cat $(addsuffix .log,$@); rm $@; false)

test/ir-c/%.c: test/%.mb mbc
	@mkdir -p $(dir $@)
	@echo -e " MBC\t$<"
	@./mbc --target ir $< -o $@ 2>$(addsuffix .log,$@) || (cat $@; cat $(addsuffix .log,$@); rm $@; false)

test/c-bin/%: test/c/%.c test/%.txt
	@echo Running $(patsubst test/c/%.c,%,$<)...
	@mkdir -p $(dir $@)
	@gcc -m32 $< -o $@ || (cat $<; false)

test/ir-c-bin/%: test/ir-c/%.c test/%.txt
	@echo Running $(patsubst test/ir-c/%.c,%,$<) '(IR cross-compiler)'...
	@mkdir -p $(dir $@)
	@gcc -m32 $< -o $@ || (cat $<; false)

test/asm-bin/%.bin: test/%.mb mbc
	@echo Running $(patsubst test/ir-c/%.c,%,$<) '(ASM and sim)'...
	@mkdir -p $(dir $@)
	@./mbc --target asm $< -o $@ >$(addsuffix .log,$@) || (cat $(addsuffix .log,$@); rm $@; false)

test/c-results/%.txt: test/c-bin/%
	@mkdir -p $(dir $@)
	@./$< > $@
	@diff $@ $(patsubst test/c-results/%.txt,test/%.txt,$@)

test/ir-c-results/%.txt: test/ir-c-bin/%
	@mkdir -p $(dir $@)
	@./$< > $@
	@diff $@ $(patsubst test/ir-c-results/%.txt,test/%.txt,$@)

test/asm-results/%.txt: test/asm-bin/%.bin
	@mkdir -p $(dir $@)
	@((timeout $(SIM_DURATION) $(CPU_SIM) < $< |grep 'HAS VALUE' |sed 's/.*VALUE //;s/ .*//') 2>&1) > $@
	@diff $@ $(patsubst test/asm-results/%.txt,test/%.txt,$@)

.PHONY: all docs clean run-tests check
clean:
	rm -rf *~ doc mc mbc mas unittest test/c test/c-bin test/c-results/*.txt
