BUILD=build
SRC=src

CRUST_FLAGS=-g --edition 2021 -C opt-level=0 -C panic="abort"

RSS=\
	$(SRC)/arena.rs \
	$(SRC)/b.rs \
	$(SRC)/crust.rs \
	$(SRC)/flag.rs \
	$(SRC)/lexer.rs \
	$(SRC)/nob.rs \
	$(SRC)/codegen/fasm_x86_64.rs \
	$(SRC)/codegen/gas_aarch64_linux.rs \
	$(SRC)/codegen/uxn.rs \
	$(SRC)/codegen/ir.rs \
	$(SRC)/codegen/mod.rs

# Default target Linux tests
LINUX_TESTS=\
	$(BUILD)/tests/args6 \
	$(BUILD)/tests/compare \
	$(BUILD)/tests/deref_assign \
	$(BUILD)/tests/divmod \
	$(BUILD)/tests/e \
	$(BUILD)/tests/forward-declare \
	$(BUILD)/tests/goto \
	$(BUILD)/tests/hello \
	$(BUILD)/tests/inc_dec \
	$(BUILD)/tests/lexer \
	$(BUILD)/tests/literals \
	$(BUILD)/tests/minus_2 \
	$(BUILD)/tests/recursion \
	$(BUILD)/tests/ref \
	$(BUILD)/tests/return \
	$(BUILD)/tests/switch \
	$(BUILD)/tests/stack_alloc \
	$(BUILD)/tests/ternary-side-effect \
	$(BUILD)/tests/ternary \
	$(BUILD)/tests/ternary-assign \
	$(BUILD)/tests/unary_priority \
	$(BUILD)/tests/vector \
	$(BUILD)/tests/multiple-postfix \
	$(BUILD)/tests/rvalue_call

GAS_AARCH64_LINUX_TESTS=\
	$(BUILD)/tests/args6-gas-aarch64-linux \
	$(BUILD)/tests/compare-gas-aarch64-linux \
	$(BUILD)/tests/deref_assign-gas-aarch64-linux \
	$(BUILD)/tests/divmod-gas-aarch64-linux \
	$(BUILD)/tests/e-gas-aarch64-linux \
	$(BUILD)/tests/forward-declare-gas-aarch64-linux \
	$(BUILD)/tests/goto-gas-aarch64-linux \
	$(BUILD)/tests/hello-gas-aarch64-linux \
	$(BUILD)/tests/inc_dec-gas-aarch64-linux \
	$(BUILD)/tests/lexer-gas-aarch64-linux \
	$(BUILD)/tests/literals-gas-aarch64-linux \
	$(BUILD)/tests/minus_2-gas-aarch64-linux \
	$(BUILD)/tests/recursion-gas-aarch64-linux \
	$(BUILD)/tests/ref-gas-aarch64-linux \
	$(BUILD)/tests/return-gas-aarch64-linux \
	$(BUILD)/tests/switch-gas-aarch64-linux \
	$(BUILD)/tests/stack_alloc-gas-aarch64-linux \
	$(BUILD)/tests/ternary-side-effect-gas-aarch64-linux \
	$(BUILD)/tests/ternary-gas-aarch64-linux \
	$(BUILD)/tests/ternary-assign-gas-aarch64-linux \
	$(BUILD)/tests/unary_priority-gas-aarch64-linux \
	$(BUILD)/tests/vector-gas-aarch64-linux \
	$(BUILD)/tests/multiple-postfix-gas-aarch64-linux \
	$(BUILD)/tests/rvalue_call-gas-aarch64-linux

MINGW32_TESTS=\
	$(BUILD)/tests/args6.exe \
	$(BUILD)/tests/compare.exe \
	$(BUILD)/tests/deref_assign.exe \
	$(BUILD)/tests/divmod.exe \
	$(BUILD)/tests/e.exe \
	$(BUILD)/tests/forward-declare.exe \
	$(BUILD)/tests/goto.exe \
	$(BUILD)/tests/hello.exe \
	$(BUILD)/tests/inc_dec.exe \
	$(BUILD)/tests/lexer.exe \
	$(BUILD)/tests/literals.exe \
	$(BUILD)/tests/minus_2.exe \
	$(BUILD)/tests/recursion.exe \
	$(BUILD)/tests/ref.exe \
	$(BUILD)/tests/return.exe \
	$(BUILD)/tests/switch.exe \
	$(BUILD)/tests/stack_alloc.exe \
	$(BUILD)/tests/ternary-side-effect.exe \
	$(BUILD)/tests/ternary.exe \
	$(BUILD)/tests/ternary-assign.exe \
	$(BUILD)/tests/unary_priority.exe \
	$(BUILD)/tests/vector.exe \
	$(BUILD)/tests/multiple-postfix.exe \
	$(BUILD)/tests/rvalue_call.exe

UXN_TESTS=\
	$(BUILD)/tests/args6.rom \
	$(BUILD)/tests/compare.rom \
	$(BUILD)/tests/deref_assign.rom \
	$(BUILD)/tests/divmod.rom \
	$(BUILD)/tests/e.rom \
	$(BUILD)/tests/forward-declare.rom \
	$(BUILD)/tests/goto.rom \
	$(BUILD)/tests/hello.rom \
	$(BUILD)/tests/inc_dec.rom \
	$(BUILD)/tests/lexer.rom \
	$(BUILD)/tests/literals.rom \
	$(BUILD)/tests/minus_2.rom \
	$(BUILD)/tests/recursion.rom \
	$(BUILD)/tests/ref.rom \
	$(BUILD)/tests/return.rom \
	$(BUILD)/tests/switch.rom \
	$(BUILD)/tests/stack_alloc.rom \
	$(BUILD)/tests/ternary-side-effect.rom \
	$(BUILD)/tests/ternary.rom \
	$(BUILD)/tests/ternary-assign.rom \
	$(BUILD)/tests/unary_priority.rom \
	$(BUILD)/tests/vector.rom \
	$(BUILD)/tests/multiple-postfix.rom \
	$(BUILD)/tests/rvalue_call.rom

LINUX_OBJS=\
	$(BUILD)/nob.linux.o \
	$(BUILD)/flag.linux.o \
	$(BUILD)/libc.linux.o \
	$(BUILD)/arena.linux.o

MINGW32_OBJS=\
	$(BUILD)/nob.mingw32.o \
	$(BUILD)/flag.mingw32.o \
	$(BUILD)/libc.mingw32.o \
	$(BUILD)/arena.mingw32.o

$(BUILD)/b: $(RSS) $(LINUX_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) -C link-args="$(LDFLAGS) $(LINUX_OBJS) -lc -lgcc" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.linux.o: ./thirdparty/%.c | $(BUILD)
	$(CC) -fPIC -g -c $< -o $@

# Cross-compilation on Linux to Windows using mingw32-w64
# Invoked on demand by `make ./build/b.exe`
$(BUILD)/b.exe: $(RSS) $(MINGW32_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) --target x86_64-pc-windows-gnu -C link-args="$(MINGW32_OBJS) -lmingwex -lmsvcrt -lkernel32" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.mingw32.o: ./thirdparty/%.c | $(BUILD)
	x86_64-w64-mingw32-gcc -fPIC -g -c $< -o $@

$(BUILD):
	mkdir -pv $(BUILD)

.PHONY: test
test: $(LINUX_TESTS)

$(BUILD)/tests/%: ./tests/%.b ./std/test.b $(BUILD)/b FORCE | $(BUILD)/tests
	$(BUILD)/b -run -o $@ $< ./std/test.b

.PHONY: test-gas-aarch64-linux
test-gas-aarch64-linux: $(GAS_AARCH64_LINUX_TESTS)

$(BUILD)/tests/%-gas-aarch64-linux: ./tests/%.b ./std/test.b $(BUILD)/b FORCE | $(BUILD)/tests
	$(BUILD)/b -t gas-aarch64-linux -run -o $@ $< ./std/test.b

.PHONY: test-mingw32
test-mingw32: $(MINGW32_TESTS)

$(BUILD)/tests/%.exe: ./tests/%.b ./std/test.b $(BUILD)/b FORCE | $(BUILD)/tests
	$(BUILD)/b -t fasm-x86_64-windows -run -o $@ $< ./std/test.b

$(BUILD)/tests:
	mkdir -pv $(BUILD)/tests

.PHONY: test-uxn
test-uxn: $(UXN_TESTS)

$(BUILD)/tests/%.rom: ./tests/%.b ./std/test.b ./std/uxn.b $(BUILD)/b FORCE | $(BUILD)/tests
	$(BUILD)/b -t uxn -o $@ $< ./std/test.b ./std/uxn.b
	uxncli $@

# https://www.gnu.org/software/make/manual/html_node/Force-Targets.html
FORCE:
