BUILD=build
SRC=src

CRUST_FLAGS=-g --edition 2021 -C opt-level=0 -C panic="abort"

RSS=\
	$(SRC)/arena.rs \
	$(SRC)/b.rs \
	$(SRC)/crust.rs \
	$(SRC)/flag.rs \
	$(SRC)/nob.rs \
	$(SRC)/stb_c_lexer.rs \
	$(SRC)/codegen/fasm_x86_64_linux.rs \
	$(SRC)/codegen/gas_aarch64_linux.rs \
	$(SRC)/codegen/ir.rs \
	$(SRC)/codegen/mod.rs

LINUX_OBJS=\
	$(BUILD)/nob.linux.o \
	$(BUILD)/stb_c_lexer.linux.o \
	$(BUILD)/flag.linux.o \
	$(BUILD)/libc.linux.o \
	$(BUILD)/arena.linux.o

MINGW32_OBJS=\
	$(BUILD)/nob.mingw32.o \
	$(BUILD)/stb_c_lexer.mingw32.o \
	$(BUILD)/flag.mingw32.o \
	$(BUILD)/libc.mingw32.o \
	$(BUILD)/arena.mingw32.o

$(BUILD)/b: $(RSS) $(LINUX_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) -C link-args="$(LINUX_OBJS) -lc -lgcc" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.linux.o: ./thirdparty/%.c | $(BUILD)
	clang -fPIC -g -c $< -o $@

# Cross-compilation on Linux to Windows using mingw32-w64
# Invoked on demand by `make ./build/b.exe`
$(BUILD)/b.exe: $(RSS) $(MINGW32_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) --target x86_64-pc-windows-gnu -C link-args="$(MINGW32_OBJS) -lmingwex -lmsvcrt -lkernel32" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.mingw32.o: ./thirdparty/%.c | $(BUILD)
	x86_64-w64-mingw32-gcc -fPIC -g -c $< -o $@

$(BUILD):
	mkdir -pv $(BUILD)

.PHONY: test
test: $(BUILD)/b
	$(BUILD)/b -run tests/assign_ref.b
	$(BUILD)/b -run tests/compare.b
	$(BUILD)/b -run tests/divmod.b
	$(BUILD)/b -run tests/e.b
	$(BUILD)/b -run tests/hello.b
	$(BUILD)/b -run tests/inc_dec.b
	$(BUILD)/b -run tests/literals.b
	$(BUILD)/b -run tests/minus_2.b
	$(BUILD)/b -run tests/return.b
	$(BUILD)/b -run tests/ternary.b
	$(BUILD)/b -run tests/ternary-side-effect.b
	$(BUILD)/b -run tests/unary_priority.b
	$(BUILD)/b -run tests/vector.b
