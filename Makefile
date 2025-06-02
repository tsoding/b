BUILD=build
THIRDPARTY=thirdparty
SRC=src

CC=clang
LINKFLAGS=-lc -lgcc

# CC=x86_64-w64-mingw32-cc
# RUSTFLAGS=--target x86_64-pc-windows-gnu
# LINKFLAGS=-lmsvcrt -lkernel32

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
OBJS=\
	$(BUILD)/nob.o \
	$(BUILD)/stb_c_lexer.o \
	$(BUILD)/flag.o \
	$(BUILD)/stdio.o \
	$(BUILD)/arena.o

$(BUILD)/b: $(RSS) $(OBJS) | $(BUILD)
	rustc $(RUSTFLAGS) --edition 2021 -g -C opt-level=0 -C link-args="$(OBJS) $(LINKFLAGS)" -C panic="abort" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.o: $(THIRDPARTY)/%.c | $(BUILD)
	$(CC) -fPIC -g -c $< -o $@

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
