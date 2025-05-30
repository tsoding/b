BUILD=build
THIRDPARTY=thirdparty
SRC=src
RSS=\
	$(SRC)/arena.rs \
	$(SRC)/b.rs \
	$(SRC)/crust.rs \
	$(SRC)/flag.rs \
	$(SRC)/nob.rs \
	$(SRC)/stb_c_lexer.rs \
	$(SRC)/codegen/fasm_x86_64_linux.rs \
	$(SRC)/codegen/gas_aarch64_linux.rs \
	$(SRC)/codegen/uxn.rs \
	$(SRC)/codegen/ir.rs \
	$(SRC)/codegen/mod.rs
OBJS=\
	$(BUILD)/nob.o \
	$(BUILD)/stb_c_lexer.o \
	$(BUILD)/flag.o \
	$(BUILD)/arena.o

$(BUILD)/b: $(RSS) $(OBJS) | $(BUILD)
	rustc --edition 2021 -g -C opt-level=z -C link-args="$(OBJS) -lc -lgcc" -C panic="abort" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.o: $(THIRDPARTY)/%.c | $(BUILD)
	clang -fPIC -g -c $< -o $@

$(BUILD):
	mkdir -pv $(BUILD)

.PHONY: test
test: $(BUILD)/b
	$(BUILD)/b -run tests/compare.b
	$(BUILD)/b -run tests/deref_assign.b
	$(BUILD)/b -run tests/divmod.b
	$(BUILD)/b -run tests/e.b
	$(BUILD)/b -run tests/forward-declare.b
	$(BUILD)/b -run tests/hello.b
	$(BUILD)/b -run tests/inc_dec.b
	$(BUILD)/b -run tests/literals.b
	$(BUILD)/b -run tests/minus_2.b
	$(BUILD)/b -run tests/recursion.b
	$(BUILD)/b -run tests/ref.b
	$(BUILD)/b -run tests/return.b
	$(BUILD)/b -run tests/ternary.b
	$(BUILD)/b -run tests/ternary-side-effect.b
	$(BUILD)/b -run tests/unary_priority.b
	$(BUILD)/b -run tests/vector.b
