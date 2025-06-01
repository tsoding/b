BUILD=build
THIRDPARTY=thirdparty
SRC=src

RSS=\
	$(SRC)/arena.rs \
	$(SRC)/b.rs \
	$(SRC)/crust.rs \
	$(SRC)/flag.rs \
	$(SRC)/lexer.rs \
	$(SRC)/nob.rs \
	$(SRC)/codegen/fasm_x86_64_linux.rs \
	$(SRC)/codegen/gas_aarch64_linux.rs \
	$(SRC)/codegen/uxn.rs \
	$(SRC)/codegen/ir.rs \
	$(SRC)/codegen/mod.rs
OBJS=\
	$(BUILD)/nob.o \
	$(BUILD)/flag.o \
	$(BUILD)/arena.o
TESTS=\
	$(BUILD)/tests/compare \
	$(BUILD)/tests/deref_assign \
	$(BUILD)/tests/divmod \
	$(BUILD)/tests/e \
	$(BUILD)/tests/forward-declare \
	$(BUILD)/tests/goto \
	$(BUILD)/tests/hello \
	$(BUILD)/tests/inc_dec \
	$(BUILD)/tests/literals \
	$(BUILD)/tests/minus_2 \
	$(BUILD)/tests/recursion \
	$(BUILD)/tests/ref \
	$(BUILD)/tests/return \
	$(BUILD)/tests/ternary-side-effect \
	$(BUILD)/tests/ternary \
	$(BUILD)/tests/unary_priority \
	$(BUILD)/tests/vector


$(BUILD)/b: $(RSS) $(OBJS) | $(BUILD)
	rustc --edition 2021 -g -C opt-level=z -C link-args="$(OBJS) -lc -lgcc" -C panic="abort" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.o: $(THIRDPARTY)/%.c | $(BUILD)
	clang -fPIC -g -c $< -o $@

$(BUILD):
	mkdir -pv $(BUILD)

.PHONY: test
test: $(TESTS)

$(BUILD)/tests/%: ./tests/%.b ./std/test.b $(BUILD)/b FORCE | $(BUILD)/tests
	$(BUILD)/b -run -o $@ $< ./std/test.b

$(BUILD)/tests:
	mkdir -pv $(BUILD)/tests

# https://www.gnu.org/software/make/manual/html_node/Force-Targets.html
FORCE:
