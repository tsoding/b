BUILD=build
THIRDPARTY=thirdparty
SRC=src
TEST_FLAGS=-o tests/build

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
	$(BUILD)/arena.o

define tests 
	$(BUILD)/b -run tests/$(1).b -o tests/build/$(1)
endef

$(BUILD)/b: $(RSS) $(OBJS) | $(BUILD)
	rustc --edition 2021 -g -C opt-level=z -C link-args="$(OBJS) -lc -lgcc" -C panic="abort" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/%.o: $(THIRDPARTY)/%.c | $(BUILD)
	clang -fPIC -g -c $< -o $@

$(BUILD):
	mkdir -pv $(BUILD)

.PHONY: test
test: $(BUILD)/b
	mkdir -pv tests/build/

	@$(call tests,assign_ref)
	@$(call tests,compare)
	@$(call tests,divmod)
	@$(call tests,e)
	@$(call tests,hello)
	@$(call tests,inc_dec)
	@$(call tests,literals)
	@$(call tests,minus_2)
	@$(call tests,return)
	@$(call tests,ternary)
	@$(call tests,ternary-side-effect)
	@$(call tests,unary_priority) 
	@$(call tests,vector) 
