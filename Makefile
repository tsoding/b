BUILD=build
THIRDPARTY=thirdparty
SRC=src
EXAMPLES=examples

$(BUILD)/b: $(SRC)/b.rs $(SRC)/libc.rs $(SRC)/nob.rs $(SRC)/stb_c_lexer.rs $(BUILD)/nob.o $(BUILD)/stb_c_lexer.o $(BUILD)/flag.o
	rustc --edition 2021 -g -C opt-level=z -C link-args="-lc $(BUILD)/nob.o $(BUILD)/stb_c_lexer.o $(BUILD)/flag.o" -C panic="abort" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/nob.o: $(THIRDPARTY)/nob.h $(BUILD)
	clang -g -x c -DNOB_IMPLEMENTATION -c $(THIRDPARTY)/nob.h -o $(BUILD)/nob.o

$(BUILD)/stb_c_lexer.o: $(THIRDPARTY)/stb_c_lexer.h $(BUILD)
	clang -g -x c -DSTB_C_LEXER_IMPLEMENTATION -c $(THIRDPARTY)/stb_c_lexer.h -o $(BUILD)/stb_c_lexer.o

$(BUILD)/flag.o: $(THIRDPARTY)/flag.h $(BUILD)
	clang -g -x c -DFLAG_IMPLEMENTATION -c $(THIRDPARTY)/flag.h -o $(BUILD)/flag.o

$(BUILD):
	mkdir -pv $(BUILD)

.PHONY: examples
examples: $(BUILD)/hello.js $(BUILD)/hello $(BUILD)/hello.ir $(BUILD)/e $(BUILD)/e.js $(BUILD)/e.ir

$(BUILD)/hello: $(EXAMPLES)/hello.b $(BUILD)/b
	$(BUILD)/b $(EXAMPLES)/hello.b -o $(BUILD)/hello

$(BUILD)/hello.js: $(EXAMPLES)/hello.b $(BUILD)/b
	$(BUILD)/b $(EXAMPLES)/hello.b -o $(BUILD)/hello.js -target js

$(BUILD)/hello.ir: $(EXAMPLES)/hello.b $(BUILD)/b
	$(BUILD)/b $(EXAMPLES)/hello.b -o $(BUILD)/hello.ir -target ir

$(BUILD)/e: $(EXAMPLES)/e.b $(BUILD)/b
	$(BUILD)/b $(EXAMPLES)/e.b -o $(BUILD)/e

$(BUILD)/e.js: $(EXAMPLES)/e.b $(BUILD)/b
	$(BUILD)/b $(EXAMPLES)/e.b -o $(BUILD)/e.js -target js

$(BUILD)/e.ir: $(EXAMPLES)/e.b $(BUILD)/b
	$(BUILD)/b $(EXAMPLES)/e.b -o $(BUILD)/e.ir -target ir

# TODO: use nob to build the project
