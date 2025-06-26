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
	$(SRC)/targets.rs \
	$(SRC)/codegen/fasm_x86_64.rs \
	$(SRC)/codegen/gas_aarch64_linux.rs \
	$(SRC)/codegen/gas_sh4dsp_prizm.rs \
	$(SRC)/codegen/gas_x86_64.rs \
	$(SRC)/codegen/uxn.rs \
	$(SRC)/codegen/ir.rs \
	$(SRC)/codegen/mod.rs \
	$(SRC)/runner/fasm_x86_64_linux.rs \
	$(SRC)/runner/fasm_x86_64_windows.rs \
	$(SRC)/runner/gas_x86_64_linux.rs \
	$(SRC)/runner/gas_x86_64_windows.rs \
	$(SRC)/runner/gas_aarch64_linux.rs \
	$(SRC)/runner/mod.rs \
	$(SRC)/runner/mos6502.rs \
	$(SRC)/runner/uxn.rs

LINUX_OBJS=\
	$(BUILD)/nob.linux.o \
	$(BUILD)/flag.linux.o \
	$(BUILD)/libc.linux.o \
	$(BUILD)/arena.linux.o \
	$(BUILD)/fake6502.linux.o

MINGW32_OBJS=\
	$(BUILD)/nob.mingw32.o \
	$(BUILD)/flag.mingw32.o \
	$(BUILD)/libc.mingw32.o \
	$(BUILD)/arena.mingw32.o \
	$(BUILD)/fake6502.mingw32.o

.PHONY: all
all: $(BUILD)/b $(BUILD)/btest

.PHONY: test
test: $(BUILD)/b $(BUILD)/btest
	$(BUILD)/btest

$(BUILD)/b: $(RSS) $(LINUX_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) -C link-args="$(LDFLAGS) $(LINUX_OBJS) -lc -lgcc" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/btest: $(SRC)/btest.rs $(RSS) $(LINUX_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) -C link-args="$(LDFLAGS) $(LINUX_OBJS) -lc -lgcc" $(SRC)/btest.rs -o $(BUILD)/btest

$(BUILD)/%.linux.o: ./thirdparty/%.c | $(BUILD)
	$(CC) -fPIC -g -c $< -o $@ $(LDFLAGS)

# Cross-compilation on Linux to Windows using mingw32-w64
# Invoked on demand by `make ./build/b.exe`
$(BUILD)/b.exe: $(RSS) $(MINGW32_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) --target x86_64-pc-windows-gnu -C link-args="$(MINGW32_OBJS) -lmingwex -lmsvcrt -lkernel32" $(SRC)/b.rs -o $(BUILD)/b.exe

$(BUILD)/btest.exe: $(SRC)/btest.rs $(RSS) $(MINGW32_OBJS) | $(BUILD)
	rustc $(CRUST_FLAGS) --target x86_64-pc-windows-gnu -C link-args="$(MINGW32_OBJS) -lmingwex -lmsvcrt -lkernel32" $(SRC)/btest.rs -o $(BUILD)/btest.exe

$(BUILD)/%.mingw32.o: ./thirdparty/%.c | $(BUILD)
	x86_64-w64-mingw32-gcc -fPIC -g -c $< -o $@

$(BUILD):
	mkdir -pv $(BUILD)
