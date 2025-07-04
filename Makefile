BUILD = build
SRC = src
CRUST_FLAGS = -g --edition 2021 -C opt-level=0 -C panic="abort"

ifeq ($(OS),Windows_NT)
    DETECTED_OS := Windows
else
    DETECTED_OS := $(shell uname)
endif

ifeq ($(DETECTED_OS),Darwin)
    LDFLAGS = -lc
else ifeq ($(DETECTED_OS),Linux)
    LDFLAGS = -lc -lgcc
else
    LDFLAGS =
endif

RSS = \
    $(SRC)/arena.rs \
    $(SRC)/b.rs \
    $(SRC)/crust.rs \
    $(SRC)/flag.rs \
    $(SRC)/lexer.rs \
    $(SRC)/nob.rs \
    $(SRC)/targets.rs \
    $(SRC)/codegen/fasm_x86_64.rs \
    $(SRC)/codegen/gas_aarch64.rs \
    $(SRC)/codegen/uxn.rs \
    $(SRC)/codegen/ir.rs \
    $(SRC)/codegen/mod.rs \
    $(SRC)/runner/fasm_x86_64_linux.rs \
    $(SRC)/runner/fasm_x86_64_windows.rs \
    $(SRC)/runner/gas_x86_64_linux.rs \
    $(SRC)/runner/gas_x86_64_windows.rs \
    $(SRC)/runner/gas_aarch64_linux.rs \
    $(SRC)/runner/gas_aarch64_darwin.rs \
    $(SRC)/runner/mod.rs \
    $(SRC)/runner/mos6502.rs \
    $(SRC)/runner/uxn.rs

POSIX_OBJS = \
    $(BUILD)/nob.posix.o \
    $(BUILD)/flag.posix.o \
    $(BUILD)/libc.posix.o \
    $(BUILD)/arena.posix.o \
    $(BUILD)/fake6502.posix.o \
    $(BUILD)/jim.posix.o \
    $(BUILD)/jimp.posix.o

MINGW32_OBJS = \
    $(BUILD)/nob.mingw32.o \
    $(BUILD)/flag.mingw32.o \
    $(BUILD)/libc.mingw32.o \
    $(BUILD)/arena.mingw32.o \
    $(BUILD)/fake6502.mingw32.o \
    $(BUILD)/jim.mingw32.o \
    $(BUILD)/jimp.mingw32.o

.PHONY: all
all: build-linux

.PHONY: build-linux
build-linux: $(BUILD)/b $(BUILD)/btest
	@echo "[✔] Built for Linux."

.PHONY: build-windows
build-windows: $(BUILD)/b.exe $(BUILD)/btest.exe
	@echo "[✔] Built for Windows (MinGW)."

$(BUILD)/b: $(RSS) $(POSIX_OBJS) | $(BUILD)
	@echo "[🔨] Compiling b (Linux)"
	@rustc $(CRUST_FLAGS) -C link-args="$(POSIX_OBJS) $(LDFLAGS)" $(SRC)/b.rs -o $(BUILD)/b

$(BUILD)/btest: $(SRC)/btest.rs $(RSS) $(POSIX_OBJS) | $(BUILD)
	@echo "[🔨] Compiling btest (Linux)"
	@rustc $(CRUST_FLAGS) -C link-args="$(POSIX_OBJS) $(LDFLAGS)" $(SRC)/btest.rs -o $(BUILD)/btest

$(BUILD)/%.posix.o: ./thirdparty/%.c | $(BUILD)
	@echo "[📦] Compiling $< (POSIX)"
	@$(CC) -fPIC -g -c $< -o $@ $(LDFLAGS)

$(BUILD)/b.exe: $(RSS) $(MINGW32_OBJS) | $(BUILD)
	@echo "[🔨] Compiling b.exe (Windows)"
	@rustc $(CRUST_FLAGS) --target x86_64-pc-windows-gnu -C link-args="$(MINGW32_OBJS) -lmingwex -lmsvcrt -lkernel32" $(SRC)/b.rs -o $(BUILD)/b.exe

$(BUILD)/btest.exe: $(SRC)/btest.rs $(RSS) $(MINGW32_OBJS) | $(BUILD)
	@echo "[🔨] Compiling btest.exe (Windows)"
	@rustc $(CRUST_FLAGS) --target x86_64-pc-windows-gnu -C link-args="$(MINGW32_OBJS) -lmingwex -lmsvcrt -lkernel32" $(SRC)/btest.rs -o $(BUILD)/btest.exe

$(BUILD)/%.mingw32.o: ./thirdparty/%.c | $(BUILD)
	@echo "[📦] Compiling $< (MinGW)"
	@x86_64-w64-mingw32-gcc -fPIC -g -c $< -o $@

$(BUILD):
	@mkdir -p $(BUILD)

.PHONY: test
test: $(BUILD)/btest
	@echo "[🧪] Running tests..."
	@$(BUILD)/btest

.PHONY: clean
clean:
	@echo "[🧹] Cleaning build directory..."
	@rm -rf $(BUILD)

.PHONY: tasks help
tasks help:
	@echo "Available make commands:"
	@echo "  make build-linux     - Build for Linux/POSIX"
	@echo "  make build-windows   - Cross-compile for Windows using MinGW"
	@echo "  make test            - Build and run tests"
	@echo "  make clean           - Remove build artifacts"
	@echo "  make all             - Alias for 'build-linux'"
	@echo "  make help / tasks    - Show this help message"


