CC             ?= gcc
CROSS_COMPILE  ?= 
CROSS_CC       := $(CROSS_COMPILE)$(CC)

SRC            := src/main.c
TARGET         := graph_paths

TARGET_DIR     := target
DEBUG_DIR      := $(TARGET_DIR)/debug
RELEASE_DIR    := $(TARGET_DIR)/release
WIN64_DEBUG_DIR   := $(TARGET_DIR)/win64-debug
WIN64_RELEASE_DIR := $(TARGET_DIR)/win64-release

CFLAGS_COMMON  := -std=c17 -Wall -Wextra
LDFLAGS        :=

CFLAGS_DEBUG   := -O0 -g $(CFLAGS_COMMON)
CFLAGS_RELEASE := -O2 -DNDEBUG $(CFLAGS_COMMON)

# Windows-specific flags (optional: static link to avoid DLLs)
CFLAGS_WIN64   := -static

.PHONY: debug release win64-debug win64-release clean run

debug: CC := gcc
debug: CFLAGS := $(CFLAGS_DEBUG)
debug: $(DEBUG_DIR)/$(TARGET)
	@echo "[DEBUG] Linux сборка завершена: $<"

release: CC := gcc
release: CFLAGS := $(CFLAGS_RELEASE)
release: $(RELEASE_DIR)/$(TARGET)
	@echo "[RELEASE] Linux сборка завершена: $<"

win64-debug: CROSS_COMPILE := x86_64-w64-mingw32-
win64-debug: CC := gcc
win64-debug: CFLAGS := $(CFLAGS_DEBUG) $(CFLAGS_WIN64)
win64-debug: $(WIN64_DEBUG_DIR)/$(TARGET).exe
	@echo "[WIN64 DEBUG] Сборка завершена: $<"

win64-release: CROSS_COMPILE := x86_64-w64-mingw32-
win64-release: CC := gcc
win64-release: CFLAGS := $(CFLAGS_RELEASE) $(CFLAGS_WIN64)
win64-release: $(WIN64_RELEASE_DIR)/$(TARGET).exe
	@echo "[WIN64 RELEASE] Сборка завершена: $<"

# Generic rule for output
$(DEBUG_DIR)/$(TARGET) \
$(RELEASE_DIR)/$(TARGET) \
$(WIN64_DEBUG_DIR)/$(TARGET).exe \
$(WIN64_RELEASE_DIR)/$(TARGET).exe: $(SRC)
	@mkdir -p $(dir $@)
	$(CROSS_CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

run: debug
	@echo "[RUN] Запуск $(DEBUG_DIR)/$(TARGET)"
	@$(DEBUG_DIR)/$(TARGET)

clean:
	rm -rf $(TARGET_DIR)
