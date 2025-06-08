CC             ?= gcc
CROSS_COMPILE  ?= 
CROSS_CC       := $(CROSS_COMPILE)$(CC)

SRC            := src/main.c
TARGET         := graph_paths

TARGET_DIR      := target
DEBUG_DIR      := $(TARGET_DIR)/debug
RELEASE_DIR    := $(TARGET_DIR)/release

CFLAGS_COMMON  := -std=c17 -Wall -Wextra
LDFLAGS        :=

CFLAGS_DEBUG   := -O0 -g $(CFLAGS_COMMON)
CFLAGS_RELEASE := -O2 -DNDEBUG $(CFLAGS_COMMON)

.PHONY: debug release clean run

debug: CFLAGS := $(CFLAGS_DEBUG)
debug: $(DEBUG_DIR)/$(TARGET)
	@echo "[DEBUG] Сборка завершена: $<"

release: CFLAGS := $(CFLAGS_RELEASE)
release: $(RELEASE_DIR)/$(TARGET)
	@echo "[RELEASE] Сборка завершена: $<"

$(DEBUG_DIR)/$(TARGET) $(RELEASE_DIR)/$(TARGET): $(SRC)
	@mkdir -p $(dir $@)
	$(CROSS_CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

run: debug
	@echo "[RUN] Запуск $(DEBUG_DIR)/$(TARGET)"
	@$(DEBUG_DIR)/$(TARGET)

clean:
	rm -rf $(TARGET_DIR)
