# Компиляторы (можно переопределить через CC=clang или CC=cl)
CC ?= gcc  # По умолчанию GCC

# Базовые пути (используйте переменные окружения для гибкости)
SDL_PREFIX ?= /C/Users/livin/MSYS2/ucrt64
CFLAGS_COMMON = -I$(SDL_PREFIX)/include/SDL3
LDFLAGS_COMMON = -L$(SDL_PREFIX)/lib

# Определение компилятора
ifeq ($(CC),cl)
	# Для MSVC
	CFLAGS = $(CFLAGS_COMMON) /W4 /nologo /DSDL_MAIN_HANDLED
	LDFLAGS = $(LDFLAGS_COMMON)
	LDLIBS = SDL3.lib /link /SUBSYSTEM:WINDOWS
else
	# Для GCC/Clang
	CFLAGS = $(CFLAGS_COMMON) -Wall -DSDL_MAIN_HANDLED -MMD -MP
	LDLIBS = -lSDL3
endif

# Тип сборки и директория сборки
ifeq ($(filter release,$(MAKECMDGOALS)),release)
	BUILD_DIR = build/release
	ifeq ($(CC),cl)
		CFLAGS += /O2  # Оптимизации для MSVC
	else
		CFLAGS += -O3  # Оптимизации для GCC/Clang
	endif
else
	BUILD_DIR = build/debug
	CFLAGS += -g -O0  # Отладочный режим
endif

# Исходники, объекты, зависимости и цель
SRC_DIR = src
SRCS = $(wildcard $(SRC_DIR)/*.c)
OBJS = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SRCS))
DEPS = $(OBJS:.o=.d)

TARGET = $(BUILD_DIR)/main

# Правило для создания директорий
$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# Основная цель: связывание
$(TARGET): $(BUILD_DIR) $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS) $(LDLIBS)

# Компиляция исходников (автоматическая генерация зависимостей)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# Включаем зависимости, если они были сгенерированы
-include $(DEPS)

# Таргеты для удобства
release: CFLAGS += -DNDEBUG
release: $(TARGET)

debug: $(TARGET)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -rf build

.PHONY: clean run release debug
