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
	CFLAGS = $(CFLAGS_COMMON) -Wall -DSDL_MAIN_HANDLED
	LDLIBS = -lSDL3
endif

# Тип сборки
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

# Исходники и цели
TARGET = $(BUILD_DIR)/main
SRCS = $(wildcard src/*.c)
OBJS = $(patsubst src/%.c,$(BUILD_DIR)/%.o,$(SRCS))

# Правила сборки
$(TARGET): $(OBJS)
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS) $(LDLIBS)

# Компиляция отдельных файлов
$(BUILD_DIR)/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# Таргеты для удобства
release: CFLAGS += -DNDEBUG
release: $(TARGET)

debug: $(TARGET)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -rf build/*

.PHONY: clean run release debug