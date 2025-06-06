CXX        := clang++
CXXSTD     := -std=c++17
WARNINGS   := -Wall -Wextra -Wpedantic

TARGET_OS  ?= native

SRC_DIR     := src
TARGET_DIR  := target
DEBUG_DIR   := $(TARGET_DIR)/debug
RELEASE_DIR := $(TARGET_DIR)/release

SRCS        := $(wildcard $(SRC_DIR)/*.cpp)
DEPS        := $(SRC_DIR)/graphviz.hpp

ifeq ($(TARGET_OS),windows)
BINSUF      := .exe
CROSS_FLAGS := --target=x86_64-w64-mingw32
else ifeq ($(TARGET_OS),linux)
BINSUF      :=
CROSS_FLAGS :=
else
BINSUF      :=
CROSS_FLAGS :=
endif

BIN_NAME       := graph$(BINSUF)
DEBUG_OBJS     := $(patsubst $(SRC_DIR)/%.cpp,$(DEBUG_DIR)/%.o,$(SRCS))
RELEASE_OBJS   := $(patsubst $(SRC_DIR)/%.cpp,$(RELEASE_DIR)/%.o,$(SRCS))

.PHONY: all debug release run clean

all: debug

debug: CXXFLAGS := $(CXXSTD) $(WARNINGS) -g $(CROSS_FLAGS)
debug: dirs $(DEBUG_OBJS)
	$(CXX) $(CXXFLAGS) -o $(DEBUG_DIR)/$(BIN_NAME) $(DEBUG_OBJS)

release: CXXFLAGS := $(CXXSTD) $(WARNINGS) -O2 $(CROSS_FLAGS)
release: dirs $(RELEASE_OBJS)
	$(CXX) $(CXXFLAGS) -o $(RELEASE_DIR)/$(BIN_NAME) $(RELEASE_OBJS)

$(DEBUG_DIR)/%.o: $(SRC_DIR)/%.cpp $(DEPS)
	@mkdir -p $(DEBUG_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(RELEASE_DIR)/%.o: $(SRC_DIR)/%.cpp $(DEPS)
	@mkdir -p $(RELEASE_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

run: debug
ifeq ($(TARGET_OS),native)
	./$(DEBUG_DIR)/$(BIN_NAME)
else ifeq ($(TARGET_OS),linux)
	./$(DEBUG_DIR)/$(BIN_NAME)
else ifeq ($(TARGET_OS),windows)
	@echo "Исполняемый: $(DEBUG_DIR)/$(BIN_NAME)"
endif

dirs:
	@mkdir -p $(TARGET_DIR)

clean:
	rm -rf $(TARGET_DIR)
