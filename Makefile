CXX ?= g++
CXXFLAGS ?= -std=c++17 -Wall -Wextra -O2
BUILD_DIR ?= $(abspath ../../../build)

VEXEL_ROOT ?= $(if $(VEXEL_ROOT_DIR),$(VEXEL_ROOT_DIR),$(abspath ../../..))
FRONTEND_INCLUDE_DIRS := $(shell find $(VEXEL_ROOT)/frontend/src -type d -print)

CXXFLAGS += -Isrc $(addprefix -I,$(FRONTEND_INCLUDE_DIRS))

TARGET = $(BUILD_DIR)/backends/megalinker/libvexel-megalinker.a

LIB_SOURCES = $(wildcard src/*.cpp)
LIB_OBJECTS = $(patsubst src/%.cpp,$(BUILD_DIR)/backends/megalinker/%.o,$(LIB_SOURCES))

.PHONY: all clean test

all: $(TARGET)

$(TARGET): $(LIB_OBJECTS)
	@mkdir -p $(@D)
	ar rcs $@ $^

$(BUILD_DIR)/backends/megalinker/%.o: src/%.cpp
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(LIB_OBJECTS)
	rm -f $(LIB_OBJECTS:.o=.d)
	rm -f $(TARGET)
	rmdir --ignore-fail-on-non-empty $(BUILD_DIR)/backends/megalinker 2>/dev/null || true
	rmdir --ignore-fail-on-non-empty $(BUILD_DIR)/backends 2>/dev/null || true
	rmdir --ignore-fail-on-non-empty $(BUILD_DIR) 2>/dev/null || true

test:
	@bash tests/test.sh

-include $(LIB_OBJECTS:.o=.d)
