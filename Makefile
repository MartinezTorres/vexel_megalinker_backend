CXX ?= g++
CXXFLAGS ?= -std=c++17 -Wall -Wextra -O2

VEXEL_ROOT ?= $(if $(VEXEL_ROOT_DIR),$(VEXEL_ROOT_DIR),$(abspath ../../..))
VEXEL_BUILD ?= $(VEXEL_ROOT)/build
FRONTEND_INCLUDE_DIRS := $(shell find $(VEXEL_ROOT)/frontend/src -type d -print)

CXXFLAGS += -Isrc $(addprefix -I,$(FRONTEND_INCLUDE_DIRS))

TARGET = $(VEXEL_BUILD)/backends/megalinker/libvexel-megalinker.a
CLI = $(VEXEL_BUILD)/vexel-megalinker
FRONTEND_LIB = $(VEXEL_BUILD)/lib/libvexelfrontend.a

LIB_SOURCES = $(filter-out src/megalinker_main.cpp,$(wildcard src/*.cpp))
LIB_OBJECTS = $(patsubst src/%.cpp,$(VEXEL_BUILD)/backends/megalinker/%.o,$(LIB_SOURCES))
CLI_OBJECT = $(VEXEL_BUILD)/backends/megalinker/megalinker_main.o

.PHONY: all clean test

all: $(TARGET) $(CLI)

$(TARGET): $(LIB_OBJECTS)
	@mkdir -p $(@D)
	ar rcs $@ $^

$(CLI): $(CLI_OBJECT) $(TARGET) $(FRONTEND_LIB)
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) $(CLI_OBJECT) -Wl,--start-group $(TARGET) $(FRONTEND_LIB) -Wl,--end-group -o $@

$(VEXEL_BUILD)/backends/megalinker/%.o: src/%.cpp
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(LIB_OBJECTS) $(CLI_OBJECT)
	rm -f $(LIB_OBJECTS:.o=.d) $(CLI_OBJECT:.o=.d)
	rm -f $(TARGET) $(CLI)
	rmdir --ignore-fail-on-non-empty $(VEXEL_BUILD)/backends/megalinker 2>/dev/null || true
	rmdir --ignore-fail-on-non-empty $(VEXEL_BUILD)/backends 2>/dev/null || true
	rmdir --ignore-fail-on-non-empty $(VEXEL_BUILD) 2>/dev/null || true

test:
	@bash tests/test.sh

-include $(LIB_OBJECTS:.o=.d) $(CLI_OBJECT:.o=.d)
