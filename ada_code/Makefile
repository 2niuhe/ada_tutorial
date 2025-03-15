# Ada Tutorial Project Makefile

# 定义所有的源文件目录
DIRS = advanced_concurrency \
       concurrency \
       contract_programming \
       control_structures \
       data_types \
       exceptions \
       generics \
       hello_world \
       interface_abstractions \
       oop \
       packages \
       realtime_systems \
       stream_io \
       subprograms

# project_demo 使用 GPRBuild，单独处理
PROJECT_DEMO = project_demo

# 定义输出目录
BIN_DIR = bin
OBJ_DIR = obj

# 为每个目录定义目标文件
TARGETS = $(foreach dir,$(DIRS),$(BIN_DIR)/$(notdir $(dir)))

# 编译选项
GNATMAKE = gnatmake
GNATFLAGS = -Wall -gnata -v

# 定义主程序文件名映射（每行一个映射，不要有多余的空格）
define MAIN_PROGRAMS
advanced_concurrency:advanced_tasking.adb
concurrency:concurrent_demo.adb
contract_programming:contracts.adb
control_structures:control.adb
data_types:data_types.adb
exceptions:exception_demo.adb
generics:generic_demo.adb
hello_world:hello.adb
interface_abstractions:device_interfaces.adb
oop:oop_demo.adb
packages:main.adb
realtime_systems:realtime_demo.adb
stream_io:stream_demo.adb
subprograms:subprogram_demo.adb
endef
export MAIN_PROGRAMS

.PHONY: all clean $(DIRS) $(PROJECT_DEMO)

all: make_dirs compile_all $(PROJECT_DEMO)

# 创建所有需要的输出目录
make_dirs:
	@mkdir -p $(BIN_DIR)
	@mkdir -p $(OBJ_DIR)
	@for dir in $(DIRS); do \
		mkdir -p $(OBJ_DIR)/$$dir; \
	done

# 编译所有程序
compile_all:
	@for dir in $(DIRS); do \
		echo "Compiling $$dir..."; \
		main_program=$$(echo "$$MAIN_PROGRAMS" | grep "^$$dir:" | cut -d':' -f2); \
		echo "Main program for $$dir: $$main_program"; \
		if [ -n "$$main_program" ] && [ -f "$$dir/$$main_program" ]; then \
			echo "Found source file: $$dir/$$main_program"; \
			(cd $$dir && \
			echo "Compiling in directory: $$(pwd)" && \
			$(GNATMAKE) $(GNATFLAGS) -D ../$(OBJ_DIR)/$$dir $$main_program && \
			exe_name=$${main_program%.adb} && \
			echo "Moving $$exe_name to ../$(BIN_DIR)/" && \
			mv $$exe_name ../$(BIN_DIR)/) || exit 1; \
		else \
			echo "Skipping $$dir: source file not found"; \
		fi \
	done

# 使用GPRBuild编译project_demo
$(PROJECT_DEMO):
	gprbuild -P $(PROJECT_DEMO)/project_demo.gpr

# 清理所有编译产物
clean:
	@echo "Cleaning all build artifacts..."
	@rm -rf $(BIN_DIR)
	@rm -rf $(OBJ_DIR)
	gprclean -P $(PROJECT_DEMO)/project_demo.gpr -r
	rm -f *.ali *.o *~ b~*.ad[sb]
	find . -type f -name "*.ali" -o -name "*.o" -o -name "b~*.ad[sb]" -delete
