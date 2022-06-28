cmake_minimum_required(VERSION 3.10)
project(wasm-tools-examples)

add_library(wasm-tools-examples STATIC
	wasm-smith.c
)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../crates/c-api ${CMAKE_CURRENT_BINARY_DIR}/wasm-tools)

target_compile_options(wasm-tools-examples PRIVATE -Wall -Wextra -Wno-deprecated-declarations)

set_target_properties(wasm-tools-examples PROPERTIES
		OUTPUT_NAME "examples"
		RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		CXX_VISIBILITY_PRESET hidden
		POSITION_INDEPENDENT_CODE ON)

target_include_directories(wasm-tools-examples PUBLIC wasm-tools)
target_link_libraries(wasm-tools-examples PUBLIC wasm-tools)