cmake_minimum_required(VERSION 3.10)

if(WIN32)
	# Copy share library into build directory
	set(WASM_TOOLS_INSTALL_COMMAND ${CMAKE_COMMAND} -E copy_if_different
		${CMAKE_CURRENT_SOURCE_DIR}/target/release/libwasmtools.dll
		${CMAKE_BINARY_DIR})
else()
	# TODO linux
	set(WASM_TOOLS_INSTALL_COMMAND "")
endif()

include(ExternalProject)
ExternalProject_Add(
	wasm-tools-crate
	DOWNLOAD_COMMAND ""
	CONFIGURE_COMMAND ""
	INSTALL_COMMAND ${WASM_TOOLS_INSTALL_COMMAND}
	BUILD_COMMAND cargo build --release -p wasm-tools-c-api
	BINARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}
	LOG_BUILD ON
	BUILD_ALWAYS ON)
add_library(wasm-tools INTERFACE)
add_dependencies(wasm-tools wasm-tools-crate)

if(WIN32)
	target_link_libraries(wasm-tools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/target/release/libwasmtools.dll.lib)
else()
	# TODO linux
endif()

target_include_directories(wasm-tools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/crates/c-api/include)