cmake_minimum_required(VERSION 3.10)

# Copy shared library into build directory
if(WIN32)
	set(WASM_TOOLS_INSTALL_COMMAND ${CMAKE_COMMAND} -E copy_if_different
		${CMAKE_CURRENT_SOURCE_DIR}/../../target/release/wasmtools.dll
		${CMAKE_BINARY_DIR})
elseif(APPLE)
	set(WASM_TOOLS_INSTALL_COMMAND ${CMAKE_COMMAND} -E copy_if_different
		${CMAKE_CURRENT_SOURCE_DIR}/../../target/release/libwasmtools.dylib
		${CMAKE_BINARY_DIR})
else()
	set(WASM_TOOLS_INSTALL_COMMAND ${CMAKE_COMMAND} -E copy_if_different
		${CMAKE_CURRENT_SOURCE_DIR}/../../target/release/libwasmtools.so
		${CMAKE_BINARY_DIR})
endif()

include(ExternalProject)
ExternalProject_Add(
	wasm-tools-crate
	DOWNLOAD_COMMAND ""
	CONFIGURE_COMMAND ""
	INSTALL_COMMAND "${WASM_TOOLS_INSTALL_COMMAND}"
	BUILD_COMMAND cargo build --release
	BINARY_DIR ${CMAKE_CURRENT_SOURCE_DIR}
	BUILD_ALWAYS ON)
add_library(wasm-tools INTERFACE)
add_dependencies(wasm-tools wasm-tools-crate)

if(WIN32)
	target_link_libraries(wasm-tools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/../../target/release/wasmtools.dll.lib)
elseif(APPLE)
	target_link_libraries(wasm-tools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/../../target/release/libwasmtools.dylib)
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath='$ORIGIN'")
else()
	target_link_libraries(wasm-tools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/../../target/release/libwasmtools.so)
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath='$ORIGIN'")
endif()

target_include_directories(wasm-tools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)