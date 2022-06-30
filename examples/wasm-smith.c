#include <string.h>
#include <wasm-tools.h>

void test_wasm_smith() {
	struct wasm_tools_byte_vec_t module;
	const char* seed = "W3B4553MB1Y!!!!!!!!!!!!!!!!!!!!!!!!!!";
	if(!wasm_smith_create(seed, strlen(seed), &module)) {
		// Module located in module.data with size module.size
		wasm_tools_byte_vec_delete(&module);
	}
}