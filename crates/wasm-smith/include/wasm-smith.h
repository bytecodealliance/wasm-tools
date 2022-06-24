#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct WasmSmithModule {
	uint8_t* data;
	uint32_t size;
};

enum WasmSmithResult { NOT_ENOUGH_ENTROPY = -2, NULL_PTR = -1, SUCCESS = 0 };

int32_t wasm_smith_create(const char* seed, WasmSmithModule* c_output);
int32_t wasm_smith_free(WasmSmithModule* module);

#ifdef __cplusplus
}
#endif