/**
 * \file wasm-tools.h
 *
 * C API for wasm-tools
 */

#ifndef WASM_TOOLS_H
#define WASM_TOOLS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \struct wasm_tools_byte_vec_t
 * \brief A list of bytes
 *
 * Used to pass data in or pass data out of various functions.  The meaning and
 * ownership of the bytes is defined by each API that operates on this
 * datatype.
 *
 * \var wasm_tools_byte_vec_t::size
 * \brief Length of this vector.
 *
 * \var wasm_tools_byte_vec_t::data
 * \brief Pointer to the base of this vector
 *
 * \fn void wasm_tools_byte_vec_delete(wasm_tools_byte_vec_t *);
 * \brief Deletes a byte vector.
 *
 * This function will deallocate the data referenced by the argument provided.
 * This does not deallocate the memory holding the #wasm_tools_byte_vec_t itself, it's
 * expected that memory is owned by the caller.
 */
struct wasm_tools_byte_vec_t {
	uint8_t* data;
	size_t size;
};

/**
 * \enum wasm_tools_error
 * \brief Errors return by functions
 */
enum wasm_tools_error {
	WASM_TOOLS_SUCCESS              = 0,  ///< success
	WASM_TOOLS_ERROR                = -1, ///< generic error
	WASM_TOOLS_INSUFFICIENT_ENTROPY = -2, ///< wasm-smith seed too short
};

/**
 * \brief Deletes a wasm_tools_byte_vec_t instance.
 *
 * \param bytes this is the input pointer for the wasm_tools_byte_vec_t
 *   instance to be deleted
 */
void wasm_tools_byte_vec_delete(struct wasm_tools_byte_vec_t* bytes);

/**
 * \brief Generates a random valid wasm module using default settings.
 *
 * \param seed the input pointer to the binary seed
 * \param seed_len the length of `seed`, in bytes.
 * \param bytes wasm_tools_byte_vec_t instance where generated module is written
 *
 * \return WASM_TOOLS_SUCCESS if generation is successful,
 *   WASM_TOOLS_INSUFFICIENT_ENTROPY if seed has too little entropy
 *
 * This function does not take ownership of `seed`
 */
enum wasm_tools_error wasm_smith_create(const char* seed, size_t seed_len, struct wasm_tools_byte_vec_t* bytes);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // #ifdef WASM_TOOLS_H
