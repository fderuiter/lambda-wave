#ifndef RING_BUFFER_H
#define RING_BUFFER_H

#include <stdint.h>
#include <stddef.h>

void init_ring_buffer();
void write_byte(uint8_t byte);
size_t read_chunk(uint8_t* dest, size_t max_len);

#endif
