#pragma once

#include <stdint.h>

unsigned long adler32(unsigned long adler, const uint8_t *buf, unsigned int len);
unsigned long adler32_combine(unsigned long adler1, unsigned long adler2, unsigned long len2);

//EOF
