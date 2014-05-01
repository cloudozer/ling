// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#pragma once

#include <stdint.h>

#define MAKE_UINT_32(a, b, c, d) \
	(((uint32_t)(a) << 24) | \
	 ((uint32_t)(b) << 16) | \
	 ((uint32_t)(c) << 8) | \
	  (uint32_t)(d))

#define GET_UINT_16(p) \
	(((uint16_t)((uint8_t *)p)[0] << 8) | \
	  (uint16_t)((uint8_t *)p)[1])

#define GET_UINT_16_LE(p) \
	(((uint16_t)((uint8_t *)p)[1] << 8) | \
	  (uint16_t)((uint8_t *)p)[0])

#define GET_UINT_32(p) \
	(((uint32_t)((uint8_t *)p)[0] << 24) | \
	 ((uint32_t)((uint8_t *)p)[1] << 16) | \
	 ((uint32_t)((uint8_t *)p)[2] << 8) | \
	  (uint32_t)((uint8_t *)p)[3])

#define GET_UINT_32_LE(p) \
	(((uint32_t)((uint8_t *)p)[3] << 24) | \
	 ((uint32_t)((uint8_t *)p)[2] << 16) | \
	 ((uint32_t)((uint8_t *)p)[1] << 8) | \
	  (uint32_t)((uint8_t *)p)[0])

#define GET_UINT_64(p) \
	(((uint64_t)((uint8_t *)p)[0] << 56) | \
	 ((uint64_t)((uint8_t *)p)[1] << 48) | \
	 ((uint64_t)((uint8_t *)p)[2] << 40) | \
	 ((uint64_t)((uint8_t *)p)[3] << 32) | \
	 ((uint64_t)((uint8_t *)p)[4] << 24) | \
	 ((uint64_t)((uint8_t *)p)[5] << 16) | \
	 ((uint64_t)((uint8_t *)p)[6] <<  8) | \
	  (uint64_t)((uint8_t *)p)[7])

#define GET_UINT_64_LE(p) \
	(((uint64_t)((uint8_t *)p)[7] << 56) | \
	 ((uint64_t)((uint8_t *)p)[6] << 48) | \
	 ((uint64_t)((uint8_t *)p)[5] << 40) | \
	 ((uint64_t)((uint8_t *)p)[4] << 32) | \
	 ((uint64_t)((uint8_t *)p)[3] << 24) | \
	 ((uint64_t)((uint8_t *)p)[2] << 16) | \
	 ((uint64_t)((uint8_t *)p)[1] <<  8) | \
	  (uint64_t)((uint8_t *)p)[0])

#define PUT_UINT_16(p, v) do { \
	((uint8_t *)p)[0] = ((uint16_t)(v) >> 8); \
	((uint8_t *)p)[1] =  (uint16_t)(v) & 255; \
} while (0)

#define PUT_UINT_16_LE(p, v) do { \
	((uint8_t *)p)[1] = ((uint16_t)(v) >> 8); \
	((uint8_t *)p)[0] =  (uint16_t)(v) & 255; \
} while (0)

#define PUT_UINT_32(p, v) do { \
	((uint8_t *)p)[0] = ((uint32_t)(v) >> 24); \
	((uint8_t *)p)[1] = ((uint32_t)(v) >> 16) & 255; \
	((uint8_t *)p)[2] = ((uint32_t)(v) >> 8) & 255; \
	((uint8_t *)p)[3] =  (uint32_t)(v) & 255; \
} while (0)

#define PUT_UINT_32_LE(p, v) do { \
	((uint8_t *)p)[3] = ((uint32_t)(v) >> 24); \
	((uint8_t *)p)[2] = ((uint32_t)(v) >> 16) & 255; \
	((uint8_t *)p)[1] = ((uint32_t)(v) >> 8) & 255; \
	((uint8_t *)p)[0] =  (uint32_t)(v) & 255; \
} while (0)

#define PUT_UINT_64(p, v) do { \
	((uint8_t *)p)[0] = ((uint64_t)(v) >> 56); \
	((uint8_t *)p)[1] = ((uint64_t)(v) >> 48) & 255; \
	((uint8_t *)p)[2] = ((uint64_t)(v) >> 40) & 255; \
	((uint8_t *)p)[3] = ((uint64_t)(v) >> 32) & 255; \
	((uint8_t *)p)[4] = ((uint64_t)(v) >> 24) & 255; \
	((uint8_t *)p)[5] = ((uint64_t)(v) >> 16) & 255; \
	((uint8_t *)p)[6] = ((uint64_t)(v) >> 8) & 255; \
	((uint8_t *)p)[7] =  (uint64_t)(v) & 255; \
} while (0)

#define PUT_UINT_64_LE(p, v) do { \
	((uint8_t *)p)[7] = ((uint64_t)(v) >> 56); \
	((uint8_t *)p)[6] = ((uint64_t)(v) >> 48) & 255; \
	((uint8_t *)p)[5] = ((uint64_t)(v) >> 40) & 255; \
	((uint8_t *)p)[4] = ((uint64_t)(v) >> 32) & 255; \
	((uint8_t *)p)[3] = ((uint64_t)(v) >> 24) & 255; \
	((uint8_t *)p)[2] = ((uint64_t)(v) >> 16) & 255; \
	((uint8_t *)p)[1] = ((uint64_t)(v) >> 8) & 255; \
	((uint8_t *)p)[0] =  (uint64_t)(v) & 255; \
} while (0)

//EOF
