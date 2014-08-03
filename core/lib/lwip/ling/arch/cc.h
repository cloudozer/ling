#ifndef CC_H
#define CC_H

#if defined(__x86_64__)
typedef char	s8_t;
typedef short	s16_t;
typedef int		s32_t;

typedef unsigned char	u8_t;
typedef unsigned short	u16_t;
typedef unsigned int	u32_t;

typedef unsigned long	mem_ptr_t;
#else
typedef char	s8_t;
typedef short	s16_t;
typedef int		s32_t;

typedef unsigned char	u8_t;
typedef unsigned short	u16_t;
typedef unsigned int	u32_t;

typedef unsigned int	mem_ptr_t;
#endif

#define X8_F	"x"
#define U16_F	"hu"
#define S16_F	"d"
#define X16_F	"hx"
#define U32_F	"u"
#define S32_F	"d"
#define	X32_F	"x"
#define SZT_F	"u"

#ifndef BYTE_ORDER
#define BYTE_ORDER LITTLE_ENDIAN
#endif

#define PACK_STRUCT_STRUCT

void printk(const char *fmt, ...);
void fatal_error(const char *fmt, ...);

uint32_t mt_lrand(void);

#define LWIP_PLATFORM_DIAG(x)		printk x
#define LWIP_PLATFORM_ASSERT(x)		fatal_error(x)

#define LWIP_RAND()					mt_lrand()

#endif

