C Loads one word, and adds it to the subkey. Uses T0
C AES_LOAD(SRC, KEY, REG)
define(<AES_LOAD>, <
	ldrb	$3, [$1], #+1
	ldrb	T0, [$1], #+1
	orr	$3, T0, lsl #8
	ldrb	T0, [$1], #+1
	orr	$3, T0, lsl #16
	ldrb	T0, [$1], #+1
	orr	$3, T0, lsl #24
	ldr	T0, [$2], #+4
	eor	$3, T0
>)
C Stores one word. Destroys input.
C AES_STORE(DST, X)
define(<AES_STORE>, <
	strb	$2, [$1], #+1
	ror	$2, $2, #8
	strb	$2, [$1], #+1
	ror	$2, $2, #8
	strb	$2, [$1], #+1
	ror	$2, $2, #8
	strb	$2, [$1], #+1
>)

C 53 instr.
C It's tempting to use eor with rotation, but that's slower.
C AES_ENCRYPT_ROUND(x0,x1,x2,x3,w0,w1,w2,w3,key)
define(<AES_ENCRYPT_ROUND>, <
	uxtb	T0, $1 
	ldr	$5, [TABLE, T0, lsl #2]
	uxtb	T0, $2
	ldr	$6, [TABLE, T0, lsl #2]
	uxtb	T0, $3
	ldr	$7, [TABLE, T0, lsl #2]
	uxtb	T0, $4
	ldr	$8, [TABLE, T0, lsl #2]

	uxtb	T0, $2, ror #8
	add	TABLE, TABLE, #1024
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$5, $5, T0
	uxtb	T0, $3, ror #8
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$6, $6, T0
	uxtb	T0, $4, ror #8
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$7, $7, T0
	uxtb	T0, $1, ror #8
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$8, $8, T0

	uxtb	T0, $3, ror #16
	add	TABLE, TABLE, #1024
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$5, $5, T0
	uxtb	T0, $4, ror #16
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$6, $6, T0
	uxtb	T0, $1, ror #16
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$7, $7, T0
	uxtb	T0, $2, ror #16
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$8, $8, T0

	uxtb	T0, $4, ror #24
	add	TABLE, TABLE, #1024
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$5, $5, T0
	uxtb	T0, $1, ror #24
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$6, $6, T0
	uxtb	T0, $2, ror #24
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$7, $7, T0
	uxtb	T0, $3, ror #24
	ldr	T0, [TABLE, T0, lsl #2]

	ldm	$9!, {$1,$2,$3,$4}
	eor	$8, $8, T0
	sub	TABLE, TABLE, #3072
	eor	$5, $5, $1
	eor	$6, $6, $2
	eor	$7, $7, $3
	eor	$8, $8, $4
>)

define(<AES_DECRYPT_ROUND>, <
	uxtb	T0, $1
	ldr	$5, [TABLE, T0, lsl #2]
	uxtb	T0, $2
	ldr	$6, [TABLE, T0, lsl #2]
	uxtb	T0, $3
	ldr	$7, [TABLE, T0, lsl #2]
	uxtb	T0, $4
	ldr	$8, [TABLE, T0, lsl #2]

	uxtb	T0, $4, ror #8
	add	TABLE, TABLE, #1024
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$5, $5, T0
	uxtb	T0, $1, ror #8
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$6, $6, T0
	uxtb	T0, $2, ror #8
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$7, $7, T0
	uxtb	T0, $3, ror #8
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$8, $8, T0

	uxtb	T0, $3, ror #16
	add	TABLE, TABLE, #1024
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$5, $5, T0
	uxtb	T0, $4, ror #16
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$6, $6, T0
	uxtb	T0, $1, ror #16
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$7, $7, T0
	uxtb	T0, $2, ror #16
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$8, $8, T0

	uxtb	T0, $2, ror #24
	add	TABLE, TABLE, #1024
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$5, $5, T0
	uxtb	T0, $3, ror #24
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$6, $6, T0
	uxtb	T0, $4, ror #24
	ldr	T0, [TABLE, T0, lsl #2]
	eor	$7, $7, T0
	uxtb	T0, $1, ror #24
	ldr	T0, [TABLE, T0, lsl #2]

	ldm	$9!, {$1,$2,$3,$4}
	eor	$8, $8, T0
	sub	TABLE, TABLE, #3072
	eor	$5, $5, $1
	eor	$6, $6, $2
	eor	$7, $7, $3
	eor	$8, $8, $4
>)

C AES_FINAL_ROUND(a,b,c,d,key,res)
define(<AES_FINAL_ROUND>, <
	uxtb	T0, $1
	ldrb	$6, [TABLE, T0]
	uxtb	T0, $2, ror #8
	ldrb	T0, [TABLE, T0]
	eor	$6, $6, T0, lsl #8
	uxtb	T0, $3, ror #16
	ldrb	T0, [TABLE, T0]
	eor	$6, $6, T0, lsl #16
	uxtb	T0, $4, ror #24
	ldrb	T0, [TABLE, T0]
	eor	$6, $6, T0, lsl #24
	ldr	T0, [$5], #+4
	eor	$6, T0
>)
