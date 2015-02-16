
ENTRY(_start)

SECTIONS {

	. = 0x10000;
	.text : { *(.text) }

	. = ALIGN(4096);
	. += 0x200000;
	_stack_bottom = .;

	.rodata : { *(.rodata) }
	.data : { *(.data) }
	.bss : { *(.bss) }
	
	_membrk = ALIGN(4096);

}

/*EOF*/
