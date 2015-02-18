
ENTRY(_start)

SECTIONS {

	. = 0x8000;
	.text : { *(.text) }
	.rodata : { *(.rodata) }

	. = ALIGN(4096);
	. += 0x200000;
	_stack_bottom = .;

	. += 0x10000;
	_irq_stack_bottom = .;

	.data : { *(.data) }
	_bss_start = .;
	.bss : { *(.bss) }
	_bss_end = .;
	
	_membrk = ALIGN(4096);

}

