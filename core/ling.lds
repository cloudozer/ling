
ENTRY(_start)

SECTIONS {

	. = 0x10000;

	_text = .;
	.text : { *(.text) }
	_etext = .;

	.rodata : { *(.rodata) }
	.data : { *(.data) }
	
	.bss : { *(.bss) }
	
	_membrk = ALIGN(4096);

}

/*EOF*/
