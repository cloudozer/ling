
ENTRY(_start)

SECTIONS {

	_text = 0;
	.text : { *(.text) } = 0x9090
	_etext = .;

	.rodata : { *(.rodata) }
	.data : { *(.data) }
	
	.bss : { *(.bss) }
	
	_membrk = ALIGN(4096);

}

/*EOF*/
