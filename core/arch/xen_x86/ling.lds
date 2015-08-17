
ENTRY(_start)

SECTIONS {

	_text = 0;
	.text : { *(.text) } = 0x9090
	_etext = .;

	_rodata = .;
	.rodata : { *(.rodata) }
	_erodata = .;

	_data = .;
	.data : { *(.data) }
	_edata = .;
	
	_bss = .;
	.bss : { *(.bss) }
	_ebss = .;
	
	_membrk = ALIGN(4096);

}

/*EOF*/
