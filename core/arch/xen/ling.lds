
ENTRY(_start)

SECTIONS {
	. = 0;
	.text : {
		_text = . ;
		*(.text*)
		_etext = . ;
	} = 0x9090

	.rodata : {
		_rodata = . ;
		*(.rodata*)
		_erodata = . ;
	}

	.data : {
		_data = . ;
		*(.data*)
		_edata = . ;
	}
	
	.bss : {
		_bss = . ;
		*(.bss*) *(COMMON)
		_ebss = . ;
	}
	
	_membrk = ALIGN(4096);
}

/*EOF*/
