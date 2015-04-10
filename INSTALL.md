
## Prerequisites

To build LING from sources, the following prerequisites must be met:

* Erlang/OTP

   You must have [Erlang/OTP release 17 installed](https://www.erlang-solutions.com/downloads/download-erlang-otp).

* Cross-compiler (for Raspberry Pi / ARM)

   You must have a suitable bare-metal cross-compiler installed. To build your own, see [crosstool-ng](http://crosstool-ng.org). The gcc-arm-none-eabi package from Ubuntu is also known to work.

   You may also want to try [GNU Tools for ARM Embedded Processors](https://launchpad.net/gcc-arm-embedded) on Launchpad (contains multiple binary releases including ones for OS X).

* Cross-compiler (for building Xen version on OS X)

   When using OS X, you need to have a [cross-compiling toolchain](http://crossgcc.rts-software.org/doku.php?id=compiling_for_linux) installed on your system. LING has been tested to build against GCC 4.8.1 and 4.9.

## Build

```
make
make install
```

To generate a bootable/runnable kernel.img file for Raspberry Pi:

1. Use `railing` to generate an ELF kernel.
2. Convert the ELF format to raw binary suitable for booting on the target hardware.
3. Copy the binary blob to the SD card's FAT partition, overwriting kernel.img there.

## Vagrant

For convenience, a [Vagrantfile](https://www.vagrantup.com) is included. It builds LING for Raspberry Pi automatically and copies a bootable/runnable kernel.img file into `railing/kernel.img`. From there on you can proceed with copying the blob to the SD card.
