
## Prerequisites

To build LING from sources for Raspberry Pi, the following prerequisites must be met:

1. Erlang/OTP

   You must have [Erlang/OTP release 17 installed](https://www.erlang-solutions.com/downloads/download-erlang-otp).

2. Cross compiler

   You must have a suitable bare-metal cross-compiler installed. To build your own, see [crosstool-ng](http://crosstool-ng.org). The gcc-arm-none-eabi package from Ubuntu is also known to work.

   You may also want to try [GNU Tools for ARM Embedded Processors](https://launchpad.net/gcc-arm-embedded) on Launchpad (contains multiple binary releases including ones for OS X).

3. Autoconf

   The nettle 2.7.1 library requires autoconf for bootstrapping (tested with autoconf 2.69).

## Build

To generate a bootable/runnable kernel.img file:

1. Download and cross-compile libnettle.a (see build.sh lines 24-28)
2. Build LING to generate vmling.o, railing executable, and other necessary libraries.
3. Use `railing` to generate an ELF kernel.
4. Convert the ELF format to raw binary suitable for booting on the target hardware.
5. Copy the binary blob to the SD card's FAT partition, overwriting kernel.img there.

## Vagrant

For convenience, a [Vagrantfile](https://www.vagrantup.com) is included. It runs the build.sh script automatically, and will copy a bootable/runnable kernel.img file into the Vagrantfile's directory. From there, perform step 5 above.
