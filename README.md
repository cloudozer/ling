## LING

[![Build Status](https://api.travis-ci.org/cloudozer/ling.svg)](https://travis-ci.org/cloudozer/ling)

Wikipedia: -ling, an English diminutive suffix

### Erlang on Xen

#### Quick Start

The easy way is to use pre-built railing utility:

1. Grab a binary named railing-&lt;version&gt; from [releases](https://github.com/cloudozer/ling/releases).

1. Rename the binary to `railing` and move it to your Erlang project directory.

1. Create the image and the domain configuration file:

        ./railing image

1. Boot the Xen domain and get the familiar Erlang shell:

        xl create -c domain_config


### Erlang on Pi

#### Serial Console

Before you try LING on Pi you need to setup a serial console. A comprehensive setup guide can be found [here](http://elinux.org/RPi_Serial_Connection).

A serial console is the only means of communication supported by LING on Pi now.

#### Quick Start

1. Grab `kernel.img` from the Raspberry Pi 0 [release](https://github.com/cloudozer/ling/releases/tag/v0.3.2r).

1. Copy `kernel.img` to an SD card and boot your Raspberry Pi using the card.

1. After a few second you will see the Erlang prompt on the serial console.

1. Learn more about Erlang [here](http://erlang.org).

### Building from Source

See [INSTALL.md](INSTALL.md).


### Using `railing`

Run `railing` without options to get help.

To generate the image and the domain configuration file use the `image`
subcommand:

    ./railing image

This instructs railing to scan all subdirectories for `*.beam` files. All files
found are embedded into the Xen image and become accessible during runtime.
Certain subdirectories can be excluded from the search using `-x` option.
