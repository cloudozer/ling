## LING

[![Build Status](https://api.travis-ci.org/cloudozer/ling.svg?branch=master)](https://travis-ci.org/cloudozer/ling)

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

### How to build/hack LING?

See [HACKING.md](HACKING.md).

### Using `railing`

Run `railing` without options to get help.

To generate the image and the domain configuration file use the `image`
subcommand:

    ./railing image

This instructs railing to scan all subdirectories for `*.beam` files. All files
found are embedded into the Xen image and become accessible during runtime.
Certain subdirectories can be excluded from the search using `-x` option.
