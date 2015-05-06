## LING: Erlang on Xen

[![Build Status](https://api.travis-ci.org/cloudozer/ling.svg)](https://travis-ci.org/cloudozer/ling)

Wikipedia: -ling, an English diminutive suffix

### How to get started?

The easy way is to use pre-built railing utility:

1. Grab a binary named railing-&lt;version&gt; from [releases](https://github.com/cloudozer/ling/releases).

1. Rename the binary to `railing` and move it to your Erlang project directory.

1. Create the image and the domain configuration file:

        ./railing image

1. Boot the Xen domain and get the familiar Erlang shell:

        xl create -c domain_config


### How to build/hack LING?

See [HACKING.md](HACKING.md).

### How to use railing?

Run `railing` without options to get help.

To generate the image and the domain configuration file use the `image`
subcommand:
```
./railing image
```
This instructs railing to scan all subdirectories for `*.beam` files. All files
found are embedded into the Xen image and become accessible during runtime.
Certain subdirectories can be excluded from the search using `-x` option.

### How to report issues in LING/railing

Please open a ticket in [GitHub](https://github.com/cloudozer/ling/issues).
Tickets in [Bugzilla](http://issues.erlangonxen.org/buglist.cgi?bug_status=__open__&order=changeddate%20DESC) will eventually be migrated to GitHub too.
