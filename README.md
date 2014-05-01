## LING: Erlang on Xen

Wikipedia: -ling, an English diminutive suffix

### How to get started?

The easy way is to use pre-built binaries:

1. Grab the archive named ling-[version].tar.gz.

1. Extract it locally. This creates directory &lt;path&gt;/ling.

1. Go to your Erlang project directory and run:

```
	<path>/ling/bin/railing image dconf
```

1. This creates vmling, the Xen image file and domain\_config, the Xen domain
configuration file.

1. Launch the Xen domain and get the familiar Erlang shell:

```
	xl create -c domain_config
```

### Building from sources

The recommended way to build LING from sources is to use a Docker container with
the right environment already set up for you. See DOCKER.md for details.

If you are not easily daunted, then you may try to build everything yourself.
See INSTALL for instructions.

### Creating LING images

To create LING-based images for your Erlang projects you need a utility called
_railing_. Railing is in a way similar to reltool. The typical invocation of
railing is:

	railing image

This instructs railing to read railing.config and perform all steps necessary to
build an image named vmling.

To launch a Xen image you also need a domain configuration file. You may start
with a skeleton domain\_config file created as follows:

	railing dconf

### railing.config

The railing.config contains a series of options represented as Erlang terms. The
following options are recongized:

	{import,<path>}.

The option imports files referred to by &lt;path&gt; to the image. They become
accessible inside the VM. The option can be repeated multiple times. Example:
`{import,"priv/*/*"}`.

	{import_lib,<std_app>}.

The option imports a standard library from the installed Erlang/OTP. Example:
`{import_lib,crypto}`. stdlib and kernel applications are imported
automatically. The option can also contain a list of libraries to import.

	{build_config,<config>}}.

The option sets the build configuration for the image. Currently, the only
recognized configuration is 'fastest'.

