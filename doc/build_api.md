Copyright:  2013, Cloudozer LLP. All Rights Reserved.
base header level:  2
latex mode: memoir  
latex input:    mmd-article-begin-doc
latex footer:   mmd-memoir-footer
css: mmd.css

# Background

The primary way for packaging an Erlang application with LING virtual ine is to
use a build service. The build service publishes a REST interface that exposes
most of its operations. 

The build workflow is further simplified by a plugin to the standard Erlang
build tool -- rebar. The plugin is called ling\_builder. ling\_builder relies
exclusively of the facilities of the build service API.

The purpose of the present document is to document the build service API and the
configuration of the ling\_builder plugin. The design goal of the plugin and the
API is to simplify the workflow, make it as natural as possible.

The document should be kept in sync with the contents of the Erlang on Xen build
service website [#lbs].

# REST API

A user must register before using the API. The credentials are passed along with
requests using the basic HTTP authentication. Consequently, the build service is
accessed over HTTPS only.

The primary data format is JSON. Most requests will also provide sensible
textual representation, if Accept header is set to 'text/plain'.

## GET /1/about

Returns a descriptive string about the service including its version. No
authorization required.

## GET /1/stats

Returns the current service statistics. For JSON requests, the service returns a
JSON object with the following fields:

num\_users
: the total number of users registered;

num\_projs
: the total number of projectes created;

num\_builds
: the total number of successful builds;

cur\_queue\_len
: the current length of the build queue;

avg\_queue\_len
: the average length of the build queue;

avg\_build\_time
: the average build time across projects in seconds.

The textual response is the concatenation of field values using a pipe (|)
character. The order of the fields is as follows: num\_users, num\_projs,
num\_builds, cur\_queue\_len, avg\_queue\_len, avg\_build\_time. The request does not
require authorization.

## GET /1/status

Returns the current status of the service. For JSON requests, a number is
returned.

Status | Meaning
------:|--------
0 | idle
1 | building
99 | down

For textual requests, the service returns a descriptive string. The request does
not require authorization.

## POST /1/users

Creates a new user. The body of the request is a JSON object describing the
user. The following three fields must be present: user\_name, email, password.
The request does not require authorization.

## GET /1/users/user\_name

Retrieves information about the user. The only bit of information returned by
the call is the user's email.

## PUT /1/users/user\_name

Updates the user information. The body is a JSON object that may contain the
following fields: email, password.

## DELETE /1/users/user\_name

Removes the user from the build service. All project uploaded by the users are
deleted along with the user information.

## PUT /1/projects/proj\_name

Creates a new project for the user or updates files of the existing project. The
project name may not contain characters other than letters, digits, and
underscores. The body of the request is the project archive. It must have a MIME
type of 'application/zip'.

Typically, most of the project files are .beam modules. They are automatically
renamed into .ling modules and transformed accordingly. All files from the
archive will be imported into the image. The directory structure from the
project archive is preserved under the directory named after the project. For
example, for the project named 'sample1', a file 'priv/www/index.html' will be
accessible by the application code as /sample1/priv/www/index.html'. 

## GET /1/projects/proj\_name

Retrieves the project archive.

## DELETE /1/projects/proj\_name

Removes the project including the cached images, if any.

## GET /1/projects

Retrieves a list of projects created by the user. The list is a JSON list or a
textual string that contains project names separated by pipe (|) character.

## POST /1/build/proj\_name

Request the image build for the project. The cached image is removed upon the
request.

The body of the request may be empty or contain a JSON object with the following
fields:

import\_lib
: a list of standard libraries/applications to import. 'kernel' and 'stdlib' are
auto-imported and must not be listed. Defaults to [].

If the body of the request is empty, default values for build parameters are
assumed.

## GET /1/build/proj\_name/image

Retrieve the newly built (or cached) image.

## GET /1/build/proj\_name/status

Retrieve the status of the last build. The reply is a single integer value
formatted as JSON:

Status | Meaning
------:|--------
0 | build incomplete/not started
1 | build ok
99 | build failed

If the textual representation is requested a corresponding descriptive string is
returned.

# ling\_builder plugin

## Plugin configuration

The ling\_builder plugin is open source [#plugin-repo]. To use it the following
should be added to rebar.config:

	{plugins,[ling_builder]}.

	{deps,[
		{ling_builder,".*",
				{git,"git://github.com/maximk/ling_builder.git","master"}}
	]}.

	{ling_builder_opts,[
		...
		{Option,Value}
		...
	]}.		

The following plugin options are recognized:

build\_host
: the location of the build service. Defaults to "build.erlangonxen.org:8080".

username
: a username registered with the build service.

password
: a password associated with the username.

import
: a miscellaneous file to add to the project archive. May contain wildcards,
e.g. "priv/\*/\*", and appear multiple times.

import\_lib
: a name of the standard library/application to import into the image. The
corresponding modules and auxilliary files will be accessible at the standard
location under /erlang/lib. May appear multiple times.

build\_host, username and password are connection options. They are needed to
find and access the build service.

import option affects only the contents of the project archive. import\_lib is
the only option that is passed along in the body of the build request.

## Invoking ling\_builder

The ling\_builder plugin adds a few subcommands to rebar.

### rebar ling-build

The subcommand traverses the project directories and compiles a list of files to
include into the project archive. The project archive gets uploaded to the build
service. Then a build request is issued.

The subcommand returns either an error message or a message that the build
request is enqueued successfully and the estimated time to build.

### rebar ling-image

Retrieves the image built (or cached from the previous build). The image is
saved to a file named 'vmling'. The subcommand may return an error if the image
is not (yet) ready.

### rebar ling-build-image

Combines the effects of ling-build and ling-image. The subcommand starts the
build process, waits for its completion, and retrieves the generated image.

# Lauching an instance

An application packaged with LING VM must be able to find its code modules as
well as modules of standard libraries/applications it requires. This creates a
bootstrapping problem. Some code modules must be embedded into the image
(imported) because they are needed early in the boot process, when the ability
to load code remotely is not yet available. Such modules constitute about 50% of
kernel and stdlib applications.

The trade-off chosen by Erlang on Xen is to import complete kernel and stdlib
applications into the image. Thus no attempt at remote networking is made before
the image boots to the command prompt.

The compiled code of the project must be imported into the image too because it
is the only way to transform .beam files into corresponding .ling and thus make
the code runnable by LING VM. All other files may or may not be imported. Before
launching the application remote filesystems may be mounted to get access to
required files.

A complete list of files to be imported must be known during the compile time.
On the other hand, the information about remote filesystems may vary from one
instance invocation to the other. Such information is passed to the instance
using a command line.

## Build time parameters

All information needed for a successful image generation is the project archive
that contains all project files to be imported into the image and the list of
standard/libraries to import.

The local 9p server and 9p mounter make the project files accessible under
/&lt;project-name&gt; subtree, the code of the standard libraries/applications -
under /erlang/lib.

## Command line parameters

The command line parameters should configure security, networking, and remote
filesystems. An empty command line should result in a sensible configuration of
the node (without networking).

### Networking parameters

The preferred way to setup networking statically. This ensures the fastest
startup. Alternatively, networking may be set up using DHCP. DHCP configuration
is required by Amazon EC2.

-ipaddr _xxx.xxx.xxx.xxx_
: A static ip address assigned to the external network interface.

-netmask _xxx.xxx.xxx.xxx_
: A network mask.

-gateway _xxx.xxx.xxx.xxx_
: A default network gateway address.

-dhcp
: Requests DHCP configuration of the external network interface.

It must be possible to wait until the DHCP configuration is complete. This
dictates implementation of the network configuration as an outlet.

If any of the above options is present, then the external network interface is
created and configured accordingly. Otherwise, the node starts without an
external network interface.

## Remote filesystems

Erlang on Xen accesses remote (and local) filesystems exclusively using 9p
protocol. The standard 9p reexporting server available on Linux is called diod
[#diod]. 9p connections may be added/removed dynamically. The following
describes how to configure initial 9p connections, which are created at the end
of the boot sequence before starting applications.

-9p host[:port] attach\_to mount\_at ...
-9p- host[:port] attach\_to mount\_at ...
: opens a 9p connection using the standard 9p\_tcp transport to the host. Port
defaults to 564. Then mounts remote subtrees according to attach/mount pairs.
-9p/-9p- control the priority of newly added connection with respect to
union mounts. -9p assigns the highest priority, -9p- - the lowest.

-secret secret1 secret2
: the two keys that are needed for authentication between Erlang on Xen nodes
(MUMBLE) and between Erlang on Xen and external 9p servers (MUNGE).
secret1 and secret2 are represented as hexadecimal strings of variable length.

A MUNGE authentication must be enabled on the 9p server accessed from Erlang on
Xen. There is a small utility - mungeling - handy for synchronisations of
authentication tokens [#mungeling]. Running mungeling on the host of the 9p
server rints out the values for the -secret option.

## Empty command line

An empty command line results in a valid default configuration of the node. The
default configuration is as follows:

* Local networking only. No external network interface is created.
* Imported files only. The filesystem is limited to the imported files.
* Console only. The console is the only way to access the node.
* Home directory is set to the project directory.
* Code path does NOT include project subdirectories.

## Domain configuration

Launching a node using Xen or libvirt stack requires a configuration file. The
minimal such file for Xen toolstack is:

	name = "sample1"
	kernel = "vmling"
	memory = 512

A typical Xen doman configuration file includes more items:
	
	name = "sample1"
	kernel = "vmling"
	memory = 512
	extra = "-dhcp -secret xxxx xxxx -9p 192.168.0.1 / /linux"
	extra = " -pz ebin -pz deps/proj1/ebin -s sample1_app"
	vif = ["bridge=br0",]
	on_crash = "destroy"

An example of libvirt XML configuration file:

	<domain type='xen'>
  		<name>sample1</name>
  		<memory unit='KiB'>524288</memory>
  		<currentMemory unit='KiB'>524288</currentMemory>
  		<vcpu>1</vcpu>
  		<bootloader></bootloader>
  		<os>
    		<type>linux</type>
    		<kernel>vmling</kernel>
			<cmdline>-ipaddr 192.168.0.4 -netmask 255.255.255.0 -gateway 192.168.0.1</cmdline>
		</os>
		<on_poweroff>destroy</on_poweroff>
		<on_reboot>restart</on_reboot>
		<on_crash>restart</on_crash>
		<devices>
			<interface type='ethernet'>
				<script path='/etc/xen/scripts/vif-bridge'/>
			</interface>
		</devices>
	</domain>

[#lbs]: http://build.erlangonxen.org

[#plugin-repo]: https://github.com/maximk/ling_builder

[#diod]: https://github.com/chaos/diod

[#mungeling]: https://github.com/maximk/mungeling

