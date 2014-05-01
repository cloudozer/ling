latex input:	mmd-article-header
Title:	A vision of ephemeral cloud
	v0.1
Author:	Maxim Kharchenko
Date:	March 2, 2013
Copyright:  2013, Cloudozer LLP. All Rights Reserved.
base header level:  2
latex mode: memoir  
latex input:    mmd-article-begin-doc
latex footer:   mmd-memoir-footer
css: mmd.css

# Rediscovering a truly elastic cloud

Today's computing clouds, often advertised as elastic, are rather rigid. When
compared to stiffness of iron, they achieve elasticity of wood. Rubber-like
clouds are still on drawing boards.

Let us have a peek at the sketch of the future cloud we, the Erlang on Xen
creators, have on our drawing board.

There is a list of statements in the center. The top three say:

* Smaller cheap-to-create OS-less instances _provisioned on demand_
* Reduced cloud stack, sharing infrastructure with user applications
* System administrators not necessary

There are a few more items on the list. Some are trivial, some, we believe, are
too valuable to spoil. So I am clipping the rest, including the one that
mentions 'robotics'.

OS-less instances is what Erlang on Xen all about. Such instances start so fast
you do not need to have anything pre-started. When your running application
wants to use, for example, a message queue, one of these happens:

* no message queue started &mdash; start it, then use it
* message queue is available &mdash; use it
* message queue is busy &mdash; spawn a copy and use it

Note that instances are only spawned by other instances just like Unix process
forked from existing processes. On as-needed basis.

The startling outcome of the on-demand provisioning is that an application that
does not do a useful work consumes no resources. Ten physical servers
may now host a million client applications. A single Facebook-scale
infrastructure may host a Facebook-like application for each human on Earth.

How is that for _efficiency_?

\*\*\*

In the year 3000, everything will be instant... but the cloud stack will still
take, like, nine f\*\*king seconds. Our [instance-per-request
demo](http://try.erlangonxen.org/zerg) hints that this might just be the case.

Why not to apply the on-demand provisioning principle to the components of the
cloud stack? It must be elastic too after all.

\*\*\*

The bottom of the drawing board is all about how to make clouds a welcoming home
for a database. Databases need a finer grain of control over their instances.
First, a database may request two instances never to share a single
physical node. These instance may contain replicas of an in-memory database
and hosting them side by side negates their purpose of existence. Second, a
database may stumble upon a query that requires scanning almost all its data
scattered over several physical disks. The best strategy for such query is to
spawn instances of the nodes that have these disks attached and skim the data
without shoveling everything through the network.

I imagine, a cloud provider may charge extra for such 'separate nodes' and 'disk
node only' instances. It may do so exactly because they are so valuable for
performant cloud databases.

\*\*\*

Virtualization is featured profoundly throughout our vision. Same goes for
OpenFlow-aware network switches. Everything else is taken with a grain of salt -
is it there to replicate a homely computing world of the 90s or it truly helps
to weave a fabric of the future cloud?

\*\*\*

On the left of the drawing board is a mockup of the cloud's GUI. Frankly
speaking, it resembles Visual Studio-style IDE more than anything else. The bulk
of it is about editing source code. It also allows selection of
services/components the cloud application needs. The trick is that all these
services are _ephemeral_. None of them exist. They get created when first used.

The dark machinery behind the IDE bakes instance images and deploy them to the
cloud the moment the user clicks the 'Run' button. The running application can
be paused, variable values inspected, breakpoints set. All the usual debugging
stuff is possible.

The remarkable observation is that there is no separate 'administrator' GUI, as
well as no mention of Chef or Puppet. Instances are provisioned and configured
by the application code. The monitoring is done by a logging/monitoring
component added to the application. What other tasks justify a separate
interface for an administrator? While you are inventing such task, we move on
without.

\*\*\*

----

BELOW ARE OLD NOTES -- DISREGARD

----

Virtual instances will be somewhat smaller but there will be plenty of them.

The components of cloud stack and components of user applications will share the
same infrastructure. 

## Cloud-aware applications

* NewStack
* Cloud 2.0
* [DEFUC: Destination future cloud]

In the year 3000, everything will be instant... but the cloud stack will still
take, like, nine freaking milliseconds.

The transition to the cloud makes us rethink the architecture of software. Most
cloud applications today mimick traditional software. The driving force behind
this is desire to reuse existing source code, skills, even, software licences to
the largest extent. The modern cloud stacks mostly caters to this desire.

The inertia is there yet we have to be prepared to redesign our application
spefically to truly unlock the advantage of cloud environments. These
cloud-aware applications &ndash; or cApps &ndash; should have an new, yet
unknown, architecture.

The following are a few highlights of how a cloud running cApps may look like.

## Cloud instances are spawned exclusively by other cloud instances

The same concept as forking a process in Unix. For any Unix process there is a
parent process. For any instance there should be a parent instance. Instances
are spawned not by an almighty administator's hand in the sky but by the
application code.

The application logic is the best place to make a decision that a new instance
is needed. A web server instance may spawn a sibling instance when it already
handles too many concurrent requests. A database server instance may decide that
a particular query requires many transient workers to handle it efficiently.

[mention robotics]

## The bedrock of the future cloud

The necessary foundation for a cloud are physical nodes (some of them with disks
attached), virtualization layer, and OpenFlow-aware switches. Everything else
are cApps. Monitoring, Resource Management, Security, Network Configuration,
Data Storage are all cloud-aware elastic applications sharing infrastructure
with user cApps.

We are not starting with a clean slate. Virtualization is a necessary ingredient
of the future cloud. There are also OpenFlow controllers that let you redefine
the network configuration rapidly. Anything else, should be taken with a grain
of salt - does it exist to replicate a homely computing world of the 90s in the
cloud or it helps to weave a farbic of the future cloud?

## Common cloud services are cApps too

The common cloud services should also be elastic and it is only logical to
implement them as cApps not as supposedly available/elastic/redundant external
entities.

A network configuration engine may decide to spawn a hundred instances of dhcp
servers when thousands client instances are about to fire up.

[Mapping between applications and administative domains - clear separation of
clients.]

## Running database queries near data

The outcome of structuring a cloud around cApps is that the data access layer
belongs to the client administrative domain. An elastic database application
creates a set of instances that run on hosts with physical disks (spin-disks or
SSDs) attached. Then the database servers knows everything there is to know
about data location. It may decide to spawn (unmovable) instances on the
physical nodes that controls the phyical storage devices and run code that scans
the data efficiently.

Currently a database server is presented with a virtual block device with
largely unpredictable properties. The device may be mapped to a SAN disk array,
or it may be mapped to a file in a filesystem hosted on remote SMB server. Who
knows. Such setup is good enough in many cases, but it does not fly in case of a
database.

[Code mobility, fast network, relatively cheap CPU]

[New stored procedures]

## Zero-footprint applications

App that does nothing consumes no resources.

The cApp that does not do any useful work should consume no resources. The whole
application should be started as soon as new request arrives. The critical
parameter here is a startup latency. It must be much lower when the have today.
OS-less instances such as Erlang on Xen does get rid of the startup latency
inside the instance. The remaining bit is the latency of the cloud stack. The
NewStack should be orders of magnitude faster to provide for zero-footprint
clouds.

Zero-footprint clouds have a far-reaching economic implications. A ten physical
server have host a million cApps. A single Facebook-scale infrastructure can
host a Facebook-like app for each human on Earth.

## Configuration management

Chef/Puppet are interim solutions mimicking actions of human beings.

Right size of instances. No mega servers. Instance migration.

## A diagram - mapping betweem old and new architectures

Cloud 1.0 | Cloud 2.0
----------|---------
Process | Instance
Thread | Green thread (goroutine, Erlang process)

## Missing/existing bits.

Erlang on Xen is the harbinger of the future clouds.

## Links/Quotes

* A relevant blog post:
[0](http://gould.cx/ted/blog/Cloud_2.0)

>And that, not knowing how many servers you're actually running, that is Cloud
>2.0

* A presentation from HP Labs on Apache CloudStack:
[1](http://www.slideshare.net/steve_l/application-architecture-for-the-cloud)

* Architectures for data security:
[2](http://www.infoq.com/articles/regulatory-compliant-cloud-computing)

* A post from a same-minded person:
[3](http://robhirschfeld.com/2011/11/02/451-cloudscape-report-strikes-chord)

* A blog post on swarm computing mentions cloud 2.0:
[4](https://ralfschnell.wordpress.com/2013/02/11/swarm-computing-cloud-2-0)

>Imagine Swarm Computing applied to Infrastructure Management, Automation,
Monitoring, Performance and Capacity Management. Rather than following the
traditional approach of defining, provisioning and registering instances, we’d
have those instances established with some basic self-awareness and the ability
to communicate and react to other instances.
