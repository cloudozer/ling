
The document describes lower layers of the network stack. The primary purpose is
to facilitate introduction of multiple network interfaces.

There used to be two LING configurations with respect to low-level networking:
lwip and pure. The configuration was selected by a USE\_LWIP compile switch. The
pure configuration should be discarded.

The low-level network stack consists of a Xen front-end driver (FE), a lwIP network
interface (NI), and a raw LING outlet (OL). Currently, FE is a set of global
variables, and NI is a singleton structure. The new architecture should look as
follows:

	---------  --------- 
	| FE/0  |  | FE/1  |	...
	---------  ---------
		|          |
		v      	   |
	---------      |
	| NI/0  |      \------------\
	---------                   |
		|      	                |
		v      	                |
	-------------------------   |
	| lwIP TCP/IP machinery |   |
	-------------------------   |
	        |                   |
			v                   |
	----------------        --------
	| inet sockets |        | OL/0 |
	----------------        --------

Each FE has two connection points: one for NI and one for OL. One or both points
can be disconnected. When FE receives a packet it forward it to all connected
points.

## Initialization as it is now

The low-level network initialization, the call to netif\_init\_ling is the last
step of the first initialisation stage. There is also high-level network
initialization that happens during the boot sequence when the command-line
parameters are known, the call to lwip:setup().

### netif\_init\_ling 

1. lwip\_init - initializes loopback interface among other things
2. netfe\_init - initialize (all) FEs

At this point FE is functional and no NI exists.

### lwip:setup/4

1. netif\_add - takes addrs and callbacks for init and input
2. netif\_set\_status\_callback
3. netif\_set\_default

4. either dhcp\_start or
	netif\_set\_up	

DHCP negotiations culminate in netif\_set\_up. netif\_setup fires the status
callback that sends netif\_up message to the init process.

If networking options are not found on the command line, lwip:setup/4 never gets
called and we end up with a disconnected FE (if any) and no NIs.

## Initialization (new)

The initialization works the same way as it is now with the following changes:

1. There can be many FEs. FE/0 is the only interface that can be bound to a NI.

## Raw sockets

OLs should attach directly to FEs. NI and OL attached to the same FE should act
independently.

