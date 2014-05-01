latex input:	mmd-article-header
Title:	Goo FS
	Implementation Notes 
	v0.2
Author:	Maxim Kharchenko
Date:	March 2, 2013
Copyright:  2013, Cloudozer LLP. All Rights Reserved.
base header level:  2
latex mode: memoir  
latex input:    mmd-article-begin-doc
latex footer:   mmd-memoir-footer
css: mmd.css

# GooFS: the filesystem we thought we do not need

## Legacy applications call for a compromise

The recurring idea of Erlang on Xen is to keep an interface and change its
implementation according to the vision of a truly scalable cloud. One such
interface is a 'filesystem'. It was fully reimplemented to expose mostly
synthetic files and files accessible over the network. Contrary to the industry
custom, our current implementation does not embrace locally-attached block
devices. For such devices, a key/value datastore is being designed, that should
expose a different interface -- 'ets/dets'.

We strive to support all important Erlang applications with our LING VM.
Adaptation of ejabberd proved that we need to support at least a mockery of a
filesystem over a writable block device. Such mockery is the GooFS filesystem
described here.

## A typical usage scenario

A GooFS formatted block device usually serves as a backend for a mnesia database
that keeps data in several dets tables mapped one-to-one to files. For example,
the mnesia database of ejabberd uses 29 files all created in a single directory.
The longest file name is 23 characters. This directly translates into design
constraints of the new filesystem.

## A simplest filesystem that fills the need

* A flat filesystem, no directories

* A limited number of files, at least 256

* A file name may be long, at least 256 chars

* Must not get corrupted easily, should survive domain\_poweroff()

* Must be easy to implement, in a few days

Note that GooFS is not the only modern filesystem, which is flat. See Amazon S3.

## Inside a GooFS volume

Every GooFS volume has 6 areas:

* superblock

* root record

* file directory

* transaction journal

* allocation table

* data extents

The superblock is a single sector (sector 0) that contains the general information
about the volume.

The root record contains information about the root directory. The information
is similar to what is kept for ordinary files. The root record occupies a single
sector (sector 1).

The file directory contains information about all files that exist on the volume.
Each directory entry occupies a single sector starting with sector 1. The length
of the file directory -- nr\_files -- is set upon formatting of the volume. It
must be at least 255. Each file is addressed using an integer value, which is
actually an index into the file directory. nr\_files must be less then 65536.

The transaction journal contains information about the current outstanding
transaction. It occupies exactly 32 blocks. Each journal entry is 16 bytes long
limiting the transaction to 1024 blocks. The number of entries actually used --
trans\_len -- is recorded in the superblock. Note that updating the entire
allocation table requires writing to no more than 512 blocks. The transaction
journal starts at sector 1 + nr\_files.

The allocation table keeps track of chunks of disk space -- extents -- allocated
to files. The size of a data extent -- extent\_size -- is fixed upon formatting.
A minimum value of extent\_size is 8192 sectors (4M). The number of data extents --
nr\_extents -- must be less than or equal to 65536. The allocation table has an
entry for each extent. It starts at sector 1 + nr\_files + 32. Each allocation
table entry is 4 bytes long. Thus each sector holds 512/4 = 128 such entries.
Hence the size of the allocation table -- atab\_size -- is
(nr\_extents+127)/128.

The data extents area is where all file contents is stored. It immediately
follows the allocation table. Thus the area starts at sector 1 + nr\_files + 32 + atab\_size.

Note that by increasing the extent\_size value the filesystem may occupy volumes
of almost any practical size. On average, a half of extent\_size will be
'wasted' for each file.

### The superblock

Starts at | Size
----------|-----
0 | 1

The byte ordering here are later in the document is big-endian.

Offset | Type | Description
-------|------|------------
0 | uint16\_t | nr\_files
2 | uint16\_t | nr\_extents
4 | uint32\_t | extent\_size
8 | uint64\_t | 0x2f476f6f2f46532f, magic
16 | uint16\_t | version, must be 0x1
18 | uint16\_t | trans\_len, 0 - no outstanding transaction
20 | - | UNUSED

### The root record

Start at | Size
---------|-----
1 | 1

Offset | Type | Description
-------|------|------------
0 | uint32\_t | version, incremented upon each modification
4 | uint64\_t | object unique id
12 | uint32\_t | mode, opaque
16 | uint32\_t | atime, access time, Unix timestamp
20 | uint32\_t | mtime, modification time, Unix timestamp
24 | - | UNUSED

### The file directory

Starts at | Size
----------|-----
2 | nr\_files

Each directory entry occupies a whole sector. The layout of the entry is as
follows:

Offset | Type | Description
-------|------|------------
0 | uint16\_t | 0 - free entry, 0xffff - occupied entry
2 | uint64\_t | file size
10 | uint32\_t | version, incremented upon each modification
14 | uint64\_t | file unique id
22 | uint32\_t | mode, opaque
26 | uint32\_t | atime, access time, Unix timestamp
30 | uint32\_t | mtime, modification time, Unix timestamp
34 | uint16\_t | name len
36 | char[len]	| name

Note that the maximum file name length is 476.

### The transaction journal

Starts at | Size
----------|-----
2 + nr\_files | 32

Each transaction log entry has the following layout:

Offset | Type | Description
-------|------|------------
0 | uint64\_t | source sector
8 | uint64\_t | target sector

The number of active elements in the transaction log -- trans\_len -- is
recorded in the superblock.

### The allocation table

Starts at | Size
----------|-----
2 + nr\_files + 32 | atab\_size

Each allocation table entry has the following layout:

Offset | Type | Description
-------|------|------------
0 | uint16\_t | file index, 0xffff - unallocated extent
2 | uint16\_t | extent offset

A file that occupies N extents will have exactly N entries in the allocation
table. The entries will have offset fields set to all numbers from 0 to N-1. All
entries will have their file index field set accordingly.

Note that the maximum allowed file index is 65534 (0xfffe).

## Formatting a volume

Let us assume we need to format a volume that has a certain number of sectors --
nr\_sectors. nr\_files is another input parameter that defaults to 255.

The number of sectors left for the data extent area is nr\_data\_sectors\_g =
nr\_sectors - 1 (superblock) - 1 (root record) - nr\_files (file directory) - 32
(transaction journal) - 512 (maximum allocation table).

Now we need to select extent\_size that lets map all nr\_data\_sectors\_g given the
limitation that nr\_extents must be no more than 65536.

The minimum extent\_size is 128 sectors. We start with this number and double it
until extent\_size \* 65536 is at least nr\_data\_sectors\_g. At this point, we fix
the extent\_size value, and calculate nr\_extents\_g as nr\_data\_sectors\_g /
extent\_size and atab\_size\_g as (nr\_extents\_g+127)/128.

We may try another round of reshuffling to improve the estimate for nr\_extents.
For simplicity we just use nr\_extents\_g  for nr\_extents (and atab\_size\_g
for atab\_size). Most probably, some sectors will be left inaccessible at the
end of the volume.

Now we proceed to writing a superblock using nr\_files, nr\_extents, and
extent\_size. The formatting operation must be atomic. To provide for atomicity
we set trans\_len field in the superblock to 0xffff for the duration of
formatting. Then we initialize the root record. After that we write nr\_files
zeroed-out sectors that constitute a file directory. Then we skip 32 blocks for
the transaction journal and write atab\_size sectors with all bytes set to 0xff
as the allocation table.

After rewriting superblock and setting trans\_len to 0, the volume is ready for
use.

## How transactions work

Transactions provide atomicity to all filesystem metadata updates. Failure to
complete the sequence of modifications to the file directory and the allocation
table will in most cases leave the entire filesystem unusable.

To insure atomicity filesystem metadata is updated in two stages.

1. Write blocks to unallocated space and populate the transaction journal

2. Write blocks to intended places and reset the journal

Suppose we need to update a directory entry at sector 10 and write the
allocation information to sector 33. First we find two unallocated sectors.
Let's assume their numbers are 1200 and 1201. Then we write a copy of the data
intended for sector 10 to sector 1200, for sector 33 - to sector 1201. Then we
write two entries to the transaction journal: 10 &rarr; 1200, 33 &rarr; 1201.
Everything is ready to start the transaction. It is started by setting the
trans\_len field in the superblock to 2. 

Now we proceed to writing data to their true locations, sectors 1200 and 1201.
When we are done, we reset the trans\_len field to zero to end the transaction.

In case a catastophic event happens during the first stage, before we set
trans\_len to 2 in the superblock. The data written to sectors 1200 and 1201
(and the transaction journal) is disregarded. If the event interrupts the second
stage, the transaction journal is rerun restoring the filesystem to fully
consistent state.

The filesystem should issue a 'write barrier' after each stage of the
transaction.

## File operations dissected

### In-memory structures

The software layer that exposes a filesystem interface using GooFS may maintain
in-memory representation of the file directory of the allocation table. Yet
these representations must be fully synchronised. The layer must be ready for a
'power off' at any time.

Such approach assumes that the actual disk driver underneath the virtual block
device performs caching and the database that uses GooFS does its own caching.

The filesystem should flush caches periodically (every second by default) to
increase durability of writes.

### List a directory

The directory listing may use in-memory metadata only. Access time should be
updated in the root record.

### Create a file

* Check that the number of files is less than nr\_files

* Find an unoccupied slot in the file directory

* Update the file directory and the root record, both on disk and in memory

### Delete a file

* Retrieve the list of extents occupied by the file

* Mark the directory entry as unoccupied

* Mark all file extents as unoccupied

* Update both the file directory, the root record, and the allocation table, in
memory and on disk

### Read a file

* Resolve read offset and size into a list of chunks, so that each chunk is
within a single extent

* Read the chunks and concatenate them

* Update the access time of the file, both in memory and on disk

### Change a file size

* Determine if the file shrinks or grows

* If the file shrinks, determine the extents that are no longer needed and mark
them as unallocated

* If the file grows, determine the number of additional extents it needs

* If there is enough free space available, find unallocated extents and mark
them as occupied by the file in the allocation table

* Update the file modification time and the allocation table, both in memory and
on disk

### Write a file

* Check if the write operation changes the size of the file, perform the change
size procedure if needed

* Resolve write offset and size into a list of chunks, so that each chunk
belongs to a single extent

* Write data to the chunks

* Update the modification time of the file, both in memory and on disk 

## Known limitations

### Writes may not be durable

Due to caching on various levels writes reach the disk media before the
operation completes. This may lead to corruption of the file data in case of a
'power off' event.

