latex input:	mmd-article-header
Title:	MUMBLE: Shared secret-based authentication for Erlang on Xen	
	Request for Comments
	v0.9
Author:	Maxim Kharchenko  
Date:	December 19, 2012  
Copyright:	2012, Cloudozer LLP. All Rights Reserved.
base header level:	2
latex mode:	memoir
latex input:	mmd-article-begin-doc
latex footer:	mmd-memoir-footer
css: mmd.css

# Purpose

The purpose of the scheme described here --- MUMBLE --- is to authenticate 9p
connections between Erlang on Xen nodes running 9P2000.e protocol. MUMBLE is
inspired by a MUNGE authentication scheme [#munge].

# Overview

All nodes of the Erlang on Xen cluster share a secret key. The secret key is
passed on from parent to child nodes.

When a node establishes a 9p connection to another node, it issues a TAuth
message and the write a single MUMBLE message to the Afid. The MUMBLE message
contains the node and group ids of the originating node authenicated by MAC
based on the secret key. A correct MAC confirms that the message was encoded by
the node belonging to the same Erlang on Xen cluster. The recieving node then
decides whether to grant access based on the provided credentials.

The MUMBLE message also carries a session key that may be used for
reestablishing 9p connections when a transport connection is lost. Additional
information may be encoded in the Extra field.

# A MUMBLE message

A MUMBLE message can be represented in binary or textual format. The binary
encoding of a MUMBLE message is given below. The number encoding is
**little-endian** (LSB).

Size | Description | Notes
-----:|-------------|------
2 | Version number | 0x0100
20 | MAC | HMAC-SHA1 of the entire message
8 | Session key |
4 | Time encoded | time_t
4 | Time to live | in seconds
2 | L1
L1 | Node id | e.g. "550e8400-e29b-41d4-a716-446655440000"
2 | L2
L2 | Group id | e.g. "swarm1"
4 | L3
L3 | Extra |

Version
:	A version of the MUMBLE message, must be 0x0100.

MAC
:	A authentication code produced by running HMAC-SHA1 algorithm on the entire
	message when MAC field itself is set to zeros.

Session key
:	The identifier of the current session. The session key can be set to all
	zeros to waive the possibility of reestablishing a session.

Time endcoded and time to live
:	The moment of time (Unix timestamp) when the message was encoded and the
	number of seconds the message should be considered valid. Not used.

Node and group ids
:	The node and group identifiers of the originating node. Typically the
	authorization depends on whether the group id matches the group of the
	receiving node.

Extra
:	Any additional information.

The textual representation of the MUMBLE message can be obtained by
base64-encoding of the binary format, prepending a string "MUMBLE:" to the
beginning of the string and ":" to its end.

# Limitations

MUMBLE messages are not encrypted. It is assumed that the cluster uses a
secure private network most of the time. For inter-datacenter communications, 9p
connections should use an encrypted tunnel, protecting MUMBLE messages along the
way.

[#munge]: MUNGE project (http://code.google.com/p/munge/).

