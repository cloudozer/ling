latex input:            mmd-article-header
Title:			Tubes
Author:			Maxim Kharchenko, Cloudozer LLP
Date:			07/07/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc

# Overview

[Why do we need this - performance difference]
[XenSocket]

[Dynamic vs static]

All domains can establish a tube

[Add watch function to xenstore]

Each domain creates, allows universal access, and watches /local/domain/5/tube.

Assume the domain 7 want to establish a new tube to the domain 5.

1. It create a path tube/7/0 under /local/domain/5.
2. It creates

tube/7/0/tx-ring-ref
tube/7/0/rx-ring-ref
tube/7/0/event-channel

3. It writes "4" to tube/7/0/state

```
/local/domain/5/device/tube/0
	backend
	state

/local/domain/7/backend/tube/5/0
	frontend
	state
```

