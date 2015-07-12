latex input:            mmd-article-header-mk
Title:					Tubes
Author:					Maxim Kharchenko, Cloudozer LLP
Date:					07/07/2014
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

Each domain creates, allows universal access, and watches /local/domain/5/data/tube.

Assume the domain 7 (nock) want to establish a new tube to the domain 5 (tip).

The functioning tube:

```
/local/domain/7/data/nock/5/0
	state

/local/domain/5/data/tip/7/0
	tx-ring-ref
	rx-ring-ref
	event-channel
	state
```

On domain 7:

1. Create /local/domain/7/data/nock/5/0
1. Write .../tip = /local/domain/5/data/tip/7/0 [?]
1. Write .../state = 2 (InitWait)
1. Create /local/domain/5/data/tip/7/0
1. Write .../nock = /local/domain/7/data/nock/5/0
1. Setup watch on /local/domain/5/data/tip/7/0/state

On domain 5:

1. Receive watch on /local/domain/5/data/tip/7/0/nock
1. Read .../nock
1. Set up watch on /local/domain/7/data/nock/5/0
1. Write .../state = 1 (Initialising)

Domain are now watch each other states.

On domain 5:

1. Open tube port, retrieve refs and event channel
1. Start transaction
1. Write /local/domain/5/data/tip/7/0/tx-ring-ref = (ref1)
1. Write .../rx-ring-ref = (ref2)
1. Write .../event-channel = (evtchn)
1. Write .../state = 3 (Initialised)
1. Commit transaction

On domain 7:

1. Receive watch on /local/domain/5/data/tip/7/0/state
1. Check that state is Initialised (3)
1. Read .../tx-ring-ref
1. Read .../rx-ring-ref
1. Read .../event-channel
1. Open port and pass refs and event channel to it
1. Write /local/domain/7/data/nock/5/0/state = 4 (Connected)

On domain 5:

1. Receive watch on /local/domain/7/data/nock/5/0/state
1. Write /local/domain/5/data/tip/7/0/state = 4 (Connected)
1. Issue port\_control command to the port to start listening for events

---

Open

1. Domain A wants to establish a new tube to domain B
1. Domain B starts outlet on its side and confirms
1. Domain A recieve the confirmation and starts the outlet, or
1. Domain A timeouts and returns an error

Close

1. Domain want to close the tube to domain B
1. Domain B stops outlet and confirms
1. Domain A receives confirmation and stops the outlet, or
1. Domain A timeouts and stops the outlet anyway

Domain shuts down

1. Domain A detect that domain B has been shut down
1. Domain A stops all outlets that connect to domain B

External tools must be able to inspect tubes.

