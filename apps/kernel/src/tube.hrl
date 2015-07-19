
-define(STATE_INITIALISING, "1").
-define(STATE_INIT_WAIT,	"2").
-define(STATE_INITIALISED,	"3").
-define(STATE_CONNECTED,	"4").
-define(STATE_CLOSING,		"5").
-define(STATE_CLOSED,		"6").

-define(TUBE_REQ_OPEN,	 1).
-define(TUBE_REQ_ATTACH, 2).

-define(TUBE_REP_OK,	0).
-define(TUBE_REP_ERROR, 1).

%%-define(g(Fmt, As), ok).
-define(g(Fmt, As), io:format(Fmt, As)).

