
-define(TUBE_PORT_OPTS, [binary]).

-define(TUBE_REQ_OPEN,	 	 1).
-define(TUBE_REQ_ATTACH,	 2).
-define(TUBE_REQ_SEND_SLOTS, 3).

-define(TUBE_REP_OK,	0).
-define(TUBE_REP_ERROR, 1).

-define(g(Fmt, As), ok).
%%-define(g(Fmt, As), io:format(Fmt, As)).

