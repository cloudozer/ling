%%%
%%%
%%%

-record(mumble, {ver,
				 session_key,
				 node,
				 node_group,
				 extra}).

-record(munge, {ver,
				realm,
				origin,
				uid,
				gid,
				dec_uid,
				dec_gid,
				payload}).

%%EOF
