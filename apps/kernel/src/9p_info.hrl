%%%
%%%
%%%

% an acceptor loop state
-record(ac, {sock,
			 trans_mod,
			 trans_conf,
			 trace =false,
			 started,
			 is_local,
			 basic_auth =false,
			 session_key,
			 ver,
			 auth_path,
			 auth_user,
			 auth_uid,
			 peer_node,
			 peer_group,
			 unix_user,
			 unix_group,
		 	 msize,
		 	 fids}).

