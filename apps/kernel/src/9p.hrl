%%
%%
%%

%-define(DEBUG_9P, 1).
-ifdef(DEBUG_9P).
%-define(dbg(Fmt, Args), (io:format((Fmt), (Args)))).
-define(dbg(Fmt, Args), (erlang:display({?MODULE,Fmt,Args}))).
-else.
-define(dbg(Fmt, Args), ok).
-endif.

%% The maximum message size must correspond to the value of TCP_MIN_RECV_BUF.
%% The small maximum message size of 2048 ensures that a single page is
%% initially allocated for a TCP/IP outlet. Values exceeding about 3200 will
%% allocation of at least two pages per TCP/IP outlet.
%%
-define(MSIZE, 8192).
-define(IOHDRSZ, 27).				%% 9P read/write header size
-define(CSIZE, ?MSIZE -?IOHDRSZ).	%% Maximum chunk size of read/write

-define(QTDIR,		16#80).	% Directory
-define(QTAPPEND,	16#40).	% Append only
-define(QTEXCL,		16#20).	% Exclusive use
-define(QTMOUNT,	16#10).	% Mounted channel
-define(QTAUTH,		16#08).	% Authentication
-define(QTTMP,		16#04).	% Temporary
-define(QTLINK,		16#02).	% Temporary
-define(QTFILE,		16#00).	% plain file ??

-define(DMDIR,      16#80000000).
-define(DMAPPEND,   16#40000000).
-define(DMEXCL,     16#20000000).
-define(DMMOUNT,    16#10000000).
-define(DMAUTH,     16#08000000).
-define(DMTMP,      16#04000000).
-define(DMSYMLINK,  16#02000000).
-define(DMDEVICE,   16#00800000).
-define(DMNAMEDPIPE,16#00200000).
-define(DMSOCKET,   16#00100000).
-define(DMSETUID,   16#00080000).
-define(DMSETGID,   16#00040000).
-define(DMREAD,     16#4).
-define(DMWRITE,    16#2).
-define(DMEXEC,     16#1).

-record(rstatfs, {tag, type =-1, bsize =-1, blocks =-1, bfree =-1, bavail =-1,
		files =-1, ffree =-1, fsid =-1, namelen =-1}).

-record(rgetattr, {tag, valid =0, mode =0, uid =0, gid =0, nlink =0, rdev =0,
		ino =0, size =0, blksize =0, blocks =0, atime_sec =0, atime_nsec =0,
		mtime_sec =0, mtime_nsec =0, ctime_sec =0, ctime_nsec =0, btime_sec =0,
		btime_nsec =0, gen =0, data_version =0}).

%% returned/consumed by stat/wstat operations
-record(stat, {ver =e,	%% 9P2000.e or 9P2000.u
			   type =0,	%% for kernel use?
			   dev =0,	%% for kernel use?
			   qid,
			   mode,
			   atime,
			   mtime,
			   length,
			   name,
			   uid = <<>>,
			   gid = <<>>,
			   muid = <<>>,
		   	   ext = <<>>,
		   	   num_uid =0,
		   	   num_gid =0,
		   	   num_muid =0}).

%% 9P2000.L only

%%-define(Tlerror,	6).
-define(Rlerror,	7).
-define(Tstatfs,	8).
-define(Rstatfs,	9).
-define(Tlopen,		12).
-define(Rlopen,		13).
-define(Tlcreate,	14).
-define(Rlcreate, 	15).
-define(Tsymlink,	16).
-define(Rsymlink,	17).
-define(Tmknod,		18).
-define(Rmknod,		19).
-define(Trename,	20).
-define(Rrename,	21).
-define(Treadlink,	22).
-define(Rreadlink,	23).
-define(Tgetattr,	24).
-define(Rgetattr,	25).
-define(Tsetattr,	26).
-define(Rsetattr,	27).
-define(Txattrwalk,		30).
-define(Rxattrwalk, 	31).
-define(Txattrcreate,	32).
-define(Rxattrcreate,	33).
-define(Treaddir,	40).
-define(Rreaddir,	41).
-define(Tfsync,		50).
-define(Rfsync,		51).
-define(Tlock,		52).
-define(Rlock,		53).
-define(Tgetlock,	54).
-define(Rgetlock,	55).
-define(Tlink,		70).
-define(Rlink,		71).
-define(Tmkdir,		72).
-define(Rmkdir,		73).
-define(Trenameat,	74).
-define(Rrenameat,	75).
-define(Tunlinkat,	76).
-define(Runlinkat,	77).

%% 9P2000 and 9P2000.u

-define(Tversion,	100).
-define(Rversion,	101).
-define(Tauth,		102).
-define(Rauth,		103).
-define(Tattach,	104).
-define(Rattach,	105).
%%-define(Terror,	106).
-define(Rerror,		107).
-define(Tflush,		108).
-define(Rflush,		109).
-define(Twalk,		110).
-define(Rwalk,		111).
-define(Topen,		112).
-define(Ropen,		113).
-define(Tcreate,	114).
-define(Rcreate,	115).
-define(Tread,		116).
-define(Rread,		117).
-define(Twrite,		118).
-define(Rwrite,		119).
-define(Tclunk,		120).
-define(Rclunk,		121).
-define(Tremove,	122).
-define(Rremove,	123).
-define(Tstat,		124).
-define(Rstat,		125).
-define(Twstat,		126).
-define(Rwstat,		127).

%% 9P2000.e

-define(Tsession,	150).
-define(Rsession,	151).
-define(Tsread,		152).
-define(Rsread,		153).
-define(Tswrite,	154).
-define(Rswrite,	155).

-define(NOTAG, 16#ffff).
-define(NOFID, 16#ffffffff).
-define(NOUID, 16#ffffffff).

%%EOF
