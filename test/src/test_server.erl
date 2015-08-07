-module(test_server).
-compile(export_all).

lookup_config(Key, Config) -> proplists:get_value(Key, Config).

fail() -> error(failed).
fail(Reason) -> error(Reason).

