-define(fsm_test(Id, CmdList),
    fun() -> [eunit_fsm:translateCmd(Id, Cmd) || Cmd <- CmdList] end).
