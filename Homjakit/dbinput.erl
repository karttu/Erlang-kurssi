
-module(dbinput).
-include("homjakit.hrl").
-export([]).


dbinloop(TimeNow) ->
  receive
    {Timestamp,Fromnode,FromPID,AgentID} ->
      if CellGen /= Gen
            ->
           io:format("Supervisor received in generation ~w state signal from the cell ~w (state=~w) claiming to be in generation ~w. ~w messages has been already received from the cells.~n",
                        [Gen,CellName,CellState,CellGen,NMsgs]),
           exit(cells_not_in_sync);
       true ->
            supervise(Size,NCells,Gen,NMsgs+1,Cellnames,Board,show_cell(Board,CellName,CellState,AllDrawnCircles))
      end;
    stop ->
      io:format("Supervisor stopped in generation ~w~n",
                   [Gen]),
      exit(stopped)
  after 10000 ->
      io:format("Supervisor timed out in generation ~w after having received ~w messages from the cells.~n",
                        [Gen,NMsgs]),
      exit(supervisor_timedout)
  end.


