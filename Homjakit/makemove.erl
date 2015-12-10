
-module(makemove).

-include("homjakit.hrl").
-export([makemove/5]).
-compile(export_all).

makemove(ReceivedPID,Agentid,Agenttime,Servertime,Movetuple) ->
  spawn(?MODULE, movemaker, [ReceiverPID,Agentid,Agenttime,Servertime,Movetuple]),
  receive
    {Status,Waitnticks} -> {Status,Waitnticks}
    after ?MOVEMAKERTIMEOUT ->
      io:format("Movemaker timed out with agentid ~w with agenttime ~w with servertime ~w, with move: ~w~n",
                  [Agentid,Agenttime,Servertime,Movetuple])
  end.

movemaker(CallerPID,Agentid,Agenttime,Servertime,Movetuple) ->
  ok.

