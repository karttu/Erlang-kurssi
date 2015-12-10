%%
%% showboard.erl -- Näyttöprosessi Erlang-moniagenttipeliä varten.
%% Copyright (C) - Antti Karttunen, 2013.
%% Uses modules characters.erl,
%% and also modules showboard_wx_win & showboard_wx_board,
%% which are modified from a Sudoku sample code Copyright (C) by Ericsson.
%%

%% After editing a proper hostname to serverhostname.hrl and after starting the moveserver
%% in that host (with -sname moveserver ) you can start this for example as: showboard:init().

%%
%% Changes:
%%  2013-04-23: Now showboard_loop will receive also Agentscorelist from the moveserver,
%%              which is currently just sorted and printed out. (I leave it as an exercise
%%              for anybody to implement a more sophisticated showing of agents' scores
%%              on the same showboard as the table itself.)
%%

-module(showboard).
-include("homjakit.hrl").
-export([init/0,init/1,initlocal/0]).
-import(characters,[agentid2uchar/1]).
-import(showboard_wx_win,[new/1]).
-import(showboard_wx_board,[set_butt/1]).
-include("serverhostname.hrl").
-compile(export_all).

%% Loginboard = showboard:createLoginBoardFunction(Pid_of_Moverserver).


createLoginBoardFunction(MoveserverPid) ->
  fun(Mypid) -> MoveserverPid ! {{please,ignore,this},login_showboard,Mypid,node()} end.

init(Loginboard_to_moveserver) when is_function(Loginboard_to_moveserver) ->
  spawn(showboard,showboard_process,[Loginboard_to_moveserver]);


%% After you have started moveserver itself.
init(MoveserverPid) when is_pid(MoveserverPid) ->
  init(createLoginBoardFunction(MoveserverPid)).

init() ->
  showboard:init(rpc:call(list_to_atom(atom_to_list(moveserver@) ++ ?SERVERHOSTNAME),moveserver,pidofmoveserver,[])).

initlocal() ->
  showboard:init(whereis(moveserver)).


showboard_process(Loginboard_to_moveserver) ->
  Canvas = canvas_new(),
  Loginboard_to_moveserver(self()),
  showboard_loop(Canvas).

receive_board_ref() ->
    receive {gfx,_,Canvas} -> Canvas
            after 60000 ->
              io:format("receive_board_ref timed out!",[]),
              exit(timeout)
    end.

canvas_new() ->
  _WxWinRef = showboard_wx_win:new(self()),
  Canvas = receive_board_ref(),
  Canvas.

showboard_loop(Canvas) ->
  receive
    {Timestamp,Boardlist_diffs,Agentscorelist} ->
      Agents_sorted_by_their_scores
       = lists:sort(fun({_AID1,Score1},{_AID2,Score2}) -> Score1 >= Score2 end,Agentscorelist),
      io:format("showboard, now showing for time ~w, length(Boardlist_diffs)=~w, Agents by their scores=~w~n",
                  [Timestamp,length(Boardlist_diffs),Agents_sorted_by_their_scores]),
      show_board_on_canvas(Canvas,Timestamp,Boardlist_diffs)
%%  after 1000 -> buu
  end,
  showboard:showboard_loop(Canvas).


show_board_on_canvas(_Canvas,_Timestamp,[]) -> ok;

show_board_on_canvas(Canvas,Timestamp,[ThisSquare|Rest_of_boardlist]) ->
  {Position,What} = ThisSquare,
  show_item_on_canvas(Canvas,Position,What),
  show_board_on_canvas(Canvas,Timestamp,Rest_of_boardlist).


show_item_on_canvas(Canvas,{PosX,PosY},[]) ->
  showboard_wx_board:set_butt(Canvas,{PosY,PosX},32);

show_item_on_canvas(Canvas,{PosX,PosY},{Whatkind,Id,_Options}) ->
  case Whatkind of
    agent -> Unicodechar = characters:agentid2uchar(Id);
    passive ->
      case Id of
        food  -> Unicodechar = characters:foodchar();
        block -> Unicodechar = characters:blockchar();
        _Other -> Unicodechar = 63 % I.e. question mark. We got a bug.
      end;
    _ -> Unicodechar = 64
  end,
  showboard_wx_board:set_butt(Canvas,{PosY,PosX},Unicodechar).


