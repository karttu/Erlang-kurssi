
%% agent_bertha.erl -- A simple and quite stupid sample agent to be connected
%%                     to moveserver.erl
%%
%% After editing a proper hostname to serverhostname.hrl and after starting the moveserver
%% in that host (with -sname moveserver ) you can start this for example as: agent_bertha:init(1).

-module(agent_bertha).
-export([upgrade/1,init/1,init/2,initlocal/1]).
-include("serverhostname.hrl").
-compile(export_all).


%% agent_bertha:send_game_command(whereis(moveserver),make_ref(),login,[]).
%% agent_bertha:send_game_command(whereis(moveserver),make_ref(),makemove,{move,1}).
%% agent_bertha:send_game_command(whereis(moveserver),make_ref(),makemove,{layblockandmove,1}).

send_game_command(Gameserverpid,Myref,Cmd,Parameters) ->
  Gameserverpid ! {{node(),self(),Myref},Cmd,Parameters}.



init(Gameserverpid,Alkusuunta) ->
  register(myag,spawn(?MODULE,spawned_agentloop,[Gameserverpid,Alkusuunta])).

init(Alkusuunta) ->
  init(rpc:call(list_to_atom(atom_to_list(moveserver@) ++ ?SERVERHOSTNAME),moveserver,pidofmoveserver,[]),
       Alkusuunta).

%% Jos ajat samassa nodessa sekä moveserver:iä että tätä agenttia, niin 
%% käynnistä tällä:
initlocal(Alkusuunta) -> init(whereis(moveserver),Alkusuunta).


spawned_agentloop(Gameserverpid,Alkusuunta) ->
  Myref = make_ref(),
  send_game_command(Gameserverpid,Myref,login,[]),
  agentloop(Gameserverpid,Myref,Alkusuunta,false).


print_info_about_server_response(Packet,Agentid,Agentchar,Agentchar,Statusmessage,Servertimenow,Nexttimemoveallowed)
   ->
  io:format("~w: Olet agentti numero ~w, char=~tc (unicode ~w), serveri vastasi=~w. Voit liikkua uudestaan ajanhetkellä ~w. Muuta tietoa=~w.~n~n",
              [Servertimenow,Agentid,Agentchar,Agentchar,Statusmessage,Nexttimemoveallowed,Packet]).



agentloop(Gameserverpid,Myref,Suunta,Leavingblocks) -> % AW = Agent's own view of the World.

  receive
    WHOLE_PACKET=[
     {agentref,_Myref_now_ignored},
     {agentid,Agentid},
     {agentchar,Agentchar},
     {servertimenow,Servertimenow},
     {nexttimemoveallowed,Nexttimemoveallowed},
     {statusmessage,Statusmessage},
     {score,Score},
     {worldview,Worldview}
     | _REST_FOR_FUTURE_FUNCTIONALITY]
      -> 
      print_info_about_server_response(WHOLE_PACKET,Agentid,Agentchar,Agentchar,Statusmessage,Servertimenow,Nexttimemoveallowed),
      case Statusmessage of
        {blockedby,Jotakin} ->
          io:format("Törmättiin seinään tai johonkin Urpoon (~w), Suunta oli=~w.~n",[Jotakin,Suunta]);
        _ ->
          io:format("Ei törmätty kehenkään, status=~w, suunta oli=~w.~n",[Statusmessage,Suunta])
      end,

      Uusisuunta = find_smallest_direction_not_occupied_by_anything_but_food(Worldview,Suunta),
      send_game_command(Gameserverpid,Myref,makemove,{move,Uusisuunta}),
      agent_bertha:agentloop(Gameserverpid,Myref,
                             Uusisuunta,
                             Leavingblocks); %% Pidä Leavingblocks-flägi samana (toistaiseksi: älä jätä.)

    lopeta ->
      io:format("agentloop: lopetetaan. Gameserverpid=~w, self()=~w.~n",[Gameserverpid,self()]);

    JokuMuuViesti ->
      io:format("agentloop: outo viesti: ~w, skipataan.~n",[JokuMuuViesti]),
      agent_bertha:agentloop(Gameserverpid,Myref,Suunta,Leavingblocks)
  end.


upgrade(AW) -> AW. % By default, keep everything same.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Borrowed from moveserver.erl:
%% [computenewpos({0,0},Direction) || Direction <- lists:seq(0,7)].         
%% gives: [{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}]

computenewpos(Agentpos,Direction) ->
  {X,Y} = Agentpos,
  case Direction of
       0 -> {X+1,Y};   %% E
       1 -> {X+1,Y+1}; %% SE
       2 -> {X,Y+1};   %% S
       3 -> {X-1,Y+1}; %% SW
       4 -> {X-1,Y};   %% W
       5 -> {X-1,Y-1}; %% NW
       6 -> {X,Y-1};   %% N
       7 -> {X+1,Y-1}  %% NE
  end.



%% Worldview can be something like:
%% [{{-1,-1},{passive,block,[]}},{{0,1},{blockedby,wall}}]

%%%%%%%%%%%%%%
%% 
%% Esimerkki:
%% agent_bertha:find_smallest_direction_not_occupied_by_anything([{{1,0},{passive,block,[]}},   {{-1,-1},{passive,block,[]}} , {{0,1}, {blockedby,wall}}])
%% gives 1.
%% agent_bertha:find_smallest_direction_not_blocked_by_wall([{{1,0},{passive,block,[]}},   {{-1,-1},{passive,block,[]}} , {{0,1}, {blockedby,wall}}])
%% gives 0.

%% agent_bertha:find_smallest_direction_not_blocked_by_wall([{{1,0},{blockedby,wall}}, {{1,1},{passive,block,[]}},  {{-1,-1},{passive,block,[]}} , {{0,1}, {blockedby,wall}}]).
%% gives 1.
%% 165> agent_bertha:find_smallest_direction_not_occupied_by_anything([{{1,0},{blockedby,wall}}, {{1,1},{passive,block,[]}},  {{-1,-1},{passive,block,[]}} , {{0,1}, {blockedby,wall}}]).
%% gives 3.

%% lists:keyfind ja lists:keymember dokumentoitu: http://www.erlang.org/doc/man/lists.html

find_smallest_direction_not_occupied_by_anything(Worldview) ->
    find_smallest_direction_not_occupied_by_anything(Worldview,0). %% Aloita suuntien tsekkaus 0:sta.

find_smallest_direction_not_occupied_by_anything(Worldview,8) -> %% Ei löytynyt!, ollaan täysin blokkien ympäröimänä?!
    find_smallest_direction_not_blocked_by_wall_or_agent(Worldview); %% Try again, and be less picky this time.

find_smallest_direction_not_occupied_by_anything(Worldview,Direction) ->
    DirectionBlocked = lists:keymember(computenewpos({0,0},Direction), 1, Worldview),
    if not DirectionBlocked -> Direction;
       true ->  find_smallest_direction_not_occupied_by_anything(Worldview,Direction+1) %% Try next direction
    end.

%%%%%%%%%%%%%%


find_smallest_direction_not_occupied_by_anything_but_food(Worldview) ->
    find_smallest_direction_not_occupied_by_anything_but_food(Worldview,0).

find_smallest_direction_not_occupied_by_anything_but_food(Worldview,8) -> %% Ei löytynyt!
    find_smallest_direction_not_blocked_by_wall_or_agent(Worldview); %% Try again, and be less picky this time.

find_smallest_direction_not_occupied_by_anything_but_food(Worldview,Direction) ->
    DirectionOccupiedBySomething = lists:keyfind(computenewpos({0,0},Direction), 1, Worldview),
    case DirectionOccupiedBySomething of
       {_Pos,{blockedby,_Anything}}
           -> find_smallest_direction_not_occupied_by_anything_but_food(Worldview,Direction+1); %% Try next direction
       {_Pos,{passive,block,_Anything}}
           -> find_smallest_direction_not_occupied_by_anything_but_food(Worldview,Direction+1); %% Try next direction
       {_Pos,{agent,_,_}} when Direction > Direction
           -> find_smallest_direction_not_blocked_by_wall_or_agent(Worldview,Direction-1); %% Very bad idea.
       {_Pos,{agent,_,_}}
           -> find_smallest_direction_not_blocked_by_wall_or_agent(Worldview,Direction+1);
       _ ->  Direction %% Else this direction is not occupied by the wall or agent. Return it!
    end.

%%%%%%%%%%%%%%

find_smallest_direction_not_blocked_by_wall_or_agent(Worldview) ->
   find_smallest_direction_not_blocked_by_wall_or_agent(Worldview,0).

find_smallest_direction_not_blocked_by_wall_or_agent(_Worldview,8) -> 0; %% Should not really happen!

find_smallest_direction_not_blocked_by_wall_or_agent(Worldview,Direction) ->
    DirectionOccupiedBySomething = lists:keyfind(computenewpos({0,0},Direction), 1, Worldview),
    case DirectionOccupiedBySomething of
       {_Pos,{blockedby,_Anything}}
           -> find_smallest_direction_not_blocked_by_wall_or_agent(Worldview,Direction+1); %% Try next direction
       {_Pos,{agent,_,_}}
           -> find_smallest_direction_not_blocked_by_wall_or_agent(Worldview,Direction+1); %% Try next direction
       _ ->  Direction %% Else this direction is not occupied by the wall. Return it!
    end.

