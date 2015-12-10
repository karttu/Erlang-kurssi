
%% agent_adam2.erl -- A simple (and still quite foolish) sample agent to be connected
%%                    to moveserver.erl
%%
%% After editing a proper hostname to serverhostname.hrl and after starting the moveserver
%% in that host (with -sname moveserver ) you can start this for example as: agent_adam2:init(1).

-module(agent_adam2).
-export([upgrade/1,init/1,init/2,initlocal/1]).
-include("serverhostname.hrl").
-compile(export_all).


%% agent_adam2:send_game_command(whereis(moveserver),make_ref(),login,[]).
%% agent_adam2:send_game_command(whereis(moveserver),make_ref(),makemove,{move,1}).
%% agent_adam2:send_game_command(whereis(moveserver),make_ref(),makemove,{layblockandmove,1}).

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

      Food_items = filter_food_only(Worldview),
      case Food_items of
           [_Atleastonefoodvisible|_Rest] ->
             Uusisuunta = direction_to_nearest_food_item(Food_items),
             NewLeavingblocks = false, %% Don't leave blocks when food is visible.
             io:format("Löydettiin safkaa, suuntana nyt=~w.~n",[Uusisuunta]);

           _ -> %% No food visible?
             case Statusmessage of
                  {blockedby,Jotakin} ->
                    io:format("Törmättiin seinään tai johonkin urpoon (~w), vaihdetaan suuntaa, Suunta oli=~w.~n",[Jotakin,Suunta]),
                    Uusisuunta = ((Suunta+3) rem 8),
                    NewLeavingblocks = not Leavingblocks;
                  _ ->
                    Uusisuunta = Suunta,                %% Keep them
                    NewLeavingblocks = Leavingblocks    %% ... same.
             end
      end,

      send_game_command(Gameserverpid,Myref,makemove,
                        {if NewLeavingblocks -> layblockandmove; true -> move end,
                        Uusisuunta}),

      agent_adam2:agentloop(Gameserverpid,Myref,Uusisuunta,NewLeavingblocks);

    lopeta ->
      io:format("agentloop: lopetetaan. Gameserverpid=~w, self()=~w.~n",[Gameserverpid,self()]);

    JokuMuuViesti ->
      io:format("agentloop: outo viesti: ~w, skipataan.~n",[JokuMuuViesti]),
      agent_adam2:agentloop(Gameserverpid,Myref,Suunta,Leavingblocks)
  end.


upgrade(AW) -> AW. % By default, keep everything same.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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



filter_food_only(Worldview) ->
   lists:filter(fun({_Pos,Item}) -> case Item of {passive,food,_} -> true; _ -> false end end,Worldview).

distance_between_squared({Ax,Ay},{Bx,By}) -> (((Bx-Ax)*(Bx-Ax)) + ((By-Ay)*(By-Ay))).

distance_lte({Ax,Ay},{Bx,By}) -> ((Ax*Ax)+(Ay*Ay)) < ((Bx*Bx)+(By*By)).

sort_by_distance(Worldview) ->
   lists:sort(fun({Pa,_},{Pb,_}) -> distance_lte(Pa,Pb) end,Worldview).



direction_to_nearest_food_item(Food_items) ->
   {EkaPos,_} = hd(sort_by_distance(Food_items)),
   find_minimizing_direction_to(EkaPos).

find_minimizing_direction_to(Pos) ->
   MD = lists:min([{distance_between_squared(Pos,computenewpos({0,0},Direction)),Direction} || Direction <- lists:seq(0,7)]),
   {_,D} = MD,
   D.


