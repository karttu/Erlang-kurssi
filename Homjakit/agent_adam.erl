
%% agent_adam.erl -- A simple and absolutely stupid sample agent to be connected
%%                   to moveserver.erl
%%
%% After editing a proper hostname to serverhostname.hrl and after starting the moveserver
%% in that host (with -sname moveserver ) you can start this for example as: agent_adam:init(1).

-module(agent_adam).
-export([upgrade/1,init/1,init/2,initlocal/1]).
-include("serverhostname.hrl").
-compile(export_all).


%% agent_adam:send_game_command(whereis(moveserver),make_ref(),login,[]).
%% agent_adam:send_game_command(whereis(moveserver),make_ref(),makemove,{move,1}).
%% agent_adam:send_game_command(whereis(moveserver),make_ref(),makemove,{layblockandmove,1}).

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
          io:format("Törmättiin seinään tai johonkin Urpoon (~w), vaihdetaan suuntaa, Suunta oli=~w.~n",[Jotakin,Suunta]),
          agentloop(Gameserverpid,Myref,((Suunta+3) rem 8),not Leavingblocks); %% Kokeile jotain muuta suuntaa.
        _ ->
          send_game_command(Gameserverpid,Myref,makemove,
                            {if Leavingblocks -> layblockandmove; true -> move end,
                             Suunta}),
          agentloop(Gameserverpid,Myref,Suunta,Leavingblocks) %% Pidä suunta samana.
      end;
%%    agentloop(Gameserverpid,Myref,((Suunta+1) rem 8),Leavingblocks); %% Tämä vain pyörisi ympyrää.

    lopeta ->
      io:format("agentloop: lopetetaan. Gameserverpid=~w, self()=~w.~n",[Gameserverpid,self()]);

    JokuMuuViesti ->
      io:format("agentloop: outo viesti: ~w, skipataan.~n",[JokuMuuViesti]),
      agentloop(Gameserverpid,Myref,Suunta,Leavingblocks)
  end.


upgrade(AW) -> AW. % By default, keep everything same.

