
%% moveserver.erl -- Pääserveriprosessi Erlang-moniagenttipeliä varten.
%% Copyright (C) - Antti Karttunen, 2013.

%% Vaiheessa oleva versio April 6, 00:03. Näyttö jo paljon nopeampi,
%% koska nyt lähetetään vain diffit laudasta showboard:ille.
%% (Kokeile esim. ticker ! 500. käynnistyksen jälkeen!)

%% Now communication with agents and waking up of the sleeping ones
%% is starting (finally!) to work...

%% Todo: food & other delivery process interfaces. (Yes, done now.)
%% Todo: real views of what can be seen. (not just immediate neighbours).

-module(moveserver).

-include("homjakit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-import(lists,[foreach/2,map/2,nth/2,seq/2,filter/2]).
-import(characters,[agentid2uchar/1]).
-export([init/2,somepid/0,pidofmoveserver/0,computenewpos/2,find_what_is_in_pos_of_board/2]).
-compile(export_all).

treissaa(Funname) ->
  dbg:tracer(),
  dbg:p(all, [c]), % Too much info:  dbg:p(all, [c,call,return_to]),
  dbg:tpl({moveserver,Funname,'_'},dbg:fun2ms(fun(_) -> return_trace() end)).

somepid() -> self().

%% Call for example as: rpc:call(moveserver@U204PCOpe,moveserver,pidofmoveserver,[]).
%% from the node where you start your agent:
pidofmoveserver() -> whereis(moveserver).

%%
%% The command requested can be:
%%
%% If the agent crashes, it should start the next transaction with login.
%%

init(StartTime,Nodes2agents) ->
  register(moveserver, spawn(?MODULE, startmoveserverloop, [StartTime,Nodes2agents])).

startmoveserverloop(StartTime,Nodes2agents) ->
  ets:new(agent_requests_and_replies, [named_table]),
  ets:new(agents_active_in_the_latest_cycle, [named_table]), %% [named_table, ordered_set]
  ets:new(agents_scores, [named_table]),
  ets:new(boardshowers, [named_table]),
  ets:new(current_board, [named_table, ordered_set]), %% MUST be ordered_set !!!
  start_ticker(?TICKDEFAULT),
  moveserver:moveserverloop(StartTime,Nodes2agents,0).

start_ticker(Timepause) ->
  register(ticker, spawn_link(?MODULE, ticker, [self(), Timepause])).

stop_ticker() ->
  ticker ! stop.

%% This is spawned as a separate process, and sends "master clock pulses"
%% to the moveserver, in every TickinMS milliseconds:
ticker(MoveserverPID, TickinMS) ->
  receive
    NewtickinMS when is_integer(NewtickinMS) ->
      moveserver:ticker(MoveserverPID, NewtickinMS);

    stop ->
      ok

    after TickinMS ->
%%    MoveserverPID ! timetick,
      moveserver ! timetick,
      moveserver:ticker(MoveserverPID, TickinMS)
  end.


posofelem(Elem,Lista) ->
  posofelem(Elem,Lista,1).

posofelem(_,[],_) -> 0; %% Not found, return zero.
posofelem(Elem,[Elem|_Rest],Pos) -> Pos; %% Found first matching, return its Pos.
posofelem(Elem,[_|Rest],Pos) -> posofelem(Elem,Rest,Pos+1).

nodes2agents(WhoFromWhere,Nodes2agents) ->
  {AgentNode,_AgentPid,_AgentRef} = WhoFromWhere,
  AgentId = posofelem(AgentNode,Nodes2agents),
  case AgentId of
    0   -> {length(Nodes2agents)+1,lists:append(Nodes2agents,[AgentNode])}; % Uusi node, lisääpä listan loppuun.
    Pos -> {Pos,Nodes2agents} %% Oli listassa Nodes2agents entuudestaan.
  end.


send_worldview_to(Timestamp,Agentid) ->
    send_worldview_with_a_reply_to(Timestamp,
                                   get_a_reply_to_agent(agent_requests_and_replies,Agentid),
                                   Agentid
                                  ).


send_worldview_with_a_possible_wakeup_call_to(Timestamp,Agentid) ->
    Reply_to_agent = get_a_reply_to_agent(agent_requests_and_replies,Agentid),
    {_Agentid,WhoFromWhere,Timethen,Nexttimemoveallowed,Statusmessagethen} = Reply_to_agent,

    if (Nexttimemoveallowed < Timestamp) -> Newstatusmessage = wake_up;
       true -> Newstatusmessage = Statusmessagethen
    end,
    io:format("Time ~w: Sending new worldview to active agent ~w, which last committed a move at time ~w with status ~w. Newstatus=~w~n",
               [Timestamp,Agentid,Timethen,Statusmessagethen,Newstatusmessage]),
    send_worldview_with_a_reply_to(Timestamp,
                                   {Agentid,WhoFromWhere,Timestamp,Nexttimemoveallowed,Newstatusmessage},
                                   Agentid
                                  ).


agents_score(Agentid) ->
   [{Agentid,Score}] = ets:lookup(agents_scores,Agentid),
   Score.

inc_agents_score(Agentid,MoreScore) -> ets:insert(agents_scores,{Agentid,(agents_score(Agentid)+MoreScore)}).
set_agents_score(Agentid,Score) -> ets:insert(agents_scores,{Agentid,Score}).

send_worldview_with_a_reply_to(Timestamp,Reply_to_agent,Agentid) ->

%%io:format("size(Reply_to_agent)=~w. element(~w,Reply_to_agent)=~w~nReply_to_agent=~w~n~n",
%%           [size(Reply_to_agent),size(Reply_to_agent),element(size(Reply_to_agent),Reply_to_agent),Reply_to_agent]),

  {_Agentid,WhoFromWhere,_Timenow,Nexttimemoveallowed,Statusmessage} = Reply_to_agent,

  {_AgentNode,AgentPid,AgentRef} = WhoFromWhere,

  Worldview = worldview:world_viewed_from(current_board,find_agentspos_on_board(current_board,Agentid)),
  Agentchar = characters:agentid2uchar(Agentid),

  WHOLE_PACKET = 
    [{agentref,AgentRef},
     {agentid,Agentid},
     {agentchar,Agentchar},
     {servertimenow,Timestamp},
     {nexttimemoveallowed,Nexttimemoveallowed},
     {statusmessage,Statusmessage},
     {score,agents_score(Agentid)}, %% Zero so far.
     {worldview,Worldview},
     {whofromwhere,WhoFromWhere}, %% Extras for debugging.
     {muutatietoa,Reply_to_agent}],

  io:format("Lähetetään ~w:lle ~w.~n", [WhoFromWhere, WHOLE_PACKET]),

  AgentPid ! WHOLE_PACKET,
  ok.


%% We don't actually use this now, although it might be more fair to agents:
send_new_worldview_to_all_active_agents(Timestamp) ->
  send_new_worldview_to_all_active_agents(Timestamp,ets:first(agents_active_in_the_latest_cycle),0).

send_new_worldview_to_all_active_agents(Timestamp,'$end_of_table',CNT) ->
  io:format("Time: ~w, sent a new worldview to ~w recently active agents.~n",[Timestamp,CNT]),
  ok;

send_new_worldview_to_all_active_agents(Timestamp,Key,CNT) ->
  Stuff = hd(ets:lookup(agents_active_in_the_latest_cycle,Key)),
  Nextkey = ets:next(agents_active_in_the_latest_cycle,Key),
  ets:delete(agents_active_in_the_latest_cycle,Key),
  io:format("Sending new worldview to active agent ~w.~n",[Stuff]),
  {_,Agentid} = Stuff,
  send_worldview_to(Timestamp,Agentid),
  send_new_worldview_to_all_active_agents(Timestamp,Nextkey,CNT+1).


send_new_worldview_to_all_movable_agents(Timestamp) ->
    Movable_agents = find_movable_agents(agent_requests_and_replies,Timestamp),
    lists:foreach(fun(Agentid) -> send_worldview_with_a_possible_wakeup_call_to(Timestamp,Agentid) end,
                  Movable_agents
                 ),
    io:format("Time ~w: Sent a new worldview to ~w agents, whose time to move was now or already passed.~n",
               [Timestamp,length(Movable_agents)]).


find_movable_agents(Agents_ets,Has_been_able_to_move_since_cycle_N) ->
  ets:select(Agents_ets,
                ets:fun2ms(fun({Agentid,WhoFromWhere,Timethen,Moveallowed,Message})
                       when Moveallowed =< Has_been_able_to_move_since_cycle_N -> Agentid
                           end)
            ).



%% Called e.g. as: find_sleeping_agents(agent_requests_and_replies,(Timestamp-1)-Have_been_sleeping_for_n_cycles),
find_sleeping_agents(Agents_ets,Norequests_after_time) ->
  ets:select(Agents_ets,
                ets:fun2ms(fun({Agentid,WhoFromWhere,Timethen,Moveallowed,Message})
                                   when Timethen =< Norequests_after_time -> Agentid
                           end)
            ).


%%
%% For each agent we should store:
%%  It's allocated AgentID (a natural number from 1 onward.) (This works as a key of the table).
%%  The Agentchar (the unicode character) allocated for it.
%%  Its WhoFromWhere information.
%%  The server timetick & now() timestamp of the first time it contacted us.
%%  The server timetick & now() timestamp of the last time it contacted us.
%%  The nextmoveallowed time (milloin siirtokarenssi loppuu? Riippuu tehdystä siirrosta.)
%%  Its current score (& other holdings?)
%%  As: {agentstatus,[{score,Score}|OTHER_PROPERTIES_FOR_FUTURE_EXPANSION]}
%%
%% This status-information is updated whenever an agent sends us a command,
%% as well as when agentstatus and nextmoveallowed are known after the move
%% has been validated and committed to the system.
%%
%%

set_a_reply_to_agent(Agents_ets,Agentid,WhoFromWhere,Timenow,Movenotalloweduntil,Message) ->
  ets:insert(Agents_ets,{Agentid,WhoFromWhere,Timenow,Movenotalloweduntil,Message}),
  ok.

get_a_reply_to_agent(Agents_ets,Agentid) ->
  hd(ets:lookup(Agents_ets,Agentid)).

%%%%%%%%%%%%%%%%%%%%%

%% Lähetä pelkät diffit:
%% Kutsu funktiota: diff_sorted_key1lists(Oldlist,Newlist)


send_new_board_to_all_showers(Boardtabref,AgentScoreTabRef,Timestamp) ->
  send_new_board_to_all_showers(ets:tab2list(Boardtabref),
                                ets:tab2list(AgentScoreTabRef),
                                Timestamp,ets:first(boardshowers),0).


send_new_board_to_all_showers(_Wholeboardlist,_Agentscorelist,Timestamp,'$end_of_table',CNT) ->
  io:format("Time: ~w, sent the board list to ~w showboards.~n",[Timestamp,CNT]),
  ok;

send_new_board_to_all_showers(Wholeboardlist,Agentscorelist,Timestamp,BoardPid,CNT) ->
  io:format("Boardshower ID=~w~n",[BoardPid]),
  [{_BoardPid,PrevWholeBoard}] = ets:lookup(boardshowers,BoardPid),
  BoardPid ! {Timestamp,diff_sorted_key1lists(PrevWholeBoard,Wholeboardlist),Agentscorelist}, % Nopeampi näyttö!
  ets:insert(boardshowers,{BoardPid,Wholeboardlist}), %% Update the Prev Whole Boardlist field in tuple.
  send_new_board_to_all_showers(Wholeboardlist,Agentscorelist,Timestamp,ets:next(boardshowers,BoardPid),CNT+1).

%%%%%%%%%%%%%%%%%%%%%

moveserverloop(Timenow,Nodes2agents,Lasttime_with_itemspresent) ->
  receive

    shutdown ->
%%      exit(ticker,kill),
        io:format("Timenow=~w, Nodes2agents=~w.~n",[Timenow,Nodes2agents]),
        Nodes2agents;

    timetick ->
        NewLasttime_with_itemspresent = mannaman:addnewfood(current_board,Timenow+1,Lasttime_with_itemspresent),
        send_new_board_to_all_showers(current_board,agents_scores,Timenow+1),
%%      send_new_worldview_to_all_active_agents(Timenow+1),
        send_new_worldview_to_all_movable_agents(Timenow+1),
        moveserver:moveserverloop(Timenow+1,Nodes2agents,NewLasttime_with_itemspresent);

    {WhoFromWhere,login,Parameters} ->
%%      {AgentNode,AgentPid,AgentRef} = WhoFromWhere,
        {Agentid,NewNodes2agents} = nodes2agents(WhoFromWhere,Nodes2agents),

        io:format("moveserverloop, Login at time ~w: WhoFromWhere=~w, Parameters=~w, got Agentid=~w, Nodes2agents=~w.~n",
                    [Timenow,WhoFromWhere,Parameters,Agentid,NewNodes2agents]),

        case find_agentspos_on_board(current_board,Agentid) of
          [] ->
              Agentpos = choose_starting_position_for_agent(current_board,Agentid,Timenow),
              case Agentpos of
                false ->
                 AlreadyOnBoard = sorry_board_full, %% XXX - Palauta joku sorry, you are not logged in viesti!
                 io:format(
                      "VAKAVAA: uudelle agentille numero ~w ei löytynyt enää vapaata paikkaa, mahtaa olla ahdasta!~n",
                            [Agentid]);
                _ ->
                 AlreadyOnBoard = welcome_new_agent,
                 set_agents_score(Agentid,0), %% Start scoring from zero.
                 io:format("Agentti ~w ei ole vielä laudalla, arvotaan sille uusi paikka=~w~n",
                            [Agentid,Agentpos])
              end;
          Agentpos ->
              AlreadyOnBoard = welcome_again,
              io:format("Agentti ~w on jo laudalla, sen paikka=~w~n",
                         [Agentid,Agentpos])
        end,


        ets:insert(agents_active_in_the_latest_cycle,{now(),Agentid}),
%% XXX - After login, wait ?FORCED_WAIT_AFTER_LOGIN additional cycles.
%% This is a quick fix to avoid "relogin cheating":
        set_a_reply_to_agent(agent_requests_and_replies,
                             Agentid,WhoFromWhere,Timenow,Timenow+1+?FORCED_WAIT_AFTER_LOGIN,
                             {logged_in,AlreadyOnBoard}),
        moveserver:moveserverloop(Timenow,NewNodes2agents,Lasttime_with_itemspresent);

    {WhoFromWhere,login_showboard,BoardPid,Fromnode} ->
        io:format("moveserverloop, Time ~w: ~w, WhoFromWhere=~w~n",
                            [Timenow,{login_showboard,BoardPid,Fromnode},WhoFromWhere]),
        ets:insert(boardshowers,{BoardPid,[]}),
        moveserver:moveserverloop(Timenow,Nodes2agents,Lasttime_with_itemspresent);

    {WhoFromWhere,makemove,Movetuple} ->
        io:format("moveserverloop, Time ~w: ~w~n",
                    [Timenow,{WhoFromWhere,makemove,Movetuple}]),
%%      {AgentNode,AgentPid,AgentRef} = WhoFromWhere,
        {Agentid,NewNodes2agents} = nodes2agents(WhoFromWhere,Nodes2agents),
        case ets:lookup(agent_requests_and_replies,Agentid) of
             [{Agentid,_OldAgentRef,_Movedlastime,Nextmoveallowed,OldMessage}] ->

               if (Timenow < Nextmoveallowed) ->
                    io:format("Agent ~w (WhoFromWhere=~w) trying vainly a move at time ~w, allowed only at ~w or later. Move ignored: ~w~n",
                               [Agentid,WhoFromWhere,Timenow,Nextmoveallowed,Movetuple]),
%% XXX - TODO: We should not answer hasty clients too earnestly, otherwise we get a vicious loop between us:
                    if (OldMessage == never_send_any_too_hasty_messages) ->
%%                  if (OldMessage /= you_are_too_hasty) ->
                        ets:insert(agents_active_in_the_latest_cycle,{now(),Agentid}), %% Now first time hasty agents are active.
                        set_a_reply_to_agent(agent_requests_and_replies,Agentid,WhoFromWhere,Timenow,Nextmoveallowed,you_are_too_hasty);
                       true -> io:format("Time ~w: Ignoring repeated hasty messages from agent ~w.~n",[Timenow,Agentid])
                    end;

                  (Timenow >= Nextmoveallowed) ->
                    case makemoveon(current_board,Agentid,Timenow,Movetuple) of
                         {Status,Waitnticks,MoreScore} ->
                             inc_agents_score(Agentid,MoreScore),
                             ets:insert(agents_active_in_the_latest_cycle,{now(),Agentid}), %% Move?
                             set_a_reply_to_agent(agent_requests_and_replies,Agentid,WhoFromWhere,Timenow,Timenow+1+Waitnticks,Status)
                    end,
                    io:format("Time: ~w Agent ~w made a move ~w, whose status=~w, Have to wait ~w ticks, Score+ = ~w~n",
                               [Timenow,Agentid,Movetuple,Status,Waitnticks,MoreScore])
               end;

             [] ->
                io:format("Agent ~w trying a move at time ~w, without first logging in. Move ignored: ~w~n",
                           [Agentid,Timenow,Movetuple]),
                set_a_reply_to_agent(agent_requests_and_replies,Agentid,WhoFromWhere,Timenow,Timenow+1,please_login_first);
             KetuixMeni ->
                io:format("ets_lookup(agent_requests_and_replies,~w) mätsäsi huonosti: ~w.~n",
                            [Agentid,KetuixMeni])
        end,
        moveserver:moveserverloop(Timenow,NewNodes2agents,Lasttime_with_itemspresent);

    JokuMuuViesti ->
      io:format("moveserverloop: outo viesti: ~w, skipataan.~n",[JokuMuuViesti]),
      moveserver:moveserverloop(Timenow,Nodes2agents,Lasttime_with_itemspresent)
  end.

find_agentspos_on_board(Boardtabref,Agentid_searched) ->
   case
     ets:select(Boardtabref,
                  ets:fun2ms(fun({Position,{agent,Agentid,Agentoptions}}) when Agentid == Agentid_searched -> Position end)
               )
     of
      [] -> [];
      FoundSomething -> hd(FoundSomething)
   end.

%% Returns either an empty list [] or a list of stuff in that position.
%% E.g. [{{2,3},agent,33,[1234]}]
find_what_is_in_pos_of_board(Boardtabref,Position) ->
  {PosX,PosY} = Position,
  if ((PosX < ?BOARDMINX) orelse
      (PosY < ?BOARDMINY) orelse 
      (PosX > ?BOARDMAXX) orelse 
      (PosY > ?BOARDMAXY))
          -> {blockedby,wall};
     true ->
       case ets:lookup(Boardtabref,Position) of
%%       [{Position,{What,Id,Options}}] -> {What,Id,Options};
         [{Position,Contents}] -> Contents;
         Somethingelse -> Somethingelse %% Should be []
       end
  end.

whole_board_contents(Boardtabref) ->
  [{{PosX,PosY},find_what_is_in_pos_of_board(Boardtabref,{PosX,PosY})}
    || PosX <- seq(?BOARDMINX,?BOARDMAXX), PosY <- seq(?BOARDMINY,?BOARDMAXY)].

nonempty_board_contents(Boardtabref) -> ets:tab2list(Boardtabref).


find_what_is_in_neighbouring_position(Boardtabref,Agentpos,Direction) ->
  Newpos = moveserver:computenewpos(Agentpos,Direction),
  {Newpos, moveserver:find_what_is_in_pos_of_board(Boardtabref,Newpos)}.



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


computenewpos_and_check_its_vacancy(Boardtabref,Agentpos,Direction) ->
  {Newpos, Item} = find_what_is_in_neighbouring_position(Boardtabref,Agentpos,Direction),
  case Item of
       {blockedby,_} -> Item;
       {agent,Otheragent,_} -> {blockedby,{agent,Otheragent,[]}};
       _ -> {Newpos,Item}
  end.


%%
%% Position is currently a pair of absolute coordinates {X,Y}
%% which is followed either by a tuple
%%         {agent,Agentid,[Time_agent_last_moved,PossibleOtherAgentoptions]},
%% or by:  {passive,Itemtype,[Itemoptions]}
%% where Itemtype can be currently food or block.
%%
%% If there's a block on dest.square, it takes four wait cycles more.
%%

%% Returns {StatusMessage,MandatoryWait_of_Ncycles_before_next_move}
%% If MandatoryWait_of_Ncycles_before_next_move is zero, the agent client
%% can immediately make the next move when it has received a reply from
%% this one.

makemoveon(Boardtabref,Agentid,Timenow,Movetuple) ->
  case Movetuple of
       {waitandsee,Nticks} ->
         {waiting,Nticks,0};

       {Kindofmove,Direction} ->
         Agentoldpos = find_agentspos_on_board(Boardtabref,Agentid),
         Newpos_and_Item = computenewpos_and_check_its_vacancy(Boardtabref,Agentoldpos,Direction),
         make_a_moving_move(Boardtabref,Agentid,Timenow,Movetuple,Agentoldpos,Kindofmove,Newpos_and_Item);

       SomeOtherMove ->
         io:format("Time=~w, Agent ~w tried to made an unsupported move ~w, ignored.",
              [Timenow,Agentid,Movetuple]),
         {{unsupported_move,SomeOtherMove},0,0}
  end.

make_a_moving_move(Boardtabref,Agentid,Timenow,Movetuple,Agentoldpos,Kindofmove,Newpos_and_Item) ->
  case Newpos_and_Item of
       {blockedby,{agent,_,_}} -> {Newpos_and_Item,0,0}; %% No extra delays even for agents.
%%     {blockedby,{agent,_,_}} -> {Newpos_and_Item,random:uniform(3),0}; %% Use a random delay to avoid deadlocks of stubborn agents.
       {blockedby,_What}  -> {Newpos_and_Item,0,0}; %%
       {Newpos,Olditem} ->
             case Kindofmove of
                  move ->
                     ets:insert(Boardtabref,{Newpos,{agent,Agentid,[Timenow]}}),
                     ets:delete(Boardtabref,Agentoldpos),
                     Waitncycles = 0;
                  layblockandmove ->
%%                   ets:delete(Boardtabref,Agentoldpos),
                     ets:insert(Boardtabref,{Agentoldpos,{passive,block,[]}}),
                     ets:insert(Boardtabref,{Newpos,{agent,Agentid,[Timenow]}}),
                     Waitncycles = 1;
                  _SomeOtherMove2 ->
                     io:format("Time=~w, Agent ~w tried to made an unsupported move ~w, ignored.",
                                  [Timenow,Agentid,Movetuple]),
                     Waitncycles = 0
             end,
             case Olditem of
                  {passive,food,[{quantity,Q}|_REST_IGNORED]} ->
                     MoreScore = Q,
                     Waitncyclesmore = 0;
                  {passive,block,_} ->
%%                   ets:delete(Boardtabref,Newpos), % Not needed, and actually harmful!
                     MoreScore = 0,
                     Waitncyclesmore = 4;
                  _ ->
                     MoreScore = 0,
                     Waitncyclesmore = 0
             end,
             {moved,Waitncycles+Waitncyclesmore,MoreScore}
  end.


old_choose_starting_position_for_agent(Boardtabref,Agentid,Timenow) ->
  Newpos = {Agentid,Agentid}, %% XXX - For the moment! Not fair at all.
  ets:insert(Boardtabref,{Newpos,{agent,Agentid,[Timenow]}}),
  Newpos.


choose_starting_position_for_agent(Boardtabref,Agentid,Timenow) ->
  case lists:keyfind([],2,whole_board_contents(Boardtabref)) of
    {Freeposition,_NIL} ->
      ets:insert(Boardtabref,{Freeposition,{agent,Agentid,[Timenow]}}),
      Freeposition;
    false ->
      false
  end.


%% empty_contents({Pos,_Cont1,_Cont2,_Cont3}) -> {Pos,[]}.
empty_contents(Tuple) -> {element(1,Tuple),[]}. %% Rumempi, mutta varmempi. (Jos tuplen koko muuttuu.)

diff_sorted_key1lists(Oldlist,Newlist) ->
    diff_sorted_key1lists(Oldlist,Newlist,[]).


diff_sorted_key1lists([],[],Diffs) -> Diffs;

diff_sorted_key1lists([],Newlist,Diffs) ->
    lists:append(Diffs,Newlist);

%% Still stuff in Oldlist that is not in Newlist.
%% We have add those coordinates (with emptied contents) to the result:
diff_sorted_key1lists(Oldlist,[],Diffs) ->
    lists:append(Diffs,lists:map(fun empty_contents/1,Oldlist));

diff_sorted_key1lists([Oldtuple1|Oldrest],Newall=[Newtuple1|_Newrest],Diffs)
  when element(1,Oldtuple1) < element(1,Newtuple1) ->
    diff_sorted_key1lists(Oldrest,Newall,[empty_contents(Oldtuple1)|Diffs]);

diff_sorted_key1lists([Sametuple1|Oldrest],[Sametuple1|Newrest],Diffs) ->
    diff_sorted_key1lists(Oldrest,Newrest,Diffs); %% Skip the exactly same ones!.

diff_sorted_key1lists([Oldtuple1|Oldrest],[Newtuple1|Newrest],Diffs) %% If the same key, but different contents?
  when element(1,Oldtuple1) == element(1,Newtuple1) ->
    diff_sorted_key1lists(Oldrest,Newrest,[Newtuple1|Diffs]); %% Newtuple1 supersedes Oldtuple1 (they have same key).

%% Otherwise, element(1,Oldtuple1) > element(1,Newtuple1)
%% In this case, we wind the newtuples forward (and copying each of 
%% them to Diffs) until we catch with Oldtuples
diff_sorted_key1lists(Oldtuples,[Newtuple1|Newrest],Diffs) ->
    diff_sorted_key1lists(Oldtuples,Newrest,[Newtuple1|Diffs]).

