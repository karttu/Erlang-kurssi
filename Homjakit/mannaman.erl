
%% mannaman.erl -- Ruoanjakelualiohjelma Erlang-moniagenttipeliä varten.
%% Copyright (C) - Antti Karttunen, 2013.

%%
%% Module mannaman implements
%%  addnewfood(Boardtabref,Timenow,Lasttime_with_itemspresent)
%% which in turn uses
%%  find_positions_for_n_items(Boardtabref,N)
%% that returns coordinates for N free places in Boardtabref, good for
%% dropping food or other items.
%%


-module(mannaman).

-include("homjakit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-import(lists,[foreach/2,map/2,nth/2,seq/2,filter/2]).
-export([addnewfood/3,find_positions_for_n_items/2]).
-import(characters,[foodchar/0]).
-compile(export_all).

treissaa(Funname) ->
  dbg:tracer(),
  dbg:p(all, [c]), % Too much info:  dbg:p(all, [c,call,return_to]),
  dbg:tpl({mannaman,Funname,'_'},dbg:fun2ms(fun(_) -> return_trace() end)).



addnewfood(Boardtabref,Timenow,Lasttime_with_itemspresent) ->
  OldFood = find_food_on_board(Boardtabref),
  if length(OldFood) >= 3 -> Timenow;
     Timenow < Lasttime_with_itemspresent+?MANNAMAN_DELIVERY_PAUSE ->
         Lasttime_with_itemspresent;
     true -> %% Otherwise add some new food.
         lists:foreach(fun(Pos) ->
                         ets:insert(Boardtabref,{Pos,{passive,food,[{quantity,1},{created,Timenow}]}})
                       end,
                       find_positions_for_n_items(Boardtabref,?MANNAMAN_DELIVERY_SIZE)
                      ),
         Timenow
  end.


find_items_of_type_on_board(Boardtabref,Itemtype_searched) ->
  ets:select(Boardtabref,
             ets:fun2ms(fun({Position,{Itemtype,Itemsubtype,Itemoptions}}) when Itemtype == Itemtype_searched -> Position end)
            ).

find_passive_items_of_subtype_on_board(Boardtabref,Itemsubtype_searched) ->
  ets:select(Boardtabref,
             ets:fun2ms(fun({Position,{passive,Itemsubtype,Itemoptions}}) when Itemsubtype == Itemsubtype_searched -> Position end)
            ).

%% Doesn't work this way, because ets:select doesn't find empty squares.
ei_toimi_find_empty_squares_on_board_in_limited_region(Boardtabref,{MinX,MinY},{MaxX,MaxY}) ->
  ets:select(Boardtabref,
             ets:fun2ms(fun({{PosX,PosY},[]}) when PosX >= MinX, PosX =< MaxX, PosY >= MinY, PosY =< MaxY
                              -> {PosX,PosY} end)
            ).

find_all_squares_on_board_in_limited_region(Boardtabref,{MinX,MinY},{MaxX,MaxY}) ->
  [ets:lookup(Boardtabref,{X,Y}) || X <- seq(MinX,MaxX), Y <- seq(MinY,MaxY)].


find_all_empty_squares_on_board_in_limited_region(Boardtabref,{MinX,MinY},{MaxX,MaxY}) ->
  [{X,Y} || X <- seq(MinX,MaxX), Y <- seq(MinY,MaxY), ets:lookup(Boardtabref,{X,Y}) == []].


find_agents_on_board(Boardtabref) -> find_items_of_type_on_board(Boardtabref,agent).

find_food_on_board(Boardtabref) -> find_passive_items_of_subtype_on_board(Boardtabref,food).


%% Simple Euclidian geometry:
sum_of_distances_to_agents({PosX,PosY},Agentpositions) ->
  lists:sum(lists:map(fun({AposX,AposY}) -> math:sqrt(((PosX-AposX)*(PosX-AposX))+((PosY-AposY)*(PosY-AposY))) end,
                      Agentpositions
                     )
           ).


min_distance_to_any_agent({PosX,PosY},[]) -> %% Special case when delivering manna before anybody has logged on.
  (PosX/PosY);

min_distance_to_any_agent({PosX,PosY},Agentpositions) ->
  lists:min(lists:map(fun({AposX,AposY}) -> (((PosX-AposX)*(PosX-AposX))+((PosY-AposY)*(PosY-AposY))) end,
                      Agentpositions
                     )
           ).

sum_of_min_distances_to_any_agent(Subsquare,Agentpositions) ->
  lists:sum(lists:map(fun(Pos) -> min_distance_to_any_agent(Pos,Agentpositions) end,Subsquare)).


sort_by_most_lone_in_average_positions(Boardtabref) ->
  Agentpositions = find_agents_on_board(Boardtabref),
  Oursortfun = fun(P1,P2) -> (sum_of_distances_to_agents(P1,Agentpositions)
                               >= sum_of_distances_to_agents(P2,Agentpositions)
                             ) end,
  lists:sort(Oursortfun,squares_of_board(?BOARDMAXX,?BOARDMAXY)).


sort_by_most_lonely_positions(Boardtabref) ->
  Agentpositions = find_agents_on_board(Boardtabref),
  Oursortfun = fun(P1,P2) -> (min_distance_to_any_agent(P1,Agentpositions)
                               >= min_distance_to_any_agent(P2,Agentpositions)
                             ) end,
  lists:sort(Oursortfun,squares_of_board(?BOARDMAXX,?BOARDMAXY)).


subsquares_sorted_by_loneliness(Boardtabref) ->
  Agentpositions = find_agents_on_board(Boardtabref),
  Oursortfun = fun(SS1,SS2) -> (sum_of_min_distances_to_any_agent(SS1,Agentpositions)
                                  >= sum_of_min_distances_to_any_agent(SS2,Agentpositions)
                               ) end,
  lists:sort(Oursortfun,subsquares(?MANNAMAN_SUBSQUARE_SIZE,?BOARDMAXX,?BOARDMAXY)).


find_positions_for_atleast_n_items(Boardtabref,N) ->
  find_positions_for_atleast_n_items(Boardtabref,N,subsquares_sorted_by_loneliness(Boardtabref)).

find_positions_for_atleast_n_items(_Boardtabref,_N,[]) -> [];

find_positions_for_atleast_n_items(Boardtabref,N,[SS|SS_list]) ->
  Empty_squares = find_all_empty_squares_on_board_in_limited_region(Boardtabref,hd(SS),lists:last(SS)),
  if length(Empty_squares) >= N -> Empty_squares;
     true -> find_positions_for_atleast_n_items(Boardtabref,N,SS_list)
  end.


find_positions_for_n_items(Boardtabref,N) ->
    AtLeastN = find_positions_for_atleast_n_items(Boardtabref,N),
    if length(AtLeastN) < N -> AtLeastN; %% We have to use what we have...
       true -> %% Otherwise, take the N last ones from the squares we found, after they are randomly sorted:
          lists:nthtail(length(AtLeastN)-N,lists:sort(fun(_EV,_VK) -> random:uniform(2) == 1 end,AtLeastN))
    end.

subsquares(SquareSize, MaxX, MaxY) ->
    [subsquare_of_coordinates(SquareSize,Xoff,Yoff)
        || Xoff <- lists:seq(1,(MaxX-SquareSize)+1), Yoff <- lists:seq(1,(MaxY-SquareSize)+1)
    ].

subsquare_of_coordinates(SquareSize, X_offset, Y_offset) ->
    [{X+X_offset,Y+Y_offset} || X <- lists:seq(0,SquareSize-1), Y <- lists:seq(0,SquareSize-1)].


squares_of_board(MaxX,MaxY) -> [{X,Y} || X <- lists:seq(1,MaxX), Y <- lists:seq(1,MaxY)].
