
%%
%% Module worldview implements find_positions_for_n_items(Boardtabref,N)
%% that returns coordinates for N free places in Boardtabref, good for
%% dropping food or other items.
%%

%%
%% Changes:
%%
%%   April 16-18 2013: Ditched angles (in most places) for good, replaced with
%%                     a simple calculation derived from vector cross products.
%%
%%

-module(worldview).

-include("homjakit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-import(lists,[foreach/2,map/2,nth/2,seq/2,filter/2]).
-export([world_viewed_from/2]).
-compile(export_all).

treissaa(Funname) ->
  dbg:tracer(),
  dbg:p(all, [c]), % Too much info:  dbg:p(all, [c,call,return_to]),
  dbg:tpl({worldview,Funname,'_'},dbg:fun2ms(fun(_) -> return_trace() end)).




%% find_agentspos_on_board(Boardtabref,Agentid)
all_immediate_neighbours(Boardtabref,Boardpos) ->
  [moveserver:find_what_is_in_neighbouring_position(Boardtabref,Boardpos,Direction) || Direction <- seq(0,7)].

%% Same as above, but filter off empty squares:
nonempty_immediate_neighbours(Boardtabref,Boardpos) ->
  filter(fun({_Pos,Contents}) -> Contents =/= [] end, all_immediate_neighbours(Boardtabref,Boardpos)).

all_immediate_neighbours_with_relative_coordinates(Boardtabref,Boardpos) ->
  {PosX,PosY} = Boardpos,
  [{{Xdelta,Ydelta},moveserver:find_what_is_in_pos_of_board(Boardtabref,{PosX+Xdelta,PosY+Ydelta})}
    || Xdelta <- [-1,0,+1], Ydelta <- [-1,0,+1], (abs(Xdelta)+abs(Ydelta))/=0].

nonempty_immediate_neighbours_with_relative_coordinates(Boardtabref,Boardpos) ->
  filter(fun({_Pos,Contents}) -> Contents =/= [] end,
         all_immediate_neighbours_with_relative_coordinates(Boardtabref,Boardpos)
        ).


world_viewed_from_short_sighted(Boardtabref,Boardpos) -> %% XXX -- Quick fix!
  nonempty_immediate_neighbours_with_relative_coordinates(Boardtabref,Boardpos).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sgn(X) when X < 0 -> -1;
sgn(X) when X > 0 -> +1;
sgn(0.0) -> 0;
sgn(0) -> 0.

%% E.g. sign_of_angle_between({2,3},{3,2}) = sgn(3*3 - 2*2) = sgn(5) = 1.
%% sign_of_angle_between({Ax,Ay},{Bx,By}) -> sgn((Bx*Ay) - (Ax*By)).
sign_of_angle_between({Ax,Ay},{Bx,By}) -> sgn((Ax*By) - (Bx*Ay)).

nonnegativeangle_between({Ax,Ay},{Bx,By}) ->
    sign_of_angle_between({Ax,Ay},{Bx,By}) >= 0.

negative_angle_between({Ax,Ay},{Bx,By}) ->
    sign_of_angle_between({Ax,Ay},{Bx,By}) < 0.

acute_angle_between(A,B) -> nonnegativeangle_between(A,B).
obtuse_angle_between(A,B) -> negative_angle_between(A,B).

sector_ab_shadows_point_c({A,B},C) ->
    S = sign_of_angle_between(A,B),
    if (S < 0) -> obtuse_sector_ab_shadows_point_c({A,B},C);
       true -> acute_sector_ab_shadows_point_c({A,B},C)
    end.


acute_sector_ab_shadows_point_c({A,B},C) ->
    (nonnegativeangle_between(A,C) andalso nonnegativeangle_between(C,B)).

%% That is, case (negativeangle_between(A,C) or negativeangle_between(C,B)).
%% is forbidden, when A,B is acute.

obtuse_sector_ab_shadows_point_c({A,B},C) ->
    (nonnegativeangle_between(A,C) orelse nonnegativeangle_between(C,B)).

%% That is, only case (negativeangle_between(A,C) and negativeangle_between(C,B)).
%% is forbidden, when A,B is obtuse.


sector_ab_shadows_corners_of_square_c(AB,C) ->
    lists:all(fun(Corner) -> sector_ab_shadows_point_c(AB,Corner) end,square_corners(C)).

sector_ab_shadows_square_c_completely(Sec,Square) ->
    sector_ab_shadows_point_c(Sec,Square) andalso sector_ab_shadows_corners_of_square_c(Sec,Square).

sector_ab_shadows_sector_cd({A,B},{C,D}) ->
    sector_ab_shadows_point_c({A,B},C) andalso sector_ab_shadows_point_c({A,B},D).


%% I.e. AB is "before" CD, and they partially overlap:
sector_ab_overlaps_with_sector_cd({A,B},{C,D}) ->
            sector_ab_shadows_point_c({A,B},C)   %% C is between A and B,
    andalso sector_ab_shadows_point_c({C,D},B).  %% and B is between C and D.




%% Returns a list of four corners of a square of whose center point is given:
square_corners({PosX,PosY}) -> [{PosX+Xdelta,PosY+Ydelta} || Xdelta <- [-1/2,+1/2], Ydelta <- [-1/2,+1/2]].

%% Choose the pair (A,B) of points from (4 2) = 6 possible pairings of square's four corners,
%% for which sign_of_angle_between(EveryPoint,A) >= 0 and sign_of_angle_between(B,EveryPoint) >= 0
%% It's quadratic, but hey, the list of Points is only four elements long.
%% Of course we could use lists:sort as well, with sign_of_angle_between as our sorting function...

maxangle(Points) ->
  hd(lists:filter(fun(A) ->
                            lists:all(fun(AnyCorner) ->
                                           (AnyCorner == A) orelse sign_of_angle_between(AnyCorner,A) > 0
                                      end,
                                      Points)
                  end,
                  Points)).

minangle(Points) ->
  hd(lists:filter(fun(A) ->
                            lists:all(fun(AnyCorner) ->
                                           (AnyCorner == A) orelse sign_of_angle_between(A,AnyCorner) > 0
                                      end,
                                      Points)
                  end,
                  Points)).


max_sector_for(Square) ->
  Corners = square_corners(Square),
  {minangle(Corners),maxangle(Corners)}.

%% Ei tarvita: Sixpointpairs = [{X,Y} || X <- Corners, Y <- Corners, X < Y], %% (4 2) = 6.


square_wholly_shadowed_by_sectors(Square,Sectors) ->
  lists:any(fun(Sec) -> sector_ab_shadows_square_c_completely(Sec,Square) end,Sectors).

square_in_the_shadow_of_sectors(Square,Sectors) ->
  lists:filter(fun(Sec) -> sector_ab_shadows_square_c_completely(Sec,Square) end,Sectors).

obtuse_angle_among_sectors(Sectors) ->
  lists:any(fun({SecA,SecB}) -> obtuse_angle_between(SecA,SecB) end,Sectors).



angle_between_points({Ax,Ay},{Bx,By}) -> math:atan2((By-Ay),(Bx-Ax)). %% Gives an angle between ]-pi,pi]

sort_shadows_by_their_opening_angle(Shadows) ->
  lists:sort(fun({Shadow1A,_Shadow1B},{Shadow2A,_Shadow2B}) ->
                   angle_between_points({0,0},Shadow1A) =< angle_between_points({0,0},Shadow2A)
             end,
             Shadows).

everything_else_except_first_and_last(L) -> lists:reverse(tl(lists:reverse(tl(L)))).

merge_to_shadows(Square,Shadows) ->
  NewShadows = sort_shadows_by_their_opening_angle(merge_sorted_shadows(max_sector_for(Square),Shadows)),
%% Post-processing, check whether we can merge also the first and last sector together:
  if ([] == tl(NewShadows)) -> NewShadows; %% Just one sector left?
     true ->
        Eka = hd(NewShadows),
        Vika = lists:last(NewShadows),
        case sector_ab_overlaps_with_sector_cd(Vika,Eka) of
             false -> NewShadows;
             true ->
               {_EkaA,EkaB} = Eka,
               {VikaA,_VikaB} = Vika,
               sort_shadows_by_their_opening_angle([{VikaA,EkaB}|everything_else_except_first_and_last(NewShadows)])
          end
  end.

%%
%% We have come >= 360 degrees when finally we have just one zero or acute sector left,
%% and in the previous iteration, there was at least one obtuse sector present.
%%


%% Four cases:
%%   SqSector overlaps with Shadow1, but starting before it. Merge them together and return.
%%   SqSector overlaps with Shadow1, but starting after it, Merge them together, and also the rest if needed.
%%   (The case where SqSector would have been shadowed by Shadow1 is not possible, because it has
%%    already been checked.)
%%   In case SqSector doesn't overlap with any existing shadow, we add it to the list of shadows,
%%   (and the list is sorted on the next iteration).

merge_sorted_shadows(SqSector,[]) -> [SqSector]; %% Didn't overlap with any existing shadow?

merge_sorted_shadows(SqSector,[Shadow1|Rest]) ->
    {SqSectorA,SqSectorB} = SqSector,
    {Shadow1A,Shadow1B} = Shadow1,

    case sector_ab_overlaps_with_sector_cd(SqSector,Shadow1) of
         true -> [{SqSectorA,Shadow1B}|Rest];

         false ->
               case sector_ab_overlaps_with_sector_cd(Shadow1,SqSector) of
                    true -> merge_rest_of_shadows([{Shadow1A,SqSectorB}|Rest]); %% Then merge them.

                    false -> [Shadow1|merge_sorted_shadows(SqSector,Rest)]
         end
    end.



merge_rest_of_shadows([Shadow]) -> [Shadow]; %% Only one left.

merge_rest_of_shadows(All=[{Shadow1A,Shadow1B},{Shadow2A,Shadow2B}|Rest]) ->
    case sector_ab_overlaps_with_sector_cd({Shadow1A,Shadow1B},{Shadow2A,Shadow2B}) of
         true -> merge_rest_of_shadows([{Shadow1A,Shadow2B}|Rest]); %% Then merge them.
         false -> [hd(All)|merge_rest_of_shadows(tl(All))]
    end.



%% We don't show other walls than what are immediately next to Boardpos:
world_viewed_from(Boardtabref,Boardpos) ->
  filter(fun({{RelX,RelY},Item}) -> (Item /= {blockedby,wall}) orelse (((RelX*RelX)+(RelY*RelY)) =< 2) end,
         collect_visitems(Boardtabref,Boardpos)
        ).


collect_visitems(Boardtabref,Boardpos) ->
  collect_visitems(Boardtabref,Boardpos,nextpoint_in_spiral({0,0}),[],[],false,false).


%% We are ready, when we have switched from having an obtuse sector present, to not having it present.
collect_visitems(_,_,_,Visitems,_,false,true) -> Visitems;


collect_visitems(Boardtabref,Pos,Relpoint,Visitems,Sectors,OSNow,OSPresentLastTime) ->
%%io:format("Relpoint=~w, Sectors=~w~n~n",[Relpoint,Sectors]),
  {PosX,PosY} = Pos,
  {Xdelta,Ydelta} = Relpoint,
  AbsPoint = {PosX+Xdelta,PosY+Ydelta},
  case moveserver:find_what_is_in_pos_of_board(Boardtabref,AbsPoint) of
%% If nothing in that place, then proceed to the next point spiral-wise:
    [] -> collect_visitems(Boardtabref,Pos,nextpoint_in_spiral(Relpoint),Visitems,Sectors,OSNow,OSPresentLastTime);

    SomeItem ->
      case square_in_the_shadow_of_sectors(Relpoint,Sectors) of
           [] ->
             io:format("NOT shaded: Abs=~w (Relpoint=~w) contains ~w, Squares own shading sector=~w.~nSectors=~w~n~n",
                                [AbsPoint,Relpoint,SomeItem,max_sector_for(Relpoint),Sectors]),

             NewShadows = merge_to_shadows(Relpoint,Sectors),
             collect_visitems(Boardtabref,Pos,nextpoint_in_spiral(Relpoint),
                              [{Relpoint,SomeItem}|Visitems],
                              NewShadows,
                              obtuse_angle_among_sectors(NewShadows),
                              OSNow
                             );

           ShadowingSectors -> %% Should really be just one...
             io:format("SHADED: Abs=~w (Relpoint=~w) contains ~w, Its shading sector=~w, shadowed by=~w.~nSectors=~w~n~n",
                                [AbsPoint,Relpoint,SomeItem,max_sector_for(Relpoint),ShadowingSectors,Sectors]),
             collect_visitems(Boardtabref,Pos,nextpoint_in_spiral(Relpoint),
                              Visitems,Sectors,
                              OSNow,OSPresentLastTime)

      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



nextpoint_in_spiral({0,0}) -> {+1,0};

nextpoint_in_spiral({RelX,RelY}) when RelY > 0, RelX =< RelY, RelX > -RelY -> {RelX-1,RelY}; %% At Y plus side.

nextpoint_in_spiral({RelX,RelY}) when RelX < 0, RelY =< -RelX, RelY > RelX -> {RelX,RelY-1}; %% At X minus side.

nextpoint_in_spiral({RelX,RelY}) when RelY < 0, RelX >= RelY, RelX =< -RelY -> {RelX+1,RelY}; %% At Y minus side.

nextpoint_in_spiral({RelX,RelY}) when RelX > 0, RelX > abs(RelY) -> {RelX,RelY+1}. %% At X plus side.

%% crosser_square({PrelX,PrelY}) -> (0 == PrelY andalso PrelX < 0). %% Not needed anymore.






