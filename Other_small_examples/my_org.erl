
-module(my).

-include_lib("stdlib/include/ms_transform.hrl").
-import(lists,[append/2]).
-import(erlang,[return_trace/0]).
-export([neliojuuria/1,neliojuuria/2]).
-compile(export_all).


treissaa() ->
  dbg:tracer(),
  dbg:p(all, [c]), % Too much info:  dbg:p(all, [c,call,return_to]),
  dbg:tpl({mylists,'_','_'},dbg:fun2ms(fun(_) -> return_trace() end)).


%% Kokeile esimerkiksi my:neliojuuria(121).

neliojuuria(X) ->
  neliojuuria(X,0).

neliojuuria(X,CNT) when X < 2 ->
  io:format("No nyt loppui juurten otto, X=~w (pienempi kuin 2), Ehdittiin ottaa neliöjuuri ~w kertaa.~n",[X,CNT]),
  X;

neliojuuria(X,CNT) ->
  io:format("neliojuuria, X=~w, CNT=~w~n",[X,CNT]),
  neliojuuria(math:sqrt(X),CNT+1).


%%%%%%%%%%%%%%%%%%%

%% Listafunktiot alkaa:
%%

myappend([],Ys) -> Ys;
myappend([X|Xs],Ys) -> [X|myappend(Xs,Ys)].

mymember(_,[]) -> false;
mymember(Etsitty,Lista=[Etsitty|_Loput]) -> Lista;
mymember(Etsitty,[_Jokuvaan|Loput]) -> mymember(Etsitty,Loput).

%%%%%%%%%%%%%%%%%%%

myrev([]) -> [];
myrev([X|Xs]) -> myrev(Xs) ++ [X].

%%%%%%%%%%%%%%%%%%%



myrev_acc(X) -> myrev_acc(X,[]).

myrev_acc([],A) -> A;
myrev_acc([X|Xs],A) -> myrev_acc(Xs,[X|A]).


%%%%%%%%%%%%%%%%%%%
%%
%% Tehtävä: kirjoita häntärekursiivinen, akkumulaattoriversio append:istä,
%% nimeltään myappend_acc.
%%
%%%%%%%%%%%%%%%%%%%

sumlist([]) -> 0;
sumlist([First|Rest]) -> First + sumlist(Rest).


maxlist([]) -> 0;
maxlist([First|Rest]) -> max(First,maxlist(Rest)).

%% Tehtävä: kirjoita häntärekursiivinen versio ylläolevasta funktiosta sumlist.

maxlist_acc(L) -> maxlist_acc(L,0).

maxlist_acc([],A) -> A;
maxlist_acc([First|Rest],A) -> maxlist_acc(Rest,max(First,A)).


%% Tehtävä: kirjoita funktio iota(N) jolle annetaan kokonaisluku,
%% ja se tuottaa tulokseksi listan [1,2,3,...,N].

iota(0) -> [];
iota(N) -> iota(N-1) ++ [N].

%% Tehtävä: Tee sama häntärekursiivisesti.

%%%%%%%%%%%%%%%%%%%


mystery([]) -> [];

mystery([A]) -> [A];

mystery([X|Y]) ->
   [A|B] = mystery(Y),
   [A|mystery([X|mystery(B)])].


%%%%%%%%%%%%%%%%%%%


deeprev([]) -> []; %% An empty list deep-reversed is an empty list.

deeprev(A) when is_atom(A) -> A; %% Any nonlist item stays same also.

deeprev([A]) -> [deeprev(A)]; %% Deepreversing a list of one element creates a list of that element deepreversed.

deeprev([X|Y]) ->
   append(deeprev(Y),[deeprev(X)]).

