
-module(kukkuu).
-export([k�ki/1,alp/1,alp2/1,rotate/1,ppp/0]).
-compile(export_all).

init_cuckoo(Paussi) ->
  spawn(kukkuu,k�ki,[Paussi]).

k�ki(Paussi) ->
  io:format("Kukkuu!~n"),
  receive
     kukuhitaammin -> k�ki(2*Paussi);
     kukunopeammin -> k�ki(Paussi div 2);
     lopetajo -> ok
     after Paussi -> k�ki(Paussi) %% Paussin j�lkeen kuku uudestaan.
  end.



init_cuckoo2(Paussi) ->
  spawn(kukkuu,k�ki2,[Paussi]).

k�ki2(Paussi) ->
%%  process_flag(trap_exit,true),
  try k�ki2luuppi(Paussi)
  catch
    Millainen:Moka ->
      io:format("K�ki2 sai jonkun fitin: ~w:~w. Jatketaan...~n",
                [Millainen,Moka]),
      k�ki2(Paussi)
  end.


k�ki2luuppi(Paussi) ->
  io:format("K�ki2 kukkuu, Paussi=~w!~n",[Paussi]),
  receive
     kukuhitaammin -> k�ki2luuppi(2*Paussi);
     kukunopeammin -> k�ki2luuppi(Paussi div 2);
     kukuabsurdimmin -> k�ki2luuppi(varttia_vaille_vetopasuuna);
     lopeta -> exit(normal);
     lopetajo -> throw(enk�_lopeta);
     Muuta ->
       io:format("k�ki2luuppi: sain viestin ~w, jatketaan.~n",[Muuta]),
       k�ki2luuppi(Paussi)
     after Paussi -> k�ki2luuppi(Paussi) %% Paussin j�lkeen kuku uudestaan.
  end.



alp(X) -> %% Apply, Loop and Print.
  io:format("X=~w~n",[X]),
  receive
    Fun when is_function(Fun) -> alp(Fun(X));
    Jotainmuuta ->
      io:format("alp: Don't know what to do with ~w!~n",[Jotainmuuta]),
      alp(X)
  end.


%% Porotus = fun(Z,Fun) -> self() ! Fun, Z+2 end.
%% ALP2 = spawn(kukkuu,alp2,[1]).
%% ALP2 ! Porotus.

alp2(X) -> %% Apply, Loop and Print, Version 2.
  io:format("X=~w~n",[X]),
  receive
    Fun when is_function(Fun) -> alp2(Fun(X,Fun));
    Jotainmuuta ->
      io:format("alp: Don't know what to do with ~w!~n",[Jotainmuuta]),
      alp2(X)
  end.

rotate(L) -> (tl(L) ++ [hd(L)]).

%%
%% Mahiainen = fun(Fun,Pids) -> io:format("Mahiainen nyt solmussa ~w, Pids=~w~n",[node(),Pids]), hd(Pids) ! {Fun,kukkuu:rotate(Pids)} end.
%%

ppp() -> %% Piiri Pieni Py�rii.
  receive
    {Fun,Pids} when is_function(Fun), is_list(Pids) -> Fun(Fun,Pids), ppp();
    Jotainmuuta ->
      io:format("ppp: at node ~w: Don't know what to do with ~w!~n",[node(),Jotainmuuta]),
      ppp()
  end.
