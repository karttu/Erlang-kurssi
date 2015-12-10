
-module(kukkuu).
-export([käki/1,alp/1,alp2/1,rotate/1,ppp/0]).
-compile(export_all).

init_cuckoo(Paussi) ->
  spawn(kukkuu,käki,[Paussi]).

käki(Paussi) ->
  io:format("Kukkuu!~n"),
  receive
     kukuhitaammin -> käki(2*Paussi);
     kukunopeammin -> käki(Paussi div 2);
     lopetajo -> ok
     after Paussi -> käki(Paussi) %% Paussin jälkeen kuku uudestaan.
  end.



init_cuckoo2(Paussi) ->
  spawn(kukkuu,käki2,[Paussi]).

käki2(Paussi) ->
%%  process_flag(trap_exit,true),
  try käki2luuppi(Paussi)
  catch
    Millainen:Moka ->
      io:format("Käki2 sai jonkun fitin: ~w:~w. Jatketaan...~n",
                [Millainen,Moka]),
      käki2(Paussi)
  end.


käki2luuppi(Paussi) ->
  io:format("Käki2 kukkuu, Paussi=~w!~n",[Paussi]),
  receive
     kukuhitaammin -> käki2luuppi(2*Paussi);
     kukunopeammin -> käki2luuppi(Paussi div 2);
     kukuabsurdimmin -> käki2luuppi(varttia_vaille_vetopasuuna);
     lopeta -> exit(normal);
     lopetajo -> throw(enkä_lopeta);
     Muuta ->
       io:format("käki2luuppi: sain viestin ~w, jatketaan.~n",[Muuta]),
       käki2luuppi(Paussi)
     after Paussi -> käki2luuppi(Paussi) %% Paussin jälkeen kuku uudestaan.
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

ppp() -> %% Piiri Pieni Pyörii.
  receive
    {Fun,Pids} when is_function(Fun), is_list(Pids) -> Fun(Fun,Pids), ppp();
    Jotainmuuta ->
      io:format("ppp: at node ~w: Don't know what to do with ~w!~n",[node(),Jotainmuuta]),
      ppp()
  end.
