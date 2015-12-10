
-module(lukuesim).
-compile(export_all).

%%
%% Ei-h‰nt‰rekursiivinen toteutus:
%%

digitsum(0) -> 0;

digitsum(N) ->
  (N rem 10) + digitsum(N div 10).

%%
%% "Digital root": sovella digitsum:ia niin kauan, kunnes tulos
%% on alle kymmenen.
%% H‰nt‰rekursiivinen toteutus. Mist‰ tied‰mme ett‰ rekursio p‰‰ttyy?
%%

digitalroot(N) when N < 10 -> N;

digitalroot(N) ->
  digitalroot(digitsum(N)).



kolmioluvut(0) -> 0;
kolmioluvut(N) ->
  N + kolmioluvut(N-1).

kolmioluvut_tail(N) ->
  kolmioluvut_tail(N,0).

kolmioluvut_tail(0,Z) -> Z;
kolmioluvut_tail(N,Z) ->
  kolmioluvut_tail(N-1,N+Z).

kolmioluvut_quick(N) -> ((N*(N+1)) div 2).


gcd(A,0) -> A;
gcd(0,B) -> B;
gcd(A,B) when A > B ->
  gcd(B,A-B);
gcd(A,B) ->
  gcd(A,B-A).


fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) -> fibo(N-1)+fibo(N-2).


start_fiboserver() ->
  register(fiboserver,spawn(lukuesim,fiboserver,[0,1])).

nextfibo() ->
  fiboserver ! {self(),nextfibo},
  receive
    N -> N
    after 30000 ->
      fiboserver_vissiin_vainaa
  end.

fiboserver(N1,N2) ->
  receive
    {Pid, nextfibo} ->
       Pid ! N1,
       fiboserver(N2,N1+N2);
    OtherMessage ->
       io:format("Don't know what to do with message ~w. Please send me {self(),nextfibo}.~n",
                   [OtherMessage]),
       fiboserver(N1,N2) %% Pid‰ tilanne ennallaan
  end.

  
