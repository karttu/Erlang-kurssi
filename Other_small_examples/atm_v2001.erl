
%% Automatic Teller Machine. Versio vuoteen 2001 asti, saldo markoissa.
-module(atm).
-export([init_atmserver/1, atmserver_loop/1, upgrade/1]).

init_atmserver(Alkusaldo) ->
  register(atmserver,spawn(?MODULE,atmserver_loop,[Alkusaldo])).


atmserver_loop(Saldo) ->
  {Entuudestaan,Valuuttaa} = Saldo,
  receive
    update ->
      NewSaldo = ?MODULE:upgrade(Saldo),
      ?MODULE:atmserver_loop(NewSaldo);  %% loop in the new version of the module

    {Kuka,pano,N,Valuuttaa} ->
      io:format("atmserver: tilille lisättiin ~w ~w, saldo nyt ~w.~n",
                  [N,Valuuttaa,N+Entuudestaan]),
      Kuka ! {saldosi_nyt,N+Entuudestaan,Valuuttaa},
      atmserver_loop({N+Entuudestaan,Valuuttaa}); %% Saldo kasvaa.

    {Kuka,pano,N,JotainMuutaValuuttaa} ->
      io:format("atmserver: tilille yritettiin laittaa ~w ~w, (väärää rahaa), skipataan!~n",
                  [N,JotainMuutaValuuttaa]),
      Kuka ! {tämä_automaatti_hyväksyy_vain,Valuuttaa},
      atmserver_loop(Saldo);

    {Kuka,otto,N} when N > Entuudestaan ->
      io:format("atmserver: tililtä halutaan ottaa ~w rahaa, vaikka saldo vain ~w.~n",
                  [N,Saldo]),
      Kuka ! {sinulla_ei_ole_noin_paljoa_rahaa_maksimi_otto,Entuudestaan},
      atmserver_loop(Saldo); %% Saldo ei muutu tässä tapauksessa.

    {Kuka,otto,N} when N =< Entuudestaan ->
      io:format("atmserver: tililtä halutaan ottaa ~w rahaa, ja saldo riittää: ~w.~n",
                  [N,Saldo]),
      Kuka ! {nostit,N,Valuuttaa},
      atmserver_loop({Entuudestaan-N,Valuuttaa}); %% Saldo pienenee.

    SomeOtherMessage ->
      %% do something here
      io:format("atmserver sai oudon viestin: ~w, skipataan se.~n",[SomeOtherMessage]),
      atmserver_loop(Saldo)  %% stay in the same version no matter what.
  end.
 
upgrade(OldState) -> OldState. %% transform and return the state here.


