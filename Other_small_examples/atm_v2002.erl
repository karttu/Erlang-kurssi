
%% Automatic Teller Machine.
%% Uusi versio vuodesta 1.2.2002 l�htien, saldo euroissa,
%% upgrade muuttaa vanhat markat euroiksi.
%% Uutuutena saldokysely viesti.

-module(atm).
-export([init_atmserver/1, atmserver_loop/1, upgrade/1]).

-define(SALDOKYSELYNHINTA, 0.01). %% Emme tee mit��n ilmaiseksi!
-define(MARKAT_EUROIKSI_KERTOIMELLA, 1/6). %% Emme todellakaan!

init_atmserver(Alkusaldo) ->
  register(atmserver,spawn(?MODULE,atmserver_loop,[Alkusaldo])).


atmserver_loop(Saldo) ->
  {Entuudestaan,Valuuttaa} = Saldo,
  receive
    update ->
      NewSaldo = ?MODULE:upgrade(Saldo),
      ?MODULE:atmserver_loop(NewSaldo);  %% loop in the new version of the module

    {Kuka,pano,N,Valuuttaa} ->
      io:format("atmserver: tilille lis�ttiin ~w ~w, saldo nyt ~w.~n",
                  [N,Valuuttaa,N+Entuudestaan]),
      Kuka ! {saldosi_nyt,N+Entuudestaan,Valuuttaa},
      atmserver_loop({N+Entuudestaan,Valuuttaa}); %% Saldo kasvaa.

    {Kuka,pano,N,JotainMuutaValuuttaa} ->
      io:format("atmserver: tilille yritettiin laittaa ~w ~w, (v��r�� rahaa), skipataan!~n",
                  [N,JotainMuutaValuuttaa]),
      Kuka ! {t�m�_automaatti_hyv�ksyy_vain,Valuuttaa},
      atmserver_loop(Saldo);

    {Kuka,otto,N} when N > Entuudestaan ->
      io:format("atmserver: tililt� halutaan ottaa ~w rahaa, vaikka saldo vain ~w.~n",
                  [N,Saldo]),
      Kuka ! {sinulla_ei_ole_noin_paljoa_rahaa_maksimi_otto,Entuudestaan},
      atmserver_loop(Saldo); %% Saldo ei muutu t�ss� tapauksessa.

    {Kuka,otto,N} when N =< Entuudestaan ->
      io:format("atmserver: tililt� halutaan ottaa ~w rahaa, ja saldo riitt��: ~w.~n",
                  [N,Saldo]),
      Kuka ! {nostit,N,Valuuttaa},
      atmserver_loop({Entuudestaan-N,Valuuttaa}); %% Saldo pienenee.

    {Kuka,saldokysely} ->
      io:format("atmserver: tililt� kysyt��n saldoa, joka oli t�h�n menness�: ~w.~n",
                  [Saldo]),
      Kuka ! {saldosi_oli,Saldo},
      atmserver_loop({Entuudestaan-?SALDOKYSELYNHINTA,Valuuttaa}); %% Saldokyselyt maksavat, ?SALDOKYSELYNHINTA !

    SomeOtherMessage ->
      %% do something here
      io:format("atmserver sai oudon viestin: ~w, skipataan se.~n",[SomeOtherMessage]),
      atmserver_loop(Saldo)  %% stay in the same version no matter what.
  end.
 
upgrade({N,markkaa}) ->
  {(N*(?MARKAT_EUROIKSI_KERTOIMELLA)),euroa};

upgrade(OldSaldo) -> OldSaldo. %% Muuten oletetaan ett� Saldo jo valmiiksi euroissa.


