

-module(pl).
-compile(export_all).

planeetat() -> [merkurius,venus,maa,mars,jupiter,saturnus,uranus,neptunus].

keskietaisyys(merkurius) -> 0.387;
keskietaisyys(venus) -> 0.723;
keskietaisyys(maa) -> 1.0;
keskietaisyys(mars) -> 1.524;
keskietaisyys(jupiter) -> 5.204;
keskietaisyys(saturnus) -> 9.582;
keskietaisyys(uranus) -> 19.229;
keskietaisyys(neptunus) -> 30.104.

%% Planeetan Plan kiertoaika suunnilleen maan vuosissa. (Keplerin kolmas laki!)
kiertoaika(Plan) ->
  math:sqrt(math:pow(keskietaisyys(Plan),3)).

%% Kokeile (list comprehension):
%% [{P,kiertoaika,pl:kiertoaika(P)} || P <- pl:planeetat()].

%% Kuinka usein planeetat Plan1 ja Plan2 ovat täsmälleen samassa
%% suunnassa auringosta katsoen?
synodinen_kiertoaika(Plan1,Plan2) ->
  abs((kiertoaika(Plan1)*kiertoaika(Plan2))/(kiertoaika(Plan1)-kiertoaika(Plan2))).

%% Kokeile (monimutkaisempi list comprehension):
%% [{P1,P2,synodinen_kiertoaika,pl:synodinen_kiertoaika(P1,P2)} || P1 <- pl:planeetat(), P2 <- pl:planeetat(), pl:keskietaisyys(P2) > pl:keskietaisyys(P1)].

%% Tällä saa selville, minkä planeetta-parien asennot toistuvat kaikkein harvimmin:
%% lists:sort(fun pl:sorttaa_syn/2,[{P1,P2,synodinen_kiertoaika,pl:synodinen_kiertoaika(P1,P2)} || P1 <- pl:planeetat(), P2 <- pl:planeetat(), pl:keskietaisyys(P2) > pl:keskietaisyys(P1)]).

sorttaa_syn({_,_,_,Syn1},{_,_,_,Syn2}) -> Syn2 =< Syn1.

%% Jätä tehtäviksi:
maksimietaisyys(Sama,Sama) -> 0;

maksimietaisyys(Plan1,Plan2) ->
  keskietaisyys(Plan1)+keskietaisyys(Plan2).

minimietaisyys(Plan1,Plan2) ->
  abs(keskietaisyys(Plan1)-keskietaisyys(Plan2)).

%% Huomaa: 1 AU = 8.317 valominuuttia. Kerromme tulon vielä kahdella,
%% koska haluamme ajan mitä kuluu signaalilla kulkea edestakaisin
%% planeettojen Plan1 ja Plan2 välillä:
maxroundtrip(Plan1,Plan2) ->
  maksimietaisyys(Plan1,Plan2)*8.317*2.


