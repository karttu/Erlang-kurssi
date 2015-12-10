-module(life_wx).

-include("life_wx.hrl").


%% 
%% http://www.iki.fi/kartturi/erlang/life_gs_erl.txt
%%

%%
%% Rewrite to WX GUI library in March 2013, just for testing.
%%

%% life.erl - Yksinkertainen Erlang-toteutus John Conwayn Life-soluautomaatista
%% Copyright (C) 2005 by Antti Karttunen.
%% T‰m‰ versio k‰ytt‰‰ tilanteen tulostukseen Erlangin OTP-paketin
%% gs-grafiikkakirjastoa.
%% 
%% Lis‰tietoja: Antti<piste>Karttunen (at) gmail<piste>com

%% Erlang-k‰‰nt‰j‰ lˆytyy osoitteesta: http://www.erlang.org/

%% A concurrent version of Conway's Life, programmed with Erlang
%% Copyright (C) 2005, by Antti Karttunen, Antti.Karttunen@iki.fi
%% Last edited Dec 12 2005. This version uses the GUI-package gs
%% for output in elementary way.

%%
%% http://www.iki.fi/kartturi/erlang/life_erl.txt
%% on paremmin dokumentoitu versio.
%%


%% Talleta t‰m‰ tiedosto nimell‰ life_gs.erl vaikkapa hakemistoon /my/erlang
%% (tai vastaavaan...), ja Erlang-shellin k‰ynnistetty‰si siirry
%% samaan hakemistoon komennolla cd("/my/erlang").
%% jonka j‰lkeen voit ladata ohjelman Erlang-tulkkiin komennolla c(life_gs).
%% Simulaation voi k‰ynnist‰‰ esimerkiksi komennolla life_gs:start_spaceship().
%% tai life_gs:start_glider().
%% Simulaatio lopetetaan komennolla life_gs:stop().


-export([list_of_cellnames/1,cells_neighbours/2,spawn_cell/3,spawn_all_cells/3,cell/6,stopcell/1,nextgen/1,liferule/2,supervise/3,start/1,stop/0,start_glider/0,start_spaceship/0,canvas_new/2,show_cell/4]).
-import(lists,[append/1,foreach/2,map/2,nth/2,seq/2]).



%% T‰ll‰ luodaan rivill‰ ROW, sarakkeessa COL sijaitsevalle
%% solulle uniikki nimi. Nimi koostuu pienest‰ c-kirjaimesta
%% sek‰ argumentteja ROW ja COL vastaavista numeroista.
%% Esimerkiksi life:make_cellname(3,5) palauttaa solun nimen‰
%% atomin 'c35'.

make_cellname(ROW,COL) ->
  list_to_atom([99,48+ROW,48+COL]).


%% Funktiolla list_of_cellanems tuotetaan lista nimi‰, joita
%% k‰ytet‰‰n soluprosessien rekisterˆimiseen.
%% Esimerkiksi life:list_of_cellnames(3).
%% palauttaa listan: [c00,c01,c02,c10,c11,c12,c20,c21,c22]
%% Huomaa "list comprehension" -rakenteen k‰yttˆ.

list_of_cellnames(Size) ->
  [make_cellname(ROW,COL) || ROW <- seq(0,Size-1), COL <- seq(0,Size-1)].


%% Funktio cells_neighbours palauttaa annetun solun naapurit.
%% Se tarvitsee myˆs solumaailman koon argumenttinaan,
%% jotta se osaisi "wr‰p‰t‰" ruudukon reunojen ymp‰ri.

%% Esimerkiksi life:cells_neighbours(c36,8).
%% palauttaa listan: [c25,c26,c27,c35,c37,c45,c46,c47]
%% ja life:cells_neighbours(c07,8).
%% palauttaa listan: [c76,c77,c70,c06,c00,c16,c17,c10]
%%
%% 'c07' on siis 8x8-ruutuisen maailman oikeassa yl‰kulmassa
%% oleva solu, jolla on naapurinaan esimerkiksi vasemmassa
%% alakulmassa oleva solu c70 ja oikeassa alakulmassa
%% oleva c77.
%%
%% Huomaa "list comprehension"-rakenteessa filtterin
%% (abs(X)+abs(Y))/=0 k‰yttˆ, joka takaa ett‰ X ja Y
%% k‰yv‰t l‰pi kaikki muut -1:n, 0:n ja +1:n yhdistelm‰t
%% paitsi 0,0:n. (Solu ei ole itsens‰ naapuri!)

cells_neighbours(Cellname,Size) ->
  ROW = nth(2,atom_to_list(Cellname))-48,
  COL = nth(3,atom_to_list(Cellname))-48,
  [make_cellname(((Size+ROW+X) rem Size),((Size+COL+Y) rem Size))
    || X <- [-1,0,+1], Y <- [-1,0,+1], (abs(X)+abs(Y))/=0].


%% Funktio liferule kertoo solun oman nykyisen tilan (argumentti C)
%% ja sen elossa olevien naapurien m‰‰r‰n (argumentti NAlive)
%% perusteella, onko solu seuraavassa sukupolvessa
%% elossa (1) vai kuollut (0).
%% Edellinen toteutuu vain silloin kuin solu on joko
%% valmiiksi elossa, ja sill‰ on joko 2 tai 3 elossa
%% olevaa naapuria, tai silloin kuin se on kuollut,
%% mutta sill‰ on tasan kolme elossa olevaa naapuria.
%% T‰m‰ ehto on ytimekk‰int‰ testata 'bor' eli binary-or
%% -operaatiolla:

liferule(C,NAlive) ->
  if (3 == (NAlive bor C)) -> 1; true -> 0 end.



send_state_to_all(_,_,_,[]) ->
  ok;

send_state_to_all(Name,Gen,State,[Procname|TheRest]) ->
  Procname ! {Name,Gen,State},
  send_state_to_all(Name,Gen,State,TheRest).



%% Alkaa sukupolvesta nolla (Gen=0), kun solut silmukoinut
%% prosessi on startannut (funktiolla spawn_cell) kunkin solun
%% alkukuvion sille m‰‰r‰‰m‰ll‰ tilalla (State = 0 tai = 1).

%% Supervisor l‰hett‰‰ "next"-viestin kaikille soluille vasta kun se
%% on saanut nykyisest‰ sukupolvesta kaikkien solujen tilaviestit.

%% T‰t‰ haaraa kutsutaan A) ensimm‰iseksi kun solu on silmukoitu,
%% ja B) aina kun kaikilta kahdeksalta naapurilta on tullut tilatieto
%% ja t‰m‰n solun uusi tila on laskettu.
%% Solun tila v‰litet‰‰n supervisorille, jolta j‰‰d‰‰n sitten
%% odottamaan next-viesti‰/kuittausta, jonka j‰lkeen tila
%% v‰litet‰‰n myˆs kaikille naapureille, ja lopuksi siirryt‰‰n
%% cell-funktion kolmanteen haaraan jossa odotella viestej‰
%% naapureilta.

cell(Name,Gen,State,0,0,NNames) ->
  send_state_to_all(Name,Gen,State,[supervisor]),
%%io:format("In generation ~w cell ~w with neighbours ~w sent state=~w to supervisor, waiting for next.~n",
%%           [Gen,Name,NNames,State]),
  receive
    stop ->
      io:format("Cell ~w with neighbours ~w stopped in generation ~w~n",
                   [Name,NNames,Gen]);
    next ->
      send_state_to_all(Name,Gen,State,NNames),
      cell(Name,Gen,State,0,1,NNames)   %% Siirry kolmanteen haaraan.
  end;


%% Tilatiedot tulleet jo kaikilta kahdeksalta naapurilta?
%% Laske t‰m‰n solun uusi tila, ja kutsu sen j‰lkeen ensimm‰ist‰ haaraa.

cell(Name,Gen,State,8,NAlive_plus_1,NNames) ->
  Newstate = liferule(State,NAlive_plus_1 - 1),
%%io:format("In generation ~w cell ~w with neighbours ~w had an old state=~w. New state=~w~n",
%%          [Gen,Name,NNames,State,Newstate]),
  cell(Name,Gen+1,Newstate,0,0,NNames);


%% T‰ss‰ haarassa (Kun NMsgs < 8 ja NAlive_plus_1 > 0) pyˆrit‰‰n
%% niin kauan, kunnes kaikilta kahdeksalta naapurilta on tullut
%% niiden tilatiedot. Kun viestit kaikilta kahdeksalta on
%% otettu vastaan, siirryt‰‰n cell-funktion toiseen haaraan.

cell(Name,Gen,State,NMsgs,NAlive_plus_1,NNames) ->
%%io:format("Cell ~w with neighbours ~w called in generation ~w. NMsgs=~w~n",
%%               [Name,NNames,Gen,NMsgs]),
  receive
    next ->
      io:format("Cell ~w with neighbours ~w received premature 'next'-signal in generation ~w, while only ~w messages had been received from its neighbours!~n",
                 [Name,NNames,Gen,NMsgs]),
      exit(premature_next);
    {NName,NGen,NState} ->
      if NGen /= Gen
            ->
           io:format("Cell ~w with neighbours ~w received in generation ~w state signal from neighbour ~w (state=~w) claiming to be in generation ~w. ~w messages has been already received from its neighbours.~n",
                        [Name,NNames,Gen,NName,NState,NGen,NMsgs]),
           exit(neighbours_not_in_sync);
       true -> cell(Name,Gen,State,NMsgs+1,NAlive_plus_1+NState,NNames)
      end
  end.



spawn_cell(Cellname,Size,InitState) ->
  register(Cellname,
           spawn_link(?MODULE, cell,
                 [Cellname,0,InitState,0,0,cells_neighbours(Cellname,Size)]
                )
          ).

spawn_all_cells([],_,_) ->
  ok;

spawn_all_cells([Cellname|OtherNames],Size,[InitState|OtherStates]) ->
  spawn_cell(Cellname,Size,InitState),
  spawn_all_cells(OtherNames,Size,OtherStates).



stopcell(Cellname) ->
    Cellname ! stop.

nextgen(Cellname) ->
    Cellname ! next.

receive_board_ref() ->
    receive {gfx,_,Board} -> Board
            after 60000 ->
              io:format("otakoppi timed out!",[]),
              exit(timeout)
    end.

canvas_new(Size,CellSize) ->
  WxWinRef = life_wx_win:new(self()),
  Board = receive_board_ref(),
  Board.

show_cell(Board,CellName,CellState,AllDrawnCircles) ->
  ROW = lists:nth(2,atom_to_list(CellName))-48,
  COL = lists:nth(3,atom_to_list(CellName))-48,
  X1 = (COL*10), X2 = (COL*10)+9, Y1 = (ROW*10), Y2 = (ROW*10)+9,
  if CellState == 1 -> Colour = 9786; %% A face. Try also 9835
   true -> Colour = 32
  end,
  life_wx_board:set_butt(Board,{ROW+1,COL+1},Colour),
  AllDrawnCircles.
%% NewCircle = gs:create(oval,Board,[{coords,[{X1,Y1},{X2,Y2}]},{fill,Colour},{bw,0}]),
%%  [NewCircle|AllDrawnCircles].

supervise(Size,Cellnames,InitialStates) ->
  Board = canvas_new(Size,10),
  spawn_all_cells(Cellnames,Size,InitialStates),
  foreach(fun(Cellname) -> nextgen(Cellname) end,Cellnames),
  NCells = Size*Size,
  supervise(Size,NCells,0,0,Cellnames,Board,[]). %% Enter the main loop.

%% T‰t‰ haaraa kutsutaan silloin kuin kaikilta soluilta on tullut
%% nykyisen sukupolven tilatieto.
%% L‰hett‰‰ next-signaalin kaikille soluille, jonka j‰lkeen
%% siirtyy supervise:n seuraavaan haaraan.
supervise(Size,NCells,Gen,NMsgs,Cellnames,Board,AllDrawnCircles) when NMsgs == NCells ->
  foreach(fun(Cellname) -> nextgen(Cellname) end,Cellnames),
%%  foreach(fun(Cob) -> gs:destroy(Cob) end,AllDrawnCircles),
  supervise(Size,NCells,Gen+1,0,Cellnames,Board,[]);

%% T‰ss‰ haarassa pyˆrit‰‰n niin kauan kunnes kaikilta soluilta
%% on tullut tilatiedot, tai kunnes Timeout 10000 ylittyy.
supervise(Size,NCells,Gen,NMsgs,Cellnames,Board,AllDrawnCircles) ->
  receive
    {CellName,CellGen,CellState} ->
      if CellGen /= Gen
            ->
           io:format("Supervisor received in generation ~w state signal from the cell ~w (state=~w) claiming to be in generation ~w. ~w messages has been already received from the cells.~n",
                        [Gen,CellName,CellState,CellGen,NMsgs]),
           exit(cells_not_in_sync);
       true ->
            supervise(Size,NCells,Gen,NMsgs+1,Cellnames,Board,show_cell(Board,CellName,CellState,AllDrawnCircles))
      end;
    stop ->
      io:format("Supervisor stopped in generation ~w~n",
                   [Gen]),
      exit(stopped)
  after 10000 ->
      io:format("Supervisor timed out in generation ~w after having received ~w messages from the cells.~n",
                        [Gen,NMsgs]),
      exit(supervisor_timedout)
  end.


pattern2state("*") -> 1;
pattern2state(_) -> 0.

start(InitPattern) ->
  SquareSize = length(InitPattern),
  Pattern2state = fun(X) -> pattern2state(X) end,
  Cellnames = list_of_cellnames(SquareSize),
  InitStates = map(Pattern2state,append(InitPattern)),
  register(supervisor,
           spawn(?MODULE, supervise,
                 [SquareSize,Cellnames,InitStates])).

stop() -> supervisor ! stop.


start_glider() ->
  start([[" "," "," "," "," "," "," "," "],
        [" "," ","*"," "," "," "," "," "],
        [" "," "," ","*"," "," "," "," "],
        [" ","*","*","*"," "," "," "," "],
        [" "," "," "," "," "," "," "," "],
        [" "," "," "," "," "," "," "," "],
        [" "," "," "," "," "," "," "," "],
        [" "," "," "," "," "," "," "," "]]
       ).



start_spaceship() ->
  start(
    [[" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 1
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 2
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 3
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 4
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 5
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 6
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 7
     [" "," "," "," "," "," "," ","*"," "," "," "," "," "," "," "], % 8
     [" "," "," "," "," "," "," "," ","*"," "," "," "," "," "," "], % 9
     [" "," "," "," ","*"," "," "," ","*"," "," "," "," "," "," "], % 10
     [" "," "," "," "," ","*","*","*","*"," "," "," "," "," "," "], % 11
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 12
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 13
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "], % 14
     [" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "]] % 15
       ).



