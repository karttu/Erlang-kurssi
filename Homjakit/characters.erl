%%
%% characters.erl -- Unicode-merkit Erlang-moniagenttipeliä varten. showboard.erl käyttää näitä.
%% Copyright (C) - Antti Karttunen, 2013.
%%

-module(characters).
-include("homjakit.hrl").
-export([]).
-compile(export_all).
-import(lists,[nth/2]).

agentid2uchar(Agent_id) ->
  nth(Agent_id,
      [
       3424, %% Sarvekas alien.
       4734, %% Etiopia-"zombie"
       2823, %% Jallu luuraa. (2824 hiukan erilainen).
       6106, %% Hämy-"käärme" tai "kotilo".
%%     3178, %% Härkä isosarvista rotua. (hiukan liian pienellä).
       2951, %% Tamili-kobra.
       4332, %% "Georgian mandrake" or "carrot"?
       5871, %% Tvimadr
       2696, %% Gujarati "squirrel"
       3232, %% Kannada-TTHA
       5073, %% Hämyä.
%%     5084, %% Samoin..., mutta ei tarpeeksi.
       2912, %% Orissa korvalappumies
       5121, %% Kolmio kärki alaspäin, Canadian Aboriginics. (5123 = kärki ylöspäin)
       2539, %% Bengali-"kynsi" tai "tikari".
       8750, %% One contour-integral
       9786 %% Perus-naama (tylsähkö)
      ]
     ).
     
blockchar() -> 9839. %% Could be also 5785 (Ogham "block").

foodchar() -> grasschar(). %% Our agents are bovine creatures, they eat grass.
%% Could be also 4968. %% (Ethiopic). Could be also 8258, triple asterisk, or maybe 3314. or 4332 (Georgian "carrot"?)

grasschar() -> 3572. %% Could be food for cows?

