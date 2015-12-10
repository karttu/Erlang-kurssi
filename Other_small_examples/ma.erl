
-module(ma).

-export([myequal/2,simplematch/2,fmatch/2,fmatch/3]).
-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
-import(erlang,[return_trace/0]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

treissaa() ->
  dbg:tracer(),
  dbg:p(all, [c]), % Too much info:  dbg:p(all, [c,call,return_to]),
  dbg:tpl({ma,'_','_'},dbg:fun2ms(fun(_) -> return_trace() end)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

myequal([],[]) -> true; %% Both lists ("strings") finished at the same time, so they were equal.

myequal([X|TAIL1],[X|TAIL2]) -> myequal(TAIL1,TAIL2); %% If starting with the same letter, check their tails.

myequal(_,_) -> false. %% Otherwise, they are NOT starting with the same letter, so they are different. Fail now!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%

simplematch([],[]) -> true; %% Both pattern & string in the end, so we succeeded.

simplematch([$?|TAIL1],[_|TAIL2]) -> simplematch(TAIL1,TAIL2); %% ? in pattern matches with any character in string.

simplematch([$+|TAIL1],[_|TAIL2]) -> %% + in pattern matches _one or more_ characters in the string.
   simplematch([$+|TAIL1],TAIL2) orelse simplematch(TAIL1,TAIL2);

simplematch([$*|TAIL1],[]) -> simplematch(TAIL1,[]); %% Handle the (possible) trailing asterisks.

%% Asterisk matches _zero or more_ characters in the string.
simplematch(WHOLE1=[$*|TAIL1],WHOLE2=[_|TAIL2]) ->
          simplematch(TAIL1,WHOLE2)  %% Skip the asterisk at the left side, keep the whole string at the right side.
   orelse simplematch(WHOLE1,TAIL2); %% Or keep the asterisk at the left side, skip one char at the right side.

simplematch([X|TAIL1],[X|TAIL2]) -> simplematch(TAIL1,TAIL2); %% Literal matches after the wildcards!

simplematch(_,_) -> false. %% Otherwise, the match didn't succeed, at least not here.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Kirjoita ylläolevan perusteella funktio fmatch
%% joka palauttaa true:n, mikäli merkkijonot (siis listat) X ja Y
%% eroavat toisistaan vain kahden eroavan, puuttuvan tai lisätyn
%% merkin verran.

fmatch(X,Y) -> fmatch(X,Y,3). %% Allow max two differences (3 = 2+1).


fmatch(_,_,0) -> false; %% The limit is up, we failed!
fmatch([],[],_) -> true; %% Otherwise, both strings in final, we succeeded!

fmatch([_|TAIL1],[],DL) -> fmatch(TAIL1,[],DL-1); %% Handle the trailing differences
fmatch([],[_|TAIL2],DL) -> fmatch([],TAIL2,DL-1); %% with these two clauses.

fmatch([X|TAIL1],[X|TAIL2],DL) -> fmatch(TAIL1,TAIL2,DL); %% Literal matches do not decrement the Diff-limit.

%% Otherwise, there is one of three possible differences (a change, an insertion or a deletion).
%% Decrement the allowed differences and recurse:
fmatch(WHOLE1=[_|TAIL1],WHOLE2=[_|TAIL2],DL) ->
           fmatch(TAIL1,TAIL2,DL-1)   %% Either skip both.
    orelse fmatch(TAIL1,WHOLE2,DL-1)  %% Or skip the char at the left side, keep the whole string at the right side.
    orelse fmatch(WHOLE1,TAIL2,DL-1). %% Or keep the whole string at the left side, skip one char at the right side.



