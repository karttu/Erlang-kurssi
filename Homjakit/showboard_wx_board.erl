
-module(showboard_wx_board).

-export([new/2, set_butt/3, setup_board/2, clear_board/1, draw/3, 
	 %% Callbacks
	 init/1, handle_sync_event/3, 
	 handle_event/2, handle_info/2, handle_call/3, handle_cast/2,
	 code_change/3, terminate/2]).

-include("showboard_wx.hrl").
-include("homjakit.hrl").

-record(state, {win, parent, board=[], pen, fonts=[]}).
-record(sq, {key,val}).
-define(BRD,10). %% Marginaali?
-define(ARC_R, 10).
    
-behaviour(wx_object).

%% -include("showboard_wx.hrl").


%% API 
new(ParentObj,ParentPID) ->
    wx_object:start_link(?MODULE, [ParentObj, self(),ParentPID], []).


setup_board(Board, Init) ->
    wx_object:call(Board, {setup_board, Init}).

clear_board(Board) ->
    wx_object:call(Board, clear_board).


set_butt(Board, Indx, Val) when is_integer(Indx) ->
    {R,C,_} = showboard_wx_win:rcm(Indx),
    set_butt(Board, {R,C}, Val);

set_butt(Board, Id, Val) ->
    wx_object:call(Board, {set_butt, Id, Val}).


draw(Board, DC, Size) ->
    wx_object:call(Board, {draw, DC, Size}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentObj, ParentPid, GrandpaPid]) ->
%%  GrandpaPid ! {boardpid, self()}, %% Tell our grandpa what our PID is.
    Win = wxPanel:new(ParentObj, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setFocus(Win), %% Get keyboard focus
    wxWindow:setSizeHints(Win, {250,250}),
    wxWindow:connect(Win, paint,  [callback]),
    wxWindow:connect(Win, size,  []),
    wxWindow:connect(Win, erase_background, []),
    wxWindow:connect(Win, key_up, [{skip, true}]),
    wxWindow:connect(Win, left_down, [{skip, true}]),
    wxWindow:connect(Win, enter_window, [{skip, true}]), 

    %% Init pens and fonts
    Pen = wxPen:new({0,0,0}, [{width, 3}]),
%%  Fs0  = [{Sz,wxFont:new(Sz, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])}
    Fs0  = [{Sz,wxFont:new(Sz, ?wxDEFAULT, ?wxNORMAL, ?wxNORMAL,[{encoding,?wxFONTENCODING_UNICODE}])}
              ||
	       Sz <- [8,9,10,11,12,13,14,16,18,20,22,24,26,28,30,34,38,42,44,46]],
    TestDC  = wxClientDC:new(Win),
    CW = fun({Sz,Font},Acc) ->
		 case wxFont:ok(Font) of
		     true -> 
			 wxDC:setFont(TestDC, Font),
			 CH = wxDC:getCharHeight(TestDC), 
			 [{CH,Sz,Font} | Acc];
		     false ->
			 Acc
		 end
	 end,
    Fs = lists:foldl(CW, [], Fs0),
    wxClientDC:destroy(TestDC),    
    {Win, #state{win=Win, board=[], pen=Pen, fonts=Fs, parent=ParentPid}}.

handle_sync_event(#wx{event=#wxPaint{}}, _Obj, State = #state{win=Win}) ->
    %% io:format("EPaint~n",[]),
    Size = wxWindow:getSize(Win),
    DC = wxPaintDC:new(Win),
    wxDC:destroyClippingRegion(DC),
    redraw(DC,Size,State),
    wxPaintDC:destroy(DC),
    %%io:format("...EPaint~n",[]),
    ok.

handle_event(#wx{event=#wxSize{}}, State) ->
    redraw(State),	    
    {noreply,State};
handle_event(_Ev, State) ->
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_call({set_butt, Key, 0},_From,S0=#state{board=B0}) ->  %% Reset
    B = lists:keydelete(Key,2,B0),
    S = S0#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call({set_butt, Key, Val},_From,S0=#state{board=B0}) ->  
    case lists:keysearch(Key,2,B0) of
	{value, _} -> 
	    B = lists:keyreplace(Key, 2, B0, #sq{key=Key,val=Val});
	false ->
	    B = [#sq{key=Key, val=Val}|B0]
    end,
    S = S0#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call({setup_board, Init},_From, State) ->
    B = [#sq{key=Key, val=Val} || {Key,Val} <- Init],
    S = State#state{board=B},
    redraw(S),
    {reply, ok, S};

handle_call(clear_board,_From, State = #state{board=B0}) ->    
%%  B = [Butt || Butt = #sq{given=true} <- B0],
    B = [Butt || Butt <- B0],
    S = State#state{board=B},
    redraw(S),
%%  Given = [{Key, Val} || #sq{key=Key,val=Val,given=true} <- B],
%%  {reply, Given, S};
    {reply, ok, S};

handle_call({draw, DC, Size},_From, S) ->    
    redraw(DC,Size,S),
    {reply, ok, S}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_info(Msg, State) ->
    {stop, {info, Msg}, State}.

terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redraw(S = #state{win=Win}) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, S),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, Size, S) ->    
    wx:batch(fun() -> 
		     wxDC:setBackground(DC, ?wxWHITE_BRUSH),
		     wxDC:clear(DC),
		     CellSz = draw_board(DC,Size,S),
                     F = sel_font(CellSz,S#state.fonts),
		     [draw_cell(DC,F,CellSz,Sq) || Sq <- S#state.board]
	     end).

sel_font(_BS,[{_H,_Sz,F}]) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,_H, _BS]),
    F;
sel_font(BS,[{H,_Sz,F}|_]) when BS > (H + 6) -> 
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,H, BS]),
    F;
sel_font(BS,[_|Fs]) ->
    sel_font(BS,Fs).

draw_cell(DC,F,TBox,#sq{key={R,C},val=Num}) ->
    {X,Y} = get_coords(TBox,R-1,C-1),

%%  if Bold -> 
%%          wxFont:setWeight(F,?wxBOLD),
%%          wxDC:setTextForeground(DC,{0,0,0});
%%     Correct =:= false ->
%%          wxFont:setWeight(F,?wxNORMAL),
%%          wxDC:setTextForeground(DC,{255,40,40,255});
%%     true ->
         wxFont:setWeight(F,?wxNORMAL),
         wxDC:setTextForeground(DC,{50,50,100,255}),
%%    end,
    wxDC:setFont(DC,F),
    CH = (TBox - wxDC:getCharHeight(DC)) div 2,
    CW = (TBox - wxDC:getCharWidth(DC)) div 2,
%%  wxDC:drawText(DC, integer_to_list(Num), {X+CW,Y+CH+1}),
%%  wxDC:drawText(DC, [lists:nth(Num,[49,9733,9728,9786,9801,54,9832,9835,57,65])], {X+CW,Y+CH+1}),
    wxDC:drawText(DC, [Num], {X+CW,Y+CH+1}),
    ok.

%% Toivottavasti ei käytetä!
get_coords(TBox,R,C) ->
    {?BRD + C*TBox,
     ?BRD + R*TBox}.
%%    R1 = R div 3,
%%    R2 = R rem 3,
%%    C1 = C div 3,
%%    C2 = C rem 3,
%%    {?BRD + C1*Sz + C2*TBox,
%%     ?BRD + R1*Sz + R2*TBox}.

draw_board(DC,{W0,H0},#state{pen=Pen}) ->
    CellSz = getCellSize(W0,H0),
    BoxSzX = (?BOARDMAXX*CellSz),
    BoxSzY = (?BOARDMAXY*CellSz),
%%  BoxSz = getGeomSz(W0,H0),
    BSX = ?BRD+BoxSzX,
    BSY = ?BRD+BoxSzY,

    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0,0,0}),
    wxDC:setPen(DC,Pen),
    
    wxDC:drawRoundedRectangle(DC, {?BRD,?BRD,BoxSzX+1,BoxSzY+1}, 
			      float(?ARC_R)),

    %% Draw inside lines
    wxPen:setWidth(Pen, 1),
    wxDC:setPen(DC,Pen),

    lists:foreach(fun(I) ->
                    wxDC:drawLine(DC, {?BRD+(CellSz*I), ?BRD}, {?BRD+(CellSz*I), BSX})
                  end,
                  lists:seq(1,?BOARDMAXY-1)
                 ),

    lists:foreach(fun(I) ->
                    wxDC:drawLine(DC, {?BRD, ?BRD+(CellSz*I)}, {BSY, ?BRD+(CellSz*I)})
                  end,
                  lists:seq(1,?BOARDMAXX-1)
                 ),

    CellSz.


getCellSize(W,H) ->
    W div ?BOARDMAXY.
 
getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

