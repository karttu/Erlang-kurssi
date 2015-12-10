
-module(life_wx_board).

-export([new/2, set_butt/3, setup_board/2, clear_board/1, draw/3, 
	 %% Callbacks
	 init/1, handle_sync_event/3, 
	 handle_event/2, handle_info/2, handle_call/3, handle_cast/2,
	 code_change/3, terminate/2]).

-include("life_wx.hrl").

-record(state, {win, parent, board=[], pen, fonts=[]}).
-record(sq, {key,val}).
-define(BRD,10).
-define(ARC_R, 10).
    
-behaviour(wx_object).

%% -include("life_wx.hrl").


%% API 
new(ParentObj,ParentPID) ->
    wx_object:start_link(?MODULE, [ParentObj, self(),ParentPID], []).


setup_board(Board, Init) ->
    wx_object:call(Board, {setup_board, Init}).

clear_board(Board) ->
    wx_object:call(Board, clear_board).


set_butt(Board, Indx, Val) when is_integer(Indx) ->
    {R,C,_} = life_wx_win:rcm(Indx),
    set_butt(Board, {R,C}, Val);

set_butt(Board, Id, Val) ->
    wx_object:call(Board, {set_butt, Id, Val}).


draw(Board, DC, Size) ->
    wx_object:call(Board, {draw, DC, Size}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentObj, ParentPid, GrandpaPid]) ->
    GrandpaPid ! {boardpid, self()}, %% Tell our grandpa what our PID is.
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
		     BoxSz = draw_board(DC,Size,S),
		     F = sel_font(BoxSz div 3,S#state.fonts),
		     [draw_cell(DC,F,BoxSz,Sq) || Sq <- S#state.board]
	     end).

sel_font(_BS,[{_H,_Sz,F}]) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,_H, _BS]),
    F;
sel_font(BS,[{H,_Sz,F}|_]) when BS > (H + 6) -> 
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,H, BS]),
    F;
sel_font(BS,[_|Fs]) ->
    sel_font(BS,Fs).

draw_cell(DC,F,Sz,#sq{key={R,C},val=Num}) ->
    {X,Y} = get_coords(Sz,R-1,C-1),
    TBox = Sz div 3,

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

get_coords(Sz,R,C) ->
    TBox = Sz div 3,
    R1 = R div 3,
    R2 = R rem 3,
    C1 = C div 3,
    C2 = C rem 3,
    {?BRD + C1*Sz + C2*TBox,
     ?BRD + R1*Sz + R2*TBox}.

draw_board(DC,{W0,H0},#state{pen=Pen}) ->
    BoxSz = getGeomSz(W0,H0),
    BS = ?BRD+3*BoxSz,

    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0,0,0}),
    wxDC:setPen(DC,Pen),
    
    wxDC:drawRoundedRectangle(DC, {?BRD,?BRD,3*BoxSz+1,3*BoxSz+1}, 
			      float(?ARC_R)),
    %% Testing DrawLines
    wxDC:drawLines(DC, [{?BRD+BoxSz, ?BRD}, {?BRD+BoxSz, BS}]),
    wxDC:drawLine(DC, {?BRD+BoxSz*2, ?BRD}, {?BRD+BoxSz*2, BS}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz}, {BS, ?BRD+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*2}, {BS, ?BRD+BoxSz*2}),

    %% Draw inside lines
    wxPen:setWidth(Pen, 1),
    wxDC:setPen(DC,Pen),
    TBox = BoxSz div 3,   
    wxDC:drawLine(DC, {?BRD+TBox, ?BRD}, {?BRD+TBox, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2, ?BRD}, {?BRD+TBox*2, BS}),
    wxDC:drawLine(DC, {?BRD+TBox+BoxSz, ?BRD}, {?BRD+TBox+BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2+BoxSz, ?BRD}, {?BRD+TBox*2+BoxSz, BS}),
    wxDC:drawLine(DC, {?BRD+TBox+BoxSz*2, ?BRD}, {?BRD+TBox+BoxSz*2, BS}),
    wxDC:drawLine(DC, {?BRD+TBox*2+BoxSz*2, ?BRD}, {?BRD+TBox*2+BoxSz*2, BS}),
    %% Vert
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox}, {BS, ?BRD+TBox}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2}, {BS, ?BRD+TBox*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox+BoxSz}, {BS, ?BRD+TBox+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2+BoxSz}, {BS, ?BRD+TBox*2+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox+BoxSz*2}, {BS, ?BRD+TBox+BoxSz*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+TBox*2+BoxSz*2}, {BS, ?BRD+TBox*2+BoxSz*2}),
    BoxSz.

getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

