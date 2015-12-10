
-module(life_wx_win).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
	 terminate/2, code_change/3,
         rcm/1]).

-compile(export_all).

%% -include("/Program Files (x86)/erl5.9.3.1/lib/wx-0.99.2/include/wx.hrl").
-include("life_wx.hrl").

-behaviour(wx_object).


%% -import(sudoku_game, [indx/1]).

%%%%%%%%%%  Graphic engine %%%%%%%%%%%%%%

-record(gs,{board,show_err=true,level=hard,game,frame,orig=[], print_d, print_psdd}).

new(ParentPID) ->
    wx:new(),
%%  wx_object:start_link(?MODULE, [ParentPID], []).
    wx_object:start(?MODULE, [ParentPID], []).

%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

init([ParentPID]) ->
    {Frame, Board} = wx:batch(fun() -> create_window(ParentPID) end),
    ParentPID ! {gfx, self(), Board},
%%  {Frame, init_printer(#gs{board=Board,game=ParentPID,frame=Frame})}.
    {Frame, Board}.

otakoppi() ->
    receive A -> A
            after 10000 -> timeout
%%           io:format("otakoppi timed out!",[])
    end.

%% Otakoppi = fun() -> receive {gfx, GFX} -> GFX end end.
%% Otakoppi = fun() -> receive A -> A end end.

create_window_short(ParentPID) ->
    Frame = wxFrame:new(wx:null(), -1, "Game of Life", []),

    Panel = wxPanel:new(Frame), 

    Board = life_wx_board:new(Panel,ParentPID),

    wxWindow:show(Frame),
    {Frame, Board}.


create_window(ParentPID) ->
    Frame = wxFrame:new(wx:null(), -1, "Game of Life (Long)", []),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Opt     = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?NEW,  "&New Game"),
    wxMenu:append(File, ?OPEN, "&Open Game"),
    wxMenu:append(File, ?SAVE, "&Save Game"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?PRINT, "Print"),
    wxMenu:append(File, ?PRINT_PAGE_SETUP, "Page Setup"),
    wxMenu:append(File, ?PRINT_PRE, "Print Preview"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?QUIT, "&Quit Game"),

    wxMenu:append(Help, ?RULES, "Rules"),
    wxMenu:append(Help, ?ABOUT, "About"), 

    wxMenu:appendRadioItem(Opt, ?TRIVIAL, "Level: Trivial"),
    wxMenu:appendRadioItem(Opt, ?EASY, "Level: Easy"),
    LItem = wxMenu:appendRadioItem(Opt, ?NORMAL, "Level: Normal"),
    wxMenu:appendRadioItem(Opt, ?HARD, "Level: Hard"),
    wxMenu:appendRadioItem(Opt, ?HARDEST, "Level: Hardest"),
    wxMenu:appendSeparator(Opt),
    EItem = wxMenu:appendCheckItem(Opt, ?SHOW_ERROR, "Show errors"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Opt, "O&ptions"),
    wxMenuBar:append(MenuBar, Help, "&Help"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Top    = wxBoxSizer:new(?wxHORIZONTAL),

    Panel = wxPanel:new(Frame), 
    NewGame = wxButton:new(Panel, ?NEW, [{label,"New Game"}]),
    wxButton:connect(NewGame, command_button_clicked),
    Empty = wxButton:new(Panel, ?EMPTY, [{label,"Empty Board "}]),
    wxButton:connect(Empty, command_button_clicked),
    Clean = wxButton:new(Panel, ?CLEAR, [{label,"Clear"}]),
    wxButton:connect(Clean, command_button_clicked),
    Hint  = wxButton:new(Panel, ?HINT, [{label, "Hint"}]),
    wxButton:connect(Hint, command_button_clicked),

    wxSizer:addSpacer(Top,2),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizer:add(Top, NewGame, wxSizerFlags:left(SF)), 
    wxSizer:addSpacer(Top,3),
    wxSizer:add(Top, Empty,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   
    wxSizer:add(Top, Clean,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   
    wxSizer:add(Top, Hint,    wxSizerFlags:right(SF)),

    wxSizer:addSpacer(MainSz,5),
    wxSizer:add(MainSz, Top, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:addSpacer(MainSz,10),

    Board = life_wx_board:new(Panel,ParentPID),

    wxSizer:add(MainSz, Board, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),
    wxWindow:setSizer(Panel,MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:show(Frame),
    %% Check after append so it's initialized on all platforms
    wxMenuItem:check(LItem),
    wxMenuItem:check(EItem),
    {Frame, Board}.

status(Win, F, A) ->
    Str = lists:flatten(io_lib:format(F, A)),
    wxFrame:setStatusText(Win, Str).

%%%%%%%%%%%%%%%% Info i.e. messages %%%%%%%%%%%%%%%%%%%%%

handle_info(quit, S=#gs{game=G,frame=F}) ->
    wxWindow:close(F),
    wx_core:quit(), 
    G ! quit,
    {stop, shutdown, S};

handle_info({init, Init}, S = #gs{board=Board,frame=F}) ->
    life_wx_board:setup_board(Board, Init),
%%  status(F, "Given ~p  Left ~p", [length(Init), life_wx_board:left(Board)]),
    status(F, "Given ~p", [length(Init)]),
    {noreply, S#gs{orig=[indx(Id)||{Id,_}<-Init]}};
handle_info({set_val, ButtI, Val}, S = #gs{game=G,board=Board,orig=Orig}) ->
    case lists:member(indx(ButtI), Orig) of
	false -> set_val(ButtI, Val, Board, G);
	true ->  ignore
    end,
    {noreply, S};
handle_info({working, Done}, S = #gs{frame=F}) ->
    status(F, "Thinking: ~p%", [Done]),
    {noreply, S};
handle_info({busy, Mode},S) -> 
    case Mode of
	start -> wx_misc:beginBusyCursor();
	stop  -> wx_misc:endBusyCursor()
    end,
    {noreply, S}.

%%%%%%%%%%%%%%%%% GUI-Events %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=?HINT, event=#wxCommand{type=command_button_clicked}},
	     S = #gs{game=G}) ->
    G ! {solve,false},
    {noreply,S};

handle_event(#wx{event=#wxClose{}},
	     S = #gs{game=G,frame=F}) ->
    catch wxWindow:'Destroy'(F),
    G ! quit,
    {stop, shutdown, S};

handle_event(#wx{id=?QUIT, event=#wxCommand{type=command_menu_selected}},
	     S = #gs{game=G,frame=F}) ->
    wxWindow:close(F,[]),
    G ! quit,
    {stop, shutdown, S};

%% type=command_button_clicked,
handle_event(#wx{id=?NEW, event=#wxCommand{}},
	     S = #gs{game=G, board=Board}) ->
    G ! {op,?NEW,S#gs.level},
    life_wx_board:setup_board(Board,[]),
    {noreply, S#gs{orig=[]}};
handle_event(#wx{id=?EMPTY, event=#wxCommand{}},
	     S = #gs{game=G, board=Board}) ->
    G ! {op,?EMPTY},
    life_wx_board:setup_board(Board,[]),
    {noreply, S#gs{orig=[]}};
handle_event(#wx{id=?CLEAR, event=#wxCommand{}},
	     S = #gs{game=G,board=Board}) ->    
    Vals = life_wx_board:clear_board(Board),
    G ! {loaded, Vals},
    {noreply, S};
%% handle_event(#wx{id=ID, event=#wxCommand{}}, S) when ID > 125 ->
%%    New = dialog(ID, S),
%%    {noreply, New};
handle_event(Msg,S) ->
    io:format("~p: Unhandled event ~p~n",[?MODULE, Msg]),
    %%life_wx_board:event(Msg, Ids),
    {noreply, S}.


handle_call({set_butt, Key, Val},_From,State=#gs{game=G,board=Board}) ->
    set_val(Key, Val, Board, G),
    {noreply, State};

handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    normal.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_val(Id, Val, Board, G) ->
    life_wx_board:set_butt(Board, Id,Val),
    G ! {validate, Id, Val},
    ok.


indx(Indx) when is_integer(Indx) -> Indx;
indx({Row, Col}) ->
    indx(Row,Col);
indx({Row, Col,_}) ->
    indx(Row,Col).
indx(Row, Col) ->
    (Row-1)*9+Col.


rcm(Indx) when is_integer(Indx) ->
    rcm({((Indx-1) div 9)+1, (Indx-1) rem 9+1});
rcm({R,C}) ->
    M = mat(R,C),
    {R,C,M}.
mat(R,C) ->
    1+(C-1) div 3 + ((R-1) div 3)*3.


init_printer(S) ->
    PD   = wxPrintData:new(),

    %% You could set an initial paper size here
    %%    g_printData->SetPaperId(wxPAPER_LETTER); // for Americans
    %%    g_printData->SetPaperId(wxPAPER_A4);    // for everyone else    

    PSDD = wxPageSetupDialogData:new(PD),
    wxPageSetupDialogData:setMarginTopLeft(PSDD,{15,15}),
    wxPageSetupDialogData:setMarginBottomRight(PSDD,{15,15}),

    S#gs{print_d=PD, print_psdd=PSDD}.

