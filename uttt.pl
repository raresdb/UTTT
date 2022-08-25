:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('utils.pl').

%% helpers
% count aparitions of X in a list
% count(+L, +X, -N)
count([], X, 0).
count([X|T], X, N) :- !, count(T, X, P), N is P + 1.
count([_|T], X, N) :- count(T, X, N).

% change a value in a list at Idx pos(counting from 0)
%setElem0(+L, +Idx, Val, -NL)
setElem0(L, Idx, _, _) :- length(L, Lgth), Lgth =< Idx, !, fail.
setElem0([H|T], 0, Val, [Val|T]) :- !.
setElem0([H|T], Idx, Val, [H|R]) :- Idx_ is Idx - 1, setElem0(T, Idx_, Val, R).

% get first position where moves can be done
% getfirst_pos(+Board, +RemainingPoses, -Choice)
getFirstPos(_, [], _) :- fail.
getFirstPos(B, [H|T], H) :- getPos(B, H, ''), !.
getFirstPos(B, [H|T], R) :- getFirstPos(B, T, R).

% true for the initial state of the game
% initialState(-State)
initialState((any,[['', '','','','','','','',''],['', '','','','','','','',''],['', '','','','','','','',''],
    ['', '','','','','','','',''],['', '','','','','','','',''],['', '','','','','','','',''],
    ['', '','','','','','','',''],['', '','','','','','','',''],['', '','','','','','','','']])).

% returns the Boards of the state given
% getBoards(+State, -Boards)
getBoards((_,Boards), Boards).

% returns the board from the state given at the given position
% getBoard(+State, +UPos, -Board)
getBoard((_, Bs), UPos, Board) :- positions(Positions), nth0(Idx, Positions, UPos),
    nth0(Idx, Bs, Board).

% returns the boards of the state represented in a single board
% values being x/0 for won boards, '' for boards still in play
% and r for boards that ended in a draw
% getUBoard(state(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
getUBoard((_, Bs), UboardState) :- maplist(getBoardResult, Bs, UboardState).

% returns the value of the cell at the given position in the given state
% getPos(+State, +UPos, +Pos, -Cell).
getPos((_, Bs), UPos, Pos, Cell) :- positions(Positions), nth0(UIdx, Positions, UPos),
    nth0(Idx, Positions, Pos), nth0(UIdx, Bs, B), nth0(Idx, B, Cell).

% returns the value of the cell at the given position in the given board
% getPos(+Board, +Pos, -Cell).
getPos(Board, Pos, Cell) :- positions(Positions), nth0(Idx, Positions, Pos), nth0(Idx, Board, Cell).

% returns the next player who is in turn
% getNextPlayer((+State), -NextPlayer)
getNextPlayer((_, Bs), x) :- flatten(Bs, L), count(L, x, Nx), count(L, 0, N0), Nx =:= N0.
getNextPlayer((_, Bs), 0) :- flatten(Bs, L), count(L, x, Nx), count(L, 0, N0), Nx > N0.

% returns the next boards where moves are valid
% getNextAvailableBoards(+State, -NextBoardsPoss)
getNextAvailableBoards((any, Bs), NextBoardsPoss) :- getUBoard((any, Bs), UB), positions(Positions),
    findall(Pos, (nth0(UIdx, UB, ''), nth0(UIdx, Positions, Pos)), NextBoardsPoss), !.
getNextAvailableBoards((P, _), [P]).

% returns the result of the board represented by the same values as in
% getUBoard
% getBoardResult(+Board, -Result)
getBoardResult(Board, Result) :- player_wins(Result, Board), !.
getBoardResult(Board, '') :- member('', Board), !.
getBoardResult(Board, r).

% generates a state using the boards and the last move's position
% note that the position in the board is of interest, not the position
% of the board
% buildState(+Boards, +PreviousPos, -State)
buildState(Boards, PreviousPos, (PreviousPos, Boards)) :- positions(Positions),
    getUBoard((_, Boards), UB), nth0(Idx, Positions, PreviousPos), nth0(Idx, UB, ''), !.
buildState(Boards, _, (any, Boards)).

% checks whether the move given is valid in this state
% note that the move can be both a pair of poses when more valid boards
% are in play, or a single pos when only one board is valid
% validMove(+State, +Move)
validMove((_, Bs), _) :- getUBoard((_, Bs), UB), player_wins(R, UB), !, fail.
validMove((any, Bs), (UPos, Pos)) :- positions(Positions), getUBoard((_, Bs), UB),
    nth0(UIdx, UB, ''), nth0(UIdx, Bs, B), nth0(UIdx, Positions, UPos),
    nth0(Idx, B, ''), nth0(Idx, Positions, Pos), !.
validMove((P, Bs), Pos) :- positions(Positions), nth0(UIdx, Positions, P),
    nth0(UIdx, Bs, B), nth0(Idx, B, ''), nth0(Idx, Positions, Pos).

% returns the next state after applying the given move
% makeMove(+State, +Move, -NewState)
% Move can be both a pair of poses or a single pos
makeMove(State, Move, _) :- \+ validMove(State, Move), !, fail.
makeMove((any, Bs), (UPos, Pos), NewState) :- positions(Positions), nth0(UIdx, Positions, UPos),
    nth0(Idx, Positions, Pos), nth0(UIdx, Bs, B), getNextPlayer((_, Bs), Player),
    setElem0(B, Idx, Player, NB), setElem0(Bs, UIdx, NB, NBs), buildState(NBs, Pos, NewState), !.
makeMove((UPos, Bs), Pos, NewState) :- positions(Positions), nth0(UIdx, Positions, UPos),
    nth0(Idx, Positions, Pos), nth0(UIdx, Bs, B), getNextPlayer((_, Bs), Player),
    setElem0(B, Idx, Player, NB), setElem0(Bs, UIdx, NB, NBs), buildState(NBs, Pos, NewState).

% gives the next move of the strategy
% dummy_first(+State, -NextMove)
dummy_first((any, Bs), (UPos, Pos)) :- getUBoard((_, Bs), UB), positions(Positions),
    getFirstPos(UB, Positions, UPos), nth0(UIdx, Positions, UPos), nth0(UIdx, Bs, B),
    getFirstPos(B, Positions, Pos).
dummy_first((UPos, Bs), Pos) :- getUBoard((_, Bs), UB), positions(Positions),
    nth0(UIdx, Positions, UPos), nth0(UIdx, Bs, B), getFirstPos(B, Positions, Pos).

% gives the next move of the strategy
% dummy_last(+State, -NextMove)
dummy_last((any, Bs), (UPos, Pos)) :- getUBoard((_, Bs), UB), positions(Positions),
    reverse(Positions, RPositions), getFirstPos(UB, RPositions, UPos), nth0(UIdx, Positions, UPos),
    nth0(UIdx, Bs, B), getFirstPos(B, RPositions, Pos).
dummy_last((UPos, Bs), Pos) :- getUBoard((_, Bs), UB), positions(Positions),
    reverse(Positions, RPositions), nth0(UIdx, Positions, UPos), nth0(UIdx, Bs, B),
    getFirstPos(B, RPositions, Pos).

%%% Greedy strategy %%%

% Returns the priority of a given move in a given board.
% movePriority(+Player, +Board, +Move, -Priority)
movePriority(_, Board, Move, _) :- positions(Positions), nth0(Idx, Positions, Move),
    \+ nth0(Idx, Board, ''), !, fail.

movePriority(Player, Board, Move, 0) :- positions(Positions), nth0(Idx, Positions, Move),
    setElem0(Board, Idx, Player, NB), player_wins(Player, NB), !.

movePriority(Player, Board, Move, 1) :- positions(Positions), nth0(Idx, Positions, Move),
    nextPlayer(Player, NxtP), setElem0(Board, Idx, NxtP, NB), player_wins(NxtP, NB), !.

movePriority(Player, Board, Move, 2) :- count(Board, x, 0), count(Board, 0, 0),
    member(Move, [ne, nw, se, sw]), !.

movePriority(Player, Board, Move, 3) :- count(Board, Player, 0), nextPlayer(Player, NxtP),
    ((nth0(4, Board, NxtP), member(Move, [ne, nw, se, sw])); (\+ nth0(4, Board, NxtP), Move = c)),
    !.

movePriority(Player, Board, Move, 4) :- positions(Positions), nth0(Idx, Positions, Move),
    setElem0(Board, Idx, Player, NB), findall(CandidateB, (nth0(ValidIdx, NB, ''),
    setElem0(NB, ValidIdx, Player, CandidateB), player_wins(Player, CandidateB)), Wins),
    length(Wins, Lgth), Lgth > 0, !.

movePriority(_, _, Move, 5) :- member(Move, [ne, nw, se, sw]), !.
movePriority(_, _, _, 6).


% next available moves ordered by priority in a given board
% bestIndividualMoves(+P, +Board, -Moves)
bestIndividualMoves(P, Board, Moves) :- positions(Positions), findall((Priority, Move),
    (member(Move, Positions), movePriority(P, Board, Move, Priority)), Pairs),
    sortMoves(Pairs, Moves).

% returns the next move of the strategy
% narrowGreedy(+State, -Move)
narrowGreedy((any, Boards), (NextUMove, NextMove)) :- getUBoard((_, Boards), UBoard),
    positions(Positions), getNextPlayer((_, Boards), P),
    bestIndividualMoves(P, UBoard, [NextUMove|_]), getBoard((_, Boards), NextUMove, Board),
    bestIndividualMoves(P, Board, [NextMove|_]).
    
narrowGreedy((NextUMove, Boards), NextMove) :- getBoard((_, Boards), NextUMove, Board),
    getNextPlayer((_, Boards), P), bestIndividualMoves(P, Board, [NextMove|_]).

% returns the priority of a given move in the whole state
% UBPriority(+Player, +State, +Move, -Priority)

% bad input
uBPriority(_, State, Move, _) :- \+ validMove(State, Move), !, fail.

% this move wins
uBPriority(Player, State, Move, 0) :- makeMove(State, Move, NextState),
    getUBoard(State, UB), player_wins(Player, UB), !.

% move oponent to a finished board
uBPriority(Player, State, Move, 15) :- makeMove(State, Move, (NextPos, _)), NextPos = any, !.

% move oponent where current player is one move away from win
uBPriority(Player, State, Move, 12) :- makeMove(State, Move, (NextUPos, NextBoards)),
    getBoard((_, NextBoards), NextUPos, NextBoard), movePriority(Player, NextBoard, _, 0), !.

% move oponent where he has no moves done yet
uBPriority(Player, State, Move, 1) :- nextPlayer(Player, NxtP),
    makeMove(State, Move, (NextUPos, NextBoards)), getBoard((_, NextBoards), NextUPos, Board),
    count(Board, NxtP, 0), !.

% move oponent where he has only one move done
uBPriority(Player, State, Move, 2) :- nextPlayer(Player, NxtP),
    makeMove(State, Move, (NextUPos, NextBoards)), getBoard((_, NextBoards), NextUPos, Board),
    count(Board, NxtP, 1), !.

% move oponent where he has more than 1 moves done(priority on greater current oponent n.o. moves
% here)
uBPriority(Player, State, Move, Priority) :- nextPlayer(Player, NxtP),
    makeMove(State, Move, (NextUPos, NextBoards)), getBoard((_, NextBoards), NextUPos, Board),
    count(Board, NxtP, c1), c > 1, count(Board, Player, c2), (\+ movePriority(_, Board, _, 0)),
    Priority is 10 - c2, !.

% move oponent where they win in one move -> that move will move the current player where they can
% win in one move or in an any state
uBPriority(Player, State, Move, 13) :- nextPlayer(Player, NxtP),
    makeMove(State, Move, (NextUPos, NextBoards)), getBoard((_, NextBoards), NextUPos, Board),
    movePriority(NxtP, Board, NextMove, 0),
    makeMove((NextUPos, NextBoards), NextMove, (ThirdUPos, ThirdBoards)),
    (NextUPos = any ; getBoard((_, ThirdBoards), ThirdUPos, ThirdBoard),
    movePriority(Player, ThirdBoard, _, 0)), !.

% same, but current player gets moved where they can't win in one move
uBPriority(Player, State, Move, 14) :- nextPlayer(Player, NxtP),
    makeMove(State, Move, (NextUPos, NextBoards)), getBoard((_, NextBoards), NextUPos, Board),
    movePriority(NxtP, Board, NextMove, 0),
    makeMove((NextUPos, NextBoards), NextMove, (ThirdUPos, ThirdBoards)),
    (NextUPos = any ; getBoard((_, ThirdBoards), ThirdUPos, ThirdBoard),
    \+ movePriority(Player, ThirdBoard, _, 0)), !.

% next move oponent wins
uBPriority(Player, State, Move, 16) :- positions(Positions), nextPlayer(Player, NxtP),
    makeMove(State, Move, NextState), findall(EndSt, (member(Mv, Positions),
    makeMove(NextState, Mv, EndSt), getUBoard(EndSt, UB), player_wins(NxtP, UB)),
    Wins), length(Wins, L), L > 0, !.

% others
uBPriority(Player, State, Move, 11).

% next available moves ordered by priority
% bestMoves(+State, -Moves)
bestMoves((UPos, Bds), Moves) :- (\+ UPos = any), !, State = (UPos, Bds), getNextPlayer(State, P),
    getBoard(State, UPos, Board), bestIndividualMoves(P, Board, Poses),
    findall((Priority, Move), (member(Move, Poses), uBPriority(P, State, Move, Priority)), Pairs),
   sortMoves(Pairs, Moves).

bestMoves(State, Moves) :- positions(Positions), getNextPlayer(State, P),
    getUBoard(State, UB), bestIndividualMoves(P, UB, UPoses), findall((Priority, (UPos, Pos)),
    (member(UPos, UPoses), getBoard(State, UPos, Board), bestIndividualMoves(P, Board, Poses),
    member(Pos, Poses), uBPriority(P, State, (UPos, Pos), Priority)), Pairs),
    sortMoves(Pairs, Moves).

% returns the next move of the strategy
% greedy(+State, -Move)
greedy(State, Move) :- bestMoves(State, [Move|_]).
