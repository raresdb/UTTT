

% returns an empty board
% empty_board(-Board)
empty_board(['', '', '', '', '', '', '', '', '']).

% player symbols
% player(-Value)
player(x).
player(0).

% returns the next player based on the last player
% nextPlayer(-Player, -NextPlayer)
nextPlayer(x, 0).
nextPlayer(0, x).

% possible positions in the game
% positions(+List)
positions([nw, n, ne, w, c, e, sw, s, se]).

% returns the winning player
% player_wins(-Player, +Board)
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, n, ne]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [w, c, e]),   getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [sw, s, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, w, sw]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [n, c, s]),   getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [ne, e, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, c, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [ne, c, sw]), getPos(Board, Pos, P)).


% sorts the moves by priority
% each pair is a pair of move and its priority
% the order is ascending
% sortMoves(+PairList, -SortedValues)
sortMoves(PriorityList, Moves) :-
	predsort(comparator, PriorityList, Sorted),
%	writeln(Sorted),
	findall(M, member((_, M), Sorted), Moves1),
	dupsout(Moves1, [], Moves).

comparator((<), (P, _), (P, _)) :- !.
comparator(Delta, (P1, _), (P2, _)) :- compare(Delta, P1, P2).
dupsout([], _, []).
dupsout([H|T], Seen, TOut) :- member(H, Seen), !, dupsout(T, Seen, TOut).
dupsout([H|T], Seen, [H|TOut]) :- dupsout(T, [H|Seen], TOut).


% apllies a series of moves specified as coordinates to a UTTT board.
% apply_moves(+Board, +Moves, -NewBoard)
apply_moves(State, [], State).
apply_moves(State, [Move], NewState) :-
		makeMove(State, Move, NewState), !.
apply_moves(State, [Move | T], NewState) :-
		makeMove(State, Move, IntermState), !,
		apply_moves(IntermState, T, NewState).
apply_moves(State, [Move | T], _) :- !,
		format("Apply moves failed in boards:~n"), printBoards(State),
		format("when trying to apply [~w], with moves remaining: ~w~n", [Move, T]),
		false.

apply_strategy(State, Strategy, NextState):-
    call(Strategy, State, Move),
    format("Selected move ~w~n", [Move]),
    makeMove(State, Move, NextState).

% plays a whole game with the given strategies, returning the winner
% and the sequence of moves
% play(+S1, +S2, -Moves, -Winner)
play(S1, S2, Moves, Winner) :- initialState(S), play_strategies(S, S1, S2, [], Moves, Winner).
play_strategies(State, _, _, History, Moves, Winner) :-
		getUBoard(State, UB),
		player_wins(Winner, UB), !,
		%  format("===================== Player ~w wins.~n", [Winner]),
		reverse(History, Moves), printBoards(State).
play_strategies(State, _, _, History, Moves, r) :-
		getUBoard(State, UB),
		\+ member('', UB), !,
		% format("===================== The game is a draw.~n"),
		reverse(History, Moves), printBoards(State).
play_strategies(State, S1, S2, H, Mvs, Win) :-
		getNextPlayer(State, P),
		call(S1, State, Move),
		(   validMove(State, Move) ->
		       % format("===================== Player ~w (~w) plays ~w. Board is now:~n", [P, S1, Move]),
		       true
		;   format("Move invalid: ~w ~n", [Move]), false),
		makeMove(State, Move, Next), !,
		% printBoards(Next),
		play_strategies(Next, S2, S1, [(P, Move) | H], Mvs, Win).
play_strategies(State, S1, _, H, _, _) :-
		getNextPlayer(State, P),
		!,
		format("Failed to get strategy ~w or to apply move for player ~w in boards:~n", [S1, P]),
		printBoards(State), reverse(H, Mvs),
		format("after moves: ~w~n", [Mvs]),
		false.
play_strategies(State, _, _, _, _, _) :-
		format("getNextPlayer failed in state: ~n"), printBoards(State),
		!, false.



% prints a cell by position
% print_cell(+Cell)
% Afișează o celulă (o poziție).
print_cell('', Sep) :- format('~w~w', ['-', Sep]).
print_cell(Cell, Sep) :- format('~w~w', [Cell, Sep]).

% prints a line from a board
% print_board_line(+SubRow, +Board)
print_board_line(SubRow, Board) :-
    IdxStart is SubRow * 3,
    IdxEnd is (SubRow + 1) * 3 - 1,
    forall(
        between(IdxStart, IdxEnd, Idx),
        (nth0(Idx, Board, Cell),
         print_cell(Cell, " "))
    ).

% prints a line from the UBoard
% print_uttt_line(+Row, +UtttBoard, +UBoard)
% Afișează un rând din tabla mare de UTTT.
print_uttt_line(Row, UtttBoard, UB) :-
    IdxStart is Row * 3,
    IdxEnd is (Row + 1) * 3 - 1,
    forall(
        between(0, 2, SubRow),
        (   forall(
                between(IdxStart, IdxEnd, Idx),
                (nth0(Idx, UtttBoard, Board),
                 write("   "),
                 (SubRow == 1 ->
			(nth0(Idx, UB, R),
			print_cell(R, "| ")) ;
			write("   ")),
                 print_board_line(SubRow, Board)
                 )),
            write("\n")
        )).

% prints all of the games boards
% printBoards(+State)
printBoards(State) :-
	getBoards(State, Boards),
	getUBoard(State, UB),
	getNextPlayer(State, P),
	getNextAvailableBoards(State, NB),
    forall(
        between(0, 2, Row),
        (print_uttt_line(Row, Boards, UB),
         write("\n"))
    ),
    format("Next player ~w will move in ~w~n", [P, NB]).

% prints the UBoard
% printBoard(+Board)
printBoard(Board) :-
	forall(
		between(0, 2, Row),
		(print_board_line(Row, Board), write("\n"))
		).
