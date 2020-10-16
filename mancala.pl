% Programmer: Maayan Kestler
% Name File: mancal.pl
% Description: console mancla game with alphabeta ai player
% Synopsys: to satrt playing mancala run the start_game predicate

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).

% the main predicate of the program, start a mancala game
% Player1- the name of the first player
% Player2- the name of the second  player
% PitsNumber- the number of pits in each row of the board
% PiecesInPit- the number of pieces in each pit at the start of the game
% Player1Func- the playing logic of the first player
% Player2Func- the playing logic of the second player
start_game(Player1, Player2, PitsNumber, PiecesInPit, Player1Func, Player2Func, Depth1, Depth2):-
    create_initialized_list(PitsNumber, PiecesInPit, Row),
    create_initialized_list(2, Row, Board),
    Player1Score is 0,
    Player2Score is 0,
    print_board(Board, Player1, Player2, Player1Score, Player2Score),
    play(Board, Player1, Player2, 1, Player1Score, Player2Score, Player1Func, Player2Func, Depth1, Depth2), !.

% start the game with deafult arguments
start_game:-
    start_game('Maayan', 'CompAI', 6, 4, user_input, alphabeta_ai,1,1), !.

% play a move in the game
% Board- the mancala board made of list containing two lists (one for each row)
% CurrentPlayerNumber- the number of the current player (1 for Player1 and 2 for Player2)
play(Board, Player1, Player2, CurrentPlayerNumber, Player1Score, Player2Score, Player1Func, Player2Func, Depth1, Depth2):-
    (
        CurrentPlayerNumber = 1,
        Player = Player1,
        Func = Player1Func,
        Colour = 'blue',
        Depth = Depth1
    ;
        CurrentPlayerNumber = 2,
        Player = Player2,
        Func = Player2Func,
        Colour = 'green',
        Depth = Depth2
    ),
    write("turn: "), ansi_format([bold,fg(Colour)], '~w', [Player]), nl,
    call(Func, Board, CurrentPlayerNumber, PitIndex, Depth),
    ansi_format([bold,fg(Colour)], '~w', [Player]), ansi_format([bold,fg(cyan)], ' move is: ~w', [PitIndex]), nl,
    (
        do_move(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer),
        print_board(NewBoard, Player1, Player2, NewPlayer1Score, NewPlayer2Score),
        next_move(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score, Player1Func, Player2Func,Depth1,Depth2)
    ;
        ansi_format([bold,fg(red)], 'invalid move, try again', []), nl,
        play(Board, Player1, Player2, CurrentPlayerNumber, Player1Score, Player2Score, Player1Func, Player2Func,Depth1,Depth2) % play the turn again in case of invalid move
    ), !.

next_move(NewBoard, Player1, Player2, _, NewPlayer1Score, NewPlayer2Score, _, _, Depth1, Depth2):-
    check_winner(NewBoard, Player1, Player2, NewPlayer1Score, NewPlayer2Score),
    write(Player1),write(Depth1),write(": "), write(NewPlayer1Score), write(" "),
    write(Player2),write(Depth2),write(": "), write(NewPlayer2Score), nl, !. % someone won, the game end

next_move(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score, Player1Func, Player2Func, Depth1, Depth2):-
    play(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score, Player1Func, Player2Func, Depth1, Depth2), !. % play the next move

% do a move in the game
% PitIndex- the of the pit the player chose to play (starting from 1)
% NewBoard- the board adter playing the move
% NextPlayer- the player that need to play the next move (can be the same player again)
do_move(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    integer(PitIndex),
    nth1(CurrentPlayerNumber, Board, Row),
    valid_move(Row, PitIndex),
    replace_nth1(Row, PitIndex, PiecesCount, 0, NewRow),
    replace_nth1(Board, CurrentPlayerNumber, _, NewRow, TempBoard),
    NewPitIndex is PitIndex + 1,
    put_pieces(TempBoard, PiecesCount, CurrentPlayerNumber, NewPitIndex, CurrentPlayerNumber, 
            Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer), !.

% check if a given pit index is a valid move (the index in range and there are pieces in the pit)
% Row- the row of the player that want to do the move
valid_move(Row, PitIndex):-
    length(Row, RowLength),
    between(1, RowLength, PitIndex),
    nth1(PitIndex, Row, Pieces),
    Pieces > 0.

% put_pieces(Board, PiecesCount, RowIndex, PitIndex, CurrentPlayerNumber, Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):
% move the pieces from the played pit to the next pits in the board
% PiecesCount- amount of pieces left to put
% RowIndex- the index of the row the pieces need to be put in
% PitIndex- th index of the pit in the row the pieces need to be put in
% 
% put last piece in result pit, the player get another turn
put_pieces(Board, 0, _, 1, CurrentPlayerNumber, Player1Score, Player2Score, Board, Player1Score, Player2Score, NextPlayer):- 
    nth1(CurrentPlayerNumber, Board, Row),
    (
        sum_list(Row, 0),
        NextPlayer is (CurrentPlayerNumber mod 2) + 1
    ;
        NextPlayer = CurrentPlayerNumber
    ), !.

% put last piece, if in empty pit get other player piece from facing pit
put_pieces(Board, 0, RowIndex, PitIndex, CurrentPlayerNumber, Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    OtherPlayer is (CurrentPlayerNumber mod 2) + 1,
    (
        CurrentPlayerNumber = RowIndex,
        LastPitIndex is PitIndex - 1,
        nth1(RowIndex, Board, Row),
        replace_nth1(Row, LastPitIndex, 1, 0, NewRow),
        nth1(OtherPlayer, Board, SecondeRow),
        length(SecondeRow, SecondeRowLength),
        SecondePitIndex is SecondeRowLength - LastPitIndex + 1,
        replace_nth1(SecondeRow, SecondePitIndex, X, 0, NewSecondeRow),
        X > 0,
        (
            CurrentPlayerNumber = 1,
            NewPlayer1Score is Player1Score + X + 1,
            NewPlayer2Score = Player2Score,
            NewBoard = [NewRow, NewSecondeRow]
        ;
            CurrentPlayerNumber = 2,
            NewPlayer2Score is Player2Score + X + 1,
            NewPlayer1Score = Player1Score,
            NewBoard = [NewSecondeRow, NewRow]
        )
    ;
        NewPlayer1Score = Player1Score,
        NewPlayer2Score = Player2Score,
        NewBoard = Board
    ),
    nth1(OtherPlayer, NewBoard, CheckRow),
    (
        sum_list(CheckRow, 0),
        NextPlayer = CurrentPlayerNumber
    ;
        NextPlayer = OtherPlayer
    ), !.

% put pieces in same row
put_pieces(Board, PiecesCount, RowIndex, PitIndex, CurrentPlayerNumber,
           Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    nth1(RowIndex, Board, Row),
    length(Row, RowLength),
    PitIndex =< RowLength,
    nth1(PitIndex, Row, X),
    NX is X + 1,
    NewPiecesCount is PiecesCount - 1,
    NewPitIndex is PitIndex + 1,
    replace_nth1(Row, PitIndex, X, NX, NewRow),
    replace_nth1(Board, RowIndex, Row, NewRow, TempBoard),
    put_pieces(TempBoard, NewPiecesCount, RowIndex, NewPitIndex, CurrentPlayerNumber,
               Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer), !.
    
% move to the next row
put_pieces(Board, PiecesCount, RowIndex, PitIndex, CurrentPlayerNumber,
           Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    nth1(RowIndex, Board, Row),
    length(Row, RowLength),
    PitIndex =:= RowLength + 1,
    NewPitIndex = 1,
    NewRowIndex is (RowIndex mod 2) + 1,
    (
        CurrentPlayerNumber = RowIndex,
        NewPiecesCount is PiecesCount - 1,
        (
            CurrentPlayerNumber = 1,
            TempPlayer1Score is Player1Score + 1,
            TempPlayer2Score is Player2Score
        ;
            CurrentPlayerNumber = 2,
            TempPlayer2Score is Player2Score + 1,
            TempPlayer1Score is Player1Score
        )
    ;
        TempPlayer2Score is Player2Score,
        TempPlayer1Score is Player1Score,
        NewPiecesCount is PiecesCount
    ),
    put_pieces(Board, NewPiecesCount, NewRowIndex, NewPitIndex, CurrentPlayerNumber,
               TempPlayer1Score, TempPlayer2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer), !.

% check if there is a winner
check_winner(Board, Player1, Player2, Player1Score, Player2Score):-
    sum_board(Board, Sum),
    Sum =:= 0.
    % declare_winner(Player1, Player2, Player1Score, Player2Score), !.

% declare who the winner is
declare_winner(Player1, _, Player1Score, Player2Score):-
    Player1Score > Player2Score, !,
    write("The winner is "), write(Player1), write("!!!!"), nl.

declare_winner(_, Player2, Player1Score, Player2Score):-
    Player1Score < Player2Score, !,
    write("The winner is "), write(Player2), write("!!!!"), nl.

declare_winner(_, _, Player1Score, Player2Score):-
    Player1Score = Player2Score, !,
    write("Tie"), nl.

sum_board(Board, Sum):-
    nth1(1, Board, Row1),
    nth1(2, Board, Row2),
    sum_list(Row1, Sum1),
    sum_list(Row2, Sum2),
    Sum is Sum1 + Sum2.

% https://www.swi-prolog.org/pldoc/doc_for?object=nth0/4
replace_nth1(List, Index, OldElem, NewElem, NewList) :-
    % predicate works forward: Index,List -> OldElem, Transfer
    nth1(Index,List,OldElem,Transfer),
    % predicate works backwards: Index,NewElem,Transfer -> NewList
    nth1(Index,NewList,NewElem,Transfer), !.

% print the mancala board
print_board(Board, Player1, Player2, Player1Score, Player2Score):-
    ansi_format([bold,fg(blue)], '~w', [Player1]),  write(" score is: "), write(Player1Score), nl,
    ansi_format([bold,fg(green)], '~w', [Player2]),  write(" score is: "), write(Player2Score), nl,
    nth1(1, Board, Player1Row),
    reverse(Player1Row, ReversePlayer1Row),
    nth1(2, Board, Player2Row),
    ansi_format([bold,fg(blue)], '~w', [Player1]), write(" row: | "), write(ReversePlayer1Row), write(" | <-"), nl,
    ansi_format([bold,fg(green)], '~w', [Player2]), write(" row: | "), write(Player2Row), write(" | ->"), nl, !.

create_initialized_list(Length, Value, List):-
    length(List,Length), 
    maplist(=(Value), List), !.

% playin logic, play random valid pit
random_ai(Board, CurrentPlayerNumber, PitIndex):-
    nth1(CurrentPlayerNumber, Board, Row),
    findall(Pit, valid_move(Row, Pit), ValidMoves),
    random_member(PitIndex, ValidMoves), !.

random_ai(Board, CurrentPlayerNumber, PitIndex, _):-
    random_ai(Board, CurrentPlayerNumber, PitIndex), !.

% playing logic, play by user input
user_input(_, _, PitIndex):-
    write("enter Pit number to play: "),
    read(PitIndex), !.

user_input(_, _, PitIndex, _):-
    user_input(_, _, PitIndex), !.

% playing logic, play by alphabeta algorithm with depth of 9
alphabeta_ai(Board, CurrentPlayerNumber, PitIndex):-
    alphabeta_ai(Board, CurrentPlayerNumber, PitIndex, 9), !.

% playing logic, play by alphabeta algorithm
alphabeta_ai(Board, CurrentPlayerNumber, PitIndex, Depth):-
    set_prolog_flag(stack_limit, 6_442_450_944), % define stack size
    sum_board(Board, Beta),
    Alpha is -Beta,
    alphabeta(mancala_pos(Board, CurrentPlayerNumber, 0, 0, -1), Alpha, Beta, mancala_pos(_, _, _, _, PitIndex), _, Depth), !.

%  The alpha-beta algorithm (based on the book)
%  use static val when Depth is 0
alphabeta(Pos, _, _, Pos, Val, 0):-
    staticval(Pos, Val), !.

% general case alpha-beta algorithm
% Pos describe with mancala_pos(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex)
% the score use for the heuristic function
% PitIndex is the index of the pit played in the last turn (-1 if it's the first runing)
alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth):-
    moves(Pos, Poslist), !,
    (
        max_to_move(Pos),
        MaxPlayer = true
    ;
        min_to_move(Pos),
        MaxPlayer = false
    ),
    (
        boundedbest(Poslist, Alpha, Beta, GoodPos, Val, Depth, MaxPlayer)
    ;
        staticval(Pos, Val)
    ).

% MaxPlayer- true if the parent node is a max player
boundedbest([Pos | Poslist], Alpha, Beta, GoodPos, GoodVal, Depth, MaxPlayer):-
    NewDepth is Depth - 1,
    alphabeta(Pos, Alpha, Beta, _, Val, NewDepth),
    goodenough(Poslist, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth, MaxPlayer).

goodenough([], _, _, Pos, Val, Pos, Val, _, _):- !. % No other candidate

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _, MaxPlayer):-
    (
        MaxPlayer, Val > Beta, ! % Maximizer attained upper bound
    ;
       not(MaxPlayer), Val < Alpha, ! % Minimizer attained lower bound
    ).

goodenough(Poslist, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth, MaxPlayer):-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta, MaxPlayer), % Refine bounds
    boundedbest(Poslist, NewAlpha, NewBeta, Pos1, Val1, Depth, MaxPlayer),
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal, MaxPlayer).

newbounds(Alpha, Beta, _, Val, Val, Beta, MaxPlayer):-
    MaxPlayer, Val > Alpha, !. % Maximizer increased lower bound

newbounds(Alpha, Beta, _, Val, Alpha, Val, MaxPlayer) :-
    not(MaxPlayer), Val < Beta, !. % Minimizer decreased upper bound

newbounds(Alpha, Beta, _, _, Alpha, Beta, _). % Otherwise bounds unchanged

betterof(Pos, Val, _, Val1, Pos, Val, MaxPlayer) :- % Pos better than Pos1
        MaxPlayer, Val > Val1, !
    ;
        not(MaxPlayer), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1, _). % Otherwise Pos1 better

% heuristic function, calculate the value of given mancala_pos
staticval(mancala_pos(_, _, Player1Score, Player2Score, _), Val):-
    Val is Player1Score - Player2Score, !.

% player1 is the max player
max_to_move(mancala_pos(_, CurrentPlayerNumber, _, _, _)):-
    CurrentPlayerNumber = 1, !.

% player2 is the min player
min_to_move(mancala_pos(_, CurrentPlayerNumber, _, _, _)):-
    CurrentPlayerNumber = 2, !.

% Poslist contain all the possible positions that can be reached by playing a move from the given mancala_pos
moves(Pos, Poslist):-
    findall(ResultPos, result_posistions(Pos, ResultPos), Poslist).

% true if ResultPos is is a mancala_pos that can be reached from the given mancala_pos
result_posistions(mancala_pos(Board, CurrentPlayerNumber, Player1Score, Player2Score, _), ResultPos):-
    nth1(CurrentPlayerNumber, Board, Row),
    findall(PitIndex, valid_move(Row, PitIndex), ValidMoves),
    member(PitIndex, ValidMoves),
    do_move(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer),
    ResultPos = mancala_pos(NewBoard, NextPlayer, NewPlayer1Score, NewPlayer2Score, PitIndex).

% alphabeta_ai([[0,0,0,0,1,1],[0,0,0,0,0,0]], 1, PitIndex, 20).
% alphabeta_ai([[4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4]], 1, Pit, 1).
% alphabeta_ai([[1, 0, 3, 12, 0, 0], [0, 0, 0, 0, 0, 0]], 1, Pit, 9).
% alphabeta_ai([[1, 4, 1, 1, 0, 7], [3, 3, 0, 3, 9, 0]], 1, Pit, 9).
% alphabeta_ai([[5, 5, 5, 4, 4, 0], [5, 5, 5, 4, 4, 0]], 2, Pit, 9).
% alphabeta_ai( [[2, 0, 3, 1, 0, 1], [3, 3, 1, 1, 0, 0]], 2, Pit, 9).

check_results:-
    between(1, 9, D1),
    between(1, 9, D2),
    check_results(D1, D2).

check_results(D1, D2):-
    start_game('AI_', 'AI_', 6, 4, alphabeta_ai, alphabeta_ai, D1, D2).