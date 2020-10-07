:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).

start_game(Player1, Player2, PitsNumber, PiecesInPit, Player1Func, Player2Func):-
    create_initialized_list(PitsNumber, PiecesInPit, Row),
    create_initialized_list(2, Row, Board),
    % Board=[[4,4,0,1,0,4], [4,4,4,4,4,4]],
    Player1Score is 0,
    Player2Score is 0,
    print_board(Board, Player1, Player2, Player1Score, Player2Score),
    play(Board, Player1, Player2, 1, Player1Score, Player2Score, Player1Func, Player2Func), !.

start_game:-
    start_game('Maayan', 'CompAI', 6, 4, user_input, alphabeta_ai), !.

play(Board, Player1, Player2, CurrentPlayerNumber, Player1Score, Player2Score, Player1Func, Player2Func):-
    (
        CurrentPlayerNumber = 1,
        Player = Player1,
        Func = Player1Func,
        Colour = 'blue'
    ;
        CurrentPlayerNumber = 2,
        Player = Player2,
        Func = Player2Func,
        Colour = 'green'
    ),
    write("turn: "), ansi_format([bold,fg(Colour)], '~w', [Player]), nl,
    call(Func, Board, CurrentPlayerNumber, PitIndex),
    ansi_format([bold,fg(Colour)], '~w', [Player]), ansi_format([bold,fg(cyan)], ' move is: ~w', [PitIndex]), nl,
    (
        do_move(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer),
        print_board(NewBoard, Player1, Player2, NewPlayer1Score, NewPlayer2Score),
        next_move(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score, Player1Func, Player2Func)
    ;
        ansi_format([bold,fg(red)], 'unvalid move, try again', []), nl,
        play(Board, Player1, Player2, CurrentPlayerNumber, Player1Score, Player2Score, Player1Func, Player2Func)
    ), !.

next_move(NewBoard, Player1, Player2, _, NewPlayer1Score, NewPlayer2Score, _, _):-
    check_winner(NewBoard, Player1, Player2, NewPlayer1Score, NewPlayer2Score), !.

next_move(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score, Player1Func, Player2Func):-
    play(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score, Player1Func, Player2Func), !.

do_move(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    integer(PitIndex),
    nth1(CurrentPlayerNumber, Board, Row),
    valid_move(Row, PitIndex),
    replace_nth1(Row, PitIndex, PiecesCount, 0, NewRow),
    replace_nth1(Board, CurrentPlayerNumber, _, NewRow, TempBoard),
    NewPitIndex is PitIndex + 1,
    put_pieces(TempBoard, PiecesCount, CurrentPlayerNumber, NewPitIndex, CurrentPlayerNumber, 
            Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer), !.

valid_move(Row, PitIndex):-
    length(Row, RowLength),
    between(1, RowLength, PitIndex),
    nth1(PitIndex, Row, Elem),
    Elem > 0.

put_pieces(Board, 0, _, 1, CurrentPlayerNumber, Player1Score, Player2Score, Board, Player1Score, Player2Score, NextPlayer):-
    nth1(CurrentPlayerNumber, Board, Row),
    (
        sum_list(Row, 0),
        NextPlayer is (CurrentPlayerNumber mod 2) + 1
    ;
        NextPlayer = CurrentPlayerNumber
    ), !.

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

check_winner(Board, Player1, Player2, Player1Score, Player2Score):-
    sum_board(Board, Sum),
    Sum =:= 0,
    (
        Player1Score > Player2Score,
        write("The winner is "), write(Player1), write("!!!!"), nl
    ;
        Player2Score > Player2Score,
        write("The winner is "), write(Player2), write("!!!!"), nl
    ;
        Player1Score = Player2Score,
        write("Tie"), nl
    ), !.

sum_board(Board, Sum):-
    nth1(1, Board, Row1),
    nth1(2, Board, Row2),
    sum_list(Row1, Sum1),
    sum_list(Row2, Sum2),
    Sum is Sum1 + Sum2.

% % https://www.swi-prolog.org/pldoc/man?predicate=nth0/4
replace_nth1(List, Index, OldElem, NewElem, NewList) :-
    % predicate works forward: Index,List -> OldElem, Transfer
    nth1(Index,List,OldElem,Transfer),
    % predicate works backwards: Index,NewElem,Transfer -> NewList
    nth1(Index,NewList,NewElem,Transfer), !.

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

random_ai(Board, CurrentPlayerNumber, PitIndex):-
    nth1(CurrentPlayerNumber, Board, Row),
    findall(Pit, valid_move(Row, Pit), ValidMoves),
    random_member(PitIndex, ValidMoves).

user_input(_, _, PitIndex):-
    write("enter Pit number to play: "),
    read(PitIndex).

alphabeta_ai(Board, CurrentPlayerNumber, PitIndex):-
    alphabeta_ai(Board, CurrentPlayerNumber, PitIndex, 20), !.

alphabeta_ai(Board, CurrentPlayerNumber, PitIndex, Depth):-
    set_prolog_flag(stack_limit, 6_147_483_648),
    sum_board(Board, Beta),
    Alpha is -Beta,
    alphabeta(mancala_pos(Board, CurrentPlayerNumber, 0, 0, -1), Alpha, Beta, mancala_pos(_, _, _, _, PitIndex), _, Depth), !.

%  The alpha-beta algorithm
alphabeta(Pos, _, _, Pos, Val, 0):-
    staticval(Pos, Val), !.


alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth):-
    moves(Pos, Poslist), !,
    % write("Poslist: "), write(Poslist), nl,
    (
        boundedbest(Poslist, Alpha, Beta, GoodPos, Val, Depth)
    ;
        staticval(Pos, Val)
    ).

boundedbest([Pos | Poslist], Alpha, Beta, GoodPos, GoodVal, Depth):-
    NewDepth is Depth - 1,
    alphabeta(Pos, Alpha, Beta, _, Val, NewDepth),
    % write("Pos: "), write(Pos), write(" Val: "), write(Val), nl,
    goodenough(Poslist, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).
    % write("GoodPoss: "), write(GoodPos), write(" GoodVal: "), write(GoodVal), nl.

% goodenough(_, _, _, Pos, Val, Pos, Val, 0):- !. % End of depth

goodenough([], _, _, Pos, Val, Pos, Val, _):- !. % No other candidate

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _):-
    % write("Alpha: "), write(Alpha), write(" Beta: "), write(Beta), write(" Val: "), write(Val), write(" Pos: "), write(Pos),  nl,
    (
        min_to_move(Pos), Val > Beta, ! % Maximizer attained upper bound
    ;
        max_to_move(Pos), Val < Alpha, ! % Minimizer attained lower bound
    ).

goodenough(Poslist, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth):-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta), % Refine bounds
    boundedbest(Poslist, NewAlpha, NewBeta, Pos1, Val1, Depth),
    % write("Pos: "), write(Pos), write(" Val: "), write(Val), nl,
    % write("Pos1: "), write(Pos1), write(" Val1: "), write(Val1), nl,
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).
    % write("GoodPos: "), write(GoodPos), write(" GoodVal: "), write(GoodVal), nl.

newbounds(Alpha, Beta, Pos, Val, Val, Beta):-
    min_to_move(Pos), Val > Alpha, !. % Maximizer increased lower bound

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
    max_to_move(Pos), Val < Beta, !. % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta). % Otherwise bounds unchanged

betterof(Pos, Val, _, Val1, Pos, Val) :- % Pos better than Pos1
        min_to_move(Pos), Val > Val1, !
    ;
        max_to_move(Pos), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better

staticval(mancala_pos(_, _, Player1Score, Player2Score, _), Val):-
    Val is Player1Score - Player2Score, !.
    % write("Val: "), write(Val), nl.

max_to_move(mancala_pos(_, CurrentPlayerNumber, _, _, _)):-
    CurrentPlayerNumber = 1, !.

min_to_move(mancala_pos(_, CurrentPlayerNumber, _, _, _)):-
    CurrentPlayerNumber = 2, !.

moves(mancala_pos(Board, CurrentPlayerNumber, Player1Score, Player2Score, _), Poslist):-
    nth1(CurrentPlayerNumber, Board, Row),
    findall(PitIndex, valid_move(Row, PitIndex), ValidMoves),
    findall(ResultPos, result_posistions(mancala_pos(Board, CurrentPlayerNumber, Player1Score, Player2Score, _), ValidMoves, ResultPos), Poslist).

result_posistions(mancala_pos(Board, CurrentPlayerNumber, Player1Score, Player2Score, _), ValidMoves, ResultPos):-
    member(PitIndex, ValidMoves),
    do_move(Board, CurrentPlayerNumber, Player1Score, Player2Score, PitIndex, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer),
    ResultPos = mancala_pos(NewBoard, NextPlayer, NewPlayer1Score, NewPlayer2Score, PitIndex).

% alphabeta_ai([[0,0,0,0,1,1],[0,0,0,0,0,0]], 1, PitIndex, 20).