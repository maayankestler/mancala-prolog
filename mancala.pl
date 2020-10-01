:- use_module(library(lists)).
:- use_module(library(clpfd)).

start_game(Player1, Player2, PitsNumber, PiecesInPit):-
    % create_initialized_list(PitsNumber, PiecesInPit, Row),
    % create_initialized_list(2, Row, Board),
    Board=[[0,0,0,0,3,0], [0,0,0,0,0,0]],
    Player1Score is 0,
    Player2Score is 0,
    print_board(Board, Player1, Player2, Player1Score, Player2Score),
    play(Board, Player1, Player2, Player1, Player1Score, Player2Score), !.

start_game:-
    start_game('Maayan', 'CompAI', 6, 4), !.


play(Board, Player1, Player2, CurrentPlayer, Player1Score, Player2Score):-
    (
        CurrentPlayer = Player1,
        RowIndex = 1,
        nth1(RowIndex, Board, Row),
        sumlist(Row, 0),
        write(Player1), write(" can't move "), nl,
        play(Board, Player1, Player2, Player2, Player1Score, Player2Score)
    ;
        CurrentPlayer = Player2,
        RowIndex = 2,
        nth1(RowIndex, Board, Row),
        sumlist(Row, 0),
        write(Player2), write(" can't move "), nl,
        play(Board, Player1, Player2, Player1, Player1Score, Player2Score)
    ), !.

play(Board, Player1, Player2, CurrentPlayer, Player1Score, Player2Score):-
    (
        CurrentPlayer = Player1,
        Colour = 'blue'
    ;
        CurrentPlayer = Player2,
        Colour = 'green'
    ),
    write("turn: "),
    ansi_format([bold,fg(Colour)], '~w', [CurrentPlayer]), nl,
    write("enter pit number to play: "),
    read(Pit), % ai func
    Pit \= 'exit', % if the user enter exit as input the game will stop
    (
        do_move(Board, Player1, Player2, CurrentPlayer, Player1Score, Player2Score, Pit, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer),
        print_board(NewBoard, Player1, Player2, NewPlayer1Score, NewPlayer2Score),
        next_move(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score)
    ;
        ansi_format([bold,fg(red)], 'unvalid move, try again', []), nl,
        play(Board, Player1, Player2, CurrentPlayer, Player1Score, Player2Score)
    ), !.

next_move(NewBoard, Player1, Player2, _, NewPlayer1Score, NewPlayer2Score):-
    check_winner(NewBoard, Player1, Player2, NewPlayer1Score, NewPlayer2Score), !.

next_move(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score):-
    play(NewBoard, Player1, Player2, NextPlayer, NewPlayer1Score, NewPlayer2Score), !.

do_move(Board, Player1, Player2, CurrentPlayer, Player1Score, Player2Score, Pit, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    integer(Pit),
    Pit > 0,
    (
        CurrentPlayer = Player1,
        RowIndex = 1
    ;
        CurrentPlayer = Player2,
        RowIndex = 2
    ),
    nth1(RowIndex, Board, Row),
    replace_nth1(Row, Pit, PiecesCount, 0, NewRow),
    PiecesCount > 0,
    replace_nth1(Board, RowIndex, _, NewRow, TempBoard),
    NewPitIndex is Pit + 1,
    put_pieces(TempBoard, PiecesCount, RowIndex, NewPitIndex, Player1, Player2, CurrentPlayer, 
            Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer), !.

put_pieces(Board, 0, _, 1, _, _, CurrentPlayer, Player1Score, Player2Score, Board, Player1Score, Player2Score, CurrentPlayer):- !.

put_pieces(Board, 0, RowIndex, PitIndex, Player1, Player2, CurrentPlayer, Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    (
        CurrentPlayer = Player1,
        NextPlayer = Player2,
        (
            RowIndex =:= 1,
            SecondeRowIndex is 2,
            IsPlayerRow = true
        ;
            RowIndex =:= 2,
            IsPlayerRow = false
        )
    ;
        CurrentPlayer = Player2,
        NextPlayer = Player1,
        (
            RowIndex =:= 2,
            SecondeRowIndex is 1,
            IsPlayerRow = true
        ;
            RowIndex =:= 1,
            IsPlayerRow = false
        )
    ),
    LastPitIndex is PitIndex - 1,
    nth1(RowIndex, Board, Row),
    (
        IsPlayerRow,
        replace_nth1(Row, LastPitIndex, 1, 0, NewRow),
        nth1(SecondeRowIndex, Board, SecondeRow),
        length(SecondeRow, SecondeRowLength),
        SecondePitIndex is SecondeRowLength - LastPitIndex + 1,
        replace_nth1(SecondeRow, SecondePitIndex, X, 0, NewSecondeRow),
        X > 0,
        (
            CurrentPlayer = Player1,
            NewPlayer1Score is Player1Score + X + 1,
            NewPlayer2Score is Player2Score,
            NewBoard = [NewRow, NewSecondeRow]
        ;
            CurrentPlayer = Player2,
            NewPlayer2Score is Player2Score + X + 1,
            NewPlayer1Score is Player1Score,
            NewBoard = [NewSecondeRow, NewRow]
        )
    ;
        NewPlayer1Score is Player1Score,
        NewPlayer2Score is Player2Score,
        NewBoard = Board
    ), !.

put_pieces(Board, PiecesCount, RowIndex, PitIndex, Player1, Player2, CurrentPlayer,
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
    put_pieces(TempBoard, NewPiecesCount, RowIndex, NewPitIndex, Player1, Player2, CurrentPlayer,
               Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer), !.
    
put_pieces(Board, PiecesCount, RowIndex, PitIndex, Player1, Player2, CurrentPlayer,
           Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer):-
    nth1(RowIndex, Board, Row),
    length(Row, RowLength),
    PitIndex =:= RowLength + 1,
    NewPitIndex = 1,
    (
        CurrentPlayer = Player1,
        (
            RowIndex = 1,
            TempPlayer1Score is Player1Score + 1,
            NewPiecesCount is PiecesCount - 1,
            NewRowIndex = 2,
            put_pieces(Board, NewPiecesCount, NewRowIndex, NewPitIndex, Player1, Player2, CurrentPlayer,
                       TempPlayer1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)
        ;
            RowIndex = 2,
            NewRowIndex = 1,
            put_pieces(Board, PiecesCount, NewRowIndex, NewPitIndex, Player1, Player2, CurrentPlayer,
                       Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)
        )
    ;
        CurrentPlayer = Player2,
        (
            RowIndex = 2,
            TempPlayer2Score is Player2Score + 1,
            NewPiecesCount is PiecesCount - 1,
            NewRowIndex = 1,
            put_pieces(Board, NewPiecesCount, NewRowIndex, NewPitIndex, Player1, Player2, CurrentPlayer,
                       Player1Score, TempPlayer2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)
        ;
            RowIndex = 1,
            NewRowIndex = 2,
            put_pieces(Board, PiecesCount, NewRowIndex, NewPitIndex, Player1, Player2, CurrentPlayer,
                       Player1Score, Player2Score, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)
        )
    ), !.

check_winner(Board, Player1, Player2, Player1Score, Player2Score):-
    nth1(1, Board, Row1),
    nth1(2, Board, Row2),
    sum_list(Row1, Sum1),
    sum_list(Row2, Sum2),
    0 =:= Sum1 + Sum2,
    (
        Player1Score > Player2Score,
        write("The winner is "), write(Player1), write("!!!!"), nl
    ;
        Player2Score > Player2Score,
        write("The winner is "), write(Player2), write("!!!!"), nl
    ;
        write("Tie"), nl
    ), !.

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

% sublist( Sublist, List ) :-
%     append( [_, Sublist, _], List ), !.
