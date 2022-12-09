% Board is represented as a list of columns, where each column is a list of cells
% (empty cell is represented as `0`, player 1 is represented as `1`, and player 2 is represented as `2`)
%
% Example:
% [
%   [0, 0, 0, 0, 0, 0], % column 1
%   [0, 0, 0, 0, 0, 0], % column 2
%   [0, 0, 0, 0, 0, 0], % column 3
%   [0, 0, 0, 0, 0, 0], % column 4
%   [0, 0, 0, 0, 0, 0], % column 5
%   [0, 0, 0, 0, 0, 0], % column 6
%   [0, 0, 0, 0, 0, 0]  % column 7
% ]

% Initialize the board
init_board(Board) :-
    Board = [
        [0, 0, 0, 0, 0, 0], % column 1
        [0, 0, 0, 0, 0, 0], % column 2
        [0, 0, 0, 0, 0, 0], % column 3
        [0, 0, 0, 0, 0, 0], % column 4
        [0, 0, 0, 0, 0, 0], % column 5
        [0, 0, 0, 0, 0, 0], % column 6
        [0, 0, 0, 0, 0, 0]  % column 7
    ].

% Place a piece on the board
%
% Arguments:
%   Board: current state of the board
%   Column: index of the column where to place the piece
%   Player: player number (1 or 2)
%   NewBoard: new state of the board after the piece is placed
place_piece(Board, Column, Player, NewBoard) :-
    nth1(Column, Board, ColumnList), % get the column where to place the piece
    append(Top, [Cell|Bottom], ColumnList), % split the column into top and bottom parts
    (
        % if the bottom cell is empty, place the piece there
        Cell = 0
        -> NewColumn = [Cell|Bottom],
           NewBoard = [Top|NewColumn|Bottom]
        ;
        % if the bottom cell is not empty, try placing the piece in the next row
        place_piece(Board, Column, Player, NewBoard)
    ).

% Check if the game is over
%
% Arguments:
%   Board: current state of the board
%   Result: one of the following values:
%       0: game is not over yet
%       1: player 1 wins
%       2: player 2 wins
%       3: game is a draw
game_over(Board, Result) :-
    % check if player 1 has won
    member(Row, Board),
    append(_, [1,1,1,1|_], Row), % player 1 has 4 consecutive pieces in a row
    Result = 1,
    !.
game_over(Board, Result) :-
    % check if player 2 has won
    member(Row, Board),
    append(_, [2,2,2,2|_], Row), % player 2 has 4 consecutive pieces in a column
    Result = 2,
    !.
game_over(Board, Result) :-
    % check if the game is a draw (all cells are occupied)
    flatten(Board, Cells),
    \+ member(0, Cells), % no empty cells
    Result = 3,
    !.
game_over(_, 0). % game is not over yet

% Minimax algorithm
%
% Arguments:
%   Board: current state of the board
%   Player: player number (1 or 2)
%   Depth: search depth (number of moves to look ahead)
%   Alpha: alpha value (used for alpha-beta pruning)
%   Beta: beta value (used for alpha-beta pruning)
%   Score: minimax score for the current player
%   Column: index of the column where to place the piece (chosen by the minimax algorithm)
minimax(Board, Player, Depth, Alpha, Beta, Score, Column) :-
    game_over(Board, Result), % check if the game is over
    (
        % if the game is over, return the score for the current player
        Result = 1
        -> Score = -100000 % player 1 wins -> score is -100000 (lowest possible score)
        ; Result = 2
        -> Score = 100000 % player 2 wins -> score = 100000 (highest possible score)
        ; Result = 3
        -> Score = 0 % draw -> score = 0
    ),
    !.
minimax(Board, Player, Depth, Alpha, Beta, Score, Column) :-
    Depth > 0, % continue searching if the search depth is not reached yet
    NextPlayer is 3 - Player, % switch players (1 -> 2, 2 -> 1)
    NewDepth is Depth - 1, % decrease the search depth
    (
        Player = 1 % player 1 is maximizing player -> choose the column with the highest score
        -> findall((NewBoard, NewScore), (
            member(Column, [1,2,3,4,5,6,7]), % try all columns
            place_piece(Board, Column, Player, NewBoard), % place a piece in the current column
            minimax(NewBoard, NextPlayer, NewDepth, Alpha, Beta, NewScore, _) % get the score for the new board state
        ), Results),
        best_column(Results, Alpha, Beta, Score, Column) % choose the column with the highest score
        ; Player = 2 % player 2 is minimizing player -> choose the column with the lowest score
        -> findall((NewBoard, NewScore), (
            member(Column, [1,2,3,4,5,6,7]), % try all columns
            place_piece(Board, Column, Player, NewBoard), % place a piece in the current column
            minimax(NewBoard, NextPlayer, NewDepth, Alpha, Beta, NewScore, _) % get the score for the new board state
        ), Results),
        worst_column(Results, Alpha, Beta, Score, Column) % choose the column with the lowest score
% Choose the column with the highest score
%
% Arguments:
%   Results: list of (board, score) pairs
%   Alpha: alpha value (used for alpha-beta pruning)
%   Beta: beta value (used for alpha-beta pruning)
%   Score: score of the chosen column
%   Column: index of the chosen column
best_column([(Board, Score)|Results], Alpha, Beta, BestScore, BestColumn) :-
    best_column(Results, Alpha, Beta, Score, Board, BestScore, BestColumn).
best_column([], _, _, BestScore, _, BestScore, _).
best_column([(Board, Score)|Results], Alpha, Beta, PrevScore, PrevBoard, BestScore, BestColumn) :-
    (
        Score > PrevScore % current score is higher than the previous best score
        -> NewBestScore is Score,
           NewBestColumn is Board,
           NewAlpha is max(Alpha, NewBestScore) % update alpha value
        ; Score = PrevScore % current score is equal to the previous best score
        -> NewBestScore is PrevScore,
           NewBestColumn is PrevBoard,
           NewAlpha is Alpha
    ),
    NewBeta is min(Beta, NewBestScore), % update beta value
    (
        NewAlpha >= NewBeta % alpha-beta pruning
        -> BestScore = NewBestScore,
           BestColumn = NewBestColumn
        ; best_column(Results, NewAlpha, NewBeta, NewBestScore, NewBestColumn, BestScore, BestColumn)
    ).

% Choose the column with the lowest score
%
% Arguments:
%   Results: list of (board, score) pairs
%   Alpha: alpha value (used for alpha-beta pruning)
%   Beta: beta value (used for alpha-beta pruning)
%   Score: score of the chosen column
%   Column: index of the chosen column
worst_column([(Board, Score)|Results], Alpha, Beta, WorstScore, WorstColumn) :-
    worst_column(Results, Alpha, Beta, Score, Board, WorstScore, WorstColumn).
worst_column([], _, _, WorstScore, _, WorstScore, _).
worst_column([(Board, Score)|Results], Alpha, Beta, PrevScore, PrevBoard, WorstScore, WorstColumn) :-
    (
        Score < PrevScore % current score is lower than the previous worst score
        -> NewWorstScore is Score,
           NewWorstColumn is Board,
           NewBeta is min(Beta, NewWorstScore) % update beta value
        ; Score = PrevScore % current score is equal to the previous worst score
        -> NewWorstScore is PrevScore,
           NewWorstColumn is PrevBoard,
           NewBeta is Beta
    ),
    NewAlpha is max(Alpha, NewWorstScore), % update alpha value
    (
        NewAlpha >= NewBeta % alpha-beta pruning
        -> WorstScore = NewWorstScore,
           WorstColumn = NewWorstColumn
        ; worst_column(Results, NewAlpha, NewBeta, NewWorstScore, NewWorstColumn, WorstScore, WorstColumn)
    ).
	
% Play a game of Connect 4 against the minimax AI
%
% Arguments:
%   Player: player number (1 or 2)
%   Depth: search depth (number of moves to look ahead)
play(Player, Depth) :-
    init_board(Board), % initialize the board
    play(Board, Player, Depth).

% Play a game of Connect 4 against the minimax AI
%
% Arguments:
%   Board: current state of the board
%   Player: player number (1 or 2)
%   Depth: search depth (number of moves to look ahead)
play(Board, Player, Depth) :-
    game_over(Board, Result), % check if the game is over
    (
        Result = 1 % player 1 wins
        -> writeln('Player 1 wins!')
        ; Result = 2 % player 2 wins
        -> writeln('Player 2 wins!')
        ; Result = 3 % game is a draw
        -> writeln('Game is a draw!')
    ),
    !.
play(Board, Player, Depth) :-
    (
        Player = 1 % player 1's turn
        -> write('Player 1, choose a column (1-7): '),
           read(Column),
           place_piece(Board, Column, Player, NewBoard) % place a piece in the chosen column
        ; Player = 2 % player 2's turn
        -> minimax(Board, Player, Depth, -100000, 100000, _, Column), % choose the best column using the minimax algorithm
           place_piece(Board, Column, Player, NewBoard) % place a piece in the chosen column
    ),
    print_board(NewBoard), % print the updated board
    NextPlayer is 3 - Player, % switch players (1 -> 2, 2 -> 1)
    play(NewBoard, NextPlayer, Depth). % continue the game

% Print the current state of the board
%
% Arguments:
%   Board: current state of the board
print_board(Board) :-
    write('-------------------'), nl,
    print_board(Board, 5). % start from the bottom row

% Print the current state of the board
%
% Arguments:
%   Board: current state of the board
%   Row: index of the current row
print_board(_, -1) :-
    write('-------------------'), nl. % done printing the board
print_board(Board, Row) :-
    Row >= 0,
    print_row(Board, Row), % print the current row
    NewRow is Row - 1, % move to the next row
    print_board(Board, NewRow). % print the next row

% Print a row of the board
%
% Arguments:
%   Board: current state of the board
%   Row: index of the row to print
print_row(Board, Row) :-
    write('|'),
    print_row(Board, Row, 0). % start from the first column
% Print a row of the board
%
% Arguments:
%   Board: current state of the board
%   Row: index of the row to print
%   Column: index of the current column
print_row(_, _, 7) :-
    write('|'), nl. % done printing the row
print_row(Board, Row, Column) :-
    Column < 7,
    nth1(Column, Board, ColumnList), % get the current column
    nth1(Row, ColumnList, Cell), % get the current cell
    (
        Cell = 0 % empty cell
        -> write(' |')
        ; Cell = 1 % player 1's piece
        -> write('X|')
        ; Cell = 2 % player 2's piece
        -> write('O|')
    ),
    NewColumn is Column + 1, % move to the next column
    print_row(Board, Row, NewColumn). % print the next column