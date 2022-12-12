% Copyright 2016 Ramon Viñas, Marc Roig
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

:- use_module(library(random)).
:- dynamic place_piece/4.
%%%%%%%%%%%%%%%%%%
%%%%% BOARD %%%%%%
%%%%%%%%%%%%%%%%%%
%Initialize empty board (matrix of dimensions [columns=7, rows=6]. This board representation will make gameplay easier than if we used [rows, columns])
initial(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']])).

%%%%%%%%%%%%%%%%%%
%%% SHOW BOARD %%%
%%%%%%%%%%%%%%%%%%
%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):- showLine(X,N,X2),
	     Ns is N-1,
	     iShow(X2,Ns).

%showLine(X,N,X2) writes N and shows first line of board X (first element of every column). X2 is X without the shown line.
showLine(X,N,X2):- write(N), write(' '),
		   iShowLine(X,X2), nl.

%iShowLine(X,X2) writes first element of every column. X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):- write(X), write(' '),
			          iShowLine(XS,XS2).

%%%%%%%%%%%%%%%%%%
%%%% GAMEPLAY %%%%
%%%%%%%%%%%%%%%%%%
% Initializes board and starts the game
connect4:- initial(X),
	   show(X),
	   nextMove('X',X), !.

%nextMove(J,X) J is the player that needs to move ('O' or 'X') and X is the board. Checks if the game has finished. If it hasn't finished, performs next move.
nextMove('X',X):- wins('O',X),
		  write('Machine O wins!').
nextMove('O',X):- wins('X',X),
		  write('Machine X win!').
nextMove(_,X):- full(X),
		write('Draw').
%nextMove('X',X):- repeat, %repeats in case a column is full
%		  readColumn(C),
%		  play('X',C,X,X2), !,
%		  show(X2),
%		  nextMove('O',X2). 
% nextMove('X',X):- machine('X','O',X,X2),
		  % show(X2),
		  % nextMove('O',X2).
% nextMove('O',X):- machine('O','X',X,X2),
		  % show(X2),
		  % nextMove('X',X2).

% 'X' is player 1		  
nextMove(Player, Board):- 
		  play(Board, NewBoard, Player, 3),
		  show(NewBoard),
		  change_player(Player, Opponent),
		  nextMove(Opponent, NewBoard).


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
minimax(Board, Player, _, _, _, Score, _) :-
	wins(Player, Board),
	Score is 10000, !.
	
minimax(Board, Player, _, _, _, Score, _) :-
	change_player(Player, Opponent),
	wins(Opponent, Board),
	Score is -10000, !.
	
minimax(Board, _, _, _, _, Score, _) :-
	full(Board),
	Score is 0, !.	

minimax(Board, Player, Depth, _, _, Score, Column) :-
	Depth = 0,
	score_Adjacency(Board, Column,Player, Score).  

minimax(Board, Player, Depth, Alpha, Beta, Score, Column) :-
    Depth > 0, % continue searching if the search depth is not reached yet
    change_player(Player, NextPlayer),
    NewDepth is Depth - 1, % decrease the search depth
    (
        Player = 'X' % player 1 is maximizing player -> choose the column with the highest score
        -> findall((NewBoard, NewScore), (
            member(Column, [0,1,2,3,4,5,6]), % try all columns
            assert(place_piece(Board, Column, Player, NewBoard)), % place a piece in the current column
            minimax(NewBoard, NextPlayer, NewDepth, Alpha, Beta, NewScore, _), % get the score for the new board state
			retract(place_piece(Board, Column, Player, NewBoard))
		), Results),
        best_column(Results, Alpha, Beta, Score, Column) % choose the column with the highest score
        ; Player = 'O' % player 2 is minimizing player -> choose the column with the lowest score
        -> findall((NewBoard, NewScore), (
            member(Column, [0,1,2,3,4,5,6]), % try all columns
            assert(place_piece(Board, Column, Player, NewBoard)), % place a piece in the current column
            minimax(NewBoard, NextPlayer, NewDepth, Alpha, Beta, NewScore, _), % get the score for the new board state
			retract(place_piece(Board, Column, Player, NewBoard))
		), Results),
        worst_column(Results, Alpha, Beta, Score, Column) % choose the column with the lowest score
	).


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

play(Board, NewBoard, Player, Depth) :-
        minimax(Board, Player, Depth, -100000, 100000, _, Column), % choose the best column using the minimax algorithm
        place_piece(Player, Column, Board, NewBoard). % place a piece in the chosen column

%play(X,P,T,T2) is satisfied if T2 is the board T after player X moves in column P
place_piece(X,P,board(T),board(T2)):- append(I,[C|F],T),
			       length(I,P), 
		               playColumn(X,C,C2),
			       append(I,[C2|F],T2).

%playColumn(X,C,C2) is satisfied if column C2 is column C after player X plays there
playColumn(X,['-'],[X]):- !. % last spot in column
playColumn(X,['-',A|AS],[X,A|AS]):- A \== ('-'), !. % play above someone's piece
playColumn(X,['-'|AS],['-'|AS2]):- playColumn(X,AS,AS2). % descend column

% 'X' is player 1 and 'O' is player 2
change_player('X','O').
change_player('O','X').

%wins(X,T) is satisfied if player X has won in board T
%check if there's a column in T with 4 connected pieces of player X
wins(X,board(T)):- append(_, [C|_], T), % check if there's a column...
	           append(_,[X,X,X,X|_],C). % ...which has 4 connected pieces of player X
%check if there's a row in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M), length(I2,M), length(I3,M), length(I4,M). %...and every piece is in the same height
%check if there's a diagonal (type \) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1. %...and every piece is within the same diagonal \
%check if there's a diagonal (type /) in T with 4 connected pieces of player X
wins(X,board(T)):- append(_,[C1,C2,C3,C4|_],T), % check if 4 connected columns exists in board...
		   append(I1,[X|_],C1), %...such that all of them contain a piece of player X...
		   append(I2,[X|_],C2),
		   append(I3,[X|_],C3),
		   append(I4,[X|_],C4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1. %...and every piece is within the same diagonal /
						
%full(T) is satisfied if there isn't any free spot ('-')
full(board(T)):- \+ (append(_,[C|_],T),
		 append(_,['-'|_],C)).
		 
		 
%%%%%%%%%%%%%%%%%%%%%
%ADJACENCY HEURISTIC%
%%%%%%%%%%%%%%%%%%%%%
score_Adjacency(Board, ColIndex,Player, Score) :-
	findall(count,caseAdjacente(Board, ColIndex ,Player),N),
	N1 is N-1,
	Score is N1^2.
score_Adjacency(_,_,_,0).

case_Adjacent(board(T), ColIndex ,Player) :-
	nth0 (ColIndex, T, Column),
	get_position_char(Player, Column, RowIndex),
	ColIndex2 is ColIndex+1, ColIndex1 is ColIndex-1,
	RowIndex2 is RowIndex+1, RowIndex1 is RowIndex-1,
	between(ColIndex1,ColIndex2,ColIndexAdj), between(RowIndex1,RowIndex2,RowIndexAdj),	
	player_token(Player, ColIndexAdj,RowIndexAdj,T).
	
%%%%%%%%%%%%%%%%%%%
%DENSITY HEURISTIC%
%%%%%%%%%%%%%%%%%%%
score_density(Player,Score,Board(T)) :- Zone is 1, scoreNbPoints(Player,T,Zone,Score).
score_density(_,_,0,_).
scoreNbPoints(_,_,Zone,Score) :- Zone>6, Score is 0.
scoreNbPoints(Player,T,Zone,Score) :- nbPointsZone(Player,T,Zone,S), NewZone is Zone+1, scoreNbPoints(Player,T,NewZone,NextScore), Score is S+NextScore.
nbPointsZone(Player,T,Zone,ScoreZone) :- nbPionsZone(Player,T,Zone,NbTokens), ScoreZone is NbTokens^2.

nbPionsZone(Player,T,Zone,NbTokens) :-
	findall(count,allZone(Zone,Player,T),NbTokens).

allZone(Zone,Joueur,T) :- player_token(Player, ColIndexAdj,RowIndexAdj,T), zone(Zone,ColIndexAdj,RowIndexAdj).
zone(1,X,Y) :- X =<3, Y =< 3.
zone(2,X,Y) :- X = 4, Y =< 3.
zone(3,X,Y) :- X > 4, Y =< 3.
zone(4,X,Y) :- X > 4, Y > 3.
zone(5,X,Y) :- X = 4, Y > 3.
zone(6,X,Y) :- X =<3, Y > 3.

%%%%%%%%%%%%%%%%%%
%%% READ MOVES %%%
%%%%%%%%%%%%%%%%%%
%reads a column
readColumn(C):- nl, write('Column: '),
		repeat,
		get_char(L),
		associateColumn(L,C),
		col(C), !.

%associateColumn(L,C) column C is the column associated with char L
associateColumn(L,C):- atom_codes(L,[La|_]),
		       C is La - 65.

%associateChar(L, C) char L is the char associated with column C
associateChar(L, C):- Ln is 65+C,
		      atom_codes(L,[Ln]).

%valid columns
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).

get_position_char(Player, [Player|_],0).

get_position_char(Player, [X|L], Position):-
	X \== Player,
	NewPos is Position-1,
	get_position_char(Player, L, NewPos).

player_token(Player, ColIndexAdj,RowIndexAdj,Board):-
	nth0(ColIndexAdj, Board, Column),
	nth0(RowIndexAdj, Column, Player).

% We consider that a good move is one allowing us to win in a column. Further improvements: rows and diagonals.
goodMove(R,Col,board(T)):- append(I,[C|_],T),
			   length(I,Col),
			   maxConnected(R,C,MaxConn),
			   MaxConn >= 4.						

% maxConnected(R,C,MaxConn) MaxConn is the maximum number of connected pieces that player R has/could have in column C
maxConnected(_,[],0).
maxConnected(R,[X|_],0):- X\=R.
maxConnected(R,['-'|X],N):- maxConnected(R,X,Ns),
			    N is Ns+1.
maxConnected(R,[R|X],N):- maxConnected(R,X,Ns),
			  N is Ns+1.

sumMaxConnected(_, 7, _).
sumMaxConnected(Player, Column, Score) :-
	maxConnected(Player, Column, MaxConn),
	Column is Column + 1,
	NewScore is Score + MaxConn,
	sumMaxConnected(Player, Column, NewScore).
