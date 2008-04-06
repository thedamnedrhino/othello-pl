/**
 * Copyright (c) 2008 Leo Arias elopio@softwarelibrecr.org Instituto Tecnológico de Costa Rica
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/**
 * Relation: init_board/1
 * Unifies Board with the initial state of the game
 * @1: Board - the initialized board
 */
init_board(Board) :-
	Board = [[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, white, black, empty, empty, empty],
		[empty, empty, empty, black, white, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty],
		[empty, empty, empty, empty, empty, empty, empty, empty]].

/**
 * Relation: print_board/1
 * Prints the board
 * @1: Board - the current board 
 */
print_board(Board) :-
	writeln('    0 1 2 3 4 5 6 7'),
	writeln('-------------------'),
	write('0| '),
	print_board(Board, 0, 0).

/**
 * Relation: print_board/3
 * Prints the board
 * @1: Board - the current board 
 * @2: Row - the current row
 * @3: Column - the current column
 */
print_board(_, 7, 8):-
	writeln(''),
	writeln(''),!.

/**
 * Relation: print_board/3
 * Prints the board
 */
print_board(Board, RowIndex, 8) :-
	NRowIndex is RowIndex + 1,
	writef('\n%d| ', [NRowIndex]),
	print_board(Board, NRowIndex, 0).

/**
 * Relation: print_board/3
 * Prints the board
 */
print_board(Board, RowIndex, ColumnIndex) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	print_piece(Piece),
	NColumnIndex is ColumnIndex + 1,
	print_board(Board, RowIndex, NColumnIndex).

/**
 * Relation: print_piece/1
 * Prints a piece
 * @1: Color - the color of the piece
 */
print_piece(black):-
	write(' X').

/**
 * Relation: print_piece/1
 * Prints a piece
 */
print_piece(white):-
	write(' 0').

/**
 * Relation: print_piece/1
 * Prints a piece
 */
print_piece(empty):-
	write(' -').

/**
 * Relation: piece/4
 * Succeds if Piece unifies with the piece of the board that is on RowIndex and ColumnIndex
 * @1: Board - the current board
 * @2: RowIndex - the index of the row
 * @3: ColumnIndex - the index of the column
 * @4: Piece - the piece at the row and column specified
 */
piece(Board, RowIndex, ColumnIndex, Piece) :-
	is_valid_index(RowIndex),
	is_valid_index(ColumnIndex),
	nth0(RowIndex, Board, Row),
	nth0(ColumnIndex, Row, Piece).

final(Board, Value):-
	full_board(Board),
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Value is BlackPieces - WhitePieces.

eval(Board, Value):-
	count_pieces(black, Board, BlackPieces, WhitePieces),
	Value is BlackPieces - WhitePieces.

empty_on_board(Board):-
	member(Row, Board),
	member(Piece, Row),
	Piece = empty,!.

full_board(Board):-
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)).

full_board(Board, Color, Value):-
	flatten(Board, PiecesList),
	list_to_set(PiecesList, PiecesSet),
	not(member(empty, PiecesSet)),
	count_pieces(Color, Board, Pieces, RivalPieces),
	Value is Pieces - RivalPieces.

find_states(State, Color, StatesList):-
	find_boards(State, Color, StatesList).
	
find_boards(Board, Color, BoardsList):-
	find_moves(Board, Color, MovesList),
	find_boards(Board, Color, OrderedBoardsList, [], MovesList),
	first_elements(OrderedBoardsList, [], BoardsList).

find_boards(Board,_, BoardsList, [], []):-
	append([], [[Board, 0]], BoardsList),!.

find_boards(_, _, BoardsList, BoardsList, []):-!.

find_boards(Board, Color, BoardsList, CurrentBoardsList, [Move|RestMovesList]):-
	set_piece(Board, Move, Color, FinalBoard),
	order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList),
	find_boards(Board, Color, BoardsList, NBoardsList, RestMovesList),!.

order_boards(Color, CurrentBoardsList, FinalBoard, NBoardsList):-
	rivalColor(Color, RivalColor),
	valid_positions(FinalBoard, RivalColor, Number),
	order_boards_aux([FinalBoard, Number], CurrentBoardsList, [], NBoardsList).

order_boards_aux(Board, [], CurrentList, FinalList):-
	append(CurrentList, [Board], FinalList),!.

order_boards_aux(Board, [First|Rest], CurrentList, FinalList):-
	nth0(1, First, Value),
	nth0(1, Board, NewValue),
	NewValue =< Value,
	append(CurrentList, [Board], TempList),
	append(TempList, [First|Rest], FinalList),!.

order_boards_aux(Board, [First|Rest], CurrentList, FinalList):-
	append(CurrentList, [First], NCurrentList),
	order_boards_aux(Board, Rest, NCurrentList, FinalList),!.

valid_positions(Board, Color, Number):-
	valid_positions(Board, Color, 0, 0, 0, Number).

valid_positions(_, _, 7, 8, Number, Number):-!.

valid_positions(Board, Color, RowIndex, 8, CurrentNumber, FinalNumber):-
	NRowIndex is RowIndex + 1,
	valid_positions(Board, Color, NRowIndex, 0, CurrentNumber, FinalNumber),!.

valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	single_valid_move(Board, RowIndex, ColumnIndex, Color),
	NCurrentNumber is CurrentNumber + 1,
	NColumnIndex is ColumnIndex + 1,
	valid_positions(Board, Color, RowIndex, NColumnIndex, NCurrentNumber, FinalNumber),!.

valid_positions(Board, Color, RowIndex, ColumnIndex, CurrentNumber, FinalNumber):-
	NColumnIndex is ColumnIndex + 1,
	valid_positions(Board, Color, RowIndex, NColumnIndex, CurrentNumber, FinalNumber),!.

single_valid_move(Board, RowIndex, ColumnIndex, Color) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(direction_offsets),
	member(DirectionOffset, direction_offsets),
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	rivalColor(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_color(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color),!.

find_moves(Board, Color, MovesList):-
	find_moves(Board, Color, 0, 0, [], MovesList).

find_moves(_, _, 7, 8, MovesList, MovesList):-!.

find_moves(Board, Color, RowIndex, 8, MovesList, FinalList):-
	NRowIndex is RowIndex + 1,
	find_moves(Board, Color, NRowIndex, 0, MovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	valid_move(Board, RowIndex, ColumnIndex, Color, Validdirection_offsets),
	append(MovesList,[[RowIndex, ColumnIndex, Validdirection_offsets]], NMovesList),
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, NMovesList, FinalList),!.

find_moves(Board, Color, RowIndex, ColumnIndex, MovesList, FinalList):-
	NColumnIndex is ColumnIndex + 1,
	find_moves(Board, Color, RowIndex, NColumnIndex, MovesList, FinalList),!.
	
valid_move(Board, RowIndex, ColumnIndex, Color, Validdirection_offsets):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	direction_offsets(direction_offsets),
	valid_move(Board, RowIndex, ColumnIndex, Color, direction_offsets, [], Validdirection_offsets).

valid_move(_, _, _, _, [], CurrentValiddirection_offsets, Validdirection_offsets):-
	CurrentValiddirection_offsets \= [],
	CurrentValiddirection_offsets = Validdirection_offsets.

valid_move(Board, RowIndex, ColumnIndex, Color, direction_offsets, CurrentValiddirection_offsets, Validdirection_offsets):-
	direction_offsets = [DirectionOffset|direction_offsetsRest],
	valid_moveOffset(Board, RowIndex, ColumnIndex, Color, DirectionOffset),
	append(CurrentValiddirection_offsets, [DirectionOffset], NCurrentValiddirection_offsets),
	valid_move(Board, RowIndex, ColumnIndex, Color, direction_offsetsRest, NCurrentValiddirection_offsets, Validdirection_offsets).

valid_move(Board, RowIndex, ColumnIndex, Color, direction_offsets, CurrentValiddirection_offsets, Validdirection_offsets):-
	direction_offsets = [_|direction_offsetsRest],
	valid_move(Board, RowIndex, ColumnIndex, Color, direction_offsetsRest, CurrentValiddirection_offsets, Validdirection_offsets).

valid_moveOffset(Board, RowIndex, ColumnIndex, Color, DirectionOffset):-
	piece(Board, RowIndex, ColumnIndex, Piece),
	Piece = empty,
	nth0(0, DirectionOffset, RowOffset),
	nth0(1, DirectionOffset, ColumnOffset),
	NeighborRow is RowIndex + RowOffset,
	NeighborColumn is ColumnIndex + ColumnOffset,
	rivalColor(Color, RivalColor),
	piece(Board, NeighborRow, NeighborColumn, NeighborPiece),
	NeighborPiece = RivalColor,
	find_color(Board, NeighborRow, NeighborColumn, RowOffset, ColumnOffset, Color).

find_color(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowOffset is RowIndex + RowOffset,
	NColumnOffset is ColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color.

find_color(Board, RowIndex, ColumnIndex, RowOffset, ColumnOffset, Color) :-
	NRowIndex is RowIndex + RowOffset,
	NColumnIndex is ColumnIndex + ColumnOffset,
	piece(Board, NRowIndex, NColumnIndex, Piece),
	rivalColor(Color, RivalColor),
	Piece = RivalColor,
	find_color(Board, NRowIndex, NColumnIndex, RowOffset, ColumnOffset, Color).

set_piece(Board, Move, Color, FinalBoard):-
	nth0(0, Move, Row),
	nth0(1, Move, Column),
 	nth0(2, Move, Validdirection_offsets),
	set_single_piece(Board, Row, Column, Color, BoardWithPiece),
	set_pieces_on_offsets(BoardWithPiece, Row, Column, Color, Validdirection_offsets, FinalBoard).

set_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	valid_move(Board, PieceRowIndex, PieceColumnIndex, Color, Validdirection_offsets),
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, BoardWithPiece),
	set_pieces_on_offsets(BoardWithPiece, PieceRowIndex, PieceColumnIndex, Color, Validdirection_offsets, FinalBoard).

set_pieces_on_offsets(FinalBoard, _, _, _, [], FinalBoard):-!.

set_pieces_on_offsets(Board, PieceRowIndex, PieceColumnIndex, Color, Validdirection_offsets, FinalBoard):-
	Validdirection_offsets = [ValidDirectionOffset|Validdirection_offsetsRest],
	set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, TempBoard),
	set_pieces_on_offsets(TempBoard, PieceRowIndex, PieceColumnIndex, Color, Validdirection_offsetsRest, FinalBoard).

set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	Piece = Color,
	Board = FinalBoard,!.

set_pieces_on_offset(Board, PieceRowIndex, PieceColumnIndex, Color, ValidDirectionOffset, FinalBoard):-
	nth0(0, ValidDirectionOffset, RowOffset),
	nth0(1, ValidDirectionOffset, ColumnOffset),
	NRowOffset is PieceRowIndex + RowOffset,
	NColumnOffset is PieceColumnIndex + ColumnOffset,
	piece(Board, NRowOffset, NColumnOffset, Piece),
	rivalColor(Color, RivalColor),
	Piece = RivalColor,
	set_single_piece(Board, NRowOffset, NColumnOffset, Color, TempBoard),
	set_pieces_on_offset(TempBoard, NRowOffset, NColumnOffset, Color, ValidDirectionOffset, FinalBoard).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, Color, FinalBoard):-
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, 0, 0, Color, [], FinalBoard, []).

set_single_piece(_, 7, _, 7, 8, _, ResultingBoard, FinalBoard, PieceRow):-
	append(ResultingBoard, [PieceRow], FinalBoard),!.

set_single_piece(_, _, _, 8, 0, _, FinalBoard, FinalBoard, _):-!.

set_single_piece(Board, PieceRowIndex, ColumnRowIndex, PieceRowIndex, 8, Color, ResultingBoard, FinalBoard, RowIndex):-
	PieceRowIndex \= 7,
	NCurrentRowIndex is PieceRowIndex + 1,
	append(ResultingBoard, [RowIndex], NResultingBoard),
	set_single_piece(Board, PieceRowIndex, ColumnRowIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, []).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, PieceColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	append(PieceRow, [Color], NPieceRow),
	NCurrentColumnIndex is PieceColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, CurrentColumnIndex, Color, ResultingBoard, FinalBoard, PieceRow):-
	CurrentColumnIndex \= PieceColumnIndex,
	piece(Board, PieceRowIndex, CurrentColumnIndex, Piece),
	append(PieceRow, [Piece], NPieceRow),
	NCurrentColumnIndex is CurrentColumnIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, PieceRowIndex, NCurrentColumnIndex, Color, ResultingBoard, FinalBoard, NPieceRow).

set_single_piece(Board, PieceRowIndex, PieceColumnIndex, CurrentRowIndex, _, Color, ResultingBoard, FinalBoard, PieceRow):-
	PieceRowIndex \= CurrentRowIndex,
	nth0(CurrentRowIndex, Board, CurrentRow),
	append(ResultingBoard, [CurrentRow], NResultingBoard),
	NCurrentRowIndex is CurrentRowIndex + 1,
	set_single_piece(Board, PieceRowIndex, PieceColumnIndex, NCurrentRowIndex, 0, Color, NResultingBoard, FinalBoard, PieceRow).

count_pieces(Color, Board, Pieces, RivalPieces) :-
	count_pieces(Color, Board, 0, 0, 0, 0, Pieces, RivalPieces).

count_pieces(_, _, 7, 8, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces):-
	Pieces is CurrentPieces,
	RivalPieces is CurrentRivalPieces,!.

count_pieces(Color, Board, RowIndex, 8, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces) :-
	NRowIndex is RowIndex + 1,
	count_pieces(Color, Board, NRowIndex, 0, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces).

count_pieces(Color, Board, RowIndex, ColumnIndex, CurrentPieces, CurrentRivalPieces, Pieces, RivalPieces) :-
	piece(Board, RowIndex, ColumnIndex, Piece),
	count_piece(Color, Piece, CurrentPieces, CurrentRivalPieces, NCurrentPieces, NCurrentRivalPieces),
	NColumnIndex is ColumnIndex + 1,
	count_pieces(Color, Board, RowIndex, NColumnIndex, NCurrentPieces, NCurrentRivalPieces, Pieces, RivalPieces).

count_piece(_, empty, CurrentPieces, CurrentRivalPieces, CurrentPieces, CurrentRivalPieces):-!.

count_piece(Color, Color, CurrentPieces, CurrentRivalPieces, NCurrentPieces, CurrentRivalPieces):-
	NCurrentPieces is CurrentPieces + 1,!.

count_piece(Color, RivalColor, CurrentPieces, CurrentRivalPieces, CurrentPieces, NCurrentRivalPieces):-
	rival_color(Color, RivalColor),
	NCurrentRivalPieces is CurrentRivalPieces + 1,!.

direction_offsets(OffsetsList) :-
	OffsetsList = [[-1, 0],
			[-1, 1],
			[0, 1],
			[1, 1],
			[1, 0],
			[1, -1],
			[0, -1],
			[-1,-1]].

is_valid_index(Index) :-
	Index >= 0,
	Index < 8.
