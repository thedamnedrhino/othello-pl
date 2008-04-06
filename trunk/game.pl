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
 * Relation: play1
 * Starts the game
 * @1: Depth - the maximum depth of the search
 */
play(Depth) :-
	init_board(Board),
	select_mode(Mode),
	game_loop(Board, Mode, Depth, black).

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 * @1: Board - the current board
 * @2: Mode - the game mode.
 *	1 for human vrs machine
 *	2 for machine vrs human
 *	3 for human vrs human
 * @3: Depth - the maximum depth of the search
 * @4: Color - the color of the player that moves next
 */
game_loop(Board, 1, Depth, black):-
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	find_moves(Board, black, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, black, FinalBoard),
	game_loop(FinalBoard, 1, Depth, white),!.

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 */
game_loop(Board, 1, Depth, white):-
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	machine_select_move(Board, Depth, white, FinalBoard),!,
	game_loop(FinalBoard, 1, Depth, black),!.

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 */
game_loop(Board, 2, Depth, black):-
	print_board(Board),
	print_player(black),
	empty_on_board(Board),
	machine_select_move(Board, Depth, black, FinalBoard),!,
	game_loop(FinalBoard, 2, Depth, white),!.

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 */
game_loop(Board, 2, Depth, white):-
	print_board(Board),
	print_player(white),
	empty_on_board(Board),
	find_moves(Board, white, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, white, FinalBoard),
	game_loop(FinalBoard, 2, Depth, black),!.

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 */
game_loop(Board, 3, Depth, Color):-
	print_board(Board),
	print_player(Color),
	empty_on_board(Board),
	find_moves(Board, Color, MovesList),
	member(_, MovesList),
	human_select_move(Move, MovesList),!,
	set_piece(Board, Move, Color, FinalBoard),
	rival_color(Color, RivalColor),
	game_loop(FinalBoard, 3, Depth, RivalColor),!.

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 */
game_loop(Board, _, _, Color):-
	full_board(Board),
	print_board(Board),
	count_pieces(Color, Board, Pieces, RivalPieces),
	writef('%d: %d\n', [Color, Pieces]),
	rival_color(Color, RivalColor),
	writef('%d: %d\n', [RivalColor, RivalPieces]),!.

/**
 * Relation: game_loop/4
 * Loops the game, switching turns, until the game ends
 */
game_loop(Board, Mode, Depth, Color):-
	find_moves(Board, Color, MovesList),!,
	not(member(_,MovesList)),!,
	print_player(Color),
	writeln('There\'s no valid move'),
	rival_color(Color, RivalColor),
	game_loop(Board, Mode, Depth, RivalColor),!.

/**
 * Relation: print_player/1
 * Prints the player that moves next
 * @1: Color - the color of the player that moves next
 */
print_player(white):-
	writeln('White player turn (0)'),!.

/**
 * Relation: print_player/1
 * Prints the player that moves next
 */
print_player(black):-
	writeln('Black player turn (X)'),!.

/**
 * Relation: human_select_move/2
 * Succeds when Move unifies with one of the possible moves in MovesList
 * @1: Move - The move selected by the human
 * @2: MovesList - The list of possible moves
 */
human_select_move(Move, MovesList):-
	write('Enter the Row: '),
	read(SelectedRow),
	writeln('Enter the Column: '),
	read(SelectedColum),
	member(Move, MovesList),!,
	nth0(0, Move, SelectedRow),
	nth0(1, Move, SelectedColum).

/**
 * Relation: human_select_move/2
 * Succeds when Move unifies with one of the possible moves in MovesList
 */
human_select_move(Move, MovesList):-
	writeln('Not a valid move'),
	writeln(''),
	human_select_move(Move, MovesList).

/**
 * Relation: machine_select_move/4
 * Selects a move from the possible ones for the player that has the next move
 * @1: Board - The current board
 * @2: Depth - The maximum depth of the search
 * @3: Color - The color of the player that moves next
 * @4: FinalBoard - The board after applying the move selected by the machine
 */
machine_select_move(Board, Depth, Color, FinalBoard):-
	garbage_collect,
	alpha_beta_prunning(Board, Depth, Color, FinalBoard, _).


select_mode(Mode):-
	writeln('Select a game mode'),
	writeln('1. human vrs machine'),
	writeln('2. machine vrs human'),
	writeln('3. human vrs human'),
	write('Enter a number: '),
	read(SelectedMode),
	enter_mode(SelectedMode, Mode).

enter_mode(SelectedMode, Mode):-
	SelectedMode is 1,
	Mode is SelectedMode,
	writeln('machine vrs human selected'),
	writeln(''),!.
enter_mode(SelectedMode, Mode):-
	SelectedMode is 2,
	Mode is SelectedMode,
	writeln('human vrs machine selected'),
	writeln(''),!.
enter_mode(SelectedMode, Mode):-
	SelectedMode is 3,
	Mode is SelectedMode,
	writeln('human vrs human selected'),
	writeln(''),!.
enter_mode(_, Mode):-
	writeln('Not a valid mode'),
	writeln(''),
	select_mode(Mode).

rival_color(white, black).
rival_color(black, white).

