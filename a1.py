
"""
Chess Game
Assignment 1
Semester 2, 2021
CSSE1001/CSSE7030
"""

from typing import Optional, Tuple

from a1_support import *

# Replace these <strings> with your name, student number and email address.
__author__ = "Xinyuan Peng, 47099625"
__email__ = "s4709962@student.uq.edu.au"


def initial_state() -> Board:
    """Return the board state for a new game.
    """
    board = ('rnbqkbnr', 'pppppppp', '........', '........',
             '........', '........', 'PPPPPPPP', 'RNBQKBNR')
    return board

def print_board(board: Board) -> None:
    """Print a human-readable board.
    """
    for i in range(8):
        print(board[i] + '  {}'.format(8-i))
    print()
    print('abcdefgh')

def square_to_position(square: str) -> Position:
    """Convert chess notation to its (row, col): Position equivalent.
    """
    a_h = 'abcdefgh'
    position = ( 8 - int(square[1]) , a_h.index(square[0]) )
    return position

def process_move(user_input: str) -> Move:
    """Assume the user_input is valid, convert the input to a move based on (row, col): Position.
    """
    move = (square_to_position(user_input[0:2]), square_to_position(user_input[-2:]))
    return move

def change_position(board: Board, position: Position, character: str) -> Board:
    """Return a copy of board with the piece at the given position changed to the given character.
    """
    board1 = ()
    # Find the row that needs changing, create a new string for that row,
    # and bind all rows together.
    for i in range(8):
        if i==position[0]:
            newStr = board[i]
            col = position[1]
            newStr  = newStr[ : col] + character + newStr[ (col+1) : ]
            board1 = board1 + (newStr, )
        else:
            board1 = board1 + (board[i], )
    return board1

def clear_position(board: Board, position: Position) -> Board:
    """Clears the piece at the given position and return the resulting board.
    """
    row, col = position

    # If that position is empty, there's no need for clearing.
    if board[row][col] == '.' :
        return board
    # Find the row that needs changing, create a new string for that row,
    # and bind all rows together.
    board1 = ()
    for i in range(8) :
        if i == row :
            newStr = board[i]
            newStr  = newStr[ : col] + '.' + newStr[ (col+1) : ]
            board1 = board1 + (newStr, )
        else:
            board1 = board1 + (board[i], )
    return board1

def update_board(board: Board, move: Move) -> Board:
    """Assume the move is valid, returns an updated version of the board with the move made.
    """
    position_from, position_to = move
    piece = piece_at_position(position_from, board)
    board1 = change_position(board, position_to, piece)
    board1 = clear_position(board1, position_from)
    return board1

def is_current_players_piece(piece: str, whites_turn: bool) -> bool:
    """Checks if the given piece belongs to the player whose turn it is.
    """
    if piece != EMPTY and ( whites_turn is (piece in WHITE_PIECES) ) :
        return True
    return False

def is_move_valid(move: Move, board: Board, whites_turn: bool) -> bool:
    """Returns true when the move is valid on the current board state for the player
       whose turn it is.
    """
    position_from, position_to = move
    piece_from = piece_at_position(position_from, board)
    piece_to = piece_at_position(position_to, board)

    if  ( not out_of_bounds(position_from) ) \
    and ( not out_of_bounds(position_to) ) \
    and ( not position_from == position_to ) \
    and is_current_players_piece(piece_from, whites_turn) \
    and ( (piece_to == EMPTY) or (not is_current_players_piece(piece_to, whites_turn)) ) \
    and position_to in get_possible_moves(position_from, board) \
    and ( not is_in_check(update_board(board, move), whites_turn) ) :
        return True
    return False

def can_move(board: Board, whites_turn: bool) -> bool:
    """Returns true only when the player can make a valid move which does not put them in check.
    """
    # Iterate through all positions on the board, find all possible moves for current player's
    # pieces. If there are at least one valid move for at least one of current player's pieces,
    # then the player can move.
    for i, row in enumerate(board):
        for j, piece in enumerate(row):
            position = (i, j)
            if is_current_players_piece(piece, whites_turn):
                all_moves = get_possible_moves(position, board)
                for one_move in all_moves:
                    if not is_in_check(update_board(board, (position, one_move)), whites_turn):
                        return True
    return False

def is_stalemate(board: Board, whites_turn: bool) -> bool:
    """Returns true only when a stalemate has been reached.
    """
    if ( not is_in_check(board, whites_turn) ) and ( not can_move(board, whites_turn) ) :
        return True
    return False

def check_game_over(board: Board, whites_turn: bool) -> bool:
    """Returns true only when the game is over (either due to checkmate or stalemate). Also
       prints information about the result if the game is over, or if the player is in check.
    """
    turn = 'White' if whites_turn else 'Black'
    if is_stalemate(board, whites_turn):
        print('\nStalemate')
        return True
    if is_in_check(board, whites_turn):
        if can_move(board, whites_turn):
            print('\n{} is in check'.format(turn))
            return False
        else:
            print('\nCheckmate')
            return True
    return False

def attempt_promotion(board: Board, whites_turn: bool) -> Board:
    """Checks whether there is a pawn on the board that needs to be promoted.
       Update the board if necessary then return the board
    """
    board1 = ()
    my_pawn = WHITE_PAWN if whites_turn else BLACK_PAWN
    # For White, pawns should reach row 0, for Black, it's row 7:
    end_row = board[0] if whites_turn else board[7]
    # Find current player's pawn in the respective end_row:
    for piece in end_row :
        if piece == my_pawn :
            change_to = input('What piece would you like (q, n, r, b)? ')
            # Prompt the user until get a valid input:
            while True:
                if change_to not in 'qnrb':
                    change_to = input('Not a valid piece. What piece would you like (q, n, r, b)? ')
                else:
                    break
            # Only row0 or row 7 needs modification:
            if whites_turn:
                row0 = board[0].replace(WHITE_PAWN, change_to.upper(), 1)
                row7 = board[7]
            else:
                row0 = board[0]
                row7 = board[7].replace(BLACK_PAWN, change_to, 1)
            # Bind row 0, row 7 with the unchanged rows in the correct order:
            board1 = board1 + (row0, )
            for i in range(1, 7) :
                board1 = board1 + (board[i], )
            board1 = board1 + (row7, )
            return board1
    return board

def is_valid_castle_attempt(move: Move, board: Board, whites_turn: bool,
castling_info: Tuple[bool, bool, bool]) -> bool:
    """Checks whether the given move is a valid attempt at castling.
    """
    position_from, position_to = move
    from_row, from_col = position_from
    to_row, to_col = position_to
    enemy_pieces = BLACK_PIECES if whites_turn else WHITE_PIECES
    my_row = 7 if whites_turn else 0
    my_rook = WHITE_ROOK if whites_turn else BLACK_ROOK

    if from_row == my_row and to_row == my_row and from_col == 4 and abs(to_col - from_col) == 2:
        # interval is the positions from the king's position to the rook's position (inclusive)
        interval = [ (from_row, col_num) for
                     col_num in range( min(from_col, to_col) , max(from_col, to_col)+1 ) ]
        enemy_possiMoves = ()
        for i, row in enumerate(board):
            for j, piece in enumerate(row):
                position = (i, j)
                if piece in enemy_pieces :
                    enemy_possiMoves = enemy_possiMoves + \
                                       get_possible_moves(position, board)
        # No place in interval is in enemy's possible moves:
        if not any([each_place in enemy_possiMoves for each_place in interval]) :
            # Checks neither of the pieces have moved and no pieces are in between them:
            if from_col < to_col and board[from_row][7] == my_rook \
            and (not castling_info[1]) and (not castling_info[2]) \
            and board[from_row][5 : 7] == EMPTY * 2 :
                return True
            elif from_col > to_col and board[from_row][0] == my_rook \
            and (not castling_info[0]) and (not castling_info[1]) \
            and board[from_row][1 : 4] == EMPTY * 3 :
                return True
    return False

def perform_castling(move: Move, board: Board) -> Board:
    """Given a valid castling move, returns the resulting board state.
    """
    position_from, position_to = move
    # Execute King's move.
    board1 = update_board(board, move)
    rook_from = (position_from[0], 0) if position_from[1] > position_to[1] \
                else (position_from[0], 7)
    rook_to = ( position_from[0] , (position_from[1] + position_to[1]) // 2 )
    rook_move = (rook_from, rook_to)
    # Execute Rook's move.
    board1 = update_board(board1, rook_move)
    return board1

def update_castling_info(move: Move, whites_turn: bool,
castling_info: Tuple[bool, bool, bool]) -> Tuple[bool, bool, bool]:
    """Returns the updated castling information for the player whose turn it is, after
       performing the given, valid move.
    """
    my_row = 7 if whites_turn else 0
    # Left Rook gets moved for the first time:
    if move[0] == (my_row, 0) and (not castling_info[0]) :
        castling_info1 = (True, ) + (castling_info[1], ) + (castling_info[2], )
        return castling_info1
    # King gets moved for the first time:
    if move[0] == (my_row, 4) and (not castling_info[1]) :
        castling_info1 = (castling_info[0], ) + (True, ) + (castling_info[2], )
        return castling_info1
    # Right Rook gets moved for the first time:
    if move[0] == (my_row, 7) and (not castling_info[2]) :
        castling_info1 = (castling_info[0], ) + (castling_info[1], ) + (True, )
        return castling_info1
    return castling_info

def is_valid_en_passant(move: Move, board: Board, whites_turn: bool,
en_passant_position: Optional[Position]) -> bool:
    """Returns true only when the supplied move constitutes a valid en passant move.
    """
    position_from, position_to = move
    vertical_move = -1 if whites_turn else 1
    my_pawn = WHITE_PAWN if whites_turn else BLACK_PAWN

    # If en_passant_position exists, and current player's pawn needs to move:
    if en_passant_position \
    and piece_at_position(position_from, board) == my_pawn :
        # Checks vertical and horizontal conditions:
        if en_passant_position[0] == position_from[0] + vertical_move \
        and abs( position_from[1] - en_passant_position[1] ) == 1 \
        and position_to == en_passant_position:
            return True
    return False

def perform_en_passant(move: Move, board: Board, whites_turn: bool) -> Board:
    """Given a valid en passant move, execute it.
    """
    enemy_pawn_position = (move[0][0], move[1][1])
    # Execute my pawn's en passant move, then clear out the enemy's pawn.
    board1 = update_board(board, move)
    board1 = clear_position(board1, enemy_pawn_position)
    return board1

def update_en_passant_position(move: Move, board: Board, whites_turn: bool) -> Optional:
    """If the current player’s pawn just moved forward two squares, returns the position that an
       opponent pawn could take to perform a valid en passant move. If no en passant move should be
       possible, returns None.
    """
    my_pawn = WHITE_PAWN if whites_turn else BLACK_PAWN
    position_from, position_to = move

    if abs( position_to[0] - position_from[0] ) == 2 and \
    piece_at_position(position_to, board) == my_pawn:
        row = int( (position_from[0] + position_to[0]) / 2 )
        return (row , position_to[1])
    return None



def main():
    """Entry point to gameplay"""
    board = initial_state()
    print_board(board)
    whites_turn = True
    en_passant_position = None
    # Store the castling info for White and Black separately.
    castling_info_black = (False, False, False)
    castling_info_white = (False, False, False)

    while not check_game_over(board, whites_turn):
        turn = 'White' if whites_turn else 'Black'
        # Decide which castling info should this round use.
        castling_info = castling_info_white if whites_turn else castling_info_black
        intake = input(f"\n{turn}'s move: ")
        if valid_move_format(intake):
            move = process_move(intake)
            # Regular moves:
            if is_move_valid(move, board, whites_turn) :
                board = update_board(board, move)
            # Castle moves:
            elif is_valid_castle_attempt(move, board, whites_turn, castling_info):
                board = perform_castling(move, board)
            # En passant moves:
            elif is_valid_en_passant(move, board, whites_turn, en_passant_position):
                board = perform_en_passant(move, board, whites_turn)
            else:
                print('Invalid move\n')
                print_board(board)
                continue
        elif intake in ('h', 'H'):
            print("\nWelcome to Chess!")
            print("When it's your turn, enter one of the following:")
            print("1) 'h' or 'H': Print the help menu")
            print("2) 'q' or 'Q': Quit the game")
            print("3) position1 position2: The positions (as letterNumber) "
                  "to move from and to respectively.\n")
            print_board(board)
            continue
        elif intake in ('q', 'Q'):
            if_quit = input("Are you sure you want to quit? ")
            if if_quit in ('y', 'Y'):
                break
            else:
                print_board(board)
                continue
        else:
            print('Invalid move\n')
            print_board(board)
            continue

        if whites_turn:
            castling_info_white = update_castling_info(move, whites_turn, castling_info)
        else:
            castling_info_black = update_castling_info(move, whites_turn, castling_info)
        en_passant_position = update_en_passant_position(move, board, whites_turn)
        # See if there's any pawn needs promotion:
        board = attempt_promotion(board, whites_turn)
        print_board(board)
        # Switch turn:
        whites_turn = not whites_turn


if __name__ == "__main__":
    main()
