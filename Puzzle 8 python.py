import heapq
import time

class Board:
    def __init__(self, board: list[list[int]]):
        self.board = board
    
    def __eq__(self, other):
        return self.board == other.board
    
    def __hash__(self):
        return hash(tuple(map(tuple, self.board)))
    
    def manhattan(self):
        goal = [ (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2) ]
        dist = 0
        for i in range(3):
            for j in range(3):            
                value = self.board[i][j] - 1
                if value == -1: continue
                (x, y) = goal[value]
                dist += (abs(x - i) + abs(y - j))
        return dist

    def emptyTile(self):
        for i in range(3):
            for j in range(3):            
                if self.board[i][j] == 0: return (i, j)

    def move(self, r, c, x, y):
        newBoard = [row[:] for row in self.board]  # Crear una copia del tablero
        newBoard[r][c], newBoard[r + x][c + y] = newBoard[r + x][c + y], newBoard[r][c]
        return newBoard

    def neighbors(self):
        moves = [ (-1, 0), (1, 0), (0, 1), (0, -1) ]
        (r, c) = self.emptyTile()
        def isValidMove(r, c):
            return r >= 0 and r < 3 and c >= 0 and c < 3

        boards = []

        for (x, y) in moves:    
            if isValidMove(r + x, c + y): 
                new_board = self.move(r, c, x, y)
                boards.append(Board(new_board))
        return boards
            
class Node:
    def __init__(self, g, h, parent, board: Board):
        self.f = g + h
        self.g = g
        self.h = h
        self.parent = parent
        self.board = board

    def __lt__(self, other):
        return self.f < other.f 

def a_star(initial):
    goal = Board([
        [1, 2, 3],[4, 5, 6],[7, 8, 0]
    ])
    open_set = []
    closed_set = set()
    
    start_node = Node(g = 0, h = initial.manhattan(), parent = None, board = initial)
    heapq.heappush(open_set, start_node)
    
    while open_set:
        current = heapq.heappop(open_set)
        
        if current.board == goal:
            return current
        
        closed_set.add(current.board)

        for neighbor in current.board.neighbors():
            if neighbor in closed_set:
                continue
            
            new_node = Node(g = current.g + 1, h = neighbor.manhattan(), parent = current, board = neighbor)
            heapq.heappush(open_set, new_node)

    return None

def print_solution(node):
    path = []
    while node:
        path.append(node.board.board)
        node = node.parent
    for step in reversed(path):
        for row in step:
            print(row)
        print()
    return path


def get_input():
    while True:
        try:
            input_line = input("Initial Board: ")
            numbers = list(map(int, input_line.split()))
            if len(numbers) != 9 or any(n < 0 or n > 8 for n in numbers):
                print("The numbers of the board must be between 0 and 8")
            else:
                board = [numbers[i:i + 3] for i in range(0, 9, 3)]
                return board
        except ValueError:
            print("Invalid Input")


current_board = Board(
    get_input()
)

start = time.perf_counter()
solution = a_star(current_board)
end = time.perf_counter()

if solution:
    path = print_solution(solution)
    print(f"Solution found in : {end - start:.6f} seg")
    print(f"Solved in {len(path)} moves")
else:
    print("No solution found.")
