(*
Sudoku Solver using brute-force back-tracking

Inspiration:
Answer @ https://codereview.stackexchange.com/questions/53752/making-backtracking-sudoku-solver-more-functional
*)

// Types
type Pos = int * int
type Sudoku = Map<Pos, int>
type Solution = Sudoku option

// Global variables
// Assumes sudoku always 9x9 grid
[<Literal>]
let WIDTH = 9;;
[<Literal>]
let HEIGHT = WIDTH;;
[<Literal>]
let WIDTH_MASK = 8;;
[<Literal>]
let HEIGHT_MASK = WIDTH_MASK;;
[<Literal>]
let BOX_WIDTH = 3;;

let isRowValid sudoku row x n =
    let rec checkRow i =
        match i with
        | w when w = WIDTH -> true
        | _ -> match Map.tryFind (i, row) sudoku with
               | None -> checkRow (i+1)
               | Some n' -> if i = x then checkRow (i+1)
                            else n <> n' && checkRow (i+1)
    checkRow 0
let isColValid sudoku col y n =
    let rec checkCol i =
        match i with
        | h when h = HEIGHT -> true
        | _ -> match Map.tryFind (col, i) sudoku with
               | None -> checkCol (i+1)
               | Some n' -> if i = y then checkCol (i+1)
                            else n <> n' && checkCol (i+1)
    checkCol 0

// Check 3x3 square does not contain same number
let isBoxValid sudoku pos n =
    let (x,y) = pos
    let dx = x / BOX_WIDTH
    let dy = y / BOX_WIDTH
    let sq1 = seq {
        yield n
        for i in [0..2] do
            for j in [0..2] do
                let optN = Map.tryFind (i + BOX_WIDTH * dx, j + BOX_WIDTH * dy) sudoku
                if not (Option.isNone optN) then yield Option.get optN
    }
    let sq2 = Seq.distinct sq1
    Seq.length sq1 = Seq.length sq2

let isSudokuValid sudoku pos n =
    let (x, y) = pos
    isRowValid sudoku y x n
    && isColValid sudoku x y n
    && isBoxValid sudoku pos n

let getNextPos pos =
    let (x, y) = pos
    if x + 1 = WIDTH then (0, y+1)
    else (x+1, y)
    
let hasReachedEnd pos =
    let (_, y) = pos
    if y = HEIGHT then true
    else false

// Main backtrack-solving function
let rec solveSudoku (currentPos:Pos) (sudoku:Sudoku) : Solution =
    let (x, y) = currentPos

    if hasReachedEnd currentPos then
        Some sudoku
    elif Map.containsKey currentPos sudoku then
        solveSudoku (getNextPos currentPos) sudoku
    else
        let solveCurrentPos n =
            if isSudokuValid sudoku currentPos n then
                let newSudoku = Map.add currentPos n sudoku
                solveSudoku (getNextPos currentPos) newSudoku
            else
                None
        Array.tryPick solveCurrentPos [|1..9|]

let createSudoku arr : Sudoku =
    let wm = Array2D.length1 arr - 1
    let hm = Array2D.length2 arr - 1

    let sudokuSeq = seq {
        for i in [0..wm] do
            for j in [0..hm] do
                if arr.[i,j] <> 0 then yield ((i,j), arr.[i,j])
    }

    Map.ofSeq sudokuSeq

let printSolution solution =
    match solution with
    | None -> printfn "No solution"
    | Some sudoku -> for i in [0..WIDTH_MASK] do
                        seq {
                            for j in [0..HEIGHT_MASK] do
                                yield (Map.find (i,j) sudoku)
                        }
                        |> Seq.toList
                        |> printfn "%A"
                        printfn "\n"

// Quick sample test
// Zero-entries indicate blank slots
let solution =
    [[5; 3; 0;  0; 7; 0;  0; 0; 0]
     [6; 0; 0;  1; 9; 5;  0; 0; 0]
     [0; 9; 8;  0; 0; 0;  0; 6; 0]

     [8; 0; 0;  0; 6; 0;  0; 0; 3]
     [4; 0; 0;  8; 0; 3;  0; 0; 1]
     [7; 0; 0;  0; 2; 0;  0; 0; 6]

     [0; 6; 0;  0; 0; 0;  2; 8; 0]
     [0; 0; 0;  4; 1; 9;  0; 0; 5]
     [0; 0; 0;  0; 8; 0;  0; 7; 9]]
    |> array2D
    |> createSudoku
    |> solveSudoku (0,0);;
printSolution solution;;
