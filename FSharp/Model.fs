module FSharp.Model

type Player =
    | X | O

type Square =
    | TopLeft    | TopMiddle    | TopRight   
    | MiddleLeft | Center       | MiddleRight
    | BottomLeft | BottomMiddle | BottomRight
       
type PlayedSquare = Square * Player

type PlayedSquares = PlayedSquare list
       
type WinCondition = PlayedSquares -> Player option
       
type WinConditions = WinCondition list

type TryFindWinner = PlayedSquares -> WinConditions -> Player option

let winConditions : WinConditions =
    let threeInRow row =
        List.map row >> List.choose id >> (function | [ player ; _ ; _ ] -> player |> Some | _ -> None)
        
    [threeInRow (function | TopLeft,    X | TopMiddle,    X | TopRight,     X -> X |> Some | _ -> None)
     threeInRow (function | MiddleLeft, X | Center,       X | MiddleRight,  X -> X |> Some | _ -> None)
     threeInRow (function | BottomLeft, X | BottomMiddle, X | BottomRight,  X -> X |> Some | _ -> None)
     threeInRow (function | TopLeft,    X | MiddleLeft,   X | BottomLeft,   X -> X |> Some | _ -> None)
     threeInRow (function | TopMiddle,  X | Center,       X | BottomMiddle, X -> X |> Some | _ -> None)
     threeInRow (function | TopRight,   X | MiddleRight,  X | BottomRight,  X -> X |> Some | _ -> None)
     threeInRow (function | TopRight,   X | Center,       X | BottomLeft,   X -> X |> Some | _ -> None)
     threeInRow (function | TopLeft,    X | Center,       X | BottomRight,  X -> X |> Some | _ -> None)
     threeInRow (function | TopLeft,    O | TopMiddle,    O | TopRight,     O -> O |> Some | _ -> None)
     threeInRow (function | MiddleLeft, O | Center,       O | MiddleRight,  O -> O |> Some | _ -> None)
     threeInRow (function | BottomLeft, O | BottomMiddle, O | BottomRight,  O -> O |> Some | _ -> None)
     threeInRow (function | TopLeft,    O | MiddleLeft,   O | BottomLeft,   O -> O |> Some | _ -> None)
     threeInRow (function | TopMiddle,  O | Center,       O | BottomMiddle, O -> O |> Some | _ -> None)
     threeInRow (function | TopRight,   O | MiddleRight,  O | BottomRight,  O -> O |> Some | _ -> None)
     threeInRow (function | TopRight,   O | Center,       O | BottomLeft,   O -> O |> Some | _ -> None)
     threeInRow (function | TopLeft,    O | Center,       O | BottomRight,  O -> O |> Some | _ -> None)]
       
let tryFindWinner (playedSquares : PlayedSquares) (winConditions : WinConditions) : Player option =
    winConditions
    |> List.map (fun winCondition -> winCondition playedSquares)
    |> List.choose id
    |> List.tryExactlyOne
        