module FSharp.TicTacToe

open FSharp.Model

type TicTacToe() =
    let mutable currentPlayer = X
    let mutable playedSquares : PlayedSquares = []
    
    let tryGetFreeSquare (square : Square) =
        playedSquares
        |> List.map fst
        |> List.tryFind ((=)square)
        |> function
            | Some _ -> None
            | None -> Some square
        
    let placeMarker =
        Option.map (fun square ->
            playedSquares <- (square, currentPlayer) :: playedSquares
            square)
        
    let alternatePlayer : Square option -> unit =
        Option.iter (fun _ ->
            currentPlayer <- match currentPlayer with
                             | X -> O
                             | O -> X)
        
    member x.GetCurrentPlayer() = currentPlayer
    
    member x.PlaceMarker(square) =
        tryGetFreeSquare square
        |> placeMarker
        |> alternatePlayer
        
    member x.GetWinner() =
        tryFindWinner playedSquares winConditions
