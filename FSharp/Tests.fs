module Tests

open FSharp
open Xunit
open FsUnit.Xunit
open TicTacToe
open Model

type TicTacToeShould () =
    let _ticTacToe = TicTacToe()

    [<Fact>]
    let MakesXTheFirstPlayer () =
        let player = _ticTacToe.GetCurrentPlayer()

        player |> should equal Player.X

    [<Fact>]
    let MakesOTheSecondPlayer() =
        _ticTacToe.PlaceMarker(TopLeft)

        let player = _ticTacToe.GetCurrentPlayer()

        player |> should equal Player.O

    [<Fact>]
    let AlternatesBetweenPlayers() =
        _ticTacToe.PlaceMarker(TopLeft)
        _ticTacToe.PlaceMarker(TopMiddle)

        let player = _ticTacToe.GetCurrentPlayer()

        player |> should equal Player.X

    [<Fact>]
    let PlayerCannotPlayOnAnAlreadyPlayedSquare () =
        _ticTacToe.PlaceMarker(TopLeft)
        _ticTacToe.PlaceMarker(TopLeft)

        let player = _ticTacToe.GetCurrentPlayer()

        player |> should equal Player.O
        
    [<Theory>]
    [<MemberData(nameof(TicTacToeShould.ExpectedWinners))>]
    let ``Player wins if she marks three in a row`` (expectedWinner, moves) =
        moves |> List.iter _ticTacToe.PlaceMarker
            
        _ticTacToe.GetWinner()
        |> Option.get
        |> should equal expectedWinner
        
    static member ExpectedWinners : obj array seq =
        seq {
            yield [|X;[TopLeft;MiddleLeft;TopMiddle;Center;TopRight]|]
            yield [|O;[Center;TopLeft;MiddleRight;TopMiddle;BottomMiddle;TopRight]|]
            yield [|X;[MiddleLeft;TopLeft;Center;TopMiddle;MiddleRight]|]
            yield [|O;[TopLeft;MiddleLeft;TopMiddle;Center;BottomMiddle;MiddleRight]|]
            yield [|X;[BottomLeft;MiddleLeft;BottomMiddle;Center;BottomRight]|]
            yield [|O;[TopLeft;BottomLeft;MiddleLeft;BottomMiddle;Center;BottomRight]|]
            yield [|X;[TopLeft;MiddleLeft;Center;MiddleRight;BottomRight]|]
            yield [|O;[TopMiddle;TopLeft;MiddleLeft;Center;MiddleRight;BottomRight]|]
            yield [|X;[TopRight;MiddleLeft;Center;MiddleRight;BottomLeft]|]
            yield [|O;[TopMiddle;TopRight;MiddleLeft;Center;MiddleRight;BottomLeft]|]
        }