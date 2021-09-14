namespace FSAI

module Minimax =
    
    //Temp functions
    let eval state = 10000
    let getWinner state = 0
    let getValidMoves state tile = [(1,2); (2,2); (3,2)]
    let MakeMove childState move tile = 0
    let OtherTile tile = 0

    let rec moveAnalysis state (validMoves: (int*int)list) tile depth a b isMaxPlayer (bestScore) =
        if depth = 0 then bestScore
        else 
            if b <= a then bestScore
            else
                let childBoard = state
                let nodeScore = moveAnalysis childBoard validMoves (OtherTile(tile)) (depth-1) a b (not isMaxPlayer) bestScore

                MakeMove childBoard validMoves.Head tile |> ignore
                if isMaxPlayer = true then
                    let bestScore =
                        if bestScore < nodeScore then
                            nodeScore
                        else
                            bestScore
                    let a =
                        if bestScore < a then
                            a
                        else bestScore
                    ()
                else
                    let bestScore =
                        if bestScore < nodeScore then
                            nodeScore
                        else
                            bestScore
                    let b =
                        if bestScore < b then
                            b
                        else bestScore
                    ()
                moveAnalysis childBoard validMoves.Tail (OtherTile(tile)) depth a b (not isMaxPlayer) bestScore
             

    let rec MiniMaxAlphaBeta state depth a b tile isMaxPlayer  =
        
        //Return Evaluation depending on state
        if depth = 0 || (getWinner state) = 0 then
            eval state
        else
            let bestScore = 0
            let validMoves = getValidMoves state tile
            if validMoves <> [] then
                moveAnalysis state validMoves tile depth a b isMaxPlayer bestScore
                0
            else MiniMaxAlphaBeta state depth a b tile isMaxPlayer

    type Class1() = 
        member this.X = "F#"
    