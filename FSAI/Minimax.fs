namespace FSAI

module Minimax =
    
    //Temp functions
    let eval state = 10000
    let getWinner state = 0
    let getValidMoves state tile = [(1,2); (2,2); (3,2)]
    let MakeMove childState move tile = 0
    let OtherTile tile = 0

    let rec moveAnalysis validMoves childBoard tile depth a b isMaxPlayer bestScore nodeScore =
        MakeMove childBoard i tile |> ignore
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

    let rec MiniMaxAlphaBeta state depth a b tile isMaxPlayer  =
        
        //Return Evaluation depending on state
        if depth = 0 || (getWinner state) = 0 then
            eval state
        else
            let bestScore = 0
            let validMoves = getValidMoves state tile
            if validMoves <> [] then
                let childBoard = state
                let nodeScore = MiniMaxAlphaBeta childBoard (depth - 1) a b (OtherTile(tile)) (not isMaxPlayer)
                moveAnalysis validMoves childBoard tile depth a b isMaxPlayer bestScore nodeScore
                0
            else MiniMaxAlphaBeta state depth a b tile isMaxPlayer

    type Class1() = 
        member this.X = "F#"
    