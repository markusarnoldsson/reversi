namespace FSAI

module Minimax =
    
    //Temp functions
    let eval state = 10000
    let getWinner state = 0
    let getValidMoves state tile = [(1,2); (2,2); (3,2)]

    let rec MiniMaxAlphaBeta state depth a b tile isMaxPlayer =
        
        //Return Evaluation depending on state
        if depth = 0 || (getWinner state) = 0 then
            eval state
        else
            let validMoves = getValidMoves state tile
            let bestScore = 0
            if validMoves <> [] then
                //FOR EACH
                0
            else MiniMaxAlphaBeta state depth a b tile isMaxPlayer

    type Class1() = 
        member this.X = "F#"
    