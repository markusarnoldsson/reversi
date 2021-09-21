namespace FSAI

module Minimax =
    
    //Temp functions
    let eval state = 10000
    let getWinner state = 0
    let getValidMoves state tile = [(1,2); (2,2); (3,2)]
    let MakeMove childState move tile = 0
    let OtherTile tile = 0


    let applyGetWinner getWinner (inputValue:byte[,]) = // En enda input
        let result:byte = getWinner inputValue
        result

    let applyEvaluation evaluation (inputValue:byte[,]) = // En enda input
        let result:int = evaluation inputValue
        result

    let applyOtherTile otherTile (inputValue:byte) = // En enda input
        let result:byte = otherTile inputValue
        result

    let applyGetValidMoves getValidMoves (firstValue:byte[,]) (secondValue:byte) = // Två inputs
        let result:(int*int)list = getValidMoves firstValue secondValue
        result

    let applyMakeMove makeMove (firstValue:byte[,]) (secondValue:(int*int)) (thirdValue:byte) = // Tre inputs
        () |> ignore

    // Minimax-algorithm med alpha-beta klippning

    let rec MiniMaxAlphaBeta state depth a b tile isMaxPlayer =

        //en rec funktion som sköter alpha-beta klippningen i stället för for-loopen
        let rec moveAnalysis (state: byte[,]) (validMoves: (int*int)list) (tile: byte) (a: int) (b: int) (isMaxPleyer: bool) (bestScore: int) =
            match validMoves with
            | [] -> bestScore
            | head::tail ->

                let childState = applyMakeMove state head tile 
                let nodeScore = MiniMaxAlphaBeta childState (depth-1) a b (OtherTile tile) (not isMaxPlayer)
                if isMaxPlayer then
                    let maxScore = max bestScore nodeScore
                    let nextA = max bestScore a

                    if b <= nextA then
                        maxScore //Basfall för den rekursiva funktionen
                    else
                        (moveAnalysis state tail tile nextA b isMaxPlayer maxScore)

                else
                    let minScore = min bestScore nodeScore
                    let nextB = min bestScore b

                    if nextB <= a then
                        minScore //Basfall för den rekursiva funktionen
                    else
                        (moveAnalysis state tail tile a nextB isMaxPlayer minScore)

        if depth = 0 || (applyGetWinner state <> empty) then
            (applyEvaluation state)
        else
            let bestScore = 
                match isMaxPlayer with
                | true -> System.Int32.MinValue
                | false -> System.Int32.MaxValue
            let validMoves = applyGetValidMoves state tile

            //titta ifall det finns några möjliga drag, isf fortsätt
            //annars byt spelare
            if validMoves.IsEmpty then
                (MiniMaxAlphaBeta state depth a b (applyOtherTile tile) (not isMaxPlayer))
            else
                (moveAnalysis state validMoves tile a b isMaxPlayer bestScore)
    type Class1() = 
        member this.X = "F#"
    