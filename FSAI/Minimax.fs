namespace FSAI

module Minimax =
    
    //values
    let empty   = byte 0
    let white   = byte 1
    let black   = byte 2
    let Valid   = byte 3
    let Tie     = byte 4

    //movableDirections - innehåller en lista av möjligla riktningar för drag
    let moveableDirections =
        [
            (-1,1);
            (0,1);
            (1,1);
            (-1,0);
            (1,0);
            (-1,-1);
            (0,-1);
            (1,-1);
        ]


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
    