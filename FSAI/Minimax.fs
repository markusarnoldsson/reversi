namespace FSAI

module Minimax =
    
    // Values
    let empty   = byte 0
    let white   = byte 1
    let black   = byte 2
    let Valid   = byte 3
    let Tie     = byte 4

    // MovableDirections - innehåller en lista av möjligla riktningar för drag
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

    // Returnerar den motsatta tile
    let OtherTile tile =
        if tile = black then
            white
        else if tile = white then
            black
        else
            byte 0

    // Returnerar true/false ifall den är på boarden
    let IsOnBoard x y =
        0 <= x && x <= 7 && 0 <= y && y <= 7

    // Returnerar antal hörn för en specifik tile
    let CountCorners (board: byte[,]) (tile: byte) =
        let mutable corners = 0

        if board.[0,0] = tile then
            corners <- corners + 1
        if board.[0,7] = tile then
            corners <- corners + 1
        if board.[7,0] = tile then
            corners <- corners + 1
        if board.[7,7] = tile then
            corners <- corners + 1
        corners

    let GetScore (board: byte[,]) (tile: byte) =
        Seq.length((Seq.filter(fun cell -> cell = tile) (Seq.cast board)))

    // Denna funktion följer en riktning och tittar ifall den är giltig, isf returnerar den true
    let rec checkValidDirection (board: byte[,]) (x: int) (y: int) (direction: (int*int)) (tile: byte) =
        if IsOnBoard x y then
            //Ifall den motsatta biten finns på denna plats -> fortsätt i den riktningen rekursivt
            if board.[x,y] = OtherTile tile then
                let directionX, directionY = direction
                checkValidDirection board (x+directionX) (y+directionY) direction tile
            elif board.[x,y] = tile then
                true
            else
                false
        else
            false

    // Denna funktion tittar alla riktningar och returnerar true ifall det befinner sig en giltig riktning
    let rec checkDirections (board: byte[,]) (x: int) (y: int) (direction: (int*int)list) (tile: byte) =
        match direction with
        //Ifall direction är tom -> finns inga giltiga riktningar -> returnerna false
        | [] -> false
        //Annars -> ta ut head(första riktningen i listan) och skicka till checkValidDirections
        | head::tail ->
            let directionX, directionY = head
            if checkValidDirection board (x+directionX) (y+directionY) head tile then
                true
            else
                checkDirections board x y tail tile

    // Funktionen tittar ifall ett drag är giltigt genom rekursivt gå igenom alla möjliga drag tills den hittar en tom ruta som också vänder bitar
    let ValidMove (board:byte[,]) (x: int) (y:int) (tile: byte) =
        if board.[x,y] = empty then
            checkDirections board x y moveableDirections tile
        else
            false

    // Funktionen hämtar alla giltiga drag genom att kontrollera alla validMoves med board
    let GetValidMoves (board: byte[,]) (tile: byte) =
        let validMoves = [
            for x in 0..7 do
               for y in 0..7 do if (ValidMove board x y tile) then yield (x,y)]
        validMoves

    // Returnerar en lista med de flippade bitarna
    let GetFlippedPieces (board: byte[,]) (move: (int*int)) (tile: byte) =
        let moveX, moveY = move
        if board.[moveX, moveY] = empty then 
            []
        else
            //funktionen tittar mot alla riktningar och summerar resultatet
            let rec GetDirectionFlippedPieces (board: byte[,]) (tile: byte) (x: int) (y: int) (direction: (int*int)list) =
                if direction = List.Empty then 
                    []
                else
                    let nextX = x + fst direction.Head
                    let nextY = y + snd direction.Head

                    //ifall man går över en motstående bit, stega vidare
                    if (IsOnBoard nextX nextY) && (board.[nextX,nextY] = OtherTile tile) then
 
                        // Rekursivt gå igenom riktningen 
                        let rec getFlippedPieceDirection (board: byte[,]) (tile: byte) (coordinates: (int*int)) (direction: (int*int)) =
                            let xCoord, yCoord = coordinates
                            let xDirection, yDirection = direction

                            // ifall vi befinner oss på boarden och på en motsatt bit, returnera listan med denna position
                            if IsOnBoard xCoord yCoord && board.[xCoord,yCoord] = OtherTile tile then
                                (xCoord, yCoord)::getFlippedPieceDirection board tile ((xCoord + xDirection),(yCoord + yDirection)) direction
                            else
                                []
                        let flippedPieces = getFlippedPieceDirection board tile (x,y) direction.Head

                        //kombinera flippedPieces listan med (rekursivt) nya listan
                        flippedPieces @ GetDirectionFlippedPieces board tile x y direction.Tail
                    else
                        GetDirectionFlippedPieces board tile x y direction.Tail

            GetDirectionFlippedPieces board tile moveX moveY moveableDirections
        
    // Byter den utvalde platsens bit samt de motsvarande bitarna
    let MakeMove (board: byte[,]) (move: (int*int)) (tile: byte) =
        let flippedPieces = GetFlippedPieces board move tile
        let childBoard = Array2D.copy board

        // Ange alla flippedPieces från flippedPieces listan till childBoard
        for flippedPiece in flippedPieces do
            childBoard.[fst flippedPiece, snd flippedPiece] <- tile

        // Ifall det finns flipped pieces -> ange tile moved to och returnera childBoard
        if flippedPieces.Length > 0 then
            childBoard.[fst move, snd move] <- tile
            childBoard
        else
            childBoard

    // Returnerar ett evalueted score för boarden
    let Evaluation (board: byte[,]) =
        let mutable evaluation = 0
        let whiteScore = GetScore board white
        let blackScore = GetScore board black
        let whiteMobility = (GetValidMoves board white).Length
        let blackMobility = (GetValidMoves board black).Length

        if whiteScore = 0 then
            -200000
        elif blackScore = 0 then
            200000
        else
            //Ifall att board skulle vara full eller att det inte finns några validMoves, titta vilken tile
            //som har högst score, returnera sedan ett värde
            if whiteScore + blackScore = 64 || whiteMobility + blackMobility = 0 then
                if blackScore < whiteScore then
                    -100000 - whiteScore + blackScore
                elif blackScore > whiteScore then
                    100000 + blackScore - whiteScore
                else int 0
            //Ifall inte.. beräkna och skicka tillbaka ett evaluated värde
            else
                evaluation <- evaluation + (blackScore - whiteScore)
                if blackScore + whiteScore > 55 then
                    blackScore - whiteScore
                else
                    evaluation <- evaluation + (blackMobility - whiteMobility) * 10
                    evaluation <- ((CountCorners board black) - (CountCorners board white)) * 100
                    evaluation

    // Returnar vinnaren, lika eller 0 beroende på hur spelet ligger till
    let GetWinner (board: byte[,]) =
        let whiteScore = GetScore board white
        let blackScore = GetScore board black
        //Ifall nån spelare har noll i score, eller att hela boarden är fylld eller att ingen spelare har några valid moves, titta vem som har vunnit eller ifall lika
        if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 || (GetValidMoves board white).Length + (GetValidMoves board black).Length = 0 then
            if blackScore > whiteScore then
                black
            elif whiteScore > blackScore then
                white
            else Tie
        else byte 0

    // Minimax-algorithm med alpha-beta klippning
    let rec MiniMaxAlphaBeta state depth a b tile isMaxPlayer =

        //en rec funktion som sköter alpha-beta klippningen i stället för for-loopen
        let rec moveAnalysis (state: byte[,]) (validMoves: (int*int)list) (tile: byte) (a: int) (b: int) (isMaxPleyer: bool) (bestScore: int) =
            match validMoves with
            | [] -> bestScore
            | head::tail ->

                let childState = MakeMove state head tile 
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

        if depth = 0 || (GetWinner state <> empty) then
            (Evaluation state)
        else
            let bestScore = 
                match isMaxPlayer with
                | true -> System.Int32.MinValue
                | false -> System.Int32.MaxValue
            let validMoves = GetValidMoves state tile

            //titta ifall det finns några möjliga drag, isf fortsätt
            //annars byt spelare
            if validMoves.IsEmpty then
                (MiniMaxAlphaBeta state depth a b (OtherTile tile) (not isMaxPlayer))
            else
                (moveAnalysis state validMoves tile a b isMaxPlayer bestScore)

    type Class1() = 
        member this.X = "F#"
    