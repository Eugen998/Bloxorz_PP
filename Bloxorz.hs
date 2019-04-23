{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

newLine :: Char
newLine = '\n'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HardTile | SoftTile | Block | Switch {pos_sw :: Position, status :: Bool} | EmptySpace | WinningTile | NewLine
    deriving (Eq,Ord)

instance Show Cell where
    show HardTile = [hardTile]
    show SoftTile = [softTile]
    show Block = [block]
    show (Switch _ _) = [switch]
    show EmptySpace = [emptySpace]
    show WinningTile = [winningTile]
    show NewLine = [newLine]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = LevelC {
    l_status :: Int,
    pos :: Position ,
    level_block1 :: Position ,
    level_block2 :: Position ,
    level_map :: A.Array Position Cell ,
    switches :: A.Array Int [Position]
}
    deriving (Eq, Ord)

{-
    *** Opțional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiati explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show.

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou.
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n".
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n".
-}

instance Show Level where
    show l = "\n" ++ (foldl (\acc1 i ->
        acc1 ++ (foldl (\acc2 j ->
            acc2 ++
                if (level_block1 l) /= (i,j) && (level_block2 l) /= (i,j) then show ((A.!) (level_map l) (i,j)) else [block])
                    "" [0..snd (pos l)]) ++ "\n")
        "" [0..fst (pos l)])
        ++ (if (gameWon l && (l_status l /= 0)) then "Congrats! You won!\n" else "")
        ++ (if (gameOver l && (l_status l /= 0)) then "Game Over\n" else "")
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel maxPos blkPos = LevelC{
    l_status = 0,
    pos = maxPos,
    level_block1 = blkPos,
    level_block2 = blkPos,
    level_map = A.array ((0,0),maxPos) ([((x,y), EmptySpace) | x <- [0..fst maxPos], y <- [0..snd maxPos]]) ,
    switches = A.array (0,((fst maxPos) * (snd maxPos))) ([(i,[]) | i <- [0..((fst maxPos) * (snd maxPos))]])
}

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat:
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile c tile_pos (LevelC l_sts l_p l_b1 l_b2 l_map l_s)
    | c == 'H'  = LevelC l_sts l_p l_b1 l_b2 (l_map A.// [(tile_pos,HardTile)]) l_s
    | c == 'S'  = LevelC l_sts l_p l_b1 l_b2 (l_map A.// [(tile_pos,SoftTile)]) l_s
    | c == 'W'  = LevelC l_sts l_p l_b1 l_b2 (l_map A.// [(tile_pos,WinningTile)]) l_s
    | otherwise = LevelC l_sts l_p l_b1 l_b2 l_map l_s

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch switch_pos switch_list (LevelC l_sts l_p l_b1 l_b2 l_map l_s) =
    LevelC l_sts l_p l_b1 l_b2 (l_map A.// [(switch_pos,Switch switch_pos False)]) (l_s A.// [(((snd l_p)*(fst switch_pos - 1) + (snd switch_pos)),switch_list)])

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică.
    În funcție de mecanica activată, vor avea loc modificări pe hartă.
-}

--Functie care verfica daca block-ul se afla pe harta sau a cazut
blockOnMap :: Level -> Bool
blockOnMap (LevelC _ _ l_b1 l_b2 l_map _)
    | l_b1 == l_b2 = if (l_b1 >= fst (A.bounds l_map) && l_b1 <= snd (A.bounds l_map)) then True else False
    | otherwise =
        if((l_b1 >= fst (A.bounds l_map) && l_b1 <= snd (A.bounds l_map)) && (l_b2 >= fst (A.bounds l_map) && l_b2 <= snd (A.bounds l_map)))
            then True
            else False

gameOver :: Level -> Bool
gameOver l
    | blockOnMap l == False = True
    | (level_block1 l) == (level_block2 l) =
        if (((A.!) (level_map l) (level_block1 l)) == EmptySpace || ((A.!) (level_map l) (level_block1 l)) == SoftTile)
            then True
            else False
    | otherwise =
        if (((A.!) (level_map l) (level_block1 l)) == EmptySpace || ((A.!) (level_map l) (level_block2 l)) == EmptySpace)
            then True
            else False

gameWon :: Level -> Bool
gameWon l
    | gameOver l == False =
        if(((level_block1 l) == (level_block2 l)) && ((A.!) (level_map l) (level_block1 l) == WinningTile))
            then True
            else False
    | otherwise = False

checkIfSwitch :: Cell -> Bool
checkIfSwitch (Switch _ _) = True
checkIfSwitch _ = False

activate :: Cell -> Level -> Level
activate sw (LevelC l_sts l_p l_b1 l_b2 l_map l_s)
    |checkIfSwitch sw == False = (LevelC l_sts l_p l_b1 l_b2 l_map l_s)
    |(status sw) == False =
        LevelC l_sts l_p l_b1 l_b2
        (new_map1 A.// [((pos_sw sw),new_switch1)])
        l_s
    | otherwise =
        LevelC l_sts l_p l_b1 l_b2
        (new_map2 A.// [((pos_sw sw),new_switch2)])
        l_s
    where
        new_map1 = l_map A.//  [(i,HardTile) | i <- ((A.!) l_s ((snd l_p) * (fst (pos_sw sw) - 1) + (snd (pos_sw sw))))]
        new_switch1 =  Switch (pos_sw sw) True
        new_map2 = l_map A.//  [(i,EmptySpace) | i <- ((A.!) l_s ((snd l_p) * (fst (pos_sw sw) - 1) + (snd (pos_sw sw))))]
        new_switch2 =  Switch (pos_sw sw) False

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
step :: Level -> Level
step l = activate ((A.!) (level_map l) (level_block1 l)) aux
    where aux = activate ((A.!) (level_map l) (level_block2 l)) l

moveNorth :: Level -> Level
moveNorth (LevelC check_status l_p l_b1 l_b2 l_map l_s)
    | l_b1 == l_b2 = step (LevelC check_status l_p ((fst l_b1 - 1),(snd l_b1)) ((fst l_b2 - 2),(snd l_b2)) l_map l_s)
    | (fst l_b1) == (fst l_b2) = step(LevelC check_status l_p ((fst l_b1 - 1),(snd l_b1)) ((fst l_b2 - 1),(snd l_b2)) l_map l_s)
    | otherwise = step (LevelC check_status l_p ((fst l_b1 - 1),(snd l_b1)) ((fst l_b2 - 2),(snd l_b2)) l_map l_s)
        where check_status = 13

moveSouth :: Level -> Level
moveSouth (LevelC check_status l_p l_b1 l_b2 l_map l_s)
    | l_b1 == l_b2 = step (LevelC check_status l_p ((fst l_b1 + 1),(snd l_b1)) ((fst l_b2 + 2),(snd l_b2)) l_map l_s)
    | (fst l_b1) == (fst l_b2) = step (LevelC check_status l_p ((fst l_b1 + 1),(snd l_b1)) ((fst l_b2 + 1),(snd l_b2)) l_map l_s)
    | otherwise = step (LevelC check_status l_p ((fst l_b1 + 1),(snd l_b1)) ((fst l_b2 + 2),(snd l_b2)) l_map l_s)
        where check_status = 13

moveEast :: Level -> Level
moveEast (LevelC check_status l_p l_b1 l_b2 l_map l_s)
    | l_b1 == l_b2 = step (LevelC check_status l_p ((fst l_b1),(snd l_b1 + 1)) ((fst l_b2),(snd l_b2 + 2)) l_map l_s)
    | (fst l_b1) == (fst l_b2) = step (LevelC check_status l_p ((fst l_b1),(snd l_b1 + 2)) ((fst l_b2),(snd l_b2 + 1)) l_map l_s)
    | otherwise = step (LevelC check_status l_p ((fst l_b1),(snd l_b1 + 1)) ((fst l_b2),(snd l_b2 + 1)) l_map l_s)
        where check_status = 13


moveWest :: Level -> Level
moveWest (LevelC check_status l_p l_b1 l_b2 l_map l_s)
    | l_b1 == l_b2 = step (LevelC check_status l_p ((fst l_b1),(snd l_b1 - 1)) ((fst l_b2),(snd l_b2 - 2)) l_map l_s)
    | (fst l_b1) == (fst l_b2) = step (LevelC check_status l_p ((fst l_b1),(snd l_b1 - 2)) ((fst l_b2),(snd l_b2 - 1)) l_map l_s)
    | otherwise = step (LevelC check_status l_p ((fst l_b1),(snd l_b1 - 1)) ((fst l_b2),(snd l_b2 - 1)) l_map l_s)
        where check_status = 13

move :: Directions -> Level -> Level
move d l
    | d             == North    = moveNorth l
    | d             == South    = moveSouth l
    | d             == East     = moveEast l
    | d             == West     = moveWest l
    | otherwise                 = l

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame l
    | gameOver l == True = False
    | gameWon l == True = False
    | otherwise = True

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.

    Hint: Un level câștigat nu are succesori!
    De asemenea, puteți ignora succesorii care
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors l = [(dir,(move dir l)) | dir <- [North,South,West,East], (gameOver (move dir l) /= True)]

    isGoal l = gameWon l

    -- Doar petru BONUS
    -- heuristic = undefined
