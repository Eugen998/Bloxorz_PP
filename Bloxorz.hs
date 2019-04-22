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

data Cell = HardTile | SoftTile | Block | Switch | EmptySpace | WinningTile | NewLine
    deriving (Eq,Ord)

instance Show Cell where
    show HardTile = [hardTile]
    show SoftTile = [softTile]
    show Block = [block]
    show Switch = [switch]
    show EmptySpace = [emptySpace]
    show WinningTile = [winningTile]
    show NewLine = [newLine]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = LevelC {
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
--[snd a | a <- A.assocs lm , fst a == (i,j)]
instance Show Level where
    --show (LevelC p lb lm) = show $ [NewLine] ++ foldl (\acc x -> acc ++ (take (snd p + 1) $ drop (x * (snd p + 1)) (A.elems lm)) ++ [NewLine]) [] [0..fst p]
    show (LevelC p lb1 lb2 lm _ ) = "\n" ++ (foldl (\acc1 i ->
        acc1 ++ (foldl (\acc2 j ->
            acc2 ++
                if lb1 /= (i,j) && lb2 /= (i,j) then show ((A.!) lm (i,j)) else [block])
                    "" [0..snd p]) ++ "\n")
        "" [0..fst p])
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel maxPos blkPos = LevelC{
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
addTile c pos (LevelC l_p l_b1 l_b2 l_map l_s)
    | c == 'H'  = LevelC l_p l_b1 l_b2 (l_map A.// [(pos,HardTile)]) l_s
    | c == 'S'  = LevelC l_p l_b1 l_b2 (l_map A.// [(pos,SoftTile)]) l_s
    | c == 'W'  = LevelC l_p l_b1 l_b2 (l_map A.// [(pos,WinningTile)]) l_s
    | otherwise = LevelC l_p l_b1 l_b2 l_map l_s

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos switch_list (LevelC l_p l_b1 l_b2 l_map l_s) =
    LevelC l_p l_b1 l_b2 (l_map A.// [(pos,Switch)]) (l_s A.// [(0,switch_list)])

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică.
    În funcție de mecanica activată, vor avea loc modificări pe hartă.
-}

activate :: Cell -> Level -> Level
activate = undefined

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move = undefined

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame = undefined

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.

    Hint: Un level câștigat nu are succesori!
    De asemenea, puteți ignora succesorii care
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined
