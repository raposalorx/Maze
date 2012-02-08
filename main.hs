import Data.List
import System.Random
import System( getArgs )
import Data.Functor

data Tile = Wall | Open | Block
  deriving (Show, Read, Eq)

data Move = Upm | Downm | Leftm | Rightm
  deriving (Show, Read, Eq)

data Maze = Maze (Int, Int) [[Tile]] [(Int, Int)] StdGen

identity :: Int -> Int -> Int -> Maze
identity w h seed =  Maze (2,2) ((take (w+2) $ repeat Block):
                (take h $ repeat $ Block:
                  (take w $ repeat Wall)++
                  [Block])++
                [(take (w+2) $ repeat Block)]) [(2,2)] $ mkStdGen seed

textRender :: [[Tile]] -> [[Char]]
textRender tiles = map (\a -> map render $ drop 1 $ take ((length a)-1) a) $ drop 1 $ take ((length tiles)-1) tiles
  where
    render Block = '*'
    render Wall = '#'
    render Open = '.'

setTile :: Tile -> Maze -> Maze
setTile new (Maze pos@(x,y) tiles jumps g) = Maze pos newYs jumps g
  where
    newYs = let (before, after') = splitAt y tiles
                after = drop 1 after'
            in before++[newXs]++after
    newXs = let (before, after') = splitAt x (tiles!!y)
                after = drop 1 after'
            in before++[new]++after

mazeStep :: Maze -> Maze
mazeStep (Maze pos tiles [] g) = Maze (0,0) tiles [] g
mazeStep maze@(Maze pos@(x,y) tiles jumps g) =  let poss = possiblities pos tiles
                                                    (r,newg) = next g
                                                in Maze (if [] /= poss then makeMove pos $ (!!(r`rem`(length poss))) poss else (!!(r`rem`(length jumps))) jumps) tiles jumps newg

makeMove :: (Int, Int) -> Move -> (Int, Int)
makeMove (x,y) Upm = (x,y-2)
makeMove (x,y) Downm = (x,y+2)
makeMove (x,y) Leftm = (x-2,y)
makeMove (x,y) Rightm = (x+2,y)

possiblities :: (Int, Int) -> [[Tile]] -> [Move]
possiblities (x,y) tiles = []++(if tiles!!(y-2)!!x == Wall then [Upm] else [])++
                              (if tiles!!(y+2)!!x == Wall then [Downm] else [])++
                              (if tiles!!y!!(x-2) == Wall then [Leftm] else [])++
                              (if tiles!!y!!(x+2) == Wall then [Rightm] else [])

jumpCands :: Maze -> Maze
jumpCands maze@(Maze (0,0) titles jumps g) = maze
jumpCands (Maze pos tiles jumps g) = Maze pos tiles (filter (\a -> [] /= possiblities a tiles) jumps ++ if pos `notElem` jumps then [pos] else []) g

setMiddle :: Maze -> Maze -> Maze
setMiddle (Maze pos@(x,y) tiles jumps g) (Maze pos2@(x2,y2) tiles2 jumps2 g2) = let (Maze _ tiles3 _ _) = setTile Open (Maze findMid tiles2 jumps2 g2)
                                                                                in (Maze pos2 tiles3 jumps2 g2)
  where
    findMid | x == x2 && y == y2+2 = (x,y-1)
            | x == x2 && y == y2-2 = (x,y+1)
            | y == y2 && x == x2+2 = (x-1,y)
            | y == y2 && x == x2-2 = (x+1,y)
            | otherwise = (x,y)

runStep :: Maze -> Maze
runStep maze = setMiddle maze $ mazeStep $ jumpCands $ setTile Open maze

runFor :: Int -> (a -> a) -> a -> a
runFor 0 _ d = d
runFor i f d = runFor (i-1) f $ f d

main = do
  args <- getArgs
  let w = read $ args!!0
  let h = read $ args!!1
  let i = read $ args!!2
  let (Maze p t j _) = setTile Block $ runFor i runStep $ identity w h 1024
  putStr $ unlines.textRender $ t

--putStr $ unlines.textRender $ (\a -> setTile (mazeStep (1,1) [(1,1)] a) Open a) $ setTile (1,1) Open $ identity 150 50

