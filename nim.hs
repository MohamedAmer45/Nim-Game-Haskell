import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe

main :: IO ()
main = nim

-- Player type declration
data PlayerIdentifier = One | Two deriving (Show, Eq)

-- Change player
change :: PlayerIdentifier -> PlayerIdentifier
change One = Two
change Two = One


--Board type declration
type Board = Seq.Seq Int


-- Starting Board Structure
startBoard :: Board
startBoard = Seq.fromList [1, 2, 3, 4, 5]


-- move function checks if the players move can be executed and returns the updated board
move :: Board -> (Int, Int) -> Maybe Board
move board (pile, blocks)
  | and [(Seq.index board pile) >= blocks,
          pile < 5] = Just (Seq.adjust (\x -> x - blocks) pile board)
  | otherwise = Nothing


-- display function transforms the basic shape of a starting board to a sequence of blocks
display :: Board -> String
display board = List.intercalate "\n" (zipWith (++) numbers (blocks board))
                where numbers = ["1. ", "2. ", "3. ", "4. ", "5. "]
                      blocks board = [(concat . take n) (repeat "# ")
                                    | n <- Fol.toList board]


-- nim is the main function and it welcomes the player and displays the starting board 
nim = do putStrLn "Welcome to Nim Game!"
         putStrLn (display startBoard)
         turn startBoard One


-- The turn method controls the game loop and displayes the player and asks for movement and checks if there
-- is any problems applying the movements.
turn :: Board -> PlayerIdentifier -> IO ()
turn board player = do putStrLn ("\nPlayer " ++ (show player) ++ " It is your turn !")
                       putStrLn "Choose a pile to remove blocks from"
                       pile <- getLine
                       putStrLn "How many blocks do you want to remove?"
                       blocks <- getLine
                       let newBoard = move board ((read pile) - 1, read blocks)
                       if newBoard == Nothing
                         then do putStrLn "Not valid movement"
                                 turn board player
                         else gameOver (fromJust newBoard) (player)


-- gameOver function checks if the game is over or the next turn should be called
gameOver :: Board -> PlayerIdentifier -> IO()
gameOver board player = do if board == Seq.fromList [0, 0, 0, 0, 0]
                           then putStrLn ("Player " ++ (show player)
                                         ++ " Congratulations, you win!")
                           else do putStrLn ""
                                   putStrLn (display board)
                                   turn board (change player)

