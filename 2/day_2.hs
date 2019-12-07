{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Array.IO
import Control.Monad
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import System.Environment (getArgs)

newtype Index = Index { getIndex :: Int }
  deriving (Eq, Show, Ix, Ord, Num, Enum)

type Intcode = IOArray Index Int

data Env = Env { pointer :: Index, prog :: Intcode }

data State = Continue Env | End Env | Unexpected Env Int

getEnv :: State -> Env
getEnv (Continue e) = e
getEnv (End e) = e
getEnv (Unexpected e _) = e

load :: Text -> IO Intcode
load text = do
  let vals = T.splitOn "," text
  arr <- newArray (0, (Index $ length vals - 1)) 0
  forM_ (zip [0..] vals) $ \(i, v) -> do
    writeArray arr (Index i) (read (T.unpack v))
  pure arr

showProgram :: Intcode -> IO String
showProgram prog = do
  (_, len) <- getBounds prog
  vals <- forM [0..len] $ \i -> readArray prog i
  pure $ intercalate "," (map show vals)

showState :: State -> IO String
showState (Continue (Env { pointer, prog })) = do
  progString <- showProgram prog
  pure $ "[CONTINUE] (" ++ (show pointer) ++ ") " ++ progString
showState (End (Env { pointer, prog })) = showProgram prog
showState (Unexpected (Env { pointer, prog }) opcode) = do
  progString <- showProgram prog
  pure $ "[UNEXPECTED - " ++ show opcode ++ "] (" ++ (show pointer) ++ ") " ++ progString

printState :: State -> IO ()
printState state = showState state >>= putStrLn

initialEnv :: Intcode -> Env
initialEnv prog = Env { pointer = 0, prog }

incrPointer :: Env -> Env
incrPointer (Env { pointer, prog }) = Env { pointer = pointer + 4, prog }

run :: State -> IO State
run r@(Unexpected _ _) = pure r
run r@(End _) = pure r
run (Continue e@(Env { pointer, prog })) = do
  opcode <- readArray prog pointer
  case opcode of
    1  -> runOp (+) e >>= run
    2  -> runOp (*) e >>= run
    99 -> pure $ End e
    _  -> pure $ Unexpected e opcode

getValueAtIndex ::  Env -> Index -> IO Int
getValueAtIndex (Env { pointer, prog }) i = do
  nextIndex <- Index <$> readArray prog i
  readArray prog nextIndex

writeValueAtIndex :: Env -> Index -> Int -> IO ()
writeValueAtIndex (Env { pointer, prog }) i val = do
  nextIndex <- Index <$> readArray prog i
  writeArray prog nextIndex val

writeValue :: Env -> Index -> Int -> IO ()
writeValue (Env { pointer, prog }) i val = writeArray prog i val

getValue :: Env -> Index -> IO Int
getValue (Env { pointer, prog }) i = readArray prog i

runOp :: (Int -> Int -> Int) -> Env -> IO State
runOp fn env@(Env { pointer, prog }) = do
  input1 <- getValueAtIndex env (pointer+1)
  input2 <- getValueAtIndex env (pointer+2)
  let val = fn input1 input2
  writeValueAtIndex env (pointer+3) val
  pure $ Continue (incrPointer env)

usage :: IO ()
usage = putStrLn "usage: runhaskell day_2.hs <program_file>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (file:_) -> do
      contents <- decodeUtf8 <$> BS.readFile file
      forM_ [(noun, verb) | noun <- [0..100], verb <- [0..100]] $ \(noun, verb) -> do
        program <- load contents
        let initial = initialEnv program
        writeValue initial 1 noun
        writeValue initial 2 verb
        final <- run (Continue initial)
        let finalEnv = getEnv final
        v <- getValue finalEnv 0
        case v of
          19690720 -> putStr $ "noun: " ++ show noun ++ ", verb: " ++ show verb
          _ -> putStr "."
