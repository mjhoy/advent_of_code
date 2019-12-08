{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Array (Array, (//), (!))
import qualified Data.Array as A
import qualified Data.ByteString as BS
import           Data.Ix (Ix)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           System.Environment (getArgs)

-- import           Debug.Trace
import           Text.Read (readMaybe)

-- i could keep building off day_2, but i will probably just rewrite.

usage :: IO ()
usage = putStrLn "usage: runhaskell day_4.hs <program_file>"

newtype Index = Index Int
  deriving (Eq, Ix, Ord, Num, Enum)

instance Show Index where
  show (Index i) = show i

-- program exceptions
data Exception = OutOfBounds Index
               | UnknownOpcode Int
               | UnknownMode Int
               | BadInput String

instance Show Exception where
  show (OutOfBounds i) = "[EXCEPTION] Out of bounds access at index " ++ show i
  show (UnknownOpcode i) = "[EXCEPTION] Unknown opcode " ++ show i
  show (UnknownMode i) = "[EXCEPTION] Unknown mode " ++ show i
  show (BadInput i) = "[EXCEPTION] Couldn't parse input " ++ show i

-- state of our program: current pointer and program code.
type Env = (Index, Array Index Int)

-- E is our program evaluation context. it has state, may fail
-- computation, and needs IO.
newtype E a = E { _runE :: ExceptT Exception (StateT Env IO) a }
  deriving (Functor, Applicative, Monad,
            MonadState Env,
            MonadError Exception,
            MonadIO)

-- run the program for text `progText`.
runE :: Text -> E a -> IO (Either Exception a, Env)
runE progText (E k) = runStateT (runExceptT k) (loadEnv progText)
  where
    loadEnv :: Text -> Env
    loadEnv text =
      let vals = zip [0..] $ T.splitOn "," $ T.strip text
          toAssoc (i, v) = ((Index i), (read (T.unpack v)))
          bounds = (0, (Index $ length vals - 1))
          program = A.array bounds (map toAssoc vals)
      in (0, program)

checkBounds :: Index -> E ()
checkBounds i = do
  (_, program) <- get
  let (lowerBounds, upperBounds) = A.bounds program
  if i < lowerBounds || i > upperBounds
    then throwError $ OutOfBounds i
    else pure ()

-- relative indices are added to the current program pointer
resolveRelative :: Index -> E Index
resolveRelative i = do
  (pointer, _) <- get
  pure $ pointer + i

data Mode = Immediate | Position

-- the ' methods use absolute indices

peek' :: Mode -> Index -> E Int
peek' Immediate i = do
  checkBounds i
  (_, program) <- get
  pure $ program ! i
peek' Position i = peek' Immediate i >>= peek' Immediate . Index

write' :: Mode -> Int -> Index -> E ()
write' Immediate v i = do
  checkBounds i
  (pointer, program) <- get
  put $ (pointer, program // [(i, v)])
write' Position v i = peek' Immediate i >>= write' Immediate v . Index

-- the non-' methods use relative

peek :: Mode -> Index -> E Int
peek m index = resolveRelative index >>= peek' m

write :: Mode -> Int -> Index -> E ()
write m v i = resolveRelative i >>= write' m v

advance :: Index -> E ()
advance n = do
  (pointer, program) <- get
  put (pointer + n, program)

getInput :: E Int
getInput = do
  liftIO $ putStr "Input: "
  res <- liftIO getLine
  case readMaybe res of
    Just x -> pure x
    Nothing -> throwError $ BadInput res

output :: Int -> E ()
output i = liftIO $ putStrLn (show i)

execute :: E ()
execute = do
  val <- peek Immediate 0
  let opcode = val `mod` 100
  let modeDigits = map (`mod` 10) $ takeWhile (> 0) $ iterate (`div` 10) $ val `div` 100
  modes <- mapM digit2Mode modeDigits

  case (opcode, modes ++ repeat Position) of

    (1, (m1:m2:m3:_)) ->
      (+) <$> peek m1 1 <*> peek m2 2 >>= writeAdvance m3 3 >> execute

    (2, (m1:m2:m3:_)) ->
      (*) <$> peek m1 1 <*> peek m2 2 >>= writeAdvance m3 3 >> execute

    (3, (m1:_)) ->
      getInput >>= writeAdvance m1 1 >> execute

    (4, (m1:_)) ->
      showAdvance m1 1 >> execute

    (99,_) -> pure ()

    (x,_)  -> throwError $ UnknownOpcode x

    where

      digit2Mode :: Int -> E Mode
      digit2Mode 0 = pure Position
      digit2Mode 1 = pure Immediate
      digit2Mode n = throwError $ UnknownMode n

      writeAdvance :: Mode -> Index -> Int -> E ()
      writeAdvance m i v = write m v i >> advance (i + 1)

      showAdvance :: Mode -> Index -> E ()
      showAdvance m i = peek m i >>= output >> advance (i + 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (file:_) -> do
      contents <- decodeUtf8 <$> BS.readFile file
      res <- runE contents execute
      putStrLn "[Program finished]"
      -- putStrLn $ show $ fst res
      -- putStrLn $ show $ snd res
