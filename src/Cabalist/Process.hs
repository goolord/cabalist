-- | Running the external tools a release needs (git, cabal, tar, curl,
-- hlint), either quietly to read their output or with every line streamed to
-- a 'Logger' so the GUI can show a command's progress as it happens.
module Cabalist.Process
  ( -- * Logging
    Logger (..)
  , nullLogger
    -- * Step failures
  , StepError (..)
  , failStep
    -- * Running commands
  , runLogged
  , runLogged_
  , exitOk
  , readCmd
  , readCmdBytes
  , readCmdOk
  , renderCommand
  , haveProgram
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, IOException, SomeException, finally, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.IO (Handle, hClose, hIsEOF, hSetBinaryMode)
import System.Process

-- | Where a command's output goes, and how a running command can be stopped.
data Logger = Logger
  { logLine :: Text -> IO ()
  -- ^ One line of output, or a note from cabalist itself.
  , logProcess :: Maybe ProcessHandle -> IO ()
  -- ^ The process now running, so that cancelling a job can terminate it.
  , logCancelled :: IO Bool
  -- ^ Whether the job was cancelled; checked before each command.
  }

nullLogger :: Logger
nullLogger = Logger (\_ -> pure ()) (\_ -> pure ()) (pure False)

-- | Why a release step stopped.
data StepError
  = StepFailed Text
  | StepCancelled
  deriving (Show)

instance Exception StepError

failStep :: Text -> IO a
failStep = throwIO . StepFailed

-- | A command line as a shell would show it, with secrets hidden: the log
-- is shown, copied and pasted.
renderCommand :: FilePath -> [String] -> Text
renderCommand prog args = T.unwords (map quote (prog : map hide args))
  where
    hide a = case break (== '=') a of
      (flag, '=' : _) | flag `elem` ["--token", "--password"] -> flag <> "=…"
      _ -> a
    quote a
      | null a = "\"\""
      | any (`elem` (" \t\"'" :: String)) a = T.pack (show a)
      | otherwise = T.pack a

haveProgram :: String -> IO Bool
haveProgram prog = isJust <$> findExecutable prog

-- | Run a command in a directory, streaming its combined stdout and stderr to
-- the logger line by line. @stdinText@ is written to its standard input, which
-- is then closed, so a command that prompts reads end-of-file rather than
-- waiting forever. Returns the exit code and every line it printed.
--
-- A program that cannot be started is reported in the log and as exit code
-- 127, like a shell would. A non-zero exit is not logged here: a caller that
-- treats it as failure does so with 'exitOk'.
runLogged :: Logger -> FilePath -> FilePath -> [String] -> Maybe Text -> IO (ExitCode, [Text])
runLogged lg dir prog args stdinText = do
  cancelled <- logCancelled lg
  when cancelled (throwIO StepCancelled)
  logLine lg ("$ " <> renderCommand prog args)
  (readEnd, writeEnd) <- createPipe
  hSetBinaryMode readEnd True
  let cp =
        (proc prog args)
          { cwd = Just dir
          , std_in = CreatePipe
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          , -- On Windows, cabal runs in a job object, so stopping it stops the
            -- GHC processes it started too. Not other tools: waiting on a job
            -- waits for every process in it, and git can leave a helper (a
            -- credential cache, say) running long after it exits.
            use_process_jobs = map toLower (takeBaseName prog) == "cabal"
          }
  started <- try (createProcess cp)
  case started of
    Left (e :: IOException) -> do
      hClose writeEnd
      hClose readEnd
      logLine lg ("cabalist: could not run " <> T.pack prog <> ": " <> T.pack (show e))
      pure (ExitFailure 127, [])
    Right (mIn, _, _, ph) -> do
      logProcess lg (Just ph)
      forM_ mIn $ \h -> void (try (mapM_ (B.hPut h . encodeUtf8) stdinText >> hClose h) :: IO (Either SomeException ()))
      linesRef <- newIORef []
      readLines readEnd (\l -> atomicModifyIORef' linesRef (\ls -> (l : ls, ())) >> logLine lg l)
        `finally` hClose readEnd
      code <- waitForProcess ph
      logProcess lg Nothing
      out <- reverse <$> readIORef linesRef
      nowCancelled <- logCancelled lg
      if nowCancelled then throwIO StepCancelled else pure (code, out)

-- | 'runLogged' that fails the step on a non-zero exit.
runLogged_ :: Logger -> FilePath -> FilePath -> [String] -> IO [Text]
runLogged_ lg dir prog args = do
  (code, out) <- runLogged lg dir prog args Nothing
  out <$ exitOk lg prog args code

-- | Fail the step, with the exit code in the log, unless a command succeeded.
exitOk :: Logger -> FilePath -> [String] -> ExitCode -> IO ()
exitOk _ _ _ ExitSuccess = pure ()
exitOk lg prog args code = do
  logLine lg ("cabalist: " <> T.pack prog <> " exited with " <> T.pack (show code))
  failStep (T.pack (takeBaseName prog) <> " " <> T.pack (unwords (take 1 args)) <> " failed")

-- | Split a byte stream into lines, dropping carriage returns, and decode each
-- as UTF-8 (leniently: build tools print whatever their locale gives them).
readLines :: Handle -> (Text -> IO ()) -> IO ()
readLines h emit = go
  where
    go = do
      eof <- hIsEOF h
      unless eof $ do
        l <- BC.hGetLine h
        emit (decodeUtf8Lenient (BC.filter (/= '\r') l))
        go

-- | Run a command quietly and return its exit code and standard output, with
-- trailing whitespace removed. Standard error is discarded. A program that
-- cannot be started gives exit code 127.
readCmd :: FilePath -> FilePath -> [String] -> IO (ExitCode, Text)
readCmd dir prog args =
  readCmdBytes dir prog args B.empty >>= \case
    Left _ -> pure (ExitFailure 127, "")
    Right (code, out, _) -> pure (code, T.stripEnd (decodeUtf8Lenient (BC.filter (/= '\r') out)))

-- | Run a command quietly with the given bytes on its standard input, which
-- is then closed, and return its exit code, standard output and standard
-- error; or why it could not be started.
readCmdBytes :: FilePath -> FilePath -> [String] -> ByteString -> IO (Either IOException (ExitCode, ByteString, ByteString))
readCmdBytes dir prog args input = try $ do
  (Just hIn, Just hOut, Just hErr, ph) <-
    createProcess (proc prog args) {cwd = Just dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  -- Drain stderr alongside stdout, so neither pipe fills and blocks.
  errVar <- newEmptyMVar
  _ <- forkIO (B.hGetContents hErr >>= putMVar errVar)
  -- A command may exit without reading its input.
  void (try (B.hPut hIn input >> hClose hIn) :: IO (Either IOException ()))
  out <- B.hGetContents hOut
  err <- takeMVar errVar
  code <- waitForProcess ph
  pure (code, out, err)

-- | The output of a command that succeeded, or 'Nothing'.
readCmdOk :: FilePath -> FilePath -> [String] -> IO (Maybe Text)
readCmdOk dir prog args =
  readCmd dir prog args >>= \case
    (ExitSuccess, out) -> pure (Just out)
    _ -> pure Nothing
