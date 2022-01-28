-- | My quick-and-dirty interprocess procedure call

module IPPC
  ( call
  , withChannel
  , Channel
  ) where


import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Text.Read       as TR
import           System.IO            (Handle, hClose, hPutStrLn)
import qualified System.Process.Typed as P


type Channel = P.Process Handle Handle ()


call :: String -> [Word] -> IO [[Word]]
call funcName args = withChannel $ \chan -> do
  let stdin = P.getStdin chan
      stdout = P.getStdout chan
  hPutStrLn stdin . unwords $ funcName : map show args
  hClose stdin
  r <- mapM (mapM (either fail (return . fst) . TR.decimal) . T.split (== ','))
    . T.words
    =<< TIO.hGetLine stdout
  -- T.words の際に空のリストが消えてしまうので加える
  return ([] : r)


withChannel :: (Channel -> IO a) -> IO a
withChannel = P.withProcessWait_ cfg
 where
  cfg = P.setStdout P.createPipe
      . P.setStdin P.createPipe
      $ P.proc "ruby" ["./powersets.rb"]
