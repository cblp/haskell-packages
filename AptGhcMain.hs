import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Lazy as BSL    ( drop
                                                , isSuffixOf
                                                , length
                                                , take
                                                )
import qualified Data.ByteString.Lazy.Char8 as BSL (lines, putStrLn)
import           System.Environment
import           System.Process.QQ

main :: IO ()
main = runScript $ do
    usage <- buildUsage

    commandAndArgs <- scriptIO getArgs
    guardNoteT usage $ length commandAndArgs >= 1
    let command:args = commandAndArgs
    proc <- tryLookup ("Unknown command " ++ show command) command commands
    proc args

    where commands = [("pkgnames", pkgnames)]

buildUsage :: Script String
buildUsage = do
    progName <- scriptIO getProgName
    return $ "Usage: " ++ progName ++ " [OPTIONS] COMMAND"

pkgnames :: [String] -> Script ()
pkgnames _ = scriptIO $ do
    libghcs <- BSL.lines <$> [cmd| apt-cache pkgnames libghc- |]
    let pkgs =  map ( BSL.drop (BSL.length "libghc-")
                    . bsl_dropEnd (BSL.length "-dev") )
                $ filter (BSL.isSuffixOf "-dev")
                $ libghcs
    mapM_ BSL.putStrLn pkgs

    where bsl_dropEnd n s = BSL.take (BSL.length s - n) s

guardNoteT :: Monad m => e -> Bool -> EitherT e m ()
guardNoteT _ True  = right ()
guardNoteT e False = left  e

tryLookup :: (Eq k, Monad m) => e -> k -> [(k, v)] -> EitherT e m v
tryLookup e k kvs = hoistEither . note e $ lookup k kvs
