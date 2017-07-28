module PlantUml (plugin) where

-- This plugin allows you to include a plantuml diagram
-- in a page like this:
--
-- ~~~ {.puml name="deployment"}
-- @startuml
-- cloud cloud1
-- cloud cloud2
-- cloud cloud3
-- cloud cloud4
-- cloud cloud5
-- cloud1 -0- cloud2
-- cloud1 -0)- cloud3
-- cloud1 -(0- cloud4
-- cloud1 -(0)- cloud5
-- @enduml
-- ~~~
--
-- The "dot" and "plantuml" executable must be in the path.
-- The generated png file will be saved in the static img directory.

import GHC.IO.Handle
import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the temporary package on HackageDB
import System.IO.Temp (withTempFile)
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Environment (unsetEnv)
import System.FilePath ((</>))
import System.FilePath
import System.IO

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (id, classes, namevals) contents) | "plantuml" `elem` classes = do
  cfg <- askConfig
  let filetype = "svg"
      outdir = staticDir cfg </> "img"
      incontent = "@startuml\n" ++ contents ++ "\n@enduml" 
--  liftIO $ withTempFile outdir "diag.puml" $ \infile inhandle -> do
  liftIO $ do
--    unsetEnv "DISPLAY"
--    hPutStr inhandle "@startuml\n"
--    hPutStr inhandle contents
--    hPutStr inhandle "\n@enduml" 
--    hClose inhandle

--    let outname = takeFileName $ infile -<.> filetype
    let (name, outfile) = case lookup "name" namevals of
                            Just fn   -> ([Str fn], fn <.> filetype)
                            Nothing   -> ([], uniqueName contents <.> filetype)
    
    (ec, stdout, stderr) <- readProcessWithExitCode "java"
                            ["-jar", "/opt/plantuml.jar",
                             "-t" ++ filetype,
                             "-p"
--                             "<", infile,
--                             ">", "/tmp/ttt.svg"
                            ] incontent
    if ec == ExitSuccess
      then do
      inh <- openFile (outdir </> outfile) ReadWriteMode
      hPutStr inh stdout
      hClose inh
      return $ Para [ Image (id,classes,namevals) [Str outfile] ("/img" </> outfile, "") ]
      else error $ "plantuml returned an error status: " ++ stderr
transformBlock x = return x


-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
