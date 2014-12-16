{-# LANGUAGE TemplateHaskell #-}
module Snap.StarterTH where

------------------------------------------------------------------------------
import qualified Data.Foldable as F
import           Data.List
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Directory (getCurrentDirectory)
import           System.Directory.Tree
import           System.FilePath
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Convenience types
type FileData = (String, String)
type DirData  = FilePath


------------------------------------------------------------------------------
-- Gets all the directories in a DirTree
--
getDirs :: [FilePath] -> DirTree a -> [FilePath]
getDirs prefix (Dir n c) = (intercalate "/" (reverse (n:prefix))) :
                           concatMap (getDirs (n:prefix)) c
getDirs _ (File _ _) = []
getDirs _ (Failed _ _) = []


------------------------------------------------------------------------------
-- Reads a directory and returns a tuple of the list of all directories
-- encountered and a list of filenames and content strings.
--
readTree :: FilePath -> IO ([DirData], [FileData])
readTree dir = do
    d <- readDirectory $ dir </> "."
    let ps = zipPaths $ "" :/ (dirTree d)
        fd = F.foldr (:) [] ps
        dirs = getDirs [] $ dirTree d
    return (drop 1 dirs, fd)


------------------------------------------------------------------------------
-- Calls readTree and returns its value in a quasiquote.
--
dirQ :: FilePath -> Q Exp
dirQ tplDir = do
    let dname = "project_template" </> tplDir
    d <- runIO . readTree $ dname
    c <- runIO getCurrentDirectory
    mapM_ (\(fname, _content) -> do
              -- 'fname' starts with dot (e.g. './src/SomeModule.hs'), hence the
              -- need to normalise.
              addDependentFile $ c </> dname </> (normalise fname)) (snd d)
    lift d


------------------------------------------------------------------------------
-- Creates a declaration assigning the specified name the value returned by
-- dirQ.
--
buildData :: String -> FilePath -> Q [Dec]
buildData dirName tplDir = do
    let dir  = mkName dirName
    typeSig <- SigD dir `fmap` [t| ([String], [(String, String)]) |]
    v       <- valD (varP dir) (normalB $ dirQ tplDir) []

    return [typeSig, v]
