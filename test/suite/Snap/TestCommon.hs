{-# LANGUAGE ScopedTypeVariables #-}

module Snap.TestCommon where

------------------------------------------------------------------------------
import Control.Concurrent   ( threadDelay                )
import Control.Exception    ( ErrorCall(..)
                            , SomeException
                            , bracket
                            , catch
                            , throwIO
                            )
import Control.Monad        ( forM_                      )
import Data.Maybe           ( fromMaybe                  )
import Data.Monoid          ( First(..), mconcat         )
import Prelude       hiding ( catch                      )
import System.Cmd           ( system                     )
import System.Directory     ( doesFileExist
                            , getCurrentDirectory
                            , findExecutable
                            , removeFile
                            )
import System.Environment   ( getEnv                     )
import System.Exit          ( ExitCode(..)               )
import System.FilePath      ( joinPath, splitPath, (</>) )
import System.FilePath.Glob ( compile, globDir1          )
import System.Process       ( runCommand
                            , terminateProcess
                            , waitForProcess
                            )

------------------------------------------------------------------------------
import SafeCWD


------------------------------------------------------------------------------
testGeneratedProject :: String  -- ^ project name and directory
                     -> String  -- ^ arguments to @snap init@
                     -> String  -- ^ arguments to @cabal install@
                     -> Int     -- ^ port to run http server on
                     -> IO ()   -- ^ action to run when the server goes up
                     -> IO ()
testGeneratedProject projName snapInitArgs cabalInstallArgs httpPort
                     testAction = do
    cwd <- getCurrentDirectory

    --------------------------------------------------------------------------
    let segments     = reverse $ splitPath cwd
        projectPath  = cwd </> "test-snap-exe" </> projName
        snapRoot     = joinPath $ reverse $ drop 1 segments
        snapRepos    = joinPath $ reverse $ drop 2 segments
        sandbox      = cwd </> "test-cabal-dev"
        cabalDevArgs = "-s " ++ sandbox
        args         = cabalDevArgs ++ " --reinstall " ++ cabalInstallArgs

        ----------------------------------------------------------------------
        initialize = do
            snapExe <- findSnap
            systemOrDie $ snapExe ++ " init " ++ snapInitArgs

            snapCoreSrc     <- fromEnv "SNAP_CORE_SRC" $
                               snapRepos </> "snap-core"
            snapServerSrc   <- fromEnv "SNAP_SERVER_SRC" $
                               snapRepos </> "snap-server"
            xmlhtmlSrc      <- fromEnv "XMLHTML_SRC" $ snapRepos </> "xmlhtml"
            heistSrc        <- fromEnv "HEIST_SRC" $ snapRepos </> "heist"
            dynLoaderSrc    <- fromEnv "DYNAMIC_LOADER_SRC" $
                               snapRepos </> "snap-loader-dynamic"
            staticLoaderSrc <- fromEnv "STATIC_LOADER_SRC" $
                               snapRepos </> "snap-loader-static"
            let snapSrc   =  snapRoot

            forM_ [ "snap-core", "snap-server", "xmlhtml", "heist", "snap"
                  , "snap-loader-static", "snap-loader-dynamic"]
                  (pkgCleanUp sandbox)

            forM_ [ snapCoreSrc, snapServerSrc, xmlhtmlSrc, heistSrc
                  , snapSrc, staticLoaderSrc, dynLoaderSrc] $ \s ->
                systemOrDie $ concat [ "cabal-dev "
                                     , cabalDevArgs
                                     , " add-source "
                                     , s
                                     ]

            systemOrDie $ "cabal-dev install " ++ args
            let cmd = ("." </> "dist" </> "build" </> projName </> projName)
                      ++ " -p " ++ show httpPort
            putStrLn $ "Running \"" ++ cmd ++ "\""
            pHandle <- runCommand cmd
            waitABit
            return pHandle

        ----------------------------------------------------------------------
        findSnap = do
            home <- fromEnv "HOME" "."
            p1   <- gimmeIfExists $ snapRoot </> "dist" </> "build"
                                             </> "snap" </> "snap"
            p2   <- gimmeIfExists $ cwd </> ".cabal-sandbox" </> "bin" </> "snap"
            p3   <- gimmeIfExists $ snapRoot </> ".cabal-sandbox" </> "bin" </> "snap"
            p4   <- gimmeIfExists $ home </> ".cabal" </> "bin" </> "snap"
            p5   <- findExecutable "snap"

            return $ fromMaybe (error "couldn't find snap executable")
                               (getFirst $ mconcat $ map First [p1,p2,p3,p4,p5])

    --------------------------------------------------------------------------
    putStrLn $ "Changing directory to " ++ projectPath
    inDir True projectPath $ bracket initialize cleanup (const testAction)
    removeDirectoryRecursiveSafe projectPath

  where
    --------------------------------------------------------------------------
    fromEnv name def = do
        r <- getEnv name `catch` \(_::SomeException) -> return ""
        if r == "" then return def else return r

    --------------------------------------------------------------------------
    cleanup pHandle = do
        terminateProcess pHandle
        waitForProcess pHandle

    --------------------------------------------------------------------------
    waitABit = threadDelay $ 2*10^(6::Int)

    --------------------------------------------------------------------------
    pkgCleanUp d pkg = do
        paths <- globDir1 (compile $ "packages*conf/" ++ pkg ++ "-*") d
        forM_ paths $ \x ->
            rm x `catch` \(_::SomeException) -> return ()

      where
        rm x = do
            putStrLn $ "removing " ++ x
            removeFile x

    --------------------------------------------------------------------------
    gimmeIfExists p = do
        b <- doesFileExist p
        if b then return (Just p) else return Nothing


------------------------------------------------------------------------------
systemOrDie :: String -> IO ()
systemOrDie s = do
    putStrLn $ "Running \"" ++ s ++ "\""
    system s >>= check

  where
    check ExitSuccess = return ()
    check _           = throwIO $ ErrorCall $ "command failed: '" ++ s ++ "'"
