module Main where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

configFile :: FilePath
configFile = "pandoc.cfg"

sourcePaths :: [FilePath]
sourcePaths = ["md", "markdown", "src", "sources"]

sourceExts :: [String]
sourceExts = ["md", "markdown", "txt", "text", "rst", "org", "lhs"]

outlineFiles :: [String]
outlineFiles = ["outline.txt"]

outputPath :: FilePath
outputPath = "_output"

pandoc :: [FilePath] -> [String] -> FilePath -> Action ()
pandoc src opt out = cmd "pandoc" src opt "-o" [out]

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = outputPath } $ do
    usingConfigFile configFile

    phony "clean" $
        removeFilesAfter outputPath ["//*"]

    outputPath </> "*" %> \output -> do
        let doc = dropExtension (takeFileName output)

        sources <- do
            optSources <- getOptions sourcePaths doc "sources"
            optFormats <- getOptions sourceExts doc "formats"
            optOutline <- getOptionsOverride outlineFiles doc "outline"
            docSources <- getSourceFiles optSources optFormats optOutline
            need docSources
            return docSources

        options <- do
            optCsl <- do
                xs <- getOptionsOverride [] doc "csl"
                need =<< filterM doesFileExist xs
                return ["--csl=" ++ f | f <- xs]

            optStyles <- do
                xs <- getOptions [] doc "styles"
                need =<< filterM doesFileExist xs
                return ["--include-in-header=" ++ f | f <- xs]

            optOptions <- getOptions [] doc "options"

            optFilters <- do
                xs <- getOptions [] doc "filters"
                need =<< filterM doesFileExist xs
                return ["--filter=" ++ f | f <- xs]

            optTemplate <- do
                xs <- getOptionsOverride [] doc "template"
                need =<< filterM doesFileExist xs
                return ["--template=" ++ f | f <- xs]

            return $ concat
                [optCsl, optStyles, optOptions, optFilters, optTemplate]

        pandoc sources options output

findSourceFile :: [FilePath] -> [String] -> FilePath -> Action FilePath
findSourceFile dirs exts f = head <$> filterM doesFileExist
    [dir </> f -<.> ext | dir <- dirs, ext <- exts]

readOutlineFile :: [FilePath] -> [String] -> FilePath -> Action [FilePath]
readOutlineFile dirs exts f =
    mapM (findSourceFile dirs exts) =<< readFileLines f

getSourceFiles :: [FilePath] -> [String] -> [FilePath] -> Action [FilePath]
getSourceFiles sourcePaths' sourceExts' outlineFiles' =
    concat <$> traverse (readOutlineFile sourcePaths' sourceExts') outlineFiles'

getConfigOpts :: String -> Action (Maybe [String])
getConfigOpts = fmap (fmap words) . getConfig

getOptions :: [String] -> String -> String -> Action [String]
getOptions def doc key = do
    baseOpts <- getConfigOpts key
    docOpts <- getConfigOpts (doc <.> key)
    return (fromMaybe def (mappend baseOpts docOpts))

getOptionsOverride :: [String] -> String -> String -> Action [String]
getOptionsOverride def doc key = do
    baseOpts <- Last <$> getConfigOpts key
    docOpts <- Last <$> getConfigOpts (doc <.> key)
    return (fromMaybe def (getLast (mappend baseOpts docOpts)))
