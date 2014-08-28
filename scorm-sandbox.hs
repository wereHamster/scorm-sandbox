{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

import           System.Exit                      ( exitFailure )
import           Control.Monad                    ( unless, forM_ )
import           Text.XML.HXT.Core                ( readDocument, runX, getAttrValue )
import           Text.XML.HXT.XPath               ( getXPathTrees )
import           Data.FileEmbed                   ( embedFile )
import           Data.Text.Lazy                   ( pack )

-- import           Text.XML.HXT.Arrow.XmlState.SystemConfig
--                                                  ( withValidate
--                                                  , no
--                                                  )
-- import           Text.XML.HXT.Arrow.WriteDocument ( writeDocument )
import           Control.Arrow                   ( (>>>) )

import qualified System.Environment              as Env
import qualified System.IO.Temp                  as Temp
import qualified System.Directory                as Dir
import qualified Codec.Archive.Zip               as Zip
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString                 as B
import qualified System.FilePath                 as Path
-- import qualified Text.XML.HXT.DOM.ShowXml        as Xml
import qualified Data.Text.Lazy.Encoding         as TE

import qualified Web.Scotty                      as Scotty
import qualified Network.Wai.Middleware.Static   as Static

printHelp :: IO ()
printHelp = do
    progName <- Env.getProgName
    putStrLn $ "Usage: " ++ progName ++ " <SCORM_archive.zip>"

main :: IO ()
main = do
    args <- Env.getArgs
    let numArgs = length args

    unless (numArgs == 1) $ do
        printHelp
        exitFailure

    let scormPgkName = head args
    pkgExists <- Dir.doesFileExist scormPgkName
    unless pkgExists $ do
        putStrLn $ "File: " ++ scormPgkName ++ " does not exist"
        exitFailure

    zipArch <- Zip.toArchive `fmap` (BL.readFile $ head args)

    Temp.withSystemTempDirectory "scorm-sandbox" $ \tmpDir -> do
        Zip.extractFilesFromArchive [Zip.OptDestination tmpDir] zipArch
        let imsManifestFilePath = Path.joinPath [tmpDir, "imsmanifest.xml"]
        manifestExists <- Dir.doesFileExist imsManifestFilePath
        unless manifestExists $ do
            putStrLn $ "The manifest file doesn't exist, not a valid SCORM package."
            -- TODO (UU): Does this do proper clean up with withSystemTempDirectory?
            exitFailure

        let imsManifestDoc = readDocument [] imsManifestFilePath

        resourceHrefs <- runX $
                imsManifestDoc
            >>> getXPathTrees "/manifest/resources/resource"
            >>> getAttrValue "href"

        let validHrefs = filter (/= "") resourceHrefs

        putStrLn "Hrefs: "
        forM_ (zip [(1::Int)..] validHrefs) $ \(idx, href) -> do
            putStrLn $ (show idx) ++ ") " ++ href

        -- Present the choice to user
        let chosenHref = validHrefs !! 0

        Scotty.scotty 8000 $ do
            Scotty.get "/" $ Scotty.redirect "/index.html"
            Scotty.get "/index.html"       $ Scotty.html $ TE.decodeUtf8 $ BL.fromStrict indexHtml
            Scotty.get "/scorm-sandbox.js" $ Scotty.text $ TE.decodeUtf8 $ BL.fromStrict scormSandboxJs
            Scotty.get "/content.html"     $ Scotty.redirect $ pack chosenHref

            Scotty.middleware $ Static.staticPolicy $ Static.addBase tmpDir


indexHtml :: B.ByteString
indexHtml = $(embedFile "./index.html")

scormSandboxJs :: B.ByteString
scormSandboxJs = $(embedFile "./scorm-sandbox.js")
