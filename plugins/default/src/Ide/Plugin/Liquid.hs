{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Ide.Plugin.Liquid where

import Control.Lens ( (^.) )
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Data.Bifunctor
import Data.IntervalMap.FingerTree as IM
import Ide.Types
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Development.Shake.FilePath
import GHC.Generics
import qualified Data.Text as T
import Data.Text.Encoding as T
import Development.Shake
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Development.IDE
import Data.Maybe
import System.IO
import Development.Shake.Classes

descriptor :: PluginId -> PluginDescriptor
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginId = plId
    , pluginHoverProvider = Just liquidHoverProvider
    }

data LHAnnotations = LHAnnotations
  deriving (Eq, Show, Generic, Hashable, Binary, NFData)

type instance RuleResult LHAnnotations = RangeMap

liquidRules :: Rules ()
liquidRules =
  do
    define $ \ LHAnnotations nfp -> do
      minfo :: [LiquidError] <- fromMaybe [] <$> readVimAnnot' nfp
      let
        rangeMap :: RangeMap
        rangeMap = toRangeMap minfo
      return ([], Just rangeMap)


-- type HoverProvider = IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))

liquidHoverProvider :: HoverProvider
liquidHoverProvider ideState textDocumentPositionParams =
  runAction "liquid" ideState $
  do
    let
      textDocumentUri :: Uri
      textDocumentUri = textDocumentPositionParams ^. textDocument . uri

      textDocumentPos :: Position
      textDocumentPos = textDocumentPositionParams ^. position

    liftIO $ hPrint stderr ("looking up: ", textDocumentPos)

    minfo :: [LiquidError] <- fromMaybe [] <$> readVimAnnot textDocumentUri
    let
      rangeMap :: RangeMap
      rangeMap = toRangeMap minfo

      matching :: [(Range, T.Text)]
      matching = inRangeMap textDocumentPos rangeMap

      mhover :: Maybe Hover
      mhover =
        case matching of
          [] -> Nothing
          _  ->
            let
              (range, msg) = last matching
            in
              Just (Hover (HoverContents (unmarkedUpContent msg)) (Just range))

    return (Right mhover)

type RangeMap = IntervalMap Position T.Text
deriving instance (NFData p, NFData t) => NFData (IntervalMap p t)

-- hls positions seem to be 0-based
liquidPosToPosition :: LiquidPos -> Position
liquidPosToPosition (LP l c) = Position (l - 1) (c - 1)

toRangeMap :: [LiquidError] -> RangeMap
toRangeMap =
  foldr (\ (LE pstart pstop msg) -> IM.insert (IM.Interval (liquidPosToPosition pstart) (liquidPosToPosition pstop)) msg) IM.empty

inRangeMap :: Position -> RangeMap -> [(Range, T.Text)]
inRangeMap p m =
  first (\ (Interval pstart pstop) -> Range pstart pstop) <$> IM.search p m

-- | For a Uri representing
-- "path/to/file/Foo.hs" return
-- "path/to/file/.liquid/Foo.hs.json"
jsonAnnotFile :: Uri -> FilePath
jsonAnnotFile uri = liquidFileFor uri "json"

-- | For a Uri representing
-- "path/to/file/Foo.hs" return
-- "path/to/file/.liquid/Foo.hs.vim.annot"
vimAnnotFile :: Uri -> FilePath
vimAnnotFile uri = liquidFileFor uri "vim.annot"

vimAnnotFile' :: NormalizedFilePath -> NormalizedFilePath
vimAnnotFile' nfp = liquidFileFor' nfp "vim.annot"

liquidFileFor' :: NormalizedFilePath -> String -> NormalizedFilePath
liquidFileFor' nfp ext = toNormalizedFilePath r
  where
    fp = fromNormalizedFilePath nfp
    d  = takeDirectory fp
    f  = takeFileName fp
    r  = d </> ".liquid" </> f <.> ext

-- | For a Uri representing
-- "path/to/file/Foo.hs" return
-- "path/to/file/.liquid/Foo.hs.EXT"
liquidFileFor :: Uri -> String -> FilePath
liquidFileFor uri ext =
  case uriToFilePath uri of
    Nothing -> error $ " Liquid.vimAnnotFile:bad uri:" ++ show uri
    Just fp -> r
      where
        d = takeDirectory fp
        f = takeFileName fp
        r = d </> ".liquid" </> f <.> ext

data LiquidPos
  = LP
    { line :: Int
    , column :: Int
    } deriving (Show,Generic,Eq,Ord,FromJSON,ToJSON)

data LiquidError =
  LE
    { lestart :: LiquidPos
    , lestop  :: LiquidPos
    , lemessage :: T.Text
    } deriving (Show,Generic,Eq,FromJSON,ToJSON)

data LiquidJson
  = LJ
    { status :: T.Text
    , types  :: Value
    , errors :: [LiquidError]
    } deriving (Show,Generic,FromJSON,ToJSON) -- TODO: not a good idea to rely on Generic here

readVimAnnot' :: NormalizedFilePath -> Action (Maybe [LiquidError])
readVimAnnot' nfp = do
  let fileName = fromNormalizedFilePath (vimAnnotFile' nfp)
  liftIO $ hPutStrLn stderr fileName
  exists <- doesFileExist fileName
  if exists
    then do
      liftIO $ hPutStrLn stderr "exists and reading"
      vf <- liftIO (BS.readFile fileName)
      let r = parseType vf
      liftIO (hPrint stderr r)
      return $ r
    else return Nothing

readVimAnnot :: Uri -> Action (Maybe [LiquidError])
readVimAnnot uri = do
  let fileName = vimAnnotFile uri
  liftIO $ hPutStrLn stderr fileName
  exists <- doesFileExist fileName
  if exists
    then do
      liftIO $ hPutStrLn stderr "exists and reading"
      vf <- liftIO (BS.readFile fileName)
      let r = parseType vf
      liftIO (hPrint stderr r)
      return $ r
    else return Nothing

parseType :: BS.ByteString -> Maybe [LiquidError]
parseType bs =
  case parseOnly parseTypes bs of
    Left _  -> Nothing
    Right r -> Just r

parseTypes :: Parser [LiquidError]
parseTypes =
  parseTypeFromVim `sepBy` PC.endOfLine

readJsonAnnot :: Uri -> Action (Maybe [LiquidError])
readJsonAnnot uri = do
  let fileName = jsonAnnotFile uri
  liftIO $ hPutStrLn stderr fileName
  exists <- doesFileExist fileName
  if exists
    then do
      liftIO $ hPutStrLn stderr "exists and reading"
      jf <- liftIO (LBS.readFile fileName)
      case decode jf :: Maybe LiquidJson of
        Nothing -> return Nothing
        Just j -> do
          liftIO $ hPutStrLn stderr "decoding successful"
          return (Just (errors j))
    else return Nothing

parseTypeFromVim :: Parser LiquidError
parseTypeFromVim =
  (\ l1 c1 l2 c2 msg -> LE (LP l1 c1) (LP l2 c2) (T.decodeUtf8 msg)) <$> PC.decimal <* PC.char ':' <*> PC.decimal <* PC.char '-' <*> PC.decimal <* PC.char ':' <*> PC.decimal <* string "::" <*> PC.takeTill (== '\n')
