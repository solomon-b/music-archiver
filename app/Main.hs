{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Data.Char qualified as Char
import Data.List.NonEmpty as NE
import Data.Text qualified as Text
import Data.Text.ICU.Normalize2 qualified as N
import System.Directory (doesFileExist)
import System.FilePath (dropTrailingPathSeparator, joinPath, makeRelative, replaceExtension, takeExtension)
import Turtle
import Prelude hiding (FilePath)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  echo "Running Lidarr post-import script"

  let src_root = "/mnt/media/Music"
      dest_root = "/mnt/media/Ipod"

  -- 1. Verify Event Type
  event_type <- need "lidarr_eventtype"
  if event_type /= Just "AlbumDownload"
    then
      print $
        "Skipping event '" <> foldMap show event_type <> "'; only 'AlbumDownload' is handled."
    else do
      -- 2. Fetch Env Vars for artist name, album name, and track paths
      artist <- getArtistName
      album <- getAlbumName
      track_paths <- getTrackPaths

      -- 3. Create Destination path from destination root, artist, and album
      let dest_path = dest_root </> artist </> album

      print $ "Artist: " <> artist
      print $ "Album: " <> album
      print $ "Destination Root: " <> dest_path

      forM_ track_paths $ \track_path -> do
        doesFileExist track_path >>= \case
          False -> error $ "FilePath '" <> track_path <> "' does not exist"
          True -> do
            -- 4. Split TrackPath at album name
            let splitPath = splitTrackPath src_root track_path
            -- 5. Replace track path up to album name with destination path
            let target_track_path = replaceExtension "mp3" $ dest_path </> splitPath.relativePath
                filetype = takeExtension track_path

            print $ "Track Destination: " <> target_track_path

            -- 6. Perform Re-encode and copy.
            case toLower' filetype of
              "mp3" -> do
                print $ "Copying MP3: " <> track_path <> " -> " <> target_track_path
                cp track_path target_track_path
              _ -> do
                print $ "Converting to MP3: " <> track_path <> " -> " <> target_track_path
                let args =
                      [ "-i",
                        Text.pack track_path,
                        "-y",
                        "-codec:a",
                        "libmp3lame",
                        "-b:a",
                        "320k",
                        "-map_metadata",
                        "0",
                        Text.pack target_track_path
                      ]
                procs "ffmpeg" args mempty

getArtistName :: IO FilePath
getArtistName =
  onNothingM (error "Missing 'lidarr_artist_name'") . fmap (Text.unpack . sanitizeWord) =<< need "lidarr_artist_name"

getAlbumName :: IO FilePath
getAlbumName =
  onNothingM (error "Missing 'lidarr_album_title'") . fmap (Text.unpack . sanitizeWord) =<< need "lidarr_album_title"

getTrackPaths :: IO (NonEmpty FilePath)
getTrackPaths = do
  added_track_paths <- onNothingM (error "Missing 'lidarr_addedtrackpaths'") =<< need "lidarr_addedtrackpaths"
  let track_paths = Text.unpack . Text.strip <$> Text.splitOn "|" added_track_paths
  if null track_paths
    then error "No track paths provided"
    else pure $ NE.fromList track_paths

data SplitTrackPath = SplitTrackPath {albumRoot :: FilePath, relativePath :: FilePath}
  deriving stock (Show)

-- | Split a File's absolute source path at the album.
--
-- > splitTrackPath "/mnt/media/Music" "/mnt/media/Music/Frank Zappa/Zappa in New York (1977)/CD 01/Frank Zappa - Zappa in New York - 01 - Titties & Beer.flac"
-- SplitTrackPath {albumRoot = "/mnt/media/Music/Frank Zappa/Zappa in New York (1977)/", relativePath = "CD 01/Frank Zappa - Zappa in New York - 01 - Titties & Beer.flac"}
splitTrackPath :: FilePath -> FilePath -> SplitTrackPath
splitTrackPath rootFolder path =
  case splitDirectories $ makeRelative rootFolder path of
    artist : album : remainder -> SplitTrackPath (rootFolder </> artist </> album) (joinPath remainder)
    _ -> error "invalid TrackPath"

--------------------------------------------------------------------------------

-- | Sanitize an entire path component-by-component
sanitizePath :: FilePath -> FilePath
sanitizePath = joinPath . fmap (Text.unpack . sanitizeWord . Text.pack . dropTrailingPathSeparator) . splitDirectories

-- | Sanitize a single path segment (folder or filename)
sanitizeWord :: Text -> Text
sanitizeWord "/" = "/"
sanitizeWord xs =
  Text.filter isSafeChar
    . Text.map replaceChar
    . Text.filter Char.isAscii
    $ N.normalize N.NFKD xs
  where
    -- Replace slashes and control characters
    replaceChar c
      | c `elem` ['/', '\\', '?', '*', ':', '|', '"', '<', '>'] = '_'
      | otherwise = c

    -- Allow alphanumerics and some readable symbols
    isSafeChar c = Char.isAlphaNum c || c `elem` (" .-_()" :: String)

normalize :: String -> String
normalize = toLower' . Prelude.filter Char.isAlphaNum

toLower' :: String -> String
toLower' = Prelude.map Char.toLower

onNothingM :: (Applicative m) => m a -> Maybe a -> m a
onNothingM ma Nothing = ma
onNothingM _ (Just a) = pure a
