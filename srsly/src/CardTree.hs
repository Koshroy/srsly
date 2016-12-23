{-# LANGUAGE NoImplicitPrelude #-}

module CardTree
  ( changeCardTree
  , getDirectoryCardTree
  ) where

import Base
import Common

import qualified Control.Monad.Trans.State.Lazy as ST
import System.Directory
import System.FilePath

listDirectoryContents :: FilePath -> IO [FilePath]
listDirectoryContents dir = do
  fmap (
    \dirPaths -> filter
                 (\dirPath ->
                    (dirPath /= ".") && (dirPath /= "..")
                 )
                 dirPaths
    ) (getDirectoryContents dir)


changeCardTree :: CardTree -> GtkState ()
changeCardTree cards = do
  ctx <- ST.get
  let ts = treeStore ctx
  liftIO $ treeStoreClear ts
  liftIO $ treeStoreInsertForest ts [] 0 cards
  changeTreeStore ts


isCardJson :: FilePath -> IO Bool
isCardJson path = do
  isFile <- doesFileExist path
  return $ isFile && (".json" == takeExtension ".json")
  

getDirectoryCardTree :: FilePath -> IO CardTree
getDirectoryCardTree path = do
  let truePath = addTrailingPathSeparator . normalise $ path
  childRelPaths <- listDirectoryContents truePath
  let childPaths = fmap (\d -> path </> d) childRelPaths
  dirs <- filterM doesDirectoryExist childPaths
  cardFiles <- filterM isCardJson childPaths
  dirCards <- mapM getDirectoryCardTree dirs
  let dirAndCards = zip dirs dirCards
  let childCards = fmap (\(dir, cards) ->
                           Node { rootLabel = pack $ takeBaseName dir
                                , subForest = cards
                                }
                        ) dirAndCards
  let currCards = fmap (\card ->
                          Node { rootLabel = pack $ takeBaseName card
                               , subForest = []
                               }
                       ) cardFiles
  return $ childCards ++ currCards
