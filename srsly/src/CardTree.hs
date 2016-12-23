{-# LANGUAGE NoImplicitPrelude #-}

module CardTree
  ( changeCardTree
  , getDirectoryCardTree
  , printTreeSelectionCB
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
  setTreeStore ts


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

-- Helper function to create a callback that operates on selections
-- Given a callback of the form (Text -> IO ()) we assign this
-- callback to the callback condition when a new element in our
-- TreeView is selected.
--
-- The implementation of this function is tricky because the
-- selection callback fires on both select and deselect conditions.
-- The way this function guarantees that it is only fired once on
-- select is that:
--
-- 1. Make sure that the element we want (using TreePath) is
--    currently not selected. If it is selected, then a deselect
--    is firing on the currently selected element
--    (the element is already selected and a selection callback
--    has fired). So make sure it isn't currently selected.
--
-- 2. Make sure that no rows are currently selected. Because
--    this callback fires before selection, if the callback
--    fires on a selection event, then because only a single
--    selection is allowed at one time, no element is currently
--    selected and the given element is *about* to be selected.
makeSelectionCB :: (Text -> IO ()) -> GtkState TreeSelectionCB
makeSelectionCB cb = do
  ctx <- ST.get
  selection <- liftIO $ treeViewGetSelection (treeView ctx)
  return $ (
    \tpath -> do
      currTreeM <- treeStoreLookup (treeStore ctx) tpath
      case currTreeM of
        Nothing -> putStrLn $ pack "Error with row selection"
        Just currTree -> do
          isSelected <- treeSelectionPathIsSelected selection tpath
          rows <- treeSelectionCountSelectedRows selection    
          case isSelected of
            False  -> case rows of
                        0 -> cb $ rootLabel currTree
                        _ -> return ()
            True   -> return ()
      return True
      )

printTreeSelectionCB :: GtkState TreeSelectionCB
printTreeSelectionCB = makeSelectionCB putStrLn
