{-# LANGUAGE NoImplicitPrelude #-}

module GtkCtx
    ( GtkCtx
    , GtkState
    
    , newGtkCtx
    , showGtkCtx
    , showSubWindow
    , setTreeStore
    , mainWindow
    , cardLabel
    , treeView
    , treeStore
    , setTreeViewSelectFunc
    ) where

import Base

import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Vector as V
import System.FilePath

data WindowMode = EditCard | LearnDeck

data SubWindowCtx = SubWindowCtx { window :: Window
                                 , mode   :: WindowMode
                                 }

data TreeCtx a = TreeCtx { view        :: TreeView
                         , colNames    :: V.Vector Text
                         , cols        :: V.Vector TreeViewColumn
                         , store       :: TreeStore a
                         , renderFunc  :: a -> Text
                         }

data GtkCtx = GtkCtx { mainWindow    :: Window
                     , subWindowCtx  :: Maybe SubWindowCtx
                     , cardLabel     :: Label
                     , treeView      :: TreeView
                     , treeStore     :: TreeStore FilePath
                     }

type GtkState = ST.StateT GtkCtx IO

newGtkCtx :: IO GtkCtx
newGtkCtx = do
  w <- windowNew
  l <- labelNew $ Just "Hello World!"
  hbox <- hBoxNew False 20

  tsFolders <- treeStoreNew []
  tvDeck <- treeViewNewWithModel tsFolders
  tvDeckCol <- treeViewColumnNew
  rend <- cellRendererTextNew
  treeViewColumnPackStart tvDeckCol rend True
  cellLayoutSetAttributes tvDeckCol rend tsFolders (\row -> [cellText := takeBaseName row])
  treeViewInsertColumn tvDeck tvDeckCol (-1)
  
  set w [ windowDefaultWidth  := 640
        , windowDefaultHeight := 480
        , containerChild      := hbox
        ]

  boxPackStart hbox tvDeck PackGrow 0
  boxPackStart hbox l PackGrow 0
  onDestroy w mainQuit
  
  return GtkCtx { mainWindow = w
                , cardLabel = l
                , treeView = tvDeck
                , treeStore = tsFolders
                , subWindowCtx = Nothing
                }


showGtkCtx :: GtkState ()
showGtkCtx = do
  ctx <- ST.get
  liftIO $ widgetShowAll $ mainWindow ctx
  ST.put ctx


makeTreeCtx :: [Text] -> (a -> Text) -> IO (TreeCtx a)
makeTreeCtx cols renderer = do
  let names = fromList cols
  tmodel <- treeStoreNew []
  tv <- treeViewNewWithModel tmodel
  tvCols <- V.forM names (\_ -> treeViewColumnNew)
  rendCols <- V.forM names (\_ -> cellRendererTextNew)

  let tvAndRend = zip tvCols rendCols
  V.forM_ tvAndRend
    (\(c, r) -> treeViewColumnPackStart c r True)
  V.forM_ tvAndRend
    (\(c, r) -> cellLayoutSetAttributes c r tmodel
      (\row -> [cellText := renderer row]))
  V.forM tvCols (\c -> treeViewInsertColumn tv c (-1))

  return $ TreeCtx { view       = tv
                   , cols       = tvCols
                   , colNames   = names
                   , store      = tmodel
                   , renderFunc = renderer
                   }
  

showSubWindow :: GtkState ()
showSubWindow = do
  ctx <- ST.get
  case window <$> subWindowCtx ctx of
    Nothing -> return ()
    Just w  -> liftIO $ widgetShowAll w
  ST.put ctx


setTreeStore :: TreeStore FilePath -> GtkState ()
setTreeStore ts = do
  ctx <- ST.get
  ST.put $ GtkCtx { mainWindow    = (mainWindow ctx)
                  , cardLabel     = (cardLabel ctx)
                  , treeView      = (treeView ctx)
                  , treeStore     = ts
                  , subWindowCtx = (subWindowCtx ctx)
                  }


setTreeViewSelectFunc :: TreeSelectionCB -> GtkState ()
setTreeViewSelectFunc cb = do
  ctx <- ST.get
  let tv = treeView ctx
  selection <- liftIO $ treeViewGetSelection tv
  liftIO $ treeSelectionSetSelectFunction selection cb
  ST.put ctx
