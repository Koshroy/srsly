{-# LANGUAGE NoImplicitPrelude #-}

module GtkCtx
    ( GtkCtx
    , GtkState
    
    , newGtkCtx
    , showGtkCtx
    , setTreeStore
    , mainWindow
    , cardLabel
    , treeView
    , treeStore
    , setTreeViewSelectFunc
    ) where

import Base

import qualified Control.Monad.Trans.State.Lazy as ST
import System.FilePath


data GtkCtx = GtkCtx {
    mainWindow :: Window
  , cardLabel  :: Label
  , treeView   :: TreeView
  , treeStore  :: TreeStore FilePath
  }

type GtkState = ST.StateT GtkCtx IO

newGtkCtx :: IO GtkCtx
newGtkCtx = do
  w <- windowNew
  l <- labelNew $ Just (asText $ pack "Hello World!")
  hbox <- hBoxNew False 20

  ts <- treeStoreNew []
  tv <- treeViewNewWithModel ts
  tvCol <- treeViewColumnNew
  rend <- cellRendererTextNew
  treeViewColumnPackStart tvCol rend True
  cellLayoutSetAttributes tvCol rend ts (\row -> [cellText := takeBaseName row])
  newColNums <- treeViewInsertColumn tv tvCol (-1)
  
  set w [ windowDefaultWidth  := 640
        , windowDefaultHeight := 480
        , containerChild      := hbox
        ]

  boxPackStart hbox tv PackGrow 0
  boxPackStart hbox l PackGrow 0
  onDestroy w mainQuit
  return GtkCtx { mainWindow = w
                , cardLabel = l
                , treeView = tv
                , treeStore = ts
                }


showGtkCtx :: GtkState ()
showGtkCtx = do
  ctx <- ST.get
  liftIO $ widgetShowAll $ mainWindow ctx
  ST.put ctx


setTreeStore :: TreeStore FilePath -> GtkState ()
setTreeStore ts = do
  ctx <- ST.get
  ST.put $ GtkCtx { mainWindow = (mainWindow ctx)
                  , cardLabel  = (cardLabel ctx)
                  , treeView   = (treeView ctx)
                  , treeStore  = ts
                  }


setTreeViewSelectFunc :: TreeSelectionCB -> GtkState ()
setTreeViewSelectFunc cb = do
  ctx <- ST.get
  let tv = treeView ctx
  selection <- liftIO $ treeViewGetSelection tv
  liftIO $ treeSelectionSetSelectFunction selection cb
  ST.put ctx
