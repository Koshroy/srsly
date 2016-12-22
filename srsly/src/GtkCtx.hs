{-# LANGUAGE NoImplicitPrelude #-}

module GtkCtx
    ( GtkCtx

    , getGtkCtx
    , showGtkCtx
    ) where

import Base
import Common

data GtkCtx = GtkCtx {
  mainWindow :: Window
, cardLabel  :: Label
, treeView   :: TreeView
  } deriving (Eq)

type GtkCtxState = State GtkCtx
type GtkCtxStateIO = StateT GtkCtx IO

getGtkCtx :: IO GtkCtx
getGtkCtx = do
  w <- windowNew
  l <- labelNew $ Just (asText $ pack "Hello World!")
  hbox <- hBoxNew False 20


  let forest = fmap (\i -> Node { rootLabel = tshow i,
                                  subForest =
                                    (fmap (\i -> Node {
                                              rootLabel = tshow i,
                                              subForest = []
                                              }
                                          ) [0..5]
                                    )
                                }
                    ) [0..5]

  ts <- treeStoreNew forest
  tv <- treeViewNewWithModel ts
  tvCol <- treeViewColumnNew
  rend <- cellRendererTextNew
  treeViewColumnPackStart tvCol rend True
  cellLayoutSetAttributes tvCol rend ts (\row -> [cellText := row])
  newColNums <- treeViewInsertColumn tv tvCol (-1)
  
  set w [
      windowDefaultWidth := 640
    , windowDefaultHeight := 480
    , containerChild := hbox
    ]

  boxPackStart hbox tv PackGrow 0
  boxPackStart hbox l PackGrow 0
  onDestroy w mainQuit
  return GtkCtx { mainWindow = w, cardLabel = l, treeView = tv}

changeCardTree :: CardTree -> GtkCtxStateIO ()
changeCardTree cards = do
  ctx <- get
  treeView

showGtkCtx :: GtkCtxStateIO ()
showGtkCtx = widgetShowAll $ mainWindow ctx
