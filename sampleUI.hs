{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util

import Parse

data Name = Edit1 deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        ui = (str "Input: " <+> (hLimit 30 $ vLimit 5 e1))


appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent e = do
    zoom edit1 $ E.handleEditorEvent e

initialState :: St
initialState =
    St (F.focusRing [Edit1])
       (E.editor Edit1 Nothing "")

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const $ A.attrMap V.defAttr []
          }

main :: IO ()
main = do
    st <- M.defaultMain theApp initialState
    -- putStrLn "Input you entered:\n"
    -- putStrLn $ unlines $ E.getEditContents $ st^.edit1
    putStrLn "(parsed) input you entered:\n"
    print $ parseFromString floatWithErrorP (unlines $ E.getEditContents $ st^.edit1)

