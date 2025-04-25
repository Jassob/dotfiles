{-# LANGUAGE LambdaCase #-}

module Main where

import Graphics.X11 (Display, RROutput, xRR_Connected)
import Graphics.X11.Xlib.Display
  ( defaultRootWindow,
    openDisplay,
  )
import Graphics.X11.Xrandr

data Output = MkOutput
  { connected :: Bool,
    name :: String,
    resolution :: Maybe (Int, Int),
    position :: Maybe (Int, Int)
  }
  deriving (Show, Eq)

getOutputs :: IO [Output]
getOutputs = do
  display <- openDisplay ""
  let root = defaultRootWindow display
  Just screenResources <- xrrGetScreenResourcesCurrent display root
  mapM (getOutput display screenResources) $ xrr_sr_outputs screenResources
  where
    getOutput :: Display -> XRRScreenResources -> RROutput -> IO Output
    getOutput dis res output = do
      Just outputInfo <- xrrGetOutputInfo dis res output
      let name = xrr_oi_name outputInfo
      let connected = xrr_oi_connection outputInfo == xRR_Connected
      if xrr_oi_crtc outputInfo == 0
        then pure $ MkOutput connected name Nothing Nothing
        else do
          Just crtcInfo <- xrrGetCrtcInfo dis res $ xrr_oi_crtc outputInfo
          pure $
            MkOutput
              connected
              name
              (pure $ getResolutionFromCrtcInfo crtcInfo)
              (pure $ getPositionFromCrtcInfo crtcInfo)

    getPositionFromCrtcInfo :: XRRCrtcInfo -> (Int, Int)
    getPositionFromCrtcInfo (XRRCrtcInfo {xrr_ci_x = x, xrr_ci_y = y}) =
      (fromIntegral x, fromIntegral y)

    getResolutionFromCrtcInfo :: XRRCrtcInfo -> (Int, Int)
    getResolutionFromCrtcInfo (XRRCrtcInfo {xrr_ci_width = width, xrr_ci_height = height}) =
      (fromIntegral width, fromIntegral height)

main :: IO ()
main = do
  outputs <- getOutputs
  print outputs

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error "fromJust: Nothing"
