{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Server (server) where

import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Web.Scotty as Scot
import qualified Control.Monad.IO.Class as Cm
import qualified Data.Digest.Pure.SHA as Dps
import qualified Model as M
import Data.Word
import Codec.Picture.Repa
import Data.Array.Repa hiding ((++))


import Data.Monoid (mconcat)

htmlSourceDir :: String
htmlSourceDir = "/haskell-palette/"

samplePalette :: [String]
samplePalette = [
                "#8c2703",
                "#973c1c",
                "#a35235",
                "#ae674e",
                "#ba7d67"
                ]

generatePalette :: M.ImageRequest -> IO M.ColorsResponse
generatePalette request = do 
  -- just sample action done on request algorithm field,
  -- let's work on image field that is a string represenation of image
  (print . M.algorithm) request 
  return $ M.ColorsResponse { M.colors = samplePalette }

server :: IO ()
server = Scot.scotty 3000 $ do
  Scot.middleware logStdoutDev

  Scot.get "/" $ do
    Scot.file $ htmlSourceDir ++ "index.html"

  Scot.post "/upload" $ do
    request <- Scot.jsonData :: Scot.ActionM M.ImageRequest
    palette <- (Cm.liftIO . generatePalette) request
    Scot.json palette

  Scot.get "/test" $ do
    image <- Cm.liftIO . readImageRGB $ "/example.png"
    case image of 
      Left err -> Scot.text . TL.pack $ err
      Right i -> do
        rotated <- Cm.liftIO . computeUnboxedP $ rotate 4 (imgData i)

        Scot.text "rotated the image"

rotate :: Double -> Array D DIM3 Word8 -> Array D DIM3 Word8
rotate deg g = fromFunction (Z :. y :. x :. k) f      -- 1
    where
        sh@(Z :. y :. x :. k)   = extent g

        !theta = pi/180 * deg                         -- 2

        !st = sin theta                               -- 3
        !ct = cos theta

        !cy = fromIntegral y / 2 :: Double            -- 4
        !cx = fromIntegral x / 2 :: Double

        f (Z :. i :. j :. k)                          -- 5
          | inShape sh old = g ! old                  -- 6
          | otherwise      = 0                        -- 7
          where
            fi = fromIntegral i - cy                  -- 8
            fj = fromIntegral j - cx

            i' = round (st * fj + ct * fi + cy)       -- 9
            j' = round (ct * fj - st * fi + cx)

            old = Z :. i' :. j' :. k                  -- 10