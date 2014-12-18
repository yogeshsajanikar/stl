{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.STL.View where

 
import           Control.Lens.TH
import           Data.Int
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileUploads
import           Data.STL.Topology
import           Data.STL.Parser
import           Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString.Char8 as B

data STLApp = STLApp { _heist :: Snaplet (Heist STLApp)
                     }

instance HasHeist STLApp

makeLenses ''STLApp

maxMb :: Int64
maxMb = 100

megaByte :: Int64
megaByte = 2^(20::Int)

stlDefaultPolicy :: UploadPolicy
stlDefaultPolicy = setMaximumFormInputSize (maxMb * megaByte) defaultUploadPolicy

stlPartPolicy :: PartInfo -> PartUploadPolicy
stlPartPolicy _ = allowWithMaximumSize (maxMb * megaByte)

stlUploadDump :: MonadSnap m => m ()
stlUploadDump = do
  fields <- handleMultipart stlDefaultPolicy $ \part -> do
                  streamSTL (0.001::Float) EL.consume
                  return $ partFieldName part
  let url = B.concat fields
  redirect "/load"

stlAppInit :: SnapletInit STLApp STLApp
stlAppInit = makeSnaplet "stlview" "STL Viewing Snaplet" Nothing $ do
               hs <- nestSnaplet "heist" heist $ heistInit "templates"
               addRoutes [ ("/", writeText "Load/View STL files")
                         , ("/load", stlUploadDump) 
                         ]
               wrapSite (<|> heistServe)
               return $ STLApp hs


