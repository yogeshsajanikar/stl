{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.STL.View where

 
import           Control.Lens.TH
import           Data.Int
import           Snap
import           Snap.Types
import           Snap.Snaplet.Heist
import           Snap.Util.FileUploads
import           Data.STL.Topology
import           Data.STL.Parser
import           Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import           Data.Text as T

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

--stlUploadDump :: MonadSnap m => m ()
stlUploadDump :: MonadSnap m => m ()
stlUploadDump = do
  handleMultipart stlDefaultPolicy $ \part -> E.printChunks False
                  -- streamSTL (0.001::Float) (E.printChunks False)
                  
                  --return $ partFieldName part
  return ()
  -- writeText "Hello, Loaded"
  --redirect "/load"
      where showText x = writeText "Hello"

stlAppInit :: SnapletInit STLApp STLApp
stlAppInit = makeSnaplet "stlview" "STL Viewing Snaplet" Nothing $ do
               hs <- nestSnaplet "heist" heist $ heistInit "templates"
               addRoutes [ ("/", writeText "Load/View STL files")
                         , ("/load", stlUploadDump) 
                         ]
               wrapSite (<|> heistServe)
               return $ STLApp hs


