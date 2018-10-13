{-# OPTIONS_GHC -fplugin=TcYolo #-}

module TcYoloExample
  ( whereIsYourGodNow
  , noneShallPass
  , becauseWhyNot
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Reader (Reader)
import Control.Monad.Trans.State (StateT, get)
import Data.Functor.Identity (Identity)
import System.IO.Error (userError)
import System.Random (Random(randomIO))

whereIsYourGodNow :: Reader () Int
whereIsYourGodNow = liftIO randomIO

noneShallPass :: Identity ()
noneShallPass = liftIO . ioError . userError $ "blah"

becauseWhyNot :: StateT Int (Except String) Int
becauseWhyNot = do
  fileLength <- fmap length (liftIO . readFile $ "some-file.txt")
  extraLength <- get
  pure $ fileLength + extraLength

-- Note that the above code typechecks, but termination is a different story! 😛
