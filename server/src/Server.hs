module Server where

import           Control.Concurrent             (newMVar)
import           Data.Set                       as Set
import           HttpServer                     (httpApp)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WSHandler
import qualified Network.Wai.Logger             as WaiLogger
import qualified Network.WebSockets             as WS
import           Types
import           WsServer                       (wsApp)


runServer :: IO ()
runServer = do
  initialState <- newMVar initialServerState

  WaiLogger.withStdoutLogger $ \logger -> do
    let settings = Warp.setLogger logger . Warp.setPort (port defaultConfig) $ Warp.defaultSettings

    Warp.runSettings settings $
      WSHandler.websocketsOr WS.defaultConnectionOptions (wsApp initialState) httpApp

initialServerState :: State
initialServerState =
  State { clients = Set.empty }

defaultConfig :: Config
defaultConfig =
  Config { port = 8000 }
