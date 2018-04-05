module Main where

import Control.Exception (bracket)
import Control.Monad.Trans.Except
import Data.Semigroup ((<>))
import GHC.Word (Word16)
import Options.Applicative
import System.Modbus.TCP
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)


data Options = Options { optHost :: String
                       , optPort :: Int
                       , optCount :: Word16
                       , optStart :: Word16
                       } deriving (Show)

options :: Parser Options
options = Options
  <$> strOption ( long "host" <>
                  short 'h' <>
                  metavar "HOST" <>
                  showDefault <>
                  value "127.0.0.1" <>
                  help "Hostname or IP address with which to connect." )
  <*> option auto ( long "port" <>
                    short 'p' <>
                    metavar "PORT" <>
                    showDefault <>
                    value 502 <>
                    help "Port number to use." )
  <*> option auto ( long "count" <>
                    short 'c' <>
                    metavar "COUNT" <>
                    showDefault <>
                    value 1 <>
                    help "Number of registers to read." )
  <*> argument auto ( metavar "REGISTER" <>
                      help "Address of first register to read." )

main :: IO ()
main = runRequest =<< execParser opts
  where opts = info (options <**> helper) (
          fullDesc <>
          progDesc "Read some holding registers from the given device." )

runRequest :: Options -> IO ()
runRequest (Options host port count register) = withSocketsDo $ do
  addr <- resolve host (show port)
  bracket (open addr) close go

  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr

    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

    go sock = do
      let conn = Connection { connWrite = send sock
                            , connRead = recv sock
                            , connCommandTimeout = 1000
                            , connRetryWhen = \_ _ -> False
                            }
      result <- runExceptT $ runSession conn $ readHoldingRegisters (TransactionId 1) (ProtocolId 0) (UnitId 0) (RegAddress register) count
      case result of
        Left err -> putStrLn $ show err
        Right registers -> do
          putStrLn "Registers: "
          mapM_ (putStrLn . show) registers
