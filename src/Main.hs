module Main where

import Control.Exception (bracket)
import Control.Monad.Trans.Except
import System.Modbus.TCP
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

main :: IO ()
main = withSocketsDo $ do
  -- Default address for pymodbus's asynchronous server example.
  addr <- resolve "127.0.0.1" "5020"
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
      result <- runExceptT $ runSession conn $ readHoldingRegisters (TransactionId 1) (ProtocolId 0) (UnitId 0) 1 1
      case result of
        Left err -> putStrLn $ show err
        Right registers -> do
          putStrLn "Registers: "
          mapM_ (putStrLn . show) registers
