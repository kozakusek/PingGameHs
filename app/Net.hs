{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Net where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Net.IPv4 as IP (decode, toOctets)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)

refuseConnections :: Socket -> IO ()
refuseConnections sock = forever $
  E.bracketOnError (accept sock) (close . fst) $
    \(conn, _) -> do
      sendAll conn "Game is already in progress\n"
      close conn

waitForConnection :: IO Socket
waitForConnection = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet defaultPort localhost)
  port <- socketPort sock
  listen sock 1
  putStrLn $ "Waiting for player 2 on port: " ++ show port ++ "."
  (conn, _) <- accept sock
  sendAll conn "OK\n"
  putStrLn "Connection established"
  forkFinally (refuseConnections sock) (\_ -> close sock)
  return conn

readIP :: IO HostAddress
readIP = do
  putStrLn "Enter server IP address: "
  ip <- getLine
  case (IP.decode $ T.pack ip, ip) of
    (Just ipv4, _) -> return $ tupleToHostAddress $ IP.toOctets ipv4
    (Nothing, "") -> putStrLn "You chose 127.0.0.1" >> return localhost
    (Nothing, _) -> putStrLn "Invalid IP address" >> readIP

readPort :: IO PortNumber
readPort = do
  putStrLn "Enter server port: "
  port <- getLine
  case reads port of
    [(p, "")] -> return p
    _ -> putStrLn "Invalid port" >> readPort

connectToServer :: IO Socket
connectToServer = do
  ip <- readIP
  port <- readPort
  sock <- socket AF_INET Stream defaultProtocol
  res <- E.try $ connect sock (SockAddrInet port ip)
  case res of
    Left (e :: E.IOException) -> putStrLn "Connection refused" >> connectToServer
    Right _ -> do
      msg <- recv sock 3
      case msg of
        "OK\n" -> do
          putStrLn "Connection established"
          return sock
        _ -> putStrLn "Connection refused" >> connectToServer