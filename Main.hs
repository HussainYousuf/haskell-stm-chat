{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- in Main.hs
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (newBroadcastTChan)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Network.Socket (
    AddrInfo (addrAddress, addrFlags, addrSocketType),
    AddrInfoFlag (AI_PASSIVE),
    Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultHints,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
    socketToHandle,
    tupleToHostAddress,
 )
import Network.WebSockets (DataMessage)
import qualified Network.WebSockets as WS
import System.IO (
    BufferMode (NoBuffering),
    IOMode (ReadWriteMode),
    hClose,
    hGetContents,
    hPutStr,
    hSetBuffering,
 )

main :: IO ()
main = do
    writeChan <- atomically newBroadcastTChan
    forkIO $ WS.runServer "0.0.0.0" 9160 $ serverApp writeChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 4242 $ tupleToHostAddress (0, 0, 0, 0)
    listen sock 50
    forever $
        do
            (socket, _) <- accept sock
            forkIO $ httpHandler socket

serverApp :: TChan DataMessage -> WS.ServerApp
serverApp writeChan pendingConnection = do
    -- print $ WS.pendingRequest pendingConnection
    conn <- WS.acceptRequest pendingConnection
    readChan <- atomically $ dupTChan writeChan
    WS.withPingThread conn 30 (return ()) $ do
        forkIO $
            forever $ do
                msg <- WS.receiveDataMessage conn
                atomically $ writeTChan writeChan msg
        forever $ do
            msg <- atomically $ readTChan readChan
            WS.sendDataMessage conn msg

httpHandler :: Socket -> IO ()
httpHandler socket =
    do
        handle <- socketToHandle socket ReadWriteMode
        !input <- hGetContents handle
        index <- readFile "./index.html"
        hPutStr handle $ "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n\r\n" ++ index
        hClose handle
        `catch` (\(e :: SomeException) -> close socket)
