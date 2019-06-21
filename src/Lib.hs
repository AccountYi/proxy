module Lib where

import ClassyPrelude hiding (getContents)
import qualified Control.Exception as E
import qualified Data.ByteString as S
import Network.Socket
import qualified Network.Socket.ByteString as Nsb
import Data.List as Dl (head)




import Foreign
import Foreign.C
import Network.Socket.Internal 
import Control.Monad
import Control.Concurrent
import GHC.IO.Handle
import System.Posix.IO
import System.Posix.Types
import System.Environment

import Network.Socket.ByteString.Lazy



mySocketConn :: IO()

mySocketConn = withSocketsDo $ do

    E.bracket open close loop 
    where
        open = do
            let addrInfo = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}

            addr:_ <- getAddrInfo  -- 此函数返回的AddrInfo值包含可以直接传递给connect或bind的SockAddr值
                        (Just addrInfo) -- 首选套接字类型或协议
                        Nothing         -- 要查找的主机名
                        (Just "3000")   -- 要查找的服务名称  例如“http”或数字端口号。

            sock <- socket -- 使用给定的地址家庭，套接字类型和协议号创建一个新套接字。地址族通常是AF_INET，AF_INET6或AF_UNIX。套接字类型通常是Stream或Datagram。协议号通常是defaultProtocol。
                        (addrFamily addr)     -- 地址家庭
                        (addrSocketType addr) -- 套接字类型
                        (addrProtocol addr)   -- 协议号  

            bind sock (addrAddress addr) --将套接字绑定到一个地址。套接字必须尚未绑定。传递给bind的Family必须与传递给socket的相同。
            listen sock 5 --侦听对套接字的连接。第二个参数指定排队连接的最大数量，并且应至少为1;最大值取决于系统（通常为5）。
            return sock

        loop sock =  forever $ do
          
                        let aaa = tupleToHostAddress (115, 239, 210, 27)
                        let serverAddr = SockAddrInet 80 aaa
                      
                        let serverAI = defaultHints { addrSocketType = Stream, addrAddress = serverAddr, addrFamily = getSockAddrFamily serverAddr  }
                      
                        serverSock <- socket (addrFamily serverAI) (addrSocketType serverAI) (addrProtocol serverAI)
                        connect serverSock serverAddr


                        (conn, peer) <- accept sock -- 其中conn是可用于在连接上发送和接收数据的新套接字对象，peer是绑定到连接另一端的套接字的地址。
                        msg <- Nsb.recv conn 1024 --从套接字接收数据。套接字必须处于连接状态
                        putStrLn $ (fromString "msg :: " )++ (decodeUtf8 msg)
                        msg1 <- Nsb.recv serverSock 1024 --从套接字接收数据。套接字必须处于连接状态
                        putStrLn $ (fromString "msg1 ::  " )++ (decodeUtf8 msg)




--为公共缓冲区创建一对句柄。 第一个是读，第二个是写
createPipeHandle :: IO (Handle, Handle)

createPipeHandle = do
  (a, b) <- createPipe
  ah <- fdToHandle a
  bh <- fdToHandle b
  return (ah, bh)



runProxy :: String -> IO ()

runProxy port = do
  let proxyHints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
  proxyAI:_ <- getAddrInfo  -- 此函数返回的AddrInfo值包含可以直接传递给connect或bind的SockAddr值
                        (Just proxyHints) -- 首选套接字类型或协议
                        Nothing         -- 要查找的主机名
                        (Just port)   -- 要查找的服务名称  例如“http”或数字端口号。

  proxySock <- socket -- 使用给定的地址家庭，套接字类型和协议号创建一个新套接字。地址族通常是AF_INET，AF_INET6或AF_UNIX。套接字类型通常是Stream或Datagram。协议号通常是defaultProtocol。
                        (addrFamily proxyAI)     -- 地址家庭
                        (addrSocketType proxyAI) -- 套接字类型
                        (addrProtocol proxyAI)   -- 协议号  (addrProtocol proxyAI)
  bind proxySock (addrAddress proxyAI) --将套接字绑定到一个地址。套接字必须尚未绑定。传递给bind的Family必须与传递给socket的相同。
  
  listen proxySock 5 --侦听对套接字的连接。第二个参数指定排队连接的最大数量，并且应至少为1;最大值取决于系统（通常为5）。
  forever $ do
    (clientSock, sockAddr) <- accept proxySock -- 返回的第一个是可用于在连接上发送和接收数据的新套接字对象，第二个是绑定到连接另一端的套接字的地址。
    
{-     case sockAddr of
      SockAddrInet portNumber hostAddress ->
            traceM((show "port:") ++ (show portNumber) ++ (show "==========host:") ++ (show hostAddress)) -}


    proxySocket clientSock sockAddr


getSockAddrFamily :: SockAddr -> Family

getSockAddrFamily (SockAddrUnix _) = AF_UNIX

getSockAddrFamily (SockAddrInet _ _) = AF_INET

getSockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6    


parseSockAddrUnix :: Ptr a -> IO SockAddr

parseSockAddrUnix ptr = do
  path <- peekCString (plusPtr ptr 2)
  return $ SockAddrUnix path

parseSockAddrInet ::Ptr a ->  IO SockAddr

parseSockAddrInet sock = do
  
  return $ SockAddrInet 300 $ tupleToHostAddress (206,189,179,153)

parseSockAddrInet6 :: Ptr a -> IO SockAddr

parseSockAddrInet6 ptr = do
  port <- peekByteOff ptr 2
  flowInfo <- peekByteOff ptr 4
  addr0 <- peekByteOff ptr 8
  addr1 <- peekByteOff ptr 12
  addr2 <- peekByteOff ptr 16
  addr3<- peekByteOff ptr 20
  scopeId <- peekByteOff ptr 24
  return $ SockAddrInet6 port flowInfo (addr0, addr1, addr2, addr3) scopeId

foreign import ccall unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

getSockOrigDest :: Socket -> IO SockAddr

getSockOrigDest s = do
  let szInt = 128 :: Int -- Size of struct sockaddr_storage
      szCInt = 128 :: CInt -- Size of struct sockaddr_storage
      solIP = 0 -- Protocol level required for SO_ORIGINAL_DST
      soOrigDest = 80 -- The option name SO_ORIGINAL_DST has value 80
      familyOffset = 0 -- Offset of sin_family member of sockaddr_in
  allocaBytes szInt $ \ptr -> do
    withFdSocket s $ \fd -> with szCInt $ \ptr_sz -> do
      throwSocketErrorIfMinus1Retry_ "getSockOrigDest" $
        c_getsockopt fd solIP soOrigDest ptr ptr_sz
      family <- peekByteOff ptr familyOffset
      case unpackFamily (fromIntegral (family :: CShort)) of 
        AF_UNIX -> parseSockAddrUnix ptr
        AF_INET -> parseSockAddrInet 
        AF_INET6 -> parseSockAddrInet6 ptr
        _ -> throwSocketError ("Unsupported socket address type: " ++ show family)
  
forwardData :: Socket -> Socket -> Int -> SockAddr ->IO ()

forwardData srcSock destSock1  a addrs = do
  
  msg <- Nsb.recv srcSock 32768 --从套接字接收数据。套接字必须处于连接状态


  case a of 
    1 ->  case addrs of
              SockAddrInet portNumber hostAddress ->
                    traceM((show "port11:") ++ (show portNumber) ++ (show "==========host11:") ++ (show hostAddress) ++ (show $ "***one:    " ++ msg))
    2 ->  case addrs of
              SockAddrInet portNumber hostAddress ->
                    traceM((show "port11:") ++ (show portNumber) ++ (show "==========host11:") ++ (show hostAddress) ++ (show $ "***tow:    " ++ msg)   )


  unless (S.null msg) $ do
    
    Nsb.sendAllTo destSock1 msg addrs -- 将数据发送到服务端套接字
    --S.hPut destSock2 msg -- 将 msg 输出到指定的Handle
    forwardData srcSock destSock1  a addrs


proxySocket :: Socket -> SockAddr ->IO ()

proxySocket clientSock  addrs= do

  let aaa = tupleToHostAddress (206,189,179,153)
  let serverAddr = SockAddrInet 3000 aaa
  traceM((show "====================================="))
  case serverAddr of
    SockAddrInet portNumber hostAddress ->
          traceM((show "port:") ++ (show portNumber) ++ (show "==========host:") ++ (show hostAddress)) 
  traceM((show "+++++++++++++++++++++++++++++++++++++++"))
  --serverAddr <- getSockOrigDest clientSock
  let serverAI = defaultHints { addrSocketType = Stream, addrAddress = serverAddr, addrFamily = getSockAddrFamily serverAddr  }

  serverSock <- socket (addrFamily serverAI) (addrSocketType serverAI) (addrProtocol serverAI)
  connect serverSock serverAddr
  --(client, addrs) <- accept serverSock
  --ruu serverSock
  forkFinally (forwardData clientSock serverSock  1 serverAddr) (\_ -> close clientSock >> close serverSock)
  {- close clientSock
  close serverSock -}
  forkFinally (forwardData serverSock clientSock  2 addrs) (\_ -> close clientSock >> close serverSock)
  traceM(show "===================================================================================")
  {- close clientSock
  close serverSock -}
  return ()


main :: IO ()

main = do

  cmdArgs <- ClassyPrelude.getArgs
  runProxy (unpack (Dl.head cmdArgs))