import Network.BSD(getProtocolNumber)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.ByteString.Char8 (pack)
import Network.Socket.ByteString

port = "33434"

mkSSND :: IO (Socket)
mkSSND = do
    protocol <- getProtocolNumber "udp"
    ssnd <- socket AF_INET Datagram protocol
    return ssnd

mkSRCV :: IO (Socket)
mkSRCV = do
    protocol <- getProtocolNumber "icmp"
    srcv <- socket AF_INET Raw protocol
    return srcv

test :: Socket -> Socket -> SockAddr -> IO ()
test snd rcv addr = do
    print addr
    sendTo snd (pack "GET / HTTP/1.1") addr
    dat <- recv rcv 1024
    print dat

main = do
    host <- getLine
    addrInfo <- getAddrInfo Nothing (Just host) (Just "0")
    print $ addrInfo
    ssnd <- mkSSND
    srcv <- mkSRCV
    let addr = show $ addrAddress $ head addrInfo
    test ssnd srcv $ SockAddrUnix ((init addr) ++ port)
