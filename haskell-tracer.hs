import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket.ByteString

port = "33434"
maxHops = 30

mkSSND :: IO (Socket)
mkSSND = do
    protocol <- getProtocolNumber "udp"
    ssnd <- socket AF_INET Datagram protocol
    return ssnd

mkSRCV :: IO (Socket)
mkSRCV = do
    protocol <- getProtocolNumber "icmp"
    srcv <- socket AF_INET Raw protocol
    addr <- mkAddr Nothing (Just port)
    bind srcv addr
    return srcv

mkAddr :: (Maybe String) -> (Maybe String) -> IO (SockAddr)
mkAddr host portBind = do
    addrInfo <- getAddrInfo Nothing host portBind
    return $ addrAddress $ head addrInfo

cut :: String -> String
cut (h:t)
    | h == ':' = []
    | otherwise = h:(cut t)

format :: Int -> SockAddr -> IO ()
format ttl addr = do
    let f = (show ttl) ++ " - " ++ (cut $ show addr)
    putStrLn $ f

check :: SockAddr -> SockAddr -> Bool
check a b = (cut $ show a) /= (cut $ show b)

run :: Socket -> Socket -> SockAddr -> Int -> IO ()
run ssnd srcv addr ttl
    | ttl > maxHops = print "end"
    | otherwise = do
        setSocketOption ssnd TimeToLive ttl
        sendTo ssnd (pack " ") addr
        currentAddr <- recvFrom srcv 1024 >>= \x -> return $ snd x
        format ttl currentAddr
        case check addr currentAddr of
            True -> run ssnd srcv addr (succ ttl)
            False -> putStrLn "end"
            

main = do
    host <- getLine
    addr <- mkAddr (Just host) (Just port)
    ssnd <- mkSSND
    srcv <- mkSRCV
    run ssnd srcv addr 1
