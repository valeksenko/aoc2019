module D23P2 (
    yvalue
) where

import IntcodeV3
import Data.List
import Data.Maybe
import Data.Char
import Data.Tuple.Extra
import Data.List.Split(chunksOf)
import Debug.Trace

type Packet = (Int, Int, Int)

data NAT =
    NAT {
        nStates :: [State]
      , nQueue :: [Packet]
      , nSent :: [Int]
      , nPacket :: Maybe Packet
    } deriving(Show, Eq, Ord)

yvalue :: [Register] -> Int
yvalue = res . until (dupy . take 2 . traceShowId . nSent) run . init . mkStates
    where
        init states = foldr (\s nat -> queuePackets nat s) (NAT [] [] [] Nothing) states
        mkStates registers = map (\i -> runProgram $ mkIntcode (chr i) [i] registers) [0..49]
        run nat = (traceShow $ nQueue nat) . natCheck . foldr (transmit $ nQueue nat) (nat { nStates = [], nQueue = [] }) $ nStates nat
        transmit nqueue state nat = queuePackets nat . runProgram $ state { sOutput = [], sInput = compInput (ord $ sId state) nqueue }
        dupy sent = ((length sent) == 2) && ((head sent) == (last sent))
        res = head . nSent

compInput :: Int -> [Packet] -> [Int]
compInput cid = mkInput . foldr addPkt []
    where
        mkInput packets = if null packets then [-1] else packets
        addPkt (c,x,y) packets = if c == cid then x:y:packets else packets
    
queuePackets :: NAT -> State -> NAT
queuePackets nat state = nat { nQueue = queuePkts $ sOutput state, nStates = state:(nStates nat) }
    where
        queuePkts = foldr addPkt (nQueue nat) . chunksOf 3
        addPkt (y:x:c:_) = (:) (c, x, y)

natCheck :: NAT -> NAT
natCheck nat = checkStates $ nat { nPacket = checkPkt } 
    where
        checkPkt = checkP . filter ((==) 255 . fst3) $ nQueue nat
        checkP p = if null p then nPacket nat else Just (head p)
        checkStates nat' = if (null $ nQueue nat') then useNat nat' else nat'
        useNat nat' = nat' { nQueue = (natQueue . fromJust $ nPacket nat'), nSent = (thd3 . fromJust $ nPacket nat'):(nSent nat') }
        natQueue (_,x,y) = [(0, x, y)]

{-
https://adventofcode.com/2019/day/23#part2

Packets sent to address 255 are handled by a device called a NAT (Not Always Transmitting). The NAT is responsible for managing power consumption of the network by blocking certain packets and watching for idle periods in the computers.

If a packet would be sent to address 255, the NAT receives it instead. The NAT remembers only the last packet it receives; that is, the data in each packet it receives overwrites the NAT's packet memory with the new packet's X and Y values.

The NAT also monitors all computers on the network. If all computers have empty incoming packet queues and are continuously trying to receive packets without sending packets, the network is considered idle.

Once the network is idle, the NAT sends only the last packet it received to address 0; this will cause the computers on the network to resume activity. In this way, the NAT can throttle power consumption of the network when the ship needs power in other areas.

Monitor packets released to the computer at address 0 by the NAT. What is the first Y value delivered by the NAT to the computer at address 0 twice in a row?
-}