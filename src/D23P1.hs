module D23P1 (
    yvalue
) where

import IntcodeV3
import Data.List
import Data.Char
import Data.Tuple.Extra
import Data.List.Split(chunksOf)
import Debug.Trace

type Packet = (Int, Int, Int)

yvalue :: [Register] -> Int
yvalue = res . until overflow run . init . mkStates
    where
        init states = foldr (\s (n, ss) -> queuePackets n ss s) ([], []) states
        mkStates registers = map (\i -> runProgram $ mkIntcode (chr i) [i] registers) [0..49]
        run (nqueue, states) = foldr (transmit nqueue) ([], []) states
        transmit nqueue state (nqueue', states) = queuePackets nqueue' states . runProgram $ state { sOutput = [], sInput = compInput (ord $ sId state) nqueue }
        overflow (nqueue, _) = any ((==) 255 . fst3) nqueue
        res (nqueue, _) = thd3 . last $ filter ((==) 255 . fst3) nqueue

compInput :: Int -> [Packet] -> [Int]
compInput cid = mkInput . foldr addPkt []
    where
        mkInput packets = if null packets then [-1] else packets
        addPkt (c, x, y) packets = if c == cid then x:y:packets else packets
    
queuePackets :: [Packet] -> [State] -> State -> ([Packet], [State])
queuePackets nqueue states state = (queuePkts $ sOutput state, state:states)
    where
        queuePkts = foldr addPkt nqueue . chunksOf 3
        addPkt (y:x:c:_) = (:) (c, x, y)

{-
https://adventofcode.com/2019/day/23

The droids have finished repairing as much of the ship as they can. Their report indicates that this was a Category 6 disaster - not because it was that bad, but because it destroyed the stockpile of Category 6 network cables as well as most of the ship's network infrastructure.

You'll need to rebuild the network from scratch.

The computers on the network are standard Intcode computers that communicate by sending packets to each other. There are 50 of them in total, each running a copy of the same Network Interface Controller (NIC) software (your puzzle input). The computers have network addresses 0 through 49; when each computer boots up, it will request its network address via a single input instruction. Be sure to give each computer a unique network address.

Once a computer has received its network address, it will begin doing work and communicating over the network by sending and receiving packets. All packets contain two values named X and Y. Packets sent to a computer are queued by the recipient and read in the order they are received.

To send a packet to another computer, the NIC will use three output instructions that provide the destination address of the packet followed by its X and Y values. For example, three output instructions that provide the values 10, 20, 30 would send a packet with X=20 and Y=30 to the computer with address 10.

To receive a packet from another computer, the NIC will use an input instruction. If the incoming packet queue is empty, provide -1. Otherwise, provide the X value of the next packet; the computer will then use a second input instruction to receive the Y value for the same packet. Once both values of the packet are read in this way, the packet is removed from the queue.

Note that these input and output instructions never block. Specifically, output instructions do not wait for the sent packet to be received - the computer might send multiple packets before receiving any. Similarly, input instructions do not wait for a packet to arrive - if no packet is waiting, input instructions should receive -1.

Boot up all 50 computers and attach them to your network. What is the Y value of the first packet sent to address 255?
-}