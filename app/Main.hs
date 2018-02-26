{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Monad
import           Data.Binary
import           Data.Binary.Orphans ()
import           Data.Function (on)
import           Data.List (sort)
import           Data.Time.Clock
import           Data.Typeable
import qualified Data.Vector as V
import qualified Data.VectorClock as VC
import           System.Random.MWC

import           GHC.Generics

import           CmdLine


--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------
-- vector clocks map ProcessIds to state counters
type Clock = VC.VectorClock ProcessId Integer

-- A control message communicates the end of the send period,
-- or the time at which to print final results.
data ControlMsg = StopSend
                | PrintResult
                deriving (Generic, Typeable)
instance Binary ControlMsg

data ProtocolMsg = Num { msgValue     :: !Double
                       , msgTimestamp :: !UTCTime
                       , msgClock     :: !Clock }
                 deriving (Generic, Typeable, Show, Eq)
instance Binary ProtocolMsg

instance Ord ProtocolMsg where
  m1 `compare` m2 = case (msgClock m1) `VC.relation` (msgClock m2) of
                      VC.Causes -> LT
                      VC.CausedBy -> GT
                      -- break ties with timestamp
                      VC.Concurrent -> (msgTimestamp m1) `compare` (msgTimestamp m2)


--------------------------------------------------------------------------------
-- Worker process, run on all slaves
--------------------------------------------------------------------------------
workerLoop :: V.Vector Word32 -> Process ()
workerLoop entropy = do
  allPids <- expect :: Process [ProcessId]
  say "worker is up"

  gen <- liftIO (initialize entropy)
  clockMV <- liftIO (newMVar VC.empty)

  sendPid <- spawnLocal (sender clockMV allPids gen)
  go clockMV sendPid []

  where
    protoCmd clockMV ls msg = do
      mypid <- getSelfPid
      -- we step our clock, then `max` it with the received clock.
      let updateClock ourClock = VC.max (msgClock msg)
                                        (VC.incWithDefault mypid ourClock 1)
      liftIO (modifyMVar_ clockMV (return . updateClock))
      return (msg : ls)

    controlCmd sendPid ls StopSend = do
      exit sendPid "send period over"
      say "received StopSend"
      return ls

    controlCmd sendPid ls PrintResult = do
      say "received PrintResult"
      -- reverse the list, since most recent values are consed on the front
      let values = map msgValue (sort (reverse ls))
          total = sum (zipWith (*) values [1..])
          msg = "<" ++ show (length values) ++ ", " ++ show total ++ ">"
      msgBeginTime <- liftIO getCurrentTime
      liftIO (putStrLn msg)
      say msg
      msgEndTime <- liftIO getCurrentTime
      say ("sorting took: " ++ show (diffUTCTime msgEndTime msgBeginTime))
      terminate
      return ls

    --
    go :: MVar Clock -> ProcessId -> [ProtocolMsg] -> Process ()
    go clockMV sendPid ls = do
      ls' <- receiveWait [ match (protoCmd clockMV ls)
                         , match (controlCmd sendPid ls)
                         ]
      go clockMV sendPid ls'


-- a process to send messages
sender :: MVar Clock -> [ProcessId] -> GenIO -> Process ()
sender clockMV allPids gen = forever $ do
  -- From docs: "For floating point numbers, the range (0,1] is used."
  n <- liftIO (uniform gen)

  sendTime <- liftIO getCurrentTime

  mypid <- getSelfPid
  -- increment the clock if an entry exists, or start at one if no entry
  -- exists. clocks are initialized at 0, so would equal 1 after one increment.
  let incClock vc = VC.incWithDefault mypid vc 1
  clock <- liftIO (modifyMVar clockMV (\vc -> let x = incClock vc in return (x,x)))

  let msg = Num n sendTime clock
  mapM_ (`send` msg) allPids


$( remotable ['workerLoop] )

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable


--------------------------------------------------------------------------------
-- Master / timing coordinator process
--------------------------------------------------------------------------------
master :: Backend -> Int -> Int -> Word32 -> [NodeId] -> Process ()
master backend sendSeconds graceSeconds seed slaves = do
  say ("sendSeconds = " ++ show sendSeconds)
  say ("graceSeconds = " ++ show graceSeconds)
  say $ "slaves: " ++ show slaves

  masterGen <- liftIO (initialize (V.singleton seed))
  -- generate 258 length entropies for slaves, so we have total reproducibility
  slaveEntropies <- liftIO $ replicateM (length slaves) $ do
    asGenIO (\g -> uniformVector g 258) masterGen :: IO (V.Vector Word32)

  -- spawn slave workers
  cpids <- zipWithM (\slaveNode seed ->
                  spawn slaveNode ($(mkClosure 'workerLoop) seed))
                slaves
                slaveEntropies

  -- send all pids to each slave
  mapM (`send` cpids) cpids

  -- begin send period
  liftIO (threadDelay (sendSeconds * 10^6))
  mapM (`send` StopSend) cpids
  say "send period over"

  -- begin grace period
  -- we allow 3/4 of the grace period for computation
  -- the first 1/4 is for final messages to arrive
  let computeMicroseconds = (graceSeconds * 10^6 * 3) `div` 4
  liftIO (threadDelay ((graceSeconds * 10^6) - computeMicroseconds))
  mapM (`send` PrintResult) cpids
  say "sent PrintResult signal"
  liftIO (threadDelay computeMicroseconds)
  say "grace period over"

  -- wait a few additional seconds before terminating all
  liftIO (threadDelay (3 * 10^6))
  say "terminating"
  terminateAllSlaves backend

--------------------------------------------------------------------------------
-- Main entrypoint
--------------------------------------------------------------------------------
main :: IO ()
main = do
  (posArgs, opts) <- getOptions

  case posArgs of
    PosArgs "master" host port -> do
      backend <- initializeBackend host port myRemoteTable
      let sendSeconds = optSendFor opts
          graceSeconds = optWaitFor opts
          seed = optSeed opts
      startMaster backend (master backend sendSeconds graceSeconds seed)

    PosArgs "slave" host port -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend

