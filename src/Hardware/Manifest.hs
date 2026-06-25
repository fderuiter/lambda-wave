{-# LANGUAGE DataKinds #-}
module Hardware.Manifest where

import GHC.TypeLits (Nat)

watchdogPin :: Int
watchdogPin = 27

logicPin :: Int
logicPin = 17

configBaudRate :: Int
configBaudRate = 115200

dataBaudRate :: Int
dataBaudRate = 921600

framePeriodicityMs :: Int
framePeriodicityMs = 100

systemLatencyMs :: Int
systemLatencyMs = 50

type WatchdogTimeoutMs = 100

type SystemLatencyMs = 50
