-- DO NOT EDIT: This file is auto-generated from the hardware manifest.
{-# LANGUAGE DataKinds #-}
module Hardware.Manifest (
    watchdogPin,
    logicPin,
    configBaudRate,
    dataBaudRate,
    framePeriodicityMs,
    systemLatencyMs,
    mountingOffsetMm,
    WatchdogTimeoutMs,
    SystemLatencyMs
) where

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

mountingOffsetMm :: Double
mountingOffsetMm = 50.0

type WatchdogTimeoutMs = 100

type SystemLatencyMs = 50
