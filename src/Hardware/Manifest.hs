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
    SystemLatencyMs,
    speedOfLight,
    gatingToleranceMm,
    hysteresisMarginMm,
    targetHeightMm,
    minVelocityMs,
    maxVelocityMs,
    minAccelerationMs2,
    maxAccelerationMs2
) where

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

speedOfLight :: Double
speedOfLight = 300000000.0

gatingToleranceMm :: Double
gatingToleranceMm = 3.0

hysteresisMarginMm :: Double
hysteresisMarginMm = 0.5

targetHeightMm :: Double
targetHeightMm = 10.0

minVelocityMs :: Double
minVelocityMs = 0.01

maxVelocityMs :: Double
maxVelocityMs = 0.1

minAccelerationMs2 :: Double
minAccelerationMs2 = 0.01

maxAccelerationMs2 :: Double
maxAccelerationMs2 = 0.1

