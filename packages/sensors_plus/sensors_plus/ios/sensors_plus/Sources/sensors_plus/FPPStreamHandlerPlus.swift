// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

import CoreMotion
import Flutter
import Foundation
import UIKit

let GRAVITY = 9.81
var _motionManager: CMMotionManager!
var _altimeter: CMAltimeter!

public protocol MotionStreamHandler: FlutterStreamHandler {
  var samplingPeriod: Int { get set }
}

let timestampMicroAtBoot =
  (Date().timeIntervalSince1970 - ProcessInfo.processInfo.systemUptime) * 1_000_000

func _initMotionManager() {
  if _motionManager == nil {
    _motionManager = CMMotionManager()
    _motionManager.accelerometerUpdateInterval = 0.2
    _motionManager.deviceMotionUpdateInterval = 0.2
    _motionManager.gyroUpdateInterval = 0.2
    _motionManager.magnetometerUpdateInterval = 0.2
  }
}

func _initAltimeter() {
  if _altimeter == nil {
    _altimeter = CMAltimeter()
  }
}

func sendFlutter(
  x: Float64, y: Float64, z: Float64, timestamp: TimeInterval,
  accuracy: Float64, sink: @escaping FlutterEventSink
) {
  if _isCleanUp {
    return
  }
  // Even after [detachFromEngineForRegistrar] some events may still be received
  // and fired until fully detached.
  DispatchQueue.main.async {
    let timestampSince1970Micro = timestampMicroAtBoot + (timestamp * 1_000_000)
    let triplet = [x, y, z, timestampSince1970Micro, accuracy]
    triplet.withUnsafeBufferPointer { buffer in
      sink(FlutterStandardTypedData.init(float64: Data(buffer: buffer)))
    }
  }
}

class FPPAccelerometerStreamHandlerPlus: NSObject, MotionStreamHandler {

  var samplingPeriod = 200000 {
    didSet {
      _initMotionManager()
      _motionManager.accelerometerUpdateInterval = Double(samplingPeriod) * 0.000001
    }
  }

  func onListen(
    withArguments arguments: Any?,
    eventSink sink: @escaping FlutterEventSink
  ) -> FlutterError? {
    _initMotionManager()
    _motionManager.startAccelerometerUpdates(to: OperationQueue()) { data, error in
      if _isCleanUp {
        return
      }
      if error != nil {
        sink(
          FlutterError.init(
            code: "UNAVAILABLE",
            message: error!.localizedDescription,
            details: nil
          ))
        return
      }
      // Multiply by gravity, and adjust sign values to
      // align with Android.
      guard let acceleration = data?.acceleration else { return }

      // 1) Calculate sign-adjusted axes
      let xVal = -acceleration.x * GRAVITY
      let yVal = -acceleration.y * GRAVITY
      let zVal = -acceleration.z * GRAVITY

      let accuracy = computeAccuracy(
        sensorType: "accelerometer",
        x: xVal,
        y: yVal,
        z: zVal
      )

      sendFlutter(
        x: xVal,
        y: yVal,
        z: zVal,
        timestamp: data!.timestamp,
        accuracy: accuracy,
        sink: eventSink
      )

    }
    return nil
  }

  func onCancel(withArguments arguments: Any?) -> FlutterError? {
    _motionManager.stopAccelerometerUpdates()
    return nil
  }

  func dealloc() {
    FPPSensorsPlusPlugin._cleanUp()
  }
}

class FPPUserAccelStreamHandlerPlus: NSObject, MotionStreamHandler {

  var samplingPeriod = 200000 {
    didSet {
      _initMotionManager()
      _motionManager.deviceMotionUpdateInterval = Double(samplingPeriod) * 0.000001
    }
  }

  func onListen(
    withArguments arguments: Any?,
    eventSink sink: @escaping FlutterEventSink
  ) -> FlutterError? {
    _initMotionManager()
    _motionManager.startDeviceMotionUpdates(to: OperationQueue()) { data, error in
      if _isCleanUp {
        return
      }
      if error != nil {
        sink(
          FlutterError.init(
            code: "UNAVAILABLE",
            message: error!.localizedDescription,
            details: nil
          ))
        return
      }

      guard let accel = data?.userAcceleration else { return }
      
      // Multiply by gravity, and adjust sign values to
      // align with Android.
      let xVal = -accel.x * GRAVITY
      let yVal = -accel.y * GRAVITY
      let zVal = -accel.z * GRAVITY

      let accuracy = computeAccuracy(
        sensorType: "userAccel",
        x: xVal,
        y: yVal,
        z: zVal
      )

      sendFlutter(
        x: xVal,
        y: yVal,
        z: zVal,
        timestamp: data!.timestamp,
        accuracy: accuracy,
        sink: sink
      )

    }
    return nil
  }

  func onCancel(withArguments arguments: Any?) -> FlutterError? {
    _motionManager.stopDeviceMotionUpdates()
    return nil
  }

  func dealloc() {
    FPPSensorsPlusPlugin._cleanUp()
  }
}

class FPPGyroscopeStreamHandlerPlus: NSObject, MotionStreamHandler {

  var samplingPeriod = 200000 {
    didSet {
      _initMotionManager()
      _motionManager.gyroUpdateInterval = Double(samplingPeriod) * 0.000001
    }
  }

  func onListen(
    withArguments arguments: Any?,
    eventSink sink: @escaping FlutterEventSink
  ) -> FlutterError? {
    _initMotionManager()
    _motionManager.startGyroUpdates(to: OperationQueue()) { data, error in
      if _isCleanUp {
        return
      }
      if error != nil {
        sink(
          FlutterError(
            code: "UNAVAILABLE",
            message: error!.localizedDescription,
            details: nil
          ))
        return
      }
      guard let rotationRate = data?.rotationRate else { return }

      let xVal = rotationRate.x
      let yVal = rotationRate.y
      let zVal = rotationRate.z

      let accuracy = computeAccuracy(
        sensorType: "gyroscope",
        x: xVal,
        y: yVal,
        z: zVal
      )

      sendFlutter(
        x: xVal,
        y: yVal,
        z: zVal,
        timestamp: data!.timestamp,
        accuracy: accuracy,
        sink: sink
      )

    }
    return nil
  }

  func onCancel(withArguments arguments: Any?) -> FlutterError? {
    _motionManager.stopGyroUpdates()
    return nil
  }

  func dealloc() {
    FPPSensorsPlusPlugin._cleanUp()
  }
}

class FPPMagnetometerStreamHandlerPlus: NSObject, MotionStreamHandler {

  var samplingPeriod = 200000 {
    didSet {
      _initMotionManager()
      _motionManager.magnetometerUpdateInterval = Double(samplingPeriod) * 0.000001
    }
  }

  func onListen(
    withArguments arguments: Any?,
    eventSink sink: @escaping FlutterEventSink
  ) -> FlutterError? {
    _initMotionManager()
    _motionManager.startMagnetometerUpdates(to: OperationQueue()) { data, error in
      if _isCleanUp {
        return
      }
      if error != nil {
        sink(
          FlutterError(
            code: "UNAVAILABLE",
            message: error!.localizedDescription,
            details: nil
          ))
        return
      }
      guard let magneticField = data?.magneticField else { return }

      let xVal = magneticField.x
      let yVal = magneticField.y
      let zVal = magneticField.z

      let accuracy = computeAccuracy(
        sensorType: "magnetometer",
        x: xVal,
        y: yVal,
        z: zVal
      )

      sendFlutter(
        x: xVal,
        y: yVal,
        z: zVal,
        timestamp: data!.timestamp,
        accuracy: accuracy,
        sink: sink
      )

    }
    return nil
  }

  func onCancel(withArguments arguments: Any?) -> FlutterError? {
    _motionManager.stopDeviceMotionUpdates()
    return nil
  }

  func dealloc() {
    FPPSensorsPlusPlugin._cleanUp()
  }
}

class FPPBarometerStreamHandlerPlus: NSObject, MotionStreamHandler {

  var samplingPeriod = 200000 {
    didSet {
      _initAltimeter()
      // Note: CMAltimeter does not provide a way to set the sampling period directly.
      // The sampling period would typically be managed by starting/stopping the updates.
    }
  }

  func onListen(
    withArguments arguments: Any?,
    eventSink sink: @escaping FlutterEventSink
  ) -> FlutterError? {
    _initAltimeter()
    if CMAltimeter.isRelativeAltitudeAvailable() {
      _altimeter.startRelativeAltitudeUpdates(to: OperationQueue()) { data, error in
        if _isCleanUp {
          return
        }
        if error != nil {
          sink(
            FlutterError(
              code: "UNAVAILABLE",
              message: error!.localizedDescription,
              details: nil
            ))
          return
        }
        let pressure = data!.pressure.doubleValue * 10.0  // kPa to hPa (hectopascals)
        DispatchQueue.main.async {
          let timestampSince1970Micro = timestampMicroAtBoot + (data!.timestamp * 1_000_000)
          let pressureArray: [Double] = [pressure, timestampSince1970Micro]
          pressureArray.withUnsafeBufferPointer { buffer in
            sink(FlutterStandardTypedData.init(float64: Data(buffer: buffer)))
          }
        }
      }
    } else {
      return FlutterError(
        code: "UNAVAILABLE",
        message: "Barometer is not available on this device",
        details: nil
      )
    }
    return nil
  }

  func onCancel(withArguments arguments: Any?) -> FlutterError? {
    _altimeter.stopRelativeAltitudeUpdates()
    return nil
  }

  func dealloc() {
    FPPSensorsPlusPlugin._cleanUp()
  }
}

// Rolling buffers for each sensor:
private var accelBuffer = [Double]()
private var gyroBuffer = [Double]()
private var magnetoBuffer = [Double]()
private var userAccelBuffer = [Double]()

// How many samples to keep in the rolling buffer:
private let MAX_BUFFER_SIZE = 20

// Main function you can call from each sensor's callback:
func computeAccuracy(sensorType: String, x: Double, y: Double, z: Double) -> Double {
  // 1) Compute "magnitude" (as a single measure of the sensor reading).
  let magnitude = sqrt(x * x + y * y + z * z)

  // 2) Based on sensor type, store the magnitude in the correct buffer.
  switch sensorType {
  case "accelerometer":
    return storeAndComputeStdDev(buffer: &accelBuffer, newValue: magnitude)
  case "gyroscope":
    return storeAndComputeStdDev(buffer: &gyroBuffer, newValue: magnitude)
  case "magnetometer":
    return storeAndComputeStdDev(buffer: &magnetoBuffer, newValue: magnitude)
  case "userAccel":
    return storeAndComputeStdDev(buffer: &userAccelBuffer, newValue: magnitude)
  default:
    return 1.0  // Fallback, or handle error
  }
}

// Helper: store newValue in a rolling buffer, compute standard deviation, map to [0..1].
private func storeAndComputeStdDev(buffer: inout [Double], newValue: Double) -> Double {
  // Add the new reading:
  buffer.append(newValue)
  // Trim if over capacity:
  if buffer.count > MAX_BUFFER_SIZE {
    buffer.removeFirst(buffer.count - MAX_BUFFER_SIZE)
  }

  // If there aren't enough samples, assume "perfect" accuracy:
  if buffer.count < 2 {
    return 1.0
  }

  // Compute mean:
  let mean = buffer.reduce(0.0, +) / Double(buffer.count)
  // Compute variance:
  let variance =
    buffer.reduce(0.0) { partialResult, val in
      let diff = val - mean
      return partialResult + (diff * diff)
    } / Double(buffer.count)
  // Standard deviation:
  let stdDev = sqrt(variance)

  // Map standard deviation -> accuracy in [0..1].
  // Adjust MAX_STDDEV threshold to match your environment.
  let MAX_STDDEV = 1.0  // Example: stdev > 1.0 => 0 accuracy, stdev=0 => 1.0 accuracy
  let rawAccuracy = 1.0 - (stdDev / MAX_STDDEV)

  // Clamp to [0..1].
  return max(0.0, min(1.0, rawAccuracy))
}
