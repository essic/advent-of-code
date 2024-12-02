import Algorithms
import Foundation

private enum AppError: Error {
  case errorParser(msg: String)
  case invalidIntervalEndLesserThanOrigin
  case invalidIntervalSizeCannotBeLessOrEqualThanZero
}

private struct Interval {
  let origin: Int
  let size: Int
  var end: Int {
    origin + size - 1
  }

  init(origin o: Int, end e: Int) throws {
    if e <= o {
      throw AppError.invalidIntervalEndLesserThanOrigin
    }
    self.origin = o
    self.size = (e - o + 1)
  }

  init(origin o: Int, size s: Int) throws {
    if s <= 0 {
      throw AppError.invalidIntervalSizeCannotBeLessOrEqualThanZero
    }
    self.origin = o
    self.size = s
  }

  func includes(number: Int) -> Bool {
    return number >= origin && number <= end
  }

  func intersect(other: Interval) -> Interval? {
    if self.includes(number: other.origin) && self.includes(number: other.end) {
      return .some(other)
    } else if other.includes(number: self.origin) && other.includes(number: self.end) {
      return .some(self)
    }
    if !self.includes(number: other.origin) && !self.includes(number: other.end)
      && !other.includes(number: self.origin) && !other.includes(number: self.end)
    {
      return .none
    } else {
      if self.includes(number: other.origin) {
        return try! Interval(origin: other.origin, size: self.end - other.origin + 1)
      } else {
        return try! Interval(origin: self.origin, size: other.end - self.origin + 1)
      }
    }
  }

  static func difference(left a: Interval, right b: Interval) throws -> Interval? {
    let result = a.intersect(other: b)
    switch result {
    case .none:
      return .some(a)
    case .some(let intersection):
      if a.size == intersection.size {
        return .none
      } else {
        let newSize = a.size - intersection.size
        let newOrigin = a.origin < intersection.origin ? a.origin : intersection.end + 1
        return try! Interval(origin: newOrigin, size: newSize)
      }
    }
  }
}

// Small helper function
extension Interval {
  func show(name n: String = "interval") {
    print(n, terminator: ": ")
    for x in self.origin...self.end {
      print(x, terminator: " ")
    }
    print()
  }
}

private func getSuccessOrThrow<Value>(_ r: Result<Value, AppError>) throws -> Value {
  switch r {
  case .success(let value):
    return value
  case .failure(let err):
    throw err
  }
}

private func parse<Value>(
  _ content: String, keyWord k: String, untilTheEnd e: Bool = false,
  transform fn: ((String) -> Value)
)
  -> Result<Value, AppError> where Value: Collection
{
  let start = k
  let end = "\n\n"
  var map: Value
  if !e {
    guard let startRange = content.firstRange(of: start),
      let endRange = content[startRange.upperBound...].firstRange(of: end)
    else {
      return Result.failure(AppError.errorParser(msg: "Could not find '\(k)' marker"))
    }
    map =
      fn(
        content[startRange.upperBound..<endRange.lowerBound]
          .trimmingCharacters(in: .whitespaces))
  } else {
    guard let startRange = content.firstRange(of: start) else {
      return Result.failure(AppError.errorParser(msg: "Could not find '\(k)' marker"))
    }
    map =
      fn(
        content[startRange.upperBound...]
          .trimmingCharacters(in: .whitespaces)
      )
  }
  if map.isEmpty {
    return Result.failure(AppError.errorParser(msg: "'\(k)' elements not found !"))
  }
  return Result.success(map)
}

private func parseSeeds(_ content: String) throws -> [Int] {
  try getSuccessOrThrow(
    parse(content, keyWord: "seeds:") {
      $0.cleanSplit(separator: " ").map { Int($0)! }
    })
}

private func parse2(_ content: String, keyWord k: String, untilTheEnd e: Bool = false) -> Result<
  [IntervalMapper], AppError
> {
  parse(content, keyWord: k, untilTheEnd: e) {
    $0.cleanSplit(separator: "\n")
      .map {
        $0.trimmingCharacters(in: .whitespaces)
          .cleanSplit(separator: " ").map { Int($0)! }
      }.map(IntervalMapper.init)
  }
}

private func parseSeedToSoilMap(_ content: String) throws -> [IntervalMapper] {
  return
    try getSuccessOrThrow(parse2(content, keyWord: "seed-to-soil map:\n"))
}

private func parseSoilToFertilizerMap(_ content: String) throws -> [IntervalMapper] {
  try getSuccessOrThrow(parse2(content, keyWord: "soil-to-fertilizer map:\n"))
}

private func parseFertilizerToWaterMap(_ content: String) throws -> [IntervalMapper] {
  try getSuccessOrThrow(parse2(content, keyWord: "fertilizer-to-water map:\n"))
}

private func parseWaterToLightMap(_ content: String) throws -> [IntervalMapper] {
  try getSuccessOrThrow(parse2(content, keyWord: "water-to-light map:\n"))
}

private func parseLightToTemperatureMap(_ content: String) throws -> [IntervalMapper] {
  try getSuccessOrThrow(parse2(content, keyWord: "light-to-temperature map:\n"))
}

private func parseTemperatureToHumidityMap(_ content: String) throws -> [IntervalMapper] {
  try getSuccessOrThrow(parse2(content, keyWord: "temperature-to-humidity map:\n"))
}

private func parseHumidityToLocationMap(_ content: String) throws -> [IntervalMapper] {
  return try getSuccessOrThrow(
    parse2(content, keyWord: "humidity-to-location map:\n", untilTheEnd: true))
}

private struct IntervalMapper {
  let from: Interval
  let to: Interval

  init(data: [Int]) {
    let source = data[1]
    let destination = data[0]
    let size = data[2]

    from = try! Interval(origin: source, size: size)
    to = try! Interval(origin: destination, size: size)
  }

  func mapNumber(input n: Int) -> Int? {
    if !from.includes(number: n) {
      return .none
    }

    let offset = n - from.origin
    return .some(to.origin + offset)
  }

  func mapInterval(inputs intervals: [Interval]) -> (mapped: [Interval], unmapped: [Interval]) {
    var results = [Interval]()
    var notMapped = [Interval]()
    for input in intervals {
      if let intersection = from.intersect(other: input) {
        let offset = intersection.origin - from.origin
        let newInterval = try! Interval(origin: to.origin + offset, size: intersection.size)
        results.append(newInterval)
        let iDiffFrom = try! Interval.difference(left: input, right: from)
        iDiffFrom.map { notMapped.append($0) }
      } else {
        notMapped.append(input)
      }
    }
    return (mapped: results, unmapped: notMapped)
  }
}

extension [IntervalMapper] {
  fileprivate func computePart1(_ n: Int) -> Int {
    for im in self {
      guard let r = im.mapNumber(input: n) else {
        continue
      }
      return r
    }
    return n
  }
  fileprivate func computePart2(_ ns: [Interval]) -> [Interval] {
    var current = ns
    var success = [Interval]()
    for im in self {
      let (mapped, unmapped) = im.mapInterval(inputs: current)
      success.append(contentsOf: mapped)
      current = unmapped
    }
    success.append(contentsOf: current)
    return success
  }
}

precedencegroup CompositionPrecedence {
  associativity: left
}

infix operator |> : CompositionPrecedence

func |> <ValueSource, ValueDestination>(
  value: ValueSource, fn: @escaping ((ValueSource) -> ValueDestination)
) -> ValueDestination {
  return fn(value)
}

private struct Almanac {
  let seeds: [Int]
  let seedToSoilMap: [IntervalMapper]
  let soilToFertilizerMap: [IntervalMapper]
  let fertilizerToWaterMap: [IntervalMapper]
  let waterToLightMap: [IntervalMapper]
  let lightToTemperatureMap: [IntervalMapper]
  let temperatureToHumidityMap: [IntervalMapper]
  let humidityToLocationMap: [IntervalMapper]

  func computePart1() -> Int? {
    seeds
      .map {
        $0
          |> seedToSoilMap.computePart1
          |> soilToFertilizerMap.computePart1
          |> fertilizerToWaterMap.computePart1
          |> waterToLightMap.computePart1
          |> lightToTemperatureMap.computePart1
          |> temperatureToHumidityMap.computePart1
          |> humidityToLocationMap.computePart1
      }.min()
  }

  func computePart2() -> Int? {
    let seedIntervals =
      seeds.chunks(ofCount: 2)
      .map(Array.init)
      .map {
        try! Interval(origin: $0[0], size: $0[1])
      }
    let r =
      seedIntervals
      |> seedToSoilMap.computePart2
      |> soilToFertilizerMap.computePart2
      |> fertilizerToWaterMap.computePart2
      |> waterToLightMap.computePart2
      |> lightToTemperatureMap.computePart2
      |> temperatureToHumidityMap.computePart2
      |> humidityToLocationMap.computePart2

    return r.map { $0.origin }.min()
  }
}

private func parseAlmanac(_ data: String) throws -> Almanac {
  Almanac(
    seeds: try parseSeeds(data),
    seedToSoilMap: try parseSeedToSoilMap(data),
    soilToFertilizerMap: try parseSoilToFertilizerMap(data),
    fertilizerToWaterMap: try parseFertilizerToWaterMap(data),
    waterToLightMap: try parseWaterToLightMap(data),
    lightToTemperatureMap: try parseLightToTemperatureMap(data),
    temperatureToHumidityMap: try parseTemperatureToHumidityMap(data),
    humidityToLocationMap: try parseHumidityToLocationMap(data))
}

func day5Part1(_ data: String) -> Int {
  do {
    let almanac2 = try parseAlmanac(data)
    let result = almanac2.computePart1()
    return result!
  } catch {
    print("Error: \(error)")
    exit(1)
  }
}

func day5Part2(_ data: String) -> Int {
  do {
    let almanac2 = try parseAlmanac(data)
    let result = almanac2.computePart2()
    return result!
  } catch {
    print("Error: \(error)")
    exit(1)
  }
}
