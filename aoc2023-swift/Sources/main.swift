// The Swift Programming Language
// https://docs.swift.org/swift-book
import Foundation

var again = CommandLine.arguments[1...].first.map { $0 == "-r" } ?? false
let days: [Int: (((String) -> Int), ((String) -> Int))] =
  [
    2: (day2Part1, day2Part2),
    3: (day3Part1, day3Part2),
    4: (day4Part1, day4Part2),
    5: (day5Part1, day5Part2),
  ]

@MainActor
func aocRun(for d: Int) {
  let inputFile = "data/day\(d)-input.txt"
  do {
    let data = try String(contentsOfFile: inputFile, encoding: .utf8)
    if d == 1 {
      print("Day \(d) -> \(day1(data))")
    } else {
      guard let (part1, part2) = days[d] else {
        print("Not yet implemented !")
        return
      }
      print("Day \(d) -> Part 1: \(part1(data)) | Part 2: \(part2(data))")
    }
  } catch {
    print("Error while loading  \(inputFile). Does it exists ?")
    return
  }
}

repeat {
  print("Which day ? (or 'q' to quit) : ", terminator: "")
  let input = readLine()
  guard let day = Int(input!), day <= 25 && day >= 1 else {
    if input == "q" {
      again = false
    } else {
      print("Input \(input ?? "N/A") is invalid !")
    }
    continue
  }
  aocRun(for: day)

} while again
