// The Swift Programming Language
// https://docs.swift.org/swift-book

import Algorithms
import Foundation

private
  func findAllOccurences(on str: String, of target: String) -> (
    occurence: String, positions: [Int]
  )
{
  var startSearchFrom = str.startIndex
  var occurencePositions = [Int]()
  while startSearchFrom < str.endIndex {
    guard let occurenceRange = str[startSearchFrom...].range(of: target) else {
      break
    }

    let indexOfOccurence = str.distance(from: str.startIndex, to: occurenceRange.lowerBound)
    occurencePositions.append(indexOfOccurence)
    startSearchFrom = occurenceRange.upperBound
  }
  return (target, occurencePositions)
}

func day1(_ data: String) -> Int {

  let fromWordToDigitStr = [
    "one": "1", "two": "2", "three": "3", "four": "4", "five": "5", "six": "6", "seven": "7",
    "eight": "8", "nine": "9",
  ]

  let fromDigitStrToDigit = Dictionary.init(
    uniqueKeysWithValues: fromWordToDigitStr.values.map { ($0, Int($0)!) }
  )

  let allDigits = chain(fromWordToDigitStr.keys, fromDigitStrToDigit.keys)

  func allDigitOccurencesSortedByAppearingPosition(of str: String) -> [String] {
    return allDigits.flatMap {
      let localOccurences = findAllOccurences(on: str, of: $0)
      return localOccurences.positions.map {
        return ($0, localOccurences.occurence)
      }
    }.sorted {
      $0.0 < $1.0
    }.map { $0.1 }
  }

  var total = 0
  data.enumerateLines { line, _ -> Void in
    let digits = allDigitOccurencesSortedByAppearingPosition(of: line).map {
      if let digitStr = fromWordToDigitStr[$0] {
        return fromDigitStrToDigit[digitStr]!
      } else {
        return fromDigitStrToDigit[$0]!
      }
    }
    total += 10 * digits.first! + (digits.count > 1 ? digits.last! : digits.first!)
  }
  return total
}
