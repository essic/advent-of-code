// The Swift Programming Language
// https://docs.swift.org/swift-book

import Algorithms
import Foundation

extension String {
  func cleanSplit(separator sep: String) -> [String] {
    return
      self
      .trimmingCharacters(in: .whitespacesAndNewlines)
      .split(separator: sep)
      .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
      .map { String($0) }
  }
}

private
  enum Color: String
{
  case red
  case green
  case blue
}

private
  struct GameSet
{

  let cubes: [Color: Int]
  private static let GameSetCubeEntriesSeparator = ","

  init(cubes c: [(Color, Int)]) {
    var tmp = [Color: Int]()
    let cubesByGroup =
      c.grouped { (Color, Int) in
        Color
      }
    for (key, value) in cubesByGroup {
      let total = value.map { $0.1 }.reduce(0) { $0 + $1 }
      tmp[key] = total
    }
    cubes = tmp
  }

  init(_ data: String) {
    let entries =
      data.cleanSplit(separator: GameSet.GameSetCubeEntriesSeparator)
      .map { rawCube in

        let num = Int(
          rawCube.filter {
            $0.isNumber
          })!

        let value = rawCube.filter {
          $0.isLetter
        }
        let color = Color(rawValue: value)!  //Better ?
        return (color, num)
      }

    self.init(cubes: entries)
  }

  private func isValid(given: GameBag) -> Bool? {
    var result = Optional.some(true)  // I know it's ugly...
    for (color, count) in cubes {
      result = given.cubes[color].map { $0 >= count && result! }
    }
    return result
  }

  func isPossible(given b: GameBag) -> Bool {
    return isValid(given: b) ?? false
  }

  func getPower() -> Int {
    return cubes.values.reduce(1) { $0 * $1 }
  }
}

private
  struct GameBag
{
  let cubes: [Color: Int]
}

private
  struct Game
{
  let id: Int
  let gameSets: [GameSet]
  private static let gameIdentifierSeparator = ":"
  private static let gameSetSeparator = ";"

  init(_ data: String) {
    var tmp = [GameSet]()
    let gameParts = data.cleanSplit(separator: Self.gameIdentifierSeparator)
    self.id = Int(gameParts.first!.filter { $0.isNumber })!
    let rawSets = gameParts[1]
    for rawSet in rawSets.cleanSplit(separator: Self.gameSetSeparator) {
      tmp.append(GameSet(rawSet))
    }
    gameSets = tmp
  }

  func isPossible(bag b: GameBag) -> Bool {
    gameSets.map {
      $0.isPossible(given: b)
    }.reduce(true) {
      $0 && $1
    }
  }

  func idealSetPower(colours c: Set<Color>) -> Int {
    var result = 1
    for color in c {
      let e = gameSets.map { $0.cubes[color] ?? 0 }.max() ?? 1
      result = result * e
    }
    return result
  }

}

func day2Part1(_ data: String) -> Int {
  var result = 0
  let bag = GameBag(cubes: [Color.red: 12, Color.green: 13, Color.blue: 14])
  data.enumerateLines { line, _ in
    let g = Game(line)
    if g.isPossible(bag: bag) {
      result += g.id
    }
  }
  return result
}

func day2Part2(_ data: String) -> Int {
  var result = 0
  let onColors: Set<Color> = [Color.red, Color.green, Color.blue]
  data.enumerateLines { line, _ in
    result += Game(line).idealSetPower(colours: onColors)
  }

  return result
}
