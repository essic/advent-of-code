private
  enum Direction
{
  case up
  case down
  case left
  case right
}

private
  struct SchematicCell
{
  let row: Int
  let column: Int
}

private
  struct Query
{
  let from: SchematicCell
  let towards: Set<Direction>
}

private
  func findAllOccurences2(on str: String) -> [Int]
{
  let dot = Character(".")
  var startSearchFrom = str.startIndex
  var occurencePositions = [Int]()
  while startSearchFrom < str.endIndex {
    if !str[startSearchFrom].isNumber && str[startSearchFrom] != dot {
      let indexOfOccurence = str.distance(from: str.startIndex, to: startSearchFrom)
      occurencePositions.append(indexOfOccurence)
    }
    startSearchFrom = str.index(after: startSearchFrom)
  }
  return occurencePositions
}

private
  func getSearchDirection(
    on str: String, atRow r: Int, lastRow lr: Int
  )
  -> [Query]
{

  let occurences = findAllOccurences2(on: str)
  guard !occurences.isEmpty else {
    return []
  }

  var results = [Query]()
  for column in occurences {
    var directionsToLookup = Set<Direction>()

    if r > 0 {
      directionsToLookup.insert(.up)
    }

    if r < lr {
      directionsToLookup.insert(.down)
    }

    if column < str.count - 1 {
      directionsToLookup.insert(.left)
    }

    if column >= 0 {
      directionsToLookup.insert(.right)
    }

    let cell = SchematicCell(row: r, column: column)
    let query = Query(from: cell, towards: directionsToLookup)
    results.append(query)
  }
  return results
}

extension Character {
  fileprivate static func toInt(_ src: [Character]...) -> Int? {
    return Int(String(src.flatMap { $0 }))
  }
}

private
  func lookupOnTheSides(_ line: String, from q: Query) -> [Int]
{
  var digitsOnTheLeft = [Character]()
  var digitsOnTheRight = [Character]()
  let startIndex = line.index(line.startIndex, offsetBy: (q.from.column))
  let isCharacterAtColumnNumber = line[startIndex].isNumber

  if q.towards.contains(.left) {
    var startingSearch = true

    let rangeOfSearch = line[...startIndex].reversed()
    for char in rangeOfSearch {
      if !char.isNumber && startingSearch {
        startingSearch = false
        continue
      }
      if char.isNumber {
        digitsOnTheLeft.append(char)
        startingSearch = false
      } else {
        break
      }
    }
    digitsOnTheLeft = digitsOnTheLeft.reversed()
  }

  if q.towards.contains(.right) {
    var startingSearch = true
    let rangeOfSearch = line[startIndex...]
    for char in rangeOfSearch {
      if !char.isNumber && startingSearch {
        startingSearch = false
        continue
      }
      if char.isNumber {
        digitsOnTheRight.append(char)
        startingSearch = false
      } else {
        break
      }
    }
  }
  if isCharacterAtColumnNumber {
    if digitsOnTheRight.count >= 1 {
      digitsOnTheRight = Array(digitsOnTheRight.dropFirst())
    }
    guard let number = Character.toInt(digitsOnTheLeft, digitsOnTheRight) else {
      return []
    }
    return [number]
  }
  let numbers = Array(
    [digitsOnTheLeft, digitsOnTheRight].map { Character.toInt($0) }.compacted())
  return numbers
}

private
  func lookup(_ lines: [String], query q: Query) -> [Int]
{
  var results = lookupOnTheSides(lines[q.from.row], from: q)

  if q.towards.contains(.up) {
    let upResults = lookupOnTheSides(lines[q.from.row - 1], from: q)
    results.append(contentsOf: upResults)
  }
  if q.towards.contains(.down) {
    let downResults = lookupOnTheSides(lines[q.from.row + 1], from: q)
    results.append(contentsOf: downResults)
  }
  return results
}

private
  func lookupPart2(_ lines: [String], query q: Query) -> [Int]
{
  let results = lookup(lines, query: q)
  guard results.count == 2 else {
    return []
  }
  return [results.reduce(1) { $0 * $1 }]
}

func day3Part1(_ data: String) -> Int {

  let lines = data.components(separatedBy: "\n")
  let lastRow = lines.count - 1
  var result = 0

  for (rowIndex, content) in lines.enumerated() {
    let queries = getSearchDirection(
      on: content, atRow: rowIndex, lastRow: lastRow)
    for query in queries {
      result = lookup(lines, query: query).reduce(result) { $0 + $1 }
    }
  }

  return result
}

func day3Part2(_ data: String) -> Int {
  let lines = data.components(separatedBy: "\n")
  let lastRow = lines.count - 1
  var result = 0

  for (rowIndex, content) in lines.enumerated() {
    let queries = getSearchDirection(
      on: content, atRow: rowIndex, lastRow: lastRow)
    for query in queries {
      result = lookupPart2(lines, query: query).reduce(result) { $0 + $1 }
    }
  }

  return result
}
