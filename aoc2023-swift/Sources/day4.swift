private struct Card {
  let id: Int
  let winningNumbers: [Int]
  let gamerNumbers: [Int]

  func getNbOfMatches() -> Int {
    return winningNumbers.filter { gamerNumbers.contains($0) }.count
  }

  func computeWorth() -> Int {
    let a = Set(winningNumbers)
    let b = Set(gamerNumbers)
    return a.intersection(b).reduce(0) { worth, _ in
      return worth == 0 ? 1 : worth * 2
    }
  }
}

private func getCard(_ rawScratchCard: String) -> Card {
  let gameParts = rawScratchCard.cleanSplit(separator: ":")
  let cardId = Int(String(gameParts[0].cleanSplit(separator: " ").last!))!
  let toto = gameParts[1].cleanSplit(separator: " | ")
  let wNumbers = toto[0].cleanSplit(separator: " ").map {
    Int($0.trimmingCharacters(in: .whitespacesAndNewlines))!
  }
  let gNumbers = toto[1].cleanSplit(separator: " ").map {
    Int($0.trimmingCharacters(in: .whitespacesAndNewlines))!
  }
  return Card(id: cardId, winningNumbers: wNumbers, gamerNumbers: gNumbers)
}

func day4Part1(_ content: String) -> Int {
  let cards = content.components(separatedBy: "\n").filter {
    $0.trimmingCharacters(in: .whitespacesAndNewlines).count > 0
  }.map { getCard($0) }
  return cards.reduce(0) { $0 + $1.computeWorth() }
}

func day4Part2(_ content: String) -> Int {
  let cards = content.components(separatedBy: "\n").filter {
    $0.trimmingCharacters(in: .whitespacesAndNewlines).count > 0
  }.map { getCard($0) }

  var copies = [Int: Int](uniqueKeysWithValues: cards.map { ($0.id, 0) })
  for (index, card) in cards.enumerated() {
    let nb = card.getNbOfMatches()
    let nextIndex = index + 1
    let toIndex = nextIndex + nb
    let remainder = cards[nextIndex..<toIndex]
    for c in (remainder.map { $0.id }) {
      copies[c] = copies[c]! + copies[card.id]! + 1
    }
  }
  return copies.values.reduce(0) { $0 + $1 } + cards.count
}
