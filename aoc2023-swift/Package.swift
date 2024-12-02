// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
  name: "aoc2023-swift",
  platforms: [
    .macOS(SupportedPlatform.MacOSVersion.v14)
  ],
  dependencies: [
    .package(url: "https://github.com/apple/swift-algorithms.git", branch: "main")
    // .package(url: "https://github.com/swiftlang/swift-testing.git", branch: "main"),
  ],

  targets: [
    // Targets are the basic building blocks of a package, defining a module or a test suite.
    // Targets can depend on other targets in this package and products from dependencies.
    .executableTarget(
      name: "aoc2023",
      dependencies: [
        .product(name: "Algorithms", package: "swift-algorithms")
      ],
      path: "Sources"),
    .testTarget(
      name: "aoc2023-test",
      dependencies: [
        "aoc2023"
        // .product(name: "Testing", package: "swift-testing")
      ]),
  ]
)
