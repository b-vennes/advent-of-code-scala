# Advent Of Code -- Scala

This repository contains a set of solutions to various Advent Of Code problems
written in Scala.

## Compile

`./mill aoc.compile`

## Run

`./mill aoc.run ${year} ${day} ${part}`

Example: `./mill aoc.run 2023 1 a`

## Warp

Warp is a small library I built to handle multi-threaded programming.

It's an alternative to Cats-Effect, ZIO, or basic Scala Futures,
while also being easy to compile to Scala Native.

The general idea is that a `Warp` represents travelling from one location to
another across an unknown amount of time.  A `Warp` "warps" from a starting
value to an ending value.
