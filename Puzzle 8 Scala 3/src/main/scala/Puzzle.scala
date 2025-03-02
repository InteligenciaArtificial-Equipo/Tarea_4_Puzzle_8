package com.ludus.puzzle

import scala.annotation.tailrec

object Puzzle {

  case class PuzzleState(board: List[List[Int]]) {

    val size: Int = board.size

    def findEmpty: (Int, Int) = {
        board.zipWithIndex.flatMap { case (row, i) =>
        row.zipWithIndex.collect { case (v, j) if v == 0 => (i, j) }
        }.head
    }

    val swap: (Int, Int) => (Int, Int) => PuzzleState = (r1, c1) => (r2, c2) => {
        val updatedBoard = board
        .updated(r1, board(r1).updated(c1, board(r2)(c2)))
        val swapResult = updatedBoard
        .updated(r2, updatedBoard(r2).updated(c2, board(r1)(c1)))
        PuzzleState(swapResult)
    }

    def isInto(x: Int): Boolean = (x >= 0 && x < size)

    def neighbors: List[PuzzleState] = {

        val (emptyRow, emptyColumn) = findEmpty

        val swapWithEmpty = swap(emptyRow, emptyColumn)

        List(
        (emptyRow + 1, emptyColumn),
        (emptyRow - 1, emptyColumn),
        (emptyRow, emptyColumn + 1),
        (emptyRow, emptyColumn - 1),
        ).filter((row, column) => isInto(row) && isInto(column))
        .map { case (row, column) => swapWithEmpty(row, column) }
    }

    def manhattan(goal: PuzzleState): Int = {
        val goalPositions: Map[Int, (Int, Int)] =
        goal.board.zipWithIndex.flatMap { case (row, i) =>
            row.zipWithIndex.map { case (value, j) => (value, (i, j)) }
        }.toMap
        board.zipWithIndex.flatMap { case (row, i) =>
        row.zipWithIndex.collect {
            case (v, j) if v != 0 =>
            val (goalRow, goalCol) = goalPositions(v)
            math.abs(i - goalRow) + math.abs(j - goalCol)
        }
        }.sum
    }

    def move(x: Int, y: Int): PuzzleState = {
        val (emptyRow, emptyColumn) = findEmpty
        val newRow = emptyRow + x
        val newColumn = emptyColumn + y
        if (isInto(newRow) && isInto(newColumn)) then
          swap(emptyRow, emptyColumn)(newRow, newColumn)
        else
          PuzzleState(board)
    }

    override def toString: String = board.map(_.mkString(" ")).mkString("\n")

  }

  def apply(board: List[List[Int]]): PuzzleState = {
    PuzzleState(board)
  }

  case class Node(state: PuzzleState, g: Int, h: Int, parent: Option[Node]) {
    def f: Int = g + h
  }

  def aStar(initial: PuzzleState, goal: PuzzleState): Option[Node] = {
    @tailrec
    def search(open: List[Node], closed: Set[PuzzleState]): Option[Node] = {
        if (open.isEmpty) None
        else {
          val current = open.minBy(_.f)
        if (current.state == goal) Some(current)
        else {
          val newOpen = open.filterNot(_ == current)
          val neighbors = current.state.neighbors.map { ns =>
          Node(ns, current.g + 1, ns.manhattan(goal), Some(current))
          }
          val filteredNeighbors = neighbors.filterNot(n => closed.contains(n.state))
          search(newOpen ++ filteredNeighbors, closed + current.state)
        }
      }
    }
    val startNode = Node(initial, 0, initial.manhattan(goal), None)
    search(List(startNode), Set.empty)
  }

  def reconstructPath(node: Node): List[PuzzleState] = {
    @tailrec
    def helper(n: Node, acc: List[PuzzleState]): List[PuzzleState] = n.parent match {
      case Some(p) => helper(p, n.state :: acc)
      case None    => n.state :: acc
    }
    helper(node, Nil)
  }
    
}
