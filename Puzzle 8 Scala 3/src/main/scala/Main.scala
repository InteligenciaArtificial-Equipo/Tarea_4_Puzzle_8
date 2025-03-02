package com.ludus.puzzle

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.layout.{GridPane, VBox, HBox}
import scalafx.geometry.Insets
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.util.Duration
import scalafx.scene.input.{KeyCode, KeyEvent}
import scala.annotation.tailrec

object Main extends JFXApp3 {

  var currentState = Puzzle(List(
    List(1, 2, 3),
    List(4, 5, 6),
    List(7, 0, 8)
  ))
  val goalState = Puzzle(List(
    List(1, 2, 3),
    List(4, 5, 6),
    List(7, 8, 0)
  ))

  override def start(): Unit = {

    val gridPane = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(10)
    }

    def updateGrid(state: Puzzle.PuzzleState): Unit = {
      gridPane.children.clear()
      for (i <- 0 until state.size; j <- 0 until state.size) {
        val tile = state.board(i)(j)
        val btn = new Button(if (tile == 0) "" else tile.toString) {
          prefWidth = 100
          prefHeight = 100
          focusTraversable = false
        }
        gridPane.add(btn, j, i)
      }
      currentState = state
    }

    def animateSolution(path: List[Puzzle.PuzzleState]): Unit = {
      var index = 0
      val timeline = new Timeline {
        cycleCount = path.length
        keyFrames = Seq(KeyFrame(Duration(1000), onFinished = _ => {
          updateGrid(path(index))
          index += 1
        }))
      }
      timeline.play()
    }

    updateGrid(currentState)

    val movesLabel = new Label("--")
    val timeLabel = new Label("--")

    val stepsBox = new HBox {
      spacing = 5
      padding = Insets(10)
    }

    val stepsScrollPane = new ScrollPane {
      focusTraversable = false
      content = stepsBox
      hbarPolicy = ScrollBarPolicy.AsNeeded
      vbarPolicy = ScrollBarPolicy.Never
      prefViewportHeight = 50
    }

    val solveButton = new Button("Solve") {
      focusTraversable = false
      onAction = handle {
        val startTime = System.currentTimeMillis()
        stepsBox.children.clear()
        Puzzle.aStar(currentState, goalState) match {
          case Some(solutionNode) =>
            val path = Puzzle.reconstructPath(solutionNode)
            val elapsedTime = System.currentTimeMillis() - startTime
            movesLabel.text = s"Solved in ${path.length - 1} moves"
            timeLabel.text = s"Time: $elapsedTime ms"
            path.zipWithIndex.foreach { case (state, index) =>
              val stepButton = new Button(s"$index") {
                focusTraversable = false
                onAction = handle {
                  updateGrid(state)
                }
              }
              stepsBox.children.add(stepButton)
            }
            animateSolution(path)
          case None =>
            movesLabel.text = s"Solution not found"
            val elapsedTime = System.currentTimeMillis() - startTime
            timeLabel.text = s"Time: $elapsedTime ms"
        }
      }
      gridPane.requestFocus()
    }

    def move(x: Int, y: Int): Unit = {
      currentState = currentState.move(x, y)
      updateGrid(currentState)
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Puzzle 8"
      scene = new Scene(350, 500) {
        root = new VBox {
          spacing = 10
          padding = Insets(10)
          children = Seq(
            gridPane,
            solveButton,
            movesLabel,
            timeLabel,
            stepsScrollPane
          )
        }
        onKeyPressed = (ke: KeyEvent) => {
          val (emptyRow, emptyCol) = currentState.findEmpty
          ke.code match {
            case KeyCode.Up =>
              move(-1, 0)
            case KeyCode.Down =>
              move(1, 0)
            case KeyCode.Left =>
              move(0, -1)
            case KeyCode.Right =>
              move(0, 1)
            case _ =>
          }
        }
        requestFocus()
      }
    }

  }

}
