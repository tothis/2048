import java.awt.event.KeyEvent.{VK_DOWN, VK_LEFT, VK_RIGHT, VK_UP}
import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{Color, Dimension, Font, Graphics}
import java.util.Random
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import javax.swing.{JFrame, JPanel, SwingUtilities}
import scala.collection.mutable.Seq as MutSeq

@main
def main(): Unit = {
  SwingUtilities.invokeLater(() =>
    new JFrame {
      setDefaultCloseOperation(EXIT_ON_CLOSE)
      setTitle("2048")
      add(Panel())
      setVisible(true)
      pack()
      setLocationRelativeTo(null)
    }
  )
}

class Panel extends JPanel {

  import Panel.*

  setPreferredSize(Dimension(Width, Height))
  setBackground(Color(0xfaf8ef))
  setFocusable(true)

  addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case VK_UP =>
          for (index <- 0 until GridNumber) {
            var currentOpt: Option[Int] = None
            var mergedIndex = 0

            tiles.map(_(index)).zipWithIndex.foreach { case (itOpt, i) =>
              (itOpt, currentOpt) match
                case (Some(it), Some(current)) =>
                  tiles(i)(index) = None
                  if (it == current) {
                    tiles(mergedIndex - 1)(index) = Some(current * 2)
                    currentOpt = None
                  } else {
                    tiles(mergedIndex)(index) = itOpt
                    mergedIndex += 1
                    currentOpt = itOpt
                  }
                case (Some(_), None) =>
                  tiles(i)(index) = None
                  tiles(mergedIndex)(index) = itOpt
                  mergedIndex += 1
                  currentOpt = itOpt
                case _ =>
            }
          }
        case VK_DOWN =>
          for (index <- 0 until GridNumber) {
            var currentOpt: Option[Int] = None
            var mergedIndex = GridNumber - 1

            tiles.map(_(index)).zipWithIndex.reverse.foreach {
              case (itOpt, i) =>
                (itOpt, currentOpt) match
                  case (Some(it), Some(current)) =>
                    tiles(i)(index) = None
                    if (it == current) {
                      tiles(mergedIndex + 1)(index) = Some(current * 2)
                      currentOpt = None
                    } else {
                      tiles(mergedIndex)(index) = itOpt
                      mergedIndex -= 1
                      currentOpt = itOpt
                    }
                  case (Some(_), None) =>
                    tiles(i)(index) = None
                    tiles(mergedIndex)(index) = itOpt
                    mergedIndex -= 1
                    currentOpt = itOpt
                  case _ =>
            }
          }
        case VK_LEFT =>
          for (index <- 0 until GridNumber) {
            var currentOpt: Option[Int] = None
            var mergedIndex = 0

            tiles(index).zipWithIndex.foreach { case (itOpt, i) =>
              (itOpt, currentOpt) match
                case (Some(it), Some(current)) =>
                  tiles(index)(i) = None
                  if (it == current) {
                    tiles(index)(mergedIndex - 1) = Some(current * 2)
                    currentOpt = None
                  } else {
                    tiles(index)(mergedIndex) = itOpt
                    mergedIndex += 1
                    currentOpt = itOpt
                  }
                case (Some(_), None) =>
                  tiles(index)(i) = None
                  tiles(index)(mergedIndex) = itOpt
                  mergedIndex += 1
                  currentOpt = itOpt
                case _ =>
            }
          }
        case VK_RIGHT =>
          for (index <- 0 until GridNumber) {
            var currentOpt: Option[Int] = None
            var mergedIndex = GridNumber - 1

            tiles(index).zipWithIndex.reverse.foreach { case (itOpt, i) =>
              (itOpt, currentOpt) match
                case (Some(it), Some(current)) =>
                  tiles(index)(i) = None
                  if (it == current) {
                    tiles(index)(mergedIndex + 1) = Some(current * 2)
                    currentOpt = None
                  } else {
                    tiles(index)(mergedIndex) = itOpt
                    mergedIndex -= 1
                    currentOpt = itOpt
                  }
                case (Some(_), None) =>
                  tiles(index)(i) = None
                  tiles(index)(mergedIndex) = itOpt
                  mergedIndex -= 1
                  currentOpt = itOpt
                case _ =>
            }
          }
        case it =>
          System.err.println(s"不支持 `${KeyEvent.getKeyText(it)}` 键")
          return
      }

      // 生成一个 Tile
      genTile()
      // 重绘 Panel
      repaint()
    }
  })

  override def paintComponent(it: Graphics): Unit = {
    super.paintComponent(it)

    it.setColor(gridColor)
    it.fillRoundRect(X, Y, Size, Size, Arc, Arc)

    for (rowIndex <- 0 until GridNumber; index <- 0 until GridNumber)
      tiles(rowIndex)(index) match
        case Some(value) =>
          // 使用换底公式计算以 2 为底 value 的对数，即 2 的几次方为 value
          val log = (math.log(value) / log2).toInt
          it.setColor(colorTable(math.min(log, colorTable.size) - 1))
          it.fillRoundRect(
            GridX + index * GridFullSize,
            GridY + rowIndex * GridFullSize,
            GridSize,
            GridSize,
            GridArc,
            GridArc
          )

          // 根据 value 的值范围设置不同的字体颜色和字体大小
          val (fontColor, fontSize) = if (value < 8) {
            (smallFontColor, 55)
          } else if (value < 128) {
            (bigFontColor, 45)
          } else {
            (bigFontColor, 35)
          }
          it.setColor(fontColor)
          it.setFont(Font(null, Font.BOLD, fontSize))

          val tile = value.toString
          val metrics = it.getFontMetrics
          it.drawString(
            tile,
            GridX + index * GridFullSize + {
              // 保持 tile 在 grid 水平居中
              (GridSize - metrics.stringWidth(tile)) / 2
            },
            GridY + rowIndex * GridFullSize + {
              // 保持 tile 在 grid 垂直居中
              (GridSize - metrics.getHeight) / 2 + metrics.getAscent
            }
          )
        case None =>
          it.setColor(emptyColor)
          it.fillRoundRect(
            GridX + index * GridFullSize,
            GridY + rowIndex * GridFullSize,
            GridSize,
            GridSize,
            GridArc,
            GridArc
          )
  }
}

object Panel {
  private final val Width = 800
  private final val Height = 600
  private final val Arc = 12
  private final val GridNumber = 4
  private final val GridArc = 6
  // 单个 grid 的大小
  private final val GridSize = 100

  private final val GridFullSize = GridSize + Arc
  private final val Size = GridFullSize * GridNumber + Arc
  private final val X = (Width - Size) / 2
  private final val Y = (Height - Size) / 2
  private final val GridX = X + Arc
  private final val GridY = Y + Arc

  private val log2: Double = math.log(2)
  private val colorTable: Seq[Color] = {
    given Conversion[Int, Color] = Color(_)
    import scala.language.implicitConversions
    Seq(
      // 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048
      0xeee4da, 0xeee1c9, 0xf3b27a, 0xf69664, 0xf77c5f, 0xf75f3b, 0xedd073,
      0xedcc62, 0xedc950, 0xedc53f, 0xedc22e
    )
  }
  private val gridColor: Color = Color(0xbbada0)
  private val emptyColor: Color = Color(238, 228, 218, Math.round(.35f * 255f))
  private val smallFontColor: Color = Color(0x776e65)
  private val bigFontColor: Color = Color(0xf9f6f2)
  private val random: Random = Random()
  private val tiles: Seq[MutSeq[Option[Int]]] =
    Seq.fill(GridNumber)(
      MutSeq.fill[Option[Int]](GridNumber)(None)
    )

  // 生成两个 Tile
  for _ <- 0 until 2 do genTile()

  private def genTile(): Unit = {
    val noneTiles = tiles.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.filter(_._1.isEmpty).map(it => (i, it._2))
    }
    if (noneTiles.isEmpty)
      println("Game over!")
      return

    val (xIndex, yIndex) =
      if (noneTiles.size == 1)
        noneTiles.head
      else
        noneTiles(random.nextInt(noneTiles.size - 1))

    tiles(xIndex)(yIndex) = Some(2)
  }
}
