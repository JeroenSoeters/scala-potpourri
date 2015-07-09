import java.util.concurrent.{Callable, Executors}

import scala.collection.mutable.ArrayBuffer

/** This class stores a dense two-dimensional matrix of finite size. */
class Matrix(private val repr : Array[Array[Double]]) {
  /** Access the row at idx (0-based).  Returns a column-ordered Seq of the values in the row. */
  def row(idx : Int) : Seq[Double] = {
    repr(idx)
  }
  /** Access the column at idx (0-based).  Returns a row-ordered Seq of the values in the column. */
  def col(idx : Int) : Seq[Double] = {
    repr.foldLeft(ArrayBuffer[Double]()) {
      (buffer, currentRow) =>
        buffer.append(currentRow(idx))
        buffer
    } toArray
  }
  /** The number of rows in the matrix. */
  lazy val rowRank = repr.size
  /** The number of columns in the matrix. */
  lazy val colRank = if(rowRank > 0) repr(0).size else 0
  /** Pretty-prints the matrix */
  override def toString = "Matrix" + repr.foldLeft("") { (msg, row) => msg + row.mkString("\r\n|", " | ", "|")}
}

trait ThreadStrategy {
  def execute[A](func: Function0[A]) : Function0[A]
}

object MatrixUtils {
  def multiply(a: Matrix,
               b: Matrix)
              (implicit threading: ThreadStrategy): Matrix = {
    assert(a.colRank == b.rowRank)
    val buffer = new Array[Array[Double]](a.rowRank)
    for (i <- 0 until a.rowRank)
      buffer(i) = new Array[Double](b.colRank)

    def computeValue(row: Int, col: Int) : Unit = {
      val pairwiseElements =
        a.row(row).zip(b.col(col))
      val products =
        for((x, y) <- pairwiseElements)
        yield x * y
      val result = products.sum
      buffer(row)(col) = result
    }

    val computations = for {
      i <- 0 until a.rowRank
      j <- 0 until b.colRank
    } yield threading.execute { () => computeValue(i, j) }

    computations.foreach(_())
    new Matrix(buffer)
  }
}

object SameThreadStrategy extends ThreadStrategy {
  override def execute[A](func: () => A): () => A = func
}

object ThreadPoolStrategy extends ThreadStrategy {
  var pool = Executors.newFixedThreadPool(
    java.lang.Runtime.getRuntime.availableProcessors)

  def execute[A](func: () => A) : () => A = {
    val future = pool.submit(new Callable[A] {
      def call(): A = {
        println("Executing function on thread: " +
          Thread.currentThread.getName)
        func()
      }
    })
    () => future.get
  }
}

implicit val ts = ThreadPoolStrategy

val x = new Matrix(Array(Array(1, 2, 3), Array(4, 5, 6)))
val y = new Matrix(Array(Array(1), Array(1), Array(1)))

MatrixUtils.multiply(x, y)

