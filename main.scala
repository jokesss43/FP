import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

object App {

  def count(data: Seq[Double], pred: Double => Boolean, threadsNumber: Int): Int = {
    implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadsNumber))

    val chunkSize = Math.max(1, data.length / threadsNumber)
    val chunks = data.grouped(chunkSize).toList

    val countFutures = chunks.map { chunk =>
      Future {
        chunk.count(pred)
      }
    }

    val countsPerChunk = Await.result(Future.sequence(countFutures), 30.seconds)

    val totalCount = countsPerChunk.sum
    totalCount * (totalCount - 1) / 2
  }
}

object main {
  def main(args: Array[String]): Unit = {

    def isEven(x: Double): Boolean = x % 2 == 0

    val testData = Seq(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)

    println("Тестовые данные: " + testData.mkString(", "))
    println("Предикат: число четное")

    val evenNumbers = testData.filter(isEven)
    println(s"Четные числа в коллекции: ${evenNumbers.mkString(", ")}")
    println(s"Количество четных чисел: ${evenNumbers.size}")

    val n = evenNumbers.size
    val expectedPairs = n * (n - 1) / 2
    println(s"Ожидаемое количество пар (C($n,2) = $n*${n-1}/2): $expectedPairs")

    val result = App.count(testData, isEven, 4)
    println(s"Результат работы программы: $result")
    println(s"Тест пройден: ${result == expectedPairs}")
  }
}