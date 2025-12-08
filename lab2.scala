import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

object App {

  type PairPredicate = (Double, Double) => Boolean

  def countPairs(data: Seq[Double], pred: PairPredicate, threadsNumber: Int): Int = {
    implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadsNumber))

    val n = data.length

    val allPairs = for {
      i <- 0 until n
      j <- i + 1 until n
    } yield (i, j)

    val chunkSize = Math.max(1, allPairs.length / threadsNumber)
    val chunkIndices = allPairs.grouped(chunkSize).toList

    val countFutures = chunkIndices.map { chunk =>
      Future {
        chunk.count { case (i, j) =>
          pred(data(i), data(j))
        }
      }
    }

    val countsPerChunk = Await.result(Future.sequence(countFutures), 30.seconds)
    countsPerChunk.sum
  }
}

object main {
  def main(args: Array[String]): Unit = {
    def bothEven(x: Double, y: Double): Boolean = (x % 2 == 0) && (y % 2 == 0)

    def sumEven(x: Double, y: Double): Boolean = (x + y) % 2 == 0

    val testData = Seq(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)

    println("Тестовые данные: " + testData.mkString(", "))
    println("Предикат: оба числа четные")
    println()

    val evenNumbers = testData.filter(_ % 2 == 0)
    val n = evenNumbers.size
    val expectedPairs = n * (n - 1) / 2

    println(s"Четные числа в коллекции: ${evenNumbers.mkString(", ")}")
    println(s"Количество четных чисел: $n")
    println(s"Ожидаемое количество пар четных чисел (C($n,2) = $n*${n-1}/2): $expectedPairs")
    println()

    val result = App.countPairs(testData, bothEven, 4)
    println(s"Результат работы программы: $result")
    println(s"Тест пройден: ${result == expectedPairs}")
    println()

    println("Тест с предикатом: сумма чисел четная")

    val manualCount = (for {
      i <- testData.indices
      j <- i + 1 until testData.length
      if (testData(i) + testData(j)) % 2 == 0
    } yield 1).sum

    println(s"Ожидаемое количество пар с четной суммой (ручной подсчет): $manualCount")

    val result2 = App.countPairs(testData, sumEven, 4)
    println(s"Результат работы программы: $result2")
    println(s"Тест пройден: ${result2 == manualCount}")
  }
}