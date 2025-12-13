import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.collection.immutable.LazyList
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue
import scala.io.StdIn

object ShopSimulation {
  import ExecutionContext.Implicits.global

  case class Product(name: String)

  class ShopQueue(maxSize: Int) {
    private val queue = new ConcurrentLinkedQueue[Product]()
    private val currentSize = new AtomicInteger(0)

    def addProduct(product: Product): Boolean = {
      val canAdd = currentSize.get() < maxSize
      if (canAdd) {
        queue.offer(product)
        currentSize.incrementAndGet()
      }
      canAdd
    }

    def takeProduct(): Option[Product] = {
      Option(queue.poll()).map { product =>
        currentSize.decrementAndGet()
        product
      }
    }

    def currentQueueSize: Int = currentSize.get()
  }

  def showMenu(): (FiniteDuration, Int, Int, Int) = {
    println("\nНастройка параметров симуляции магазина")
    println("----------------------------------------")

    def readInt(prompt: String, default: Int, min: Int = 1, max: Int = 100): Int = {
      var value = default
      var valid = false

      while (!valid) {
        print(s"$prompt [$default]: ")
        StdIn.readLine().trim match {
          case "" =>
            value = default
            valid = true
          case input =>
            scala.util.Try(input.toInt).toOption match {
              case Some(num) if num >= min && num <= max =>
                value = num
                valid = true
              case Some(_) =>
                println(s"Значение должно быть от $min до $max")
              case None =>
                println("Введите целое число")
            }
        }
      }
      value
    }

    val duration = readInt("Время работы магазина (секунды)", 10, max=300).seconds
    val cashiers = readInt("Количество кассиров", 2, max=20)
    val customers = readInt("Количество покупателей", 3, max=50)
    val queueSize = readInt("Максимальный размер очереди", 5)

    println("\nПараметры установлены:")
    println(s"Время работы: $duration секунд")
    println(s"Кассиров: $cashiers")
    println(s"Покупателей: $customers")
    println(s"Размер очереди: $queueSize")

    (duration, cashiers, customers, queueSize)
  }

  def simulate(duration: FiniteDuration, cashierCount: Int, customerCount: Int, maxQueueSize: Int): Future[Unit] = {
    val shopQueue = new ShopQueue(maxQueueSize)
    val random = new Random()
    val simulationEndTime = System.currentTimeMillis() + duration.toMillis
    val stopSignal = Promise[Unit]()

    val stats = Map(
      "added" -> new AtomicInteger(0),
      "processed" -> new AtomicInteger(0),
      "failed" -> new AtomicInteger(0)
    )

    val customerTasks = (1 to customerCount).to(LazyList).map { customerId =>
      Future {
        while (System.currentTimeMillis() < simulationEndTime && !stopSignal.isCompleted) {
          val product = Product(s"Товар-$customerId-${random.nextInt(1000)}")

          shopQueue.addProduct(product) match {
            case true =>
              stats("added").incrementAndGet()
              println(s"Покупатель $customerId добавил товар")
            case false =>
              stats("failed").incrementAndGet()
              println(s"Покупатель $customerId: очередь полная")
          }

          Thread.sleep(random.nextInt(500) + 100)
        }
      }
    }

    val cashierTasks = (1 to cashierCount).to(LazyList).map { cashierId =>
      Future {
        while (System.currentTimeMillis() < simulationEndTime && !stopSignal.isCompleted) {
          shopQueue.takeProduct() match {
            case Some(product) =>
              stats("processed").incrementAndGet()
              println(s"Кассир $cashierId обработал товар")
              Thread.sleep(random.nextInt(800) + 200)
            case None =>
              Thread.sleep(100)
          }
        }
      }
    }

    val allTasks = customerTasks ++ cashierTasks

    Future {
      Thread.sleep(duration.toMillis)
      stopSignal.success(())
      println(s"\nВремя работы магазина истекло ($duration)")
    }

    Future.sequence(allTasks.take(cashierCount + customerCount)).map { _ =>
      println("\nСимуляция завершена")
      println(s"Товаров добавлено: ${stats("added").get()}")
      println(s"Товаров обработано: ${stats("processed").get()}")
      println(s"Отказов в добавлении: ${stats("failed").get()}")
      println(s"Товаров осталось в очереди: ${shopQueue.currentQueueSize}")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Симулятор магазина")

    val negativeAnswers = Set("нет", "н", "n", "no")

    LazyList.continually {
      println("\nГлавное меню:")
      println("1. Настроить и запустить симуляцию")
      println("2. Выход")
      print("Выберите действие: ")

      StdIn.readLine().trim
    }.map {
      case "1" =>
        val (duration, cashiers, customers, queueSize) = showMenu()

        println("\nЗапуск симуляции...")
        val simulation = simulate(duration, cashiers, customers, queueSize)

        try {
          Await.result(simulation, duration + 5.seconds)
        } catch {
          case e: Exception =>
            println(s"Ошибка: ${e.getMessage}")
        }

        print("\nЗапустить еще одну симуляцию? (да/нет): ")
        val answer = StdIn.readLine().trim.toLowerCase()
        !negativeAnswers.contains(answer)

      case "2" =>
        false

      case _ =>
        println("Пожалуйста, выберите 1 или 2")
        true
    }.find(!_).getOrElse(())

    println("\nПрограмма завершена")
  }
}