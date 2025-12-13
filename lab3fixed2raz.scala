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

  def readPositiveInt(prompt: String, default: Int, max: Int, stream: LazyList[String]): (Int, LazyList[String]) = {
    print(prompt)

    def processInput(inputStream: LazyList[String]): (Int, LazyList[String]) = {
      val trimmed = inputStream.head.trim
      val tailStream = inputStream.tail

      if (trimmed.isEmpty) {
        (default, tailStream)
      } else {
        try {
          val value = trimmed.toInt
          if (value > 0 && value <= max) {
            (value, tailStream)
          } else {
            println(s"Ошибка: значение должно быть от 1 до $max")
            processInput(tailStream)
          }
        } catch {
          case _: NumberFormatException =>
            println("Ошибка: введите целое число")
            processInput(tailStream)
        }
      }
    }

    processInput(stream)
  }

  def showMenu(inputStream: LazyList[String]): ((FiniteDuration, Int, Int, Int), LazyList[String]) = {
    println("\nНастройка параметров симуляции магазина")
    println("----------------------------------------")

    val (duration, stream1) = readPositiveInt("Время работы магазина (секунды) [10]: ", 10, 300, inputStream)
    val (cashiers, stream2) = readPositiveInt("Количество кассиров [2]: ", 2, 20, stream1)
    val (customers, stream3) = readPositiveInt("Количество покупателей [3]: ", 3, 50, stream2)
    val (queueSize, stream4) = readPositiveInt("Максимальный размер очереди [5]: ", 5, 100, stream3)

    ((duration.seconds, cashiers, customers, queueSize), stream4)
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

  def readYesNo(prompt: String, stream: LazyList[String]): (Boolean, LazyList[String]) = {
    print(prompt)

    def processInput(inputStream: LazyList[String]): (Boolean, LazyList[String]) = {
      val answer = inputStream.head.trim.toLowerCase
      val tailStream = inputStream.tail

      val yesAnswers = Set("да", "д", "yes", "y")
      val noAnswers = Set("нет", "н", "no", "n")

      if (yesAnswers.contains(answer)) {
        (true, tailStream)
      } else if (noAnswers.contains(answer)) {
        (false, tailStream)
      } else {
        println("Ошибка: введите 'да' или 'нет'")
        processInput(tailStream)
      }
    }

    processInput(stream)
  }

  def createMainMenu(): Map[String, String] = {
    Map(
      "1" -> "Настроить и запустить симуляцию",
      "2" -> "Выход"
    )
  }

  def printMenu(menu: Map[String, String]): Unit = {
    println("\nГлавное меню:")
    menu.toList.sortBy(_._1).foreach { case (key, description) =>
      println(s"$key. $description")
    }
  }

  def readMenuChoice(menu: Map[String, String], stream: LazyList[String]): (String, LazyList[String]) = {
    print("Выберите действие: ")

    def processInput(inputStream: LazyList[String]): (String, LazyList[String]) = {
      val choice = inputStream.head.trim
      val tailStream = inputStream.tail

      menu.get(choice) match {
        case Some(_) =>
          (choice, tailStream)
        case None =>
          println(s"Ошибка: выберите один из вариантов: ${menu.keys.mkString(", ")}")
          print("Выберите действие: ")
          processInput(tailStream)
      }
    }

    processInput(stream)
  }

  def main(args: Array[String]): Unit = {
    val mainInputStream: LazyList[String] = LazyList.continually(StdIn.readLine())
    val mainMenu = createMainMenu()

    def mainLoop(stream: LazyList[String]): Unit = {
      printMenu(mainMenu)
      val (choice, streamAfterChoice) = readMenuChoice(mainMenu, stream)

      choice match {
        case "1" =>
          val ((duration, cashiers, customers, queueSize), streamAfterMenu) =
            showMenu(streamAfterChoice)

          println("\nЗапуск симуляции...")
          val simulation = simulate(duration, cashiers, customers, queueSize)

          try {
            Await.result(simulation, duration + 5.seconds)
          } catch {
            case e: Exception =>
              println(s"Ошибка: ${e.getMessage}")
          }

          val (continue, streamAfterYesNo) = readYesNo("\nЗапустить еще одну симуляцию? (да/нет): ", streamAfterMenu)

          if (continue) {
            mainLoop(streamAfterYesNo)
          }

        case "2" =>

        case _ =>
          mainLoop(streamAfterChoice)
      }
    }

    mainLoop(mainInputStream)
  }
}