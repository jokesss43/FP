import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import scala.util.Random
import scala.collection.immutable.Queue
import scala.collection.mutable

case class Product(name: String, secretCode: Int, price: Double, weight: Double)
case class Customer(id: Int)
case class Cashier(id: Int)
case class ShopConfig(maxQueueSize: Int, numCashiers: Int, numCustomers: Int, workTime: Int)

object SimpleShopSimulation {

  // Простая потокобезопасная очередь
  class ThreadSafeQueue[A] {
    private val queue = mutable.Queue[A]()

    def enqueue(item: A): Int = queue.synchronized {
      queue.enqueue(item)
      queue.size
    }

    def dequeue(): Option[A] = queue.synchronized {
      if (queue.nonEmpty) Some(queue.dequeue()) else None
    }

    def size: Int = queue.synchronized(queue.size)
  }

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val scanner = new java.util.Scanner(System.in)

    println("Введите максимальный размер очереди: ")
    val maxQueueSize = scanner.nextInt()

    println("Введите количество кассиров: ")
    val numCashiers = scanner.nextInt()

    println("Введите количество покупателей: ")
    val numCustomers = scanner.nextInt()

    println("Введите время работы магазина (сек): ")
    val workTime = scanner.nextInt()

    val config = ShopConfig(maxQueueSize, numCashiers, numCustomers, workTime)

    // Запускаем симуляцию
    runSimulation(config)

    scanner.close()
  }

  def runSimulation(config: ShopConfig): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val availableProducts = List(
      Product("Телевизор", 1, 5400.0, 3.2),
      Product("Хлеб", 2, 45.0, 0.5),
      Product("Смартфон", 3, 8900.0, 0.3),
      Product("Молоко", 4, 85.0, 1.0)
    )

    // Используем map для создания коллекций
    val cashiers = (1 to config.numCashiers).map(id => Cashier(id)).toList
    val customers = (1 to config.numCustomers).map(id => Customer(id)).toList

    val queue = new ThreadSafeQueue[Product]()
    val random = new Random()
    val startTime = System.currentTimeMillis()

    println("\nНачало работы магазина:\n")

    // Future для покупателей
    val customerFuture = Future {
      // Lazy List бесконечных действий покупателей
      val customerActions = LazyList.continually {
        val currentTime = System.currentTimeMillis()
        if (currentTime - startTime < config.workTime * 1000) {
          if (queue.size < config.maxQueueSize && random.nextDouble() > 0.3) {
            val customer = customers(random.nextInt(customers.length))
            val product = availableProducts(random.nextInt(availableProducts.length))
            val newSize = queue.enqueue(product)
            println(s"Покупатель-${customer.id} добавил товар: ${product.name} (очередь: $newSize)")
            Thread.sleep(200 + random.nextInt(300))
          } else {
            Thread.sleep(100)
          }
          true // продолжаем
        } else {
          false // останавливаемся
        }
      }

      // Обрабатываем Lazy List пока не закончится время
      customerActions.takeWhile(identity).foreach(_ => ())
    }
    
    // Future для кассиров с использованием map
    val cashierFutures = cashiers.map { cashier =>
      Future {
        // Lazy List бесконечных действий кассиров
        val cashierActions = LazyList.continually {
          val currentTime = System.currentTimeMillis()
          if (currentTime - startTime < config.workTime * 1000) {
            queue.dequeue() match {
              case Some(product) =>
                val currentSize = queue.size
                println(s"Кассир-${cashier.id} обработал товар: ${product.name} (осталось: $currentSize)")
                Thread.sleep(300 + random.nextInt(400))
                println(s"Кассир-${cashier.id} подсчитал товар: ${product.name} - цена: ${product.price}, вес: ${product.weight}")
              case None =>
                Thread.sleep(100)
            }
            true // продолжаем
          } else {
            false // останавливаемся
          }
        }

        // Обрабатываем Lazy List
        cashierActions.takeWhile(identity).foreach(_ => ())
      }
    }

    // Ждем завершения всех Future
    Thread.sleep((config.workTime * 1000) + 1000)

    // Завершающая часть с использованием map
    println("\nМагазин закрывается...")

    val customerMessages = (1 to config.numCustomers).map(id => s"Покупатель-$id ушел из магазина")
    val cashierMessages = (1 to config.numCashiers).map(id => s"Кассир-$id закончил работу")

    (customerMessages ++ cashierMessages).foreach(println)
    println("Работа магазина завершена")
  }
}