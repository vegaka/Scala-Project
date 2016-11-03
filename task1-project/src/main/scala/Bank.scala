import exceptions._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

object Bank {

  private val idCounter = new AtomicInteger(0)

  def transaction(from: Account, to: Account, amount: Double): Unit = {
      from.withdraw(amount)
      to.deposit(amount)
  }

  @tailrec def getUniqueId: Int = {
  	val current = idCounter.get
  	val updated = current + 1
  	if (idCounter.compareAndSet(current, updated)) return updated
    getUniqueId
  }

}
