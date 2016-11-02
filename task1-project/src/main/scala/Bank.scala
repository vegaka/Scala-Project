import exceptions._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

object Bank {

  private val idCounter = new AtomicInteger(0)

  def transaction(from: Account, to: Account, amount: Double): Unit = {
  	if (amount < 0){
  		throw new IllegalAmountException("Cannot remove negative amount")
  	}
  	try{
  		from.withdraw(amount)
  		to.deposit(amount)
  	}catch{
  		case e: NoSufficientFundsException => throw new NoSufficientFundsException("No sufficient funds.")
  		case e: IllegalAmountException => throw new IllegalAmountException("Cannot deposit negative amount.")
  	}
  }

  @tailrec def getUniqueId: Int = {
  	val current = idCounter.get
  	val updated = current +1
  	if (idCounter.compareAndSet(current, updated)) return updated
    getUniqueId
  }

}
