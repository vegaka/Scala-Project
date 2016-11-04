import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import exceptions._

import scala.collection.mutable._
import scala.collection.JavaConversions._

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    val transactions: BlockingQueue[Transaction] = new LinkedBlockingQueue()

    // Remove and return the first element from the queue
    def pop: Transaction = transactions.take()

    // Return whether the queue is empty
    def isEmpty: Boolean = transactions.size() == 0

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = transactions.put(t)

    // Return the first element from the queue without removing it
    def peek: Transaction = transactions.peek()

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = transactions.iterator()
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

    var status: TransactionStatus.Value = TransactionStatus.PENDING

    override def run: Unit = {

        if (amount < 0) {
            status = TransactionStatus.FAILED
            throw new IllegalAmountException("Amount must be positive.")
        }

        var attempts = 0

        while (attempts < allowedAttemps) {
            try {
                processTransaction
                status = TransactionStatus.SUCCESS
                attempts = allowedAttemps
            } catch {
                case e: NoSufficientFundsException => {
                    attempts += 1
                    Thread.sleep(100L)
                }
            }
        }

        if (status != TransactionStatus.SUCCESS) {
            status = TransactionStatus.FAILED
        }
    }

    def processTransaction: Unit = {
        def doTransaction() = {
            from withdraw amount
            to deposit amount
        }

        if (from.uid < to.uid) from synchronized {
            to synchronized {
                doTransaction
            }
        } else to synchronized {
            from synchronized {
                doTransaction
            }
        }
    }
}
