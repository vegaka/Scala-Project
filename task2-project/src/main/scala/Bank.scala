import java.util.{Timer, TimerTask}
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new AtomicInteger(0)
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = new ForkJoinPool()

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        transactionsQueue push new Transaction(
            transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    }

    @tailrec final def generateAccountId: Int = {
        val current = uid.get
        val updated = current + 1
        if (uid.compareAndSet(current, updated)) return updated
        generateAccountId
    }

    private def processTransactions: Unit = {

        while (!transactionsQueue.isEmpty) {
            val nextTransaction = transactionsQueue.pop

            executorContext.execute(nextTransaction)

            processedTransactions.push(nextTransaction)
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

    val timer = new Timer()
    val task = new TimerTask() {
        def run() = Bank.this.processTransactions
    }
    timer.schedule(task, 1000L, 1000L)

}
