import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        transactions.values.toList
    }

    def allTransactionsCompleted: Boolean = {
        for (x: Transaction <- transactions.values){
            if (! x.isCompleted) {
                return false
            }
        }
        true
    }

    def withdraw(amount: Double): Unit =  {
        if (amount < 0) {
            throw new IllegalAmountException("Can't withdraw negative amounts.")
        } else if (amount > balance.amount){
            throw new NoSufficientFundsException("Insufficient funds in account.")
        } else {
            this.synchronized({
                balance.amount -= amount
            })
        }
    }

    def deposit(amount: Double): Unit = {
        if (amount < 0){
            throw new IllegalAmountException("Amount must be positive.")
        } else {
            this.synchronized({
                balance.amount += amount
            })
        }
    }

    def getBalanceAmount: Double = balance.amount

    def sendTransactionToBank(t: Transaction): Unit = {
        BankManager.findBank(bankId) ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
        if (!transactions.contains(t.id)) {
            transactions += (t.id -> t)
            return true
        }
        false
    }

    override def receive = {
        case IdentifyActor => sender ! this

        case TransactionRequestReceipt(to, transactionId, transaction) => {
            // Process receipt
            transactions(transactionId).status = transaction.status
        }

        case BalanceRequest => sender ! getBalanceAmount

        case t: Transaction => {
            try {
                deposit(t.amount)
                t.status = TransactionStatus.SUCCESS
            } catch {
                case _: IllegalAmountException => t.status = TransactionStatus.FAILED
            }
            BankManager.findBank(bankId) ! new TransactionRequestReceipt(t.from, t.id, t)
        }

        case msg => println("WTF")
    }


}