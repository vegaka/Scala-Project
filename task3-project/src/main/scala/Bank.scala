import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)

    def createAccount(initialBalance: Double): ActorRef = {
        BankManager.createAccount(generateAccountId.toString, bankId, initialBalance)
    }

    @tailrec final def generateAccountId: Int = {
        val current = accountCounter.get
        val updated = current + 1
        if (accountCounter.compareAndSet(current, updated)) return updated
        generateAccountId
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        // Use BankManager to look up an account with ID accountId
        try {
            val account = BankManager.findAccount(bankId, accountId)
            Option(account)
        } catch {
            case _:NoSuchElementException => None : Option[ActorRef]
        }
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        // Use BankManager to look up a different bank with ID bankId
        try {
            val bank = BankManager.findBank(bankId)
            Option(bank)
        } catch {
            case _:NoSuchElementException => None : Option[ActorRef]
        }

    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
        case GetAccountRequest(id) => sender ! findAccount(id).get // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => processTransactionReceipt(t)

        case msg => println("WTF")
    }

    def processTransactionReceipt(t: TransactionRequestReceipt): Unit = {
        val toBankId = t.toAccountNumber.substring(0, 4)

        if (toBankId == bankId) {
            findAccount(t.toAccountNumber.substring(4)).get ! t
        } else {
            BankManager.findBank(toBankId) ! t
        }
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        val isInternal = t.to.length <= 4
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = if (isInternal) t.to else t.to.substring(4)
        val transactionStatus = t.status

        // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
        // HINT: Make use of the variables that have been defined above.

        if (toBankId == bankId) {
            val otherAccount = findAccount(toAccountId)
            if (!otherAccount.isEmpty) {
                findAccount(toAccountId).get ! t
            } else {
                t.status = TransactionStatus.FAILED
                BankManager.findBank(bankId) ! new TransactionRequestReceipt(t.from, t.id, t)
            }
        } else {
            val otherBank = findOtherBank(toBankId)
            if (!otherBank.isEmpty) {
                otherBank.get ! t
            } else {
                t.status = TransactionStatus.FAILED
                BankManager.findBank(bankId) ! new TransactionRequestReceipt(t.from, t.id, t)
            }
        }

        /*
        var msg = t
        if (transactionStatus == TransactionStatus.FAILED) {
            if (isInternal) {
                findAccount(toAccountId).get ! TransactionRequestReceipt(toAccountId, t.id, t)
            } else {
                findOtherBank(toBankId).get ! TransactionRequestReceipt(toAccountId, t.id, t)
            }
        } else {
            if (isInternal) {
                findAccount(toAccountId).get ! t
            } else {
                findOtherBank(toBankId).get ! t
            }
        }*/
    }
}