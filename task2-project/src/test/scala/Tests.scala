import org.scalatest.FunSuite
import exceptions._

class SingleAccountTransferTest extends FunSuite {

    test("Test 10: Invalid transfer between accounts due to insufficient funds should lead to transaction status FAILED and no money should be transferred between accounts") {
        val bank = new Bank()
        val acc1 = new Account(bank, 100)
        val acc2 = new Account(bank, 1000)

        acc1 transferTo(acc2, 150)

        while (bank.getProcessedTransactionsAsList.size != 1) {
            Thread.sleep(100)
        }

        assert(bank.getProcessedTransactionsAsList.last.status == TransactionStatus.FAILED)
        assert((acc1.getBalanceAmount == 100) && (acc2.getBalanceAmount == 1000))

    }

}
