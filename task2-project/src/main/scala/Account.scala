import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = {
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

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }
}
