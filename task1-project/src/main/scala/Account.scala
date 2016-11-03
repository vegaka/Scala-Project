import exceptions.IllegalAmountException
import exceptions.NoSufficientFundsException

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
	
	var balance: Double = initialBalance

	def withdraw(amount: Double): Unit = {
        if (amount < 0) {
            throw new IllegalAmountException("Can't withdraw negative amounts.")
        } else if (amount > balance){
			throw new NoSufficientFundsException("Insufficient funds in account.")
		} else {
            this.synchronized({
                balance -= amount
            })
		}
	}

	def deposit(amount: Double): Unit = {
		if (amount < 0){
			throw new IllegalAmountException("Amount must be positive.")
		} else {
            this.synchronized({
                balance += amount
            })
        }
	}

	def getBalanceAmount: Double = balance
}
