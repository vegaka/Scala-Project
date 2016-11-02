import exceptions.IllegalAmountException
import exceptions.NoSufficientFundsException

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
	
	var balance: Double = initialBalance

	def withdraw(amount: Double): Unit = {
		if (amount > balance){
			throw new NoSufficientFundsException("Insufficient funds in account.")
		} else {
			balance -= amount
		}
	}

	def deposit(amount: Double): Unit = {
		if (amount < 0){
			throw new IllegalAmountException("Amount must be positive.")
		} else {
			balance += amount
		}
	}
	def getBalanceAmount: Double = balance
}
