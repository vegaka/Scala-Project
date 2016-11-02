import main.scala.exceptions.IllegalAmountException
import main.scala.exceptions.NoSufficientFundsExceptions

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
	
	var balance: Double = initialBalance

	def withdraw(amount: Double): Unit = {
		if (amount > balance){
			throw new NoSufficientFundsExceptions("Insufficient funds.")
		} else {
			balance -= amount
		}
	}

	def deposit(amount: Double): Unit = {
		if (amount < 0){
			throw new IllegalAmountException("Cannot deposit negative amount.")
		} else {
			balance += amount
		}
	}
	def getBalanceAmount: Double = balance
}
