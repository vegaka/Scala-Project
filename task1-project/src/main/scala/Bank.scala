import exceptions.NoSufficientFundsException

object Bank {

  private var idCounter: AtomInt = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = {
      try{
          from.withdraw(amount)
          to.deposit(amount)
      }catch{
  		  case e: NoSufficientFundsException => NoSufficientFundsException("No sufficient fund, transaction aborted")
  	  }
  }

  def getUniqueId: AtomInt = {
      idCounter += 1 // Can this be improved?
      idCounter
  }

}
