class AccountException(Exception):
    pass

class BankAccount:
    def __init__(self, number, balance):
        self.__number = number
        self.__balance = balance
   
    @property
    def number(self):
        """Getter for account number."""
        return self.__number
        
    @number.setter
    def number(self, value):
        """Setter for account number: Raises exception if change is attempted."""
        raise AccountException("Account number cannot be changed.")
   
    @property
    def balance(self):
        """Getter for balance."""
        return self.__balance
    
    @balance.setter
    def balance(self, value):
        """
        Setter for balance: Prevents negative values and handles audit messages.
        Note: The syntax is @balance.setter, not @balance.setter(self, value).
        """
        if value < 0:
            raise AccountException("Balance cannot be negative.")
        
        # Logic for auditing large transactions
        if abs(value - self.__balance) > 100000:
            print("AUDIT: Large transaction detected.")
            
        self.__balance = value

    @balance.deleter
    def balance(self):
        """Deleter: Prevents deletion if balance is not zero."""
        if self.__balance != 0:
            raise AccountException("Cannot delete account with a non-zero balance.")
        del self.__balance

# --- Testing the Class Behavior ---
account = BankAccount("0001", 100)

# 1. Setting the balance to 1000
account.balance = 1000
print(f"Current balance: {account.balance}")

# 2. Trying to set the balance to -200
try:
    account.balance = -200
except AccountException as e:
    print(f"Caught expected error: {e}")

# 3. Trying to set a new value for the account number
try:
    account.number = "0002"
except AccountException as e:
    print(f"Caught expected error: {e}")

# 4. Trying to deposit 1,000,000 (triggers audit)
account.balance += 1000000

# 5. Trying to delete attribute with non-zero balance
try:
    del account.balance
except AccountException as e:
    print(f"Caught expected error: {e}")