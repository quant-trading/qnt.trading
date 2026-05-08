class MobilePhone:
    def __init__(self, number):
        self.number = number
        
    def turn_on(self):
        print(f"mobile phone {self.number} is turned on")
    
    def turn_off(self):
        print(f"mobile phone is turned off")
        
    def call(self, number):
        print(f"calling {number}")


phone1 = MobilePhone("01632-960004")
phone2 = MobilePhone("01632-960012")

phone1.turn_on()
phone2.turn_on()

phone1.call("555-34343")

phone1.turn_off()
phone2.turn_off()