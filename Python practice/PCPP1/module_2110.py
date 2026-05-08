class TimeInterval:
    
    def __init__(self, hours, minutes, seconds):
        self.hours = hours
        self.minutes = minutes
        self.seconds = seconds
        
    def __str__(self):
        return str(self.hours) + ":" + str(self.minutes) + ":" + str(self.seconds)
    
    def __print__(self):
        print(str(self))
    
    def to_seconds(self):
        return(self.hours * 60 * 60 + self.minutes * 60 + self.seconds)
    
    def __add__(self, other):
        # check argument
        if type(other) is not type(self) and type(other) is not int:
            raise(TypeError())
            
        if type(other) is int:
            _seconds = self.to_seconds() + other
            return(TimeInterval(_seconds // 3600, (_seconds % 3600) // 60, (_seconds % 3600) % 60))
            
        if type(other) is self(type):
            _seconds = self.to_seconds() + other.to_seconds()
            return(TimeInterval(_seconds // 3600, (_seconds % 3600) // 60, (_seconds % 3600) % 60))

    def __sub__(self, other):
        # check argument
        if type(other) is not type(self) and type(other) is not int:
            raise(TypeError())
            
        if type(other) is int:
            _seconds = self.to_seconds() - other
            return(TimeInterval(_seconds // 3600, (_seconds % 3600) // 60, (_seconds % 3600) % 60))
            
        if type(other) is self(type):
            _seconds = self.to_seconds() - other.to_seconds()
            return(TimeInterval(_seconds // 3600, (_seconds % 3600) // 60, (_seconds % 3600) % 60))


    def __mul__(self, other):
        # check argument
        if type(other) is not int:
            raise(TypeError())
            
        if type(other) is int:
            _seconds = self.to_seconds() * other
            return(TimeInterval(_seconds // 3600, (_seconds % 3600) // 60, (_seconds % 3600) % 60))



interval1 = TimeInterval(10, 10, 10)

print(interval1 + 100)
print(interval1 - 100)
print(interval1  * 2)
