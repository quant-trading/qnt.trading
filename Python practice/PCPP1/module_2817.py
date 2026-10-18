class Tires:
    def init(self, size):
        self.size = size

    def get_pressure(self):
        pass

    def pump(self):
        pass
    
class Engine:
    def init(self, fuel_type):
        self.fuel_type = fuel_type

    def start(self):
        pass

    def stop(self):
        pass

    def get_state(self):
        pass
    
class Vehicle:
    def init(self, VIN, engine, tires):
        self.engine = engine
        self.tires = tires
        self.VIN = VIN

city_tires = Tires(15)
offroad_tires = Tires(18)

gas_engine = Engine("gas")
electric_engine = Engine("electricity")

city_car = Vehicle("33434", electric_engine, city_tires)
allterrain_car = Vehicle("dfsd", gas_engine, offroad_tires)