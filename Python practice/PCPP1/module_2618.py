from abc import ABC, abstractmethod

class Scanner(ABC):
    
    @abstractmethod
    def scan_document(self):
        pass
    
    @abstractmethod
    def get_scanner_status(self):
        pass

class Printer(ABC):

    @abstractmethod
    def print_document(self):
        pass
    
    @abstractmethod
    def get_printer_status(self):
        pass


class MFD1(Scanner, Printer):
    
    def __init__(self, sn, res, dpi):
        self.sn = sn
        self.max_dpi = dpi
        self.max_res = res
    
    def scan_document():
        return "Scanned Text"
    
    def get_scanner_status(self):
        return print(self.max_dpi, self.sn)

    def print_document(self):
        return "Printed Text"
    
    def get_printer_status(self):
        return print(self.max_res, self.sn)


mfd1 = MFD1("0000-0001", 200, 300)
mfd1.get_printer_status()