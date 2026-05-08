class LuxuryWatch:
    
    # Initialize the class variable here
    watches_created = 0
    
    def __init__(self):
        LuxuryWatch.watches_created = LuxuryWatch.watches_created + 1
    
    @classmethod    
    def get_number_of_watches_created(cls):
        return cls.watches_created
        
    @classmethod
    def create_engraved_watch(cls, engraving):
        if LuxuryWatch.validate_engraving(engraving):
            watch = cls()
            watch.engraving = engraving
            return watch
        
    @staticmethod
    def validate_engraving(txt):
        # Restriction 1: Not longer than 40 characters
        if len(txt) > 40:
            raise ValueError("Engraving is too long (max 40 characters).")
        
        # Restriction 2: Must be alphanumerical (no spaces or special characters)
        if not txt.isalnum():
            raise ValueError("Engraving must be alphanumeric (no spaces allowed).")
            
        return True
    
watch = LuxuryWatch()
print(LuxuryWatch.get_number_of_watches_created())
watch2 = LuxuryWatch.create_engraved_watch("Misha")
print(LuxuryWatch.get_number_of_watches_created())