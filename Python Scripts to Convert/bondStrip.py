class bondStrip:

    def __init__(self,unitsStripped, bondObj):
        self.unitsStripped = unitsStripped
        self.bondObj = bondObj
    
    def getUnitsStripped(self):
        return self.unitsStripped
    
    def setUnitsSold(self,newUnits):
        self.unitsStripped = newUnits

    def getBondObj(self):
        return self.bondObj
    
    def setBondObj(self,newObj):
        self.bondObj = newObj