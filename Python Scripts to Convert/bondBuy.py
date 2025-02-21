class bondBuy:

    def __init__(self, unitsBought, bondObj):
        self.unitsBought = unitsBought
        self.bondObj = bondObj
        
    def getUnitsBought(self):
        return self.unitsBought
    
    def setUnitsBought(self,newUnits):
        self.unitsBought = newUnits

    def getBondObj(self):
        return self.bondObj
    
    def setBondObj(self,newObj):
        self.bondObj = newObj