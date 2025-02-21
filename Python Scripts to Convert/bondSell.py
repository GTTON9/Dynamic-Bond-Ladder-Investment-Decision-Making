class bondSell:

    def __init__(self, unitsSold, bondObj):
        self.unitsSold = unitsSold
        self.bondObj = bondObj
        
    def getUnitsSold(self):
        return self.unitsSold
    
    def setUnitsSold(self,newUnits):
        self.unitsSold = newUnits

    def getBondObj(self):
        return self.bondObj
    
    def setBondObj(self,newObj):
        self.bondObj = newObj