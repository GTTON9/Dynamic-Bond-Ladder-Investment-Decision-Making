class bondType:

    def __init__(self, couponRate, issueDate, maturityDate, period, countingConvention, callable = False):
        self.couponRate = couponRate
        self.issueDate = issueDate
        self.maturityDate = maturityDate
        self.period = period
        self.countingConvention = countingConvention
        self.callable = callable
    
    def getCouponRate(self):
        return self.couponRate
    
    def setCouponRate(self,newRate):
        self.couponRate = newRate
    
    def getIssueDate(self):
        return self.issueDate
    
    def setIssueDate(self,newDate):
        self.issueDate = newDate
    
    def getMaturityDate(self):
        return self.maturityDate
    
    def setMaturityDate(self,newDate):
        self.maturityDate = newDate

    def getCountingConvention(self):
        return self.countingConvention
    
    def setCountingConvention(self,newConvention):
        self.countingConvention = newConvention