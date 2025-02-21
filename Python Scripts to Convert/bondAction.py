from datetime import datetime

class bondAction:

    def __init__(self,date):
        if not isinstance(date,datetime):
            raise Exception('Date is not of type datetime.')
        self.date = date
        self.actionList = []
    
    def getActionList(self):
        return self.actionList
    
    def setActionList(self,newList):
        self.actionList = newList
    
    def getDate(self):
        return self.date
    
    def setDate(self, newDate):
        self.date = newDate
    

    def insertAction(self,bondAction):
        self.actionList.append(bondAction)
    
    def removeAction(self, index = None):
        _ = self.actionList.pop(index)