import pandas as pd
from bondSell import bondSell
from bondBuy import bondBuy
from bondStrip import bondStrip

class bondCounts:
    def __init__(self):
        self.bondDF = pd.DataFrame(columns = ['Date','bondType','amountMaintained','numberStripped'])
    
    def bondAction(self,doBondAction):

        bondActionKey = {'buy':1,'sell':-1,'strip':0}
        
        newRow = [doBondAction.getDate(),doBondAction.getBondOjb(),doBondAction.getUnitsBought()]
        currBondType = newRow[1]
        if self.bondDF.empty:
            self.bondDF = pd.concat([self.bondDF,pd.DataFrame([{'Date': newRow[0], 'bondType': currBondType, 
                                                    "amountMaintained": newRow[2] * bondActionKey[doBondAction.getName()], 
            "numberStripped":(newRow[2] if doBondAction.getName() == 'strip' else 0)}])])
        else:
            append = False
            for a in range(self.bondDF.shape[0]):
                currRow = self.bondDF.iloc[a,1]

                boolList = [currRow.couponRate == newRow.couponRate,
                currRow.issueDate == newRow.issueDate,
                currRow.maturityDate == newRow.maturityDate,
                currRow.period == newRow.period,
                currRow.countingConvention == newRow.countingConvention,
                currRow.callable == newRow.callable]

                if not (False in boolList):
                    if doBondAction.getName() == 'strip':
                        self.bondDF.loc[a,"numberStripped"] += newRow[2]
                    else:
                        self.bondDF.loc[a,"amountMaintained"] += newRow[2] * bondActionKey[doBondAction.getName()]
                    append = True
                    break
            
            if not append:
                self.bondDF = pd.concat([self.bondDF,pd.DataFrame([{'Date': newRow[0], 'bondType': currBondType, 
                "amountMaintained": newRow[2] * bondActionKey[doBondAction.getName()], 
                "numberStripped":(newRow[2] if doBondAction.getName() == 'strip' else 0)}])])
                        

