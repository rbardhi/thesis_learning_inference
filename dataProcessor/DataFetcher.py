'''
Created on May 25, 2018

@author: nitesh
'''
import MySQLdb
from settings.DatabaseSettings import ATTRIBUTES_DOMAIN_SETTING

def fetchDataFromDB(dbName):
    db = MySQLdb.connect('localhost','root','root',dbName)
    cursor = db.cursor()
    showTables = "show tables"
    tableResults = dict()
    database = dict()
    try:
        cursor.execute(showTables)
        tables = cursor.fetchall()
        for row in tables:
            describe = "desc %s" % (row[0])
            cursor = db.cursor()
            cursor.execute(describe)
            description = cursor.fetchall()
            dict1 = dict()
            map1 = dict()
            list1 = dict()
            colLen = 0
            primaryKeys = []
            for r in description:
                dict1[r[0]] = set()
                list1[r[0]] = []
                map1[colLen] = r[0]
                if r[3] == 'PRI':
                    primaryKeys.append(r[0])
                colLen += 1
            list1['primaryKeys'] = primaryKeys
            sql = "select * from %s" % (row[0])
            cursor = db.cursor()
            cursor.execute(sql)
            results = cursor.fetchall()
            for r in results:
                for i in range(0, colLen):
                    dict1[map1[i]].add(r[i])
                    list1[map1[i]].append(r[i])
            tableResults[row[0]] = dict1
            database[row[0]] = list1
    except:
        print "Error: unable to fetch data"
    db.close()
    [dbStats, database] = userDefinedDBProcessing(tableResults, database)
    return [dbStats, database]

def userDefinedDBProcessing(dbStats, database):
    for tableName in ATTRIBUTES_DOMAIN_SETTING.keys():
        for columnName in ATTRIBUTES_DOMAIN_SETTING[tableName].keys():
            for i in range(0, len(database[tableName][columnName])):
                database[tableName][columnName][i] = ATTRIBUTES_DOMAIN_SETTING[tableName][columnName][str(database[tableName][columnName][i])]
            tempSet = set()
            for ele in dbStats[tableName][columnName]:
                tempSet.add(ATTRIBUTES_DOMAIN_SETTING[tableName][columnName][str(ele)])
            dbStats[tableName][columnName] = tempSet
    return [dbStats, database]

if __name__ == '__main__':
    [dbStats, dataBase] = fetchDataFromDB('Hepatitis_std')
    print dbStats['Classification']['GeneID']
    print dataBase['Genes']['Essential']
    print dataBase['registration']['primaryKeys']
    
    
    