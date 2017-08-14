import random
import statistics
import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt
import linecache
import time
import itertools
from scipy.optimize import linear_sum_assignment
import re
import os
        
def randomResources(resources, kValue):
    newResources = resources[:]
    selectedResources = []
    for i in range(kValue):
        newItem = (newResources[random.randint(0, (len(newResources) - 1))])
        selectedResources.append(newItem)
        newResources.remove(newItem)
    return selectedResources

def resourceAllocation(a, r, kValue,  initialAllocations, seed):
    resources = [i for i in range(r)]
    myFile = open('new.txt', 'w+')
    myFile.write('Agents: ' + str(a) + ' Resources: ' + str(r) + ' K Value: ' + str(kValue) + '\n') #JM made data representation the same as Selena's
    random.seed(seed)
    agentResources = [randomResources(resources, kValue) for j in range(a)]
    if initialAllocations == 0:
        for resource in agentResources:
            myFile.write(str(resource)[1: -1] + ' ' + '\n')
        myFile.close()
    else:
        randomitems = random.sample([i for i in range(r)], initialAllocations)
        for resource in agentResources:
            if len(randomitems) == 0:
                myFile.write(str(resource)[1: -1] + ' ' + '\n')
            else:
                myFile.write(str(resource)[1: -1] + ', r' + str(randomitems.pop(0)) + ' \n')
        myFile.close()
    

def resourceChecker(r, kValue):
    #creating data
    myFile = open('new.txt', 'r')
    next(myFile) #JM skips first line of file, which contains a, r, K information
    values = []
    indices = []
    for i in range(r):
        values.append(0)
        indices.append(i)
        
    for line in myFile:
        for j in range(r):
            if ('Resource ' + str(j) + ' ') in line:
                values[j] += 1
                
   #stats
    mean = statistics.mean(values)
    standard_deviation = statistics.pstdev(values)
    
   #graphing
    fig = plt.figure()
    y_pos = np.arange(len(values))
    plt.bar(y_pos, values, align = 'center', alpha = 0.5)
    plt.ylabel('Number of Occurances')
    plt.xlabel('Resources')
    plt.title('Distribution of object Occurances at k =' + str(kValue))
    plt.text(50, 20, ('Mean: ' + str(mean)))
    plt.text(50, 10, ('SD: ' + str(standard_deviation)))
    #JM saving output of graph as svg file.  
    fig.savefig('data.svg')
    plt.show() 

def RECA(preferenceFile): #JM implementing RECA
    myFile = open(preferenceFile, 'r')
    next(myFile) #skips the first line of file
    assignedObjects = []
    unassignedAgents = []
    eqClasses = {}
    currentAgent = 0
    
    #making dictionary of prefrences
    for line in myFile:
        if line.rstrip(' \n') in eqClasses:
            eqClasses[line.rstrip(' \n')].append(currentAgent)
        else:
            eqClasses[line.rstrip(' \n')] = [currentAgent]  
        currentAgent += 1
    
    #making list of total resources   
    resourceNumbers = [i for i in range(currentAgent)]
    assignments = open('Assignments.txt', 'w+')    
    for key in eqClasses:
        if len(eqClasses[key]) == 1:
            assignments.write(str(key) + ': ' + str(eqClasses[key][0]) + '\n')
            #del eqClasses[key]
            assignedObjects.append(int(key.strip('Resource ')))
        else:
            assignedAgent = random.choice(eqClasses[key])
            assignments.write(str(key) + ': ' + str(assignedAgent) + '\n')
            eqClasses[key].remove(assignedAgent) 
            assignedObjects.append(int(key.strip('Resource ')))
            for agent in eqClasses[key]:
                unassignedAgents.append(agent)
    #JM Making assignedObjects a set increases preformance considerably (O(n) --> O(1) time)
    assignedObjectsSet = set(assignedObjects)
    unassignedObjects = [x for x in resourceNumbers if x not in assignedObjectsSet]
    random.shuffle(unassignedObjects)
    random.shuffle(unassignedAgents)
    remainingAssignments = zip(unassignedObjects, unassignedAgents)
    for pair in remainingAssignments:
        assignments.write('Resource ' + str(pair[0]) + ': ' + str(pair[1]) + '\n')

def RICA(preferenceFile):
    myFile = open(preferenceFile, 'r')
    next(myFile) #skips the first line of file
    assignments = open('Assignments.txt', 'w+')    
    assignedObjects = []
    unassignedAgents = []
    eqClasses = {}
    currentAgent = 0

    #Dictionary of preferences
    for line in myFile:
        x = line.replace('Resource ', '').replace(' \n' , '')
        items = x.split(',')
        for item in items:
            if item == '' or item == '\n':
                continue
            if item in eqClasses:
                eqClasses[item].append(currentAgent)
            else:
                eqClasses[item] = [currentAgent]
        unassignedAgents.append(str(currentAgent))
        currentAgent += 1
    #making list of resources
    resourceNumbers = [i for i in range(currentAgent)]
    
    while len(eqClasses) != 0:
        #find smallest lists
        #JM this is painfully slow but works, need to speed up
        #minLength = min([len(n) for n in eqClasses.values()])
        minLength = min(map(len, eqClasses.values()))
        #keys = [k for k in eqClasses.keys() if len(eqClasses[k]) == minLength]
        #choose one randomly
        #randObject = random.choice(keys)
        for key in eqClasses:
            if len(eqClasses[key]) == minLength:
                randObject = key
                break

       #choose item from list randomly
        randAgent = random.choice(eqClasses[randObject])
        assignments.write('Resource ' + str(randObject) + ': ' + str(randAgent) + '\n')
        #remove that entry, and that item from all other lists
        eqClasses.pop(randObject)
        assignedObjects.append(randObject)
        unassignedAgents.remove(str(randAgent))
        for key in eqClasses:
            if randAgent in eqClasses[key]: eqClasses[key].remove(randAgent)
        eqClasses = {k: v for k, v in eqClasses.items() if v}
        
    #match remaining agents with remaining resources
    assignedObjectsSet = set(assignedObjects)
    unassignedObjects = [x for x in resourceNumbers if x not in assignedObjectsSet]
    random.shuffle(unassignedObjects)
    random.shuffle(unassignedAgents)
    remainingAssignments = zip(unassignedObjects, unassignedAgents)
    for pair in remainingAssignments:
        assignments.write('Resource ' + str(pair[0]) + ': ' + str(pair[1]) + '\n')


def MIR(preferenceFile):
    #arbitrarily assign An to Bn as e(n)
    myFile = open(preferenceFile, 'r')
    line = myFile.readline().split()
    #need abs(k).  if k positive, --> A > B.  Neagive --> A < B.
    n = int(line[1])
    m = int(line[3])
    k = (n - m)
    #k neg --> m > n
    #k pos --> n > m
    assignments = open('Assignments.txt', 'w+')
    #defining M:
         
    M = [[0 for x in range(max(n,m))] for y in range(max(n,m))]

    for a in range(n):
        aLine = myFile.readline().replace('Resource ', '').replace(' \n' , '')
        items = aLine.split(', ')
        print(items)
        x = items[-1]
        y = x.replace('r' , '')
        for item in items:
            M[a][int(item)] = 1
        #Finding Matching on M:
    G = np.array(M)
    row_ind, col_ind = linear_sum_assignment(G)
    weight = len(M) - G[row_ind, col_ind].sum()


    for i in range(len(M)):
        ti = 1
        for j in M[i]:
            if j == 1:
                M[i][j] = len(M) + 1
                L = np.array(M)
                row_ind1, col_ind1 = linear_sum_assignment(L)
                editWeight = len(L) - L[row_ind1, col_ind1].sum()
                if editWeight < weight:
                    M[i][j] = 1
                    ti = 0         
    S = np.array(M)
    row_ind2, col_ind2 = linear_sum_assignment(S)
    weight1 = len(M) - S[row_ind2, col_ind2].sum()
    if n < m:
        phi = zip(row_ind2[0:n], col_ind2)
    else:
        phi = zip(row_ind2, col_ind2)
        
    print(M)
    print(weight, weight1)
    for pair in phi:
        assignments.write('Resource ' + str(pair[1]) + ': ' + str(pair[0]) + '\n')
        
def paretoChecker():
    preferences = open('new.txt', 'r')
    preferences = preferences.read().splitlines()
    preferences.pop(0)
    paretoSwaps = 0
    baseAgent = 0
    
    #finding agent preferences
    assignments = open('Assignments.txt', 'r')
    assignments = assignments.read().splitlines()
    for line in preferences:
        x = line.replace('Resource ', '')
        x = line.replace(',', '')
        preference  = x.split(' ')
        preference.pop()
        preference = list(map(int, preference))
        y = linecache.getline('new.txt', baseAgent + 2)
        basePreference  = y.split(' ')
        basePreference.pop()
        basePreference = list(map(int, preference))     
        #finding primary comparator agent n
        for lines in assignments:
            agent = int(lines.split(':')[1].strip(' '))
            resource = int(lines.split(':')[0].strip('Resource '))
            if resource not in preference and agent != baseAgent:   
                #comparison
                if resource in basePreference:
                    paretoSwaps += 1
                    break
        baseAgent += 1
    paretoSwaps = paretoSwaps / 2

    return paretoSwaps

def envyChecker():
    preferences = open('new.txt', 'r')
    preferences = preferences.read().splitlines()
    preferences.pop(0)
    envyValue = 0
    baseAgent = 0
    
    #finding agent preferences
    assignments = open('Assignments.txt', 'r')
    assignments = assignments.read().splitlines()
    for line in preferences:
        x = line.replace('Resource ', '')
        x = line.replace(',', '')
        preference = x.split(' ')
        preference.pop()
        preference = list(map(int, preference))
        #finding primary comparator agent n
        for lines in assignments:
            agent = int(lines.split(':')[1].strip(' '))
            resource = int(lines.split(':')[0].strip('Resource '))
            if agent == baseAgent:
                if resource not in preference:
                    #see if any item in preference is in assignments if item is not in current preferences
                    for item in preference:
                            if item in [int(x.split(':')[0].strip('Resource ')) for x in assignments]:
                                envyValue += 1
                                break
        baseAgent += 1
    
    return envyValue

def experimentFromFile(file, k):
    oData = open(file, 'r')
    myFile = open('new.txt', 'w+')
    resources = int(re.search(r'\d+', oData.readline()).group())
    for i in range(resources):
        next(oData)
    agents = int(oData.readline().split(',')[0])
    
    myFile.write('Agents: ' + str(agents) + ' Resources: ' + str(resources) + ' K Value: ' + str(k) + '\n')

    for i in range(agents):
        p = oData.readline()
        preferences = []
        for j in range(3):
            q = p[p.find('{')+1:p.find('}')]
            try:
                preferences.append(list(map(int, q.split(','))))
            except ValueError:
                preferences.append([])
            r = '{' + q
            r += '}'
            p = p.replace(r, '')
            
        finalvalues = []
        
        if k < len(preferences[0]):
            random.shuffle(preferences[0])
            finalvalues.extend(preferences[0][0:k])

        if len(preferences[0]) <= k and k < len(preferences[1]) + len(preferences[0]):
            h = k - len(preferences[0])
            finalvalues.extend(preferences[0])
            random.shuffle(preferences[1])
            finalvalues.extend(preferences[1][0:h])

        if len(preferences[0]) + len(preferences[1]) <= k and k < len(preferences[2]):
            h = k - (len(preferences[0]) + len(preferences[1]))
            finalvalues.extend(preferences[0])
            finalvalues.extend(preferences[1])
            random.shuffle(preferences[2])
            finalvalues.extend(preferences[2][0:h])

        for x in finalvalues:
                 if finalvalues[-1] == x:
                     myFile.write(str(x) + ' ')
                 else:
                     myFile.write(str(x) + ', ')
                 
        myFile.write('\n')

        
def timeFromFile(file, algorithm, k):
    experimentFromFile(file, k)
    if algorithm == 'RICA':
            start_time = time.clock()
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            time1 = time.clock() - start_time
    elif algorithm == 'RECA':
        start_time = time.clock()
        RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        time1 = time.clock() - start_time
    elif algorithm == 'MIR':
        start_time = time.clock()
        MIR("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        time1 = time.clock() - start_time
        
    return time1

def  envyFromFile(file, algorithm, k):
    experimentFromFile(file, k)
    if algorithm == 'RICA':
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            envy = envyChecker()
    elif algorithm == 'RECA':
        RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        envy = envyChecker()
    elif algorithm == 'MIR':
        MIR("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        envy = envyChecker()

    return envy

def paretoFromFile(file, algorithm, k):
    experimentFromFile(file, k)
    if algorithm == 'RICA':
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            pareto = (paretoChecker())
    elif algorithm == 'RECA':
        RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        pareto = (paretoChecker())
    elif algorithm == 'MIR':
        MIR("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        pareto = (paretoChecker())

    return pareto

def timeGraph(maxAgents, maxResources, resolution, kValue, initAll, algorithm):
    if algorithm == 'RICA' or algorithm == 'RECA':
        assert initAll == 0
    times = []
    agents = []
    divisions = int(maxAgents / resolution)
    resourceDivisions = maxResources / resolution
    for x in range(1, resolution + 1):
        resourceAllocation(int(maxAgents / resolution * x), int(maxResources / resolution * x), kValue, initAll, random.randint(0, 100000))
        if algorithm == 'RICA':
            start_time = time.clock()
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            times.append(time.clock() - start_time)
        elif algorithm == 'RECA':
            start_time = time.clock()
            RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            times.append(time.clock() - start_time)
        elif algorithm == 'MIR':
            start_time = time.clock()
            MIR("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            times.append(time.clock() - start_time)
        agents.append(int(maxAgents / resolution * x))
    plt.plot(agents, times)
    plt.show()
    #graph times

            
def paretoGraph(maxAgents, maxResources, resolution, kValue, initAll, algorithm):
    if algorithm == 'RICA' or algorithm == 'RECA':
        assert initAll == 0
    paretos = []
    agents = []
    divisions = int(maxAgents / resolution)
    resourceDivisions = maxResources / resolution
    for x in range(1, resolution + 1):
        resourceAllocation(int(maxAgents / resolution * x), int(maxResources / resolution * x), kValue, initAll, random.randint(0, 100000))
        if algorithm == 'RICA':
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            paretos.append(paretoChecker())
        elif algorithm == 'RECA':
            RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            paretos.append(paretoChecker())
        elif algorithm == 'MIR':
            MIR("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            paretos.append(paretoChecker())
        agents.append(int(maxAgents / resolution * x))
    plt.plot(agents, paretos)
    plt.show()
    #graph
    
def envyGraph(maxAgents, maxResources, resolution, kValue, initAll, algorithm):
    if algorithm == 'RICA' or algorithm == 'RECA':
        assert initAll == 0
    envys = []
    agents = []
    divisions = int(maxAgents / resolution)
    resourceDivisions = maxResources / resolution
    for x in range(1, resolution + 1):
        resourceAllocation(int(maxAgents / resolution * x), int(maxResources / resolution * x), kValue, initAll, random.randint(0, 100000))
        if algorithm == 'RICA':
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            envys.append(envyChecker())
        elif algorithm == 'RECA':
            RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            envys.append(envyChecker())
        elif algorithm == 'MIR':
            MIR("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            envys.append(envyChecker())
        agents.append(int(maxAgents / resolution * x))
    plt.plot(agents, envys)
    plt.show()
    #graph

def envyRICA(agents, resources, maxkValue):
    envys = []
    values = []
    for x in range(1, maxkValue + 1):
         resourceAllocation(agents, resources, x, 0, random.randint(0, 100000))
         RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
         envys.append(envyChecker())
         values.append(x)
    plt.plot(values, envys)
    plt.xticks(values)
    plt.show()

def timeRICA(agents, resources, maxkValue):
    times = []
    values = []
    for x in range(1, maxkValue + 1):
        resourceAllocation(agents, resources, x, 0, random.randint(0, 100000))
        start_time = time.clock()
        RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
        times.append(time.clock() - start_time)
        values.append(x)
    plt.plot(values, times)
    plt.show()


x = envyFromFile('MD-00004-00000002.toi', 'RICA', 10)
print(x)

