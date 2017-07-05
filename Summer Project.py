import random
import statistics
import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt
import linecache
import time

def randomResources(resources, kValue):
    newResources = resources[:]
    selectedResources = []
    for i in range(kValue):
        newItem = (newResources[random.randint(0, (len(newResources) - 1))])
        selectedResources.append(newItem)
        newResources.remove(newItem)
    return selectedResources

def resourceAllocation(a, r, kValue, seed):
    agents = []
    resources = []
    for agent in range(a):
        agents.append(agent)
    for resource in range(r):
        resources.append(resource)
    myFile = open('new.txt', 'w+')
    myFile.write('Agents: ' + str(a) + ', Resources: ' + str(r) + ', K Value: ' + str(kValue) + '\n') #JM made data representation the same as Selena's
    random.seed(seed)
    for agent in agents:
        agentResources = randomResources(resources, kValue)
        for resource in agentResources:
            myFile.write('Resource ' + str(resource) + ' ')
        myFile.write('\n')
        #x = x + 1   #JD: This is probably going going to lead to highly non-random behaviours and should be removed.
                          #JM seed changer removed
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
    resourceNumbers = []
    for i in range(currentAgent):
        resourceNumbers.append(i)
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
                
    #printing remaining assignments
    #### JD: I think this line will cause some problems, depending on how it's implemented.
    # It's a double comprehension, so it may be quadtratic time. Might be good to profile it.
    # If the bottleneck is here, just re-write it as a loop with a filter. It might already be
    # doing this under the hood though.
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
    resourceNumbers = []
    eqClasses = {}
    currentAgent = 0

    #Dictionary of preferences
    for line in myFile:
        x = line.replace('Resource ', '')
        items = x.split(' ')
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
    for i in range(currentAgent):
        resourceNumbers.append(i)
        
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
        preference  = x.split(' ')
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

def timeGraph(maxAgents, maxResources, resolution, kValue, algorithm):
    times = []
    agents = []
    divisions = int(maxAgents / resolution)
    resourceDivisions = maxResources / resolution
    for x in range(1, resolution):
        resourceAllocation(int(maxAgents / resolution * x), int(maxResources / resolution * x), kValue, random.randint(0, 100000))
        if algorithm == 'RICA':
            start_time = time.clock()
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            times.append(time.clock() - start_time)
        elif algorithm == 'RECA':
            start_time = time.clock()
            RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            times.append(time.clock() - start_time)
        agents.append(int(maxAgents / resolution * x))
    plt.plot(agents, times)
    plt.show()
    #graph times

            
def paretoGraph(maxAgents, maxResources, resolution, kValue, algorithm):
    paretos = []
    agents = []
    divisions = int(maxAgents / resolution)
    resourceDivisions = maxResources / resolution
    for x in range(1, resolution):
        resourceAllocation(int(maxAgents / resolution * x), int(maxResources / resolution * x), kValue, random.randint(0, 100000))
        if algorithm == 'RICA':
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            paretos.append(paretoChecker())
        elif algorithm == 'RECA':
            RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            paretos.append(paretoChecker())
        agents.append(int(maxAgents / resolution * x))
    plt.plot(agents, paretos)
    plt.show()
    #graph
    
def envyGraph(maxAgents, maxResources, resolution, kValue, algorithm):
    envys = []
    agents = []
    divisions = int(maxAgents / resolution)
    resourceDivisions = maxResources / resolution
    for x in range(1, resolution):
        resourceAllocation(int(maxAgents / resolution * x), int(maxResources / resolution * x), kValue, random.randint(0, 100000))
        if algorithm == 'RICA':
            RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            envys.append(envyChecker())
        elif algorithm == 'RECA':
            RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
            envys.append(envyChecker())
        agents.append(int(maxAgents / resolution * x))
    plt.plot(agents, envys)
    plt.show()
    #graph

def envyRICA(agents, resources, maxkValue):
    envys = []
    values = []
    for x in range(1, maxkValue):
         resourceAllocation(agents, resources, x, random.randint(0, 100000))
         RICA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
         envys.append(envyChecker())
         values.append(x)
    plt.plot(values, envys)
    plt.xticks(values)
    plt.show()
    
envyRICA(1000, 1000, 5)





