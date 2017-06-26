import random
import statistics
import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt

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
    # plt.show() JM we know this works, so editing out for now

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
    assignments = open('RECA_Assignments.txt', 'w+')    
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
    unassignedObjects = [x for x in resourceNumbers if x not in assignedObjects]
    random.shuffle(unassignedObjects)
    random.shuffle(unassignedAgents)
    remainingAssignments = zip(unassignedObjects, unassignedAgents)
    for pair in remainingAssignments:
        assignments.write('Resource ' + str(pair[0]) + ': ' + str(pair[1]) + '\n')


resourceAllocation(100, 140, 1, 678807)

resourceChecker(100, 1)
#will have to find where your document is saved on computer and use double backslashes
RECA("C:\\Users\\Jake From State Farm\\Documents\\GitHub\\RICA-RECA\\new.txt")
