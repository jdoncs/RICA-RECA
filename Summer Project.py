import random
import statistics
import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt

def randomResources(resources, kValue, seedValue):
    random.seed(seedValue)
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
    x = seed
    for agent in agents:
        agentResources = randomResources(resources, kValue, x)
        myFile.write('Agent ' + str(agent) + ' ---> ')
        for resource in agentResources:
            myFile.write('Resource ' + str(resource) + ',' + ' ')
        myFile.write('\n')
        #CHANGE SEED FOR EACH ITERATION
        x = x + 1 #JD: This is probably going going to lead to highly non-random behaviours and should be removed.
    myFile.close()

def resourceChecker(r, kValue):
    #CREATING LIST OF DATA
    myFile = open('new.txt', 'r')
    values = []
    indices = []
    for i in range(r):
        values.append(0)
        indices.append(i)
        
    for line in myFile:
        for j in range(r):
            if ('Resource ' + str(j) + ',') in line:
                values[j] += 1
                
   #STATISTICS
    mean = statistics.mean(values)
    standard_deviation = statistics.pstdev(values)
    
   #GRAPHING
    y_pos = np.arange(len(values))
    plt.bar(y_pos, values, align = 'center', alpha = 0.5)
    plt.ylabel('Number of Occurances')
    plt.xlabel('Resources')
    plt.title('Distribution of object Occurances at k =' + str(kValue))
    plt.text(50, 20, ('Mean: ' + str(mean)))
    plt.text(50, 10, ('SD: ' + str(standard_deviation)))
    plt.show()


resourceAllocation(10000, 100, 1, 1)

resourceChecker(100, 1)
