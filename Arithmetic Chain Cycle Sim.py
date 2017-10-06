'''
Simulating the Arithmetic Chain on the n-cycle modulo p in Python
Andrew Heeszel
'''
import random
from math import log


#The following lists give input values to be simulated for the parameters n and p of the chain respectively
#and can be modified for whichever values are needed.

n_list = [2,5,10, 15]
p_list = [3]


#This generates a set containing the state space for the Arithmetic Chain as Tuples and will be used
#within the simulation function defined below.

def produce_tups(n, p):
    '''
    (int, int) -> set
    Function produces a set of all n sized tuples with integer values modulo p omitting the tuple with all zero values.
    '''
    starter = []
    for i in range(0, n):
        starter.append(0)
    start = tuple(starter)
    sample = set()
    sample.add(start)
    for i in range(0,n):
        new_sample = set()
        for j in range(1,p):
            for element in sample:
                element = list(element)
                element[n-i-1] = j
                new_sample.add(tuple(element))
            sample = sample.union(new_sample)
    sample.remove(start)
    return sample


'''
The following function will be used to simulate the markov chain and estimate the mixing time with total variation 
distance 0.25 within a factor of 2. Estimate is given by running a sample of markov chains independently, each from 
the same initial state and with the transition probabilitites of the arithmetic chain on the n-cycle modulo p, and 
taking the sample total variation distance at time (2 ** t) for t in {1,2,...}. The algorithm runs until sample 
total variation distance is below 0.25 and returns the total time the simulation ran. Initial state is the tuple
with a first value equal to 1, and all other values equal to 0. The Markov chain is ergotic if and only if
p is prime.
'''

def simulation(n,p, sample_size):
    '''
    (int, int, int) -> int
    Function estimates the total variation distance of the arithmetic chain on the n cycle modulo p by the technique
    described above. The size of the cycle is a parameter for the function, along with the prime modulo p for the
    Markov chain, and sample size for the sample total variation distance.
    '''
    pos_neg = [1,-1]
    omega = produce_tups(n,p)
    size_omega = len(omega)
    sim_dict = {}
    mixed = False
    first_it = True
    starter = [1]
    t = 2
    total = 2
    for i in range(1,n):
        starter.append(0)
    starter = tuple(starter)
    sample = []
    for i in range(0,sample_size):
        sample.append(starter)
    while mixed == False:
        updated_sample = []
        for el in omega:
            sim_dict.update({el:0})
        for el in sample:
            li_el = list(el)
            for time in range(0,t):
                vertex = random.randint(0,n-1)
                neighbor = random.choice(pos_neg)
                operation = random.choice(pos_neg)
                if vertex == (n-1) and neighbor == 1:
                    li_el[n-1] = (li_el[n-1] + operation * li_el[0]) % p
                else:
                    li_el[vertex] = (li_el[vertex] + operation * li_el[vertex + neighbor]) % p
            updated_el = tuple(li_el)
            sim_dict.update({updated_el: sim_dict[updated_el] + 1})
            updated_sample.append(updated_el)
        unif = 1 / size_omega
        var_dist = 0
        for tup in omega:
            var_dist += abs((sim_dict[tup] / sample_size) - unif)
        if var_dist <= 0.5:
            mixed = True
        if mixed == False:
            total = 2 * total
            if first_it == False:
                t = 2 * t
        first_it = False
        sample = updated_sample
    return total


'''
The following code creates a text file giving the estimated mixing time, the natural logarithm of the mixing time,
the parameter value of  n, the natural log of n, the parameter value of p, and the natural log of p, for all values
of n and p in the initial lists of the document. Each value is rounded to two decimal points. The sample size
was fixed at 5 times the the size of the state space for the chain conditioned on the values of n and p.
'''

file = open("simulation_results.txt", 'w')
line_1 = 'time,log(time),n,log(n),p,log(p)' + "\n"
file.write(line_1)
for p in p_list:
    for n in n_list:
        size = 5 * ((p ** n) - 1)
        time = simulation(n,p,size)
        line = str(time)+ ','+ str(round(log(time),2)) + ',' + str(n) + ',' + str(round(log(n),2)) + ',' \
            + str(p) + ',' + str(round(log(p),2)) + "\n"
        file.write(line)