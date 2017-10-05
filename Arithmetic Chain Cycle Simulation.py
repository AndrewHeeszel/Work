#Markov Chain Simulation for the Arithmetic Chain on the n-cycle
#Andrew Heeszel

import random

def produce_tups(n, p):
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


def a_chain_cycle(n,p, sample_size):
    tups = produce_tups(n,p)
    n_dict = {}
    mixed = False
    time = 2
    starter = [1]
    for i in range(0,n-1):
        starter.append(0)
    sample = []
    for i in range(0,sample_size):
        sample.append(starter)
    while mixed == False:
        for el in tups:
            n_dict.update({el: 0})
        for el in sample:
            for i in range(0,time):
                vertex = random.randint(0,n-1)
                neighbor = random.randint(-1,1)
                operation = random.randint(-1,1)
                if vertex == n-1 and neighbor == 1:
                    el[0] = (el[0] + operation * el[n]) % p
                else:
                    el[vertex] = (el[vertex] + operation * el[vertex + neighbor]) % p
            n_dict.update({tuple(el): n_dict[el] + 1})
        dist = 0
        for el in tups:
            dist = dist + abs((n_dict[el] / sample_size) - (1 / (p ** n - 1)))
        if dist * 0.5 <= 0.25:
            mixed = True
        if time != 2:
            time = 2 * time
    return [n,p,time,sample_size]


def simulation(n,p, sample_size):
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
    while mixed == False:
        for el in omega:
            sim_dict.append({el:0})
        if first_it == True:
            sample = []
            for i in len(sample_size):
                sample.append(starter)
        for el in sample:
            for time in range(0,t):
                vertex = random.randint(0,n-1)
                neighbor = random.choice(pos_neg)
                operation = random.choice(pos_neg)
                if vertex == (n-1) and neighbor == 1:
                    el[n-1] = (el[n-1] + operation * el[0]) % p
                else:
                    el[vertex] = (el[vertex] + operation * el[vertex + neighbor]) % p
            sim_dict.update({tuple(el): sim_dict[tuple(el)] + 1})
        unif = 1 / size_omega
        var_dist = 0
        for tup in omega:
            var_dist += abs((sim_dict[tup] / sample_size) - unif)
        if var_dist <= 0.5:
            mixed = True
            return total
        total = 2 * total
        if first_it == False:
            t = 2 * t
        first_it = False

