#Uses to add dorms to the output of a coding query, using the ID column 

import sys,string,re


observations = sys.stdin.readlines()


#Create a dictionary with DORM and ID for every token in the ppceme and ppcmbe

dormDictFile = open("calDormsPpcemeMbe.txt","r")
dormDictLines = dormDictFile.readlines()

dormDict = {}

for l in dormDictLines:
    dormId = l.split()
    dormDict[dormId[1]] = float(dormId[0])


for obs in observations:
    codes = obs.split(":")
    id = codes[len(codes)-3].strip()
    dorm = dormDict[id]
    for c in codes:
        sys.stdout.write("%s:" % c.strip())  #strips newline that the mergeMeta script adds when it adds the year.

    sys.stdout.write("%s\n" % dorm)
