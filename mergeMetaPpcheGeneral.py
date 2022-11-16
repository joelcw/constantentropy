#16Nov2022
#Script takes the output of CS extracting codes, and adds dates for ppche texts from the file metaDataPPCHE.txt. It reads the metadata by text id and date into a dictionary, strips dates of annotations (e.g. "a", "c"), always taking the earliest year if there are ranges, then matches text id, and adds date if there is one, no date if there is not.

#This script is a little different from the other one, because it does not presuppose a particular number of fields, and so is general to other coding queries

import sys,string,re

meta = open("metaDataPPCHE.txt","r").readlines()

dateDict = {}

datestrip = re.compile("[0-9]{4}")

for line in meta:
    fields = line.split()
    if datestrip.search(fields[1]) != None:
        dateDict[fields[0]] = datestrip.search(fields[1]).group(0)

lines = sys.stdin.readlines()

for l in lines:
    codes = l.split(":")
    id = codes[len(codes)-1].strip()
    ii = 0
    while ii <= len(codes)-3:
        sys.stdout.write("%s:" % codes[ii])
        ii=ii+1

    sys.stdout.write("%s:" % id)
    if id in dateDict:
        sys.stdout.write(dateDict[id])
    else:
        sys.stdout.write(codes[len(codes)-2])

    sys.stdout.write("\n")
