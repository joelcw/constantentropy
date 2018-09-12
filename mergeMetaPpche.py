#11Sept2018
#Script takes the output of CS extracting codes, and adds dates for ppche texts from the file metaDataPPCHE.txt. It reads the metadata by text id and date into a dictionary, strips dates of annotations (e.g. "a", "c"), always taking the earliest year if there are ranges, then matches text id, and adds date if there is one, no date if there is not.

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
    id = codes[5].strip()
    sys.stdout.write("%s:%s:%s:%s:%s:" % (codes[0],codes[1],codes[2],codes[3],id))
    if id in dateDict:
        sys.stdout.write(dateDict[id])
    else:
        sys.stdout.write(codes[4])

    sys.stdout.write("\n")
