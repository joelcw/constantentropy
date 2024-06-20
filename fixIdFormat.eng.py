#20June2024
#Script takes the output of CS extracting codes with printing token ids, and converts the id into a coding column for text; so it changes @ to : and removes token number for the text id, but also adds the full id including token number
#This one works for the PPCHE, *not* for IcePaHC

import sys,string,re

#Note that the second group below is made non-greedy because of some YCOE ids that have another comma in them later on...but check this behaviour 

idregex = re.compile("(@)((.*?)(,.*))$")

#function returning full id and then the textid in the right format. The textid is at the end so that the output interacts well with mergeMetaPpcheGeneral.py 

def textid(matchobj):
    return (":%s:%s" % (matchobj.group(2),matchobj.group(3).lower()))

lines = sys.stdin.readlines()

for line in lines:
    new = idregex.sub(textid,line)
    sys.stdout.write(new)
