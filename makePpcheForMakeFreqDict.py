import sys,string,re


#This converts the pos-tagged format of ppche into the same format as pos-tagged PYCCLE, so it can be fed into the makeFreqDict.py script. It omits any lines with an ID or a comment or a page number, etc.

lines = sys.stdin.readlines()

badStuff = re.compile("(^[\<\{].*)|(.*/ID$)")

wordTag = re.compile("(.*)/(.*)")

for line in lines:
    if line == "\n":
        sys.stdout.write("\n\n")
    elif badStuff.search(line) == None:
        sys.stdout.write("%s\t%s\n" % (wordTag.search(line).group(1),wordTag.search(line).group(2)))
