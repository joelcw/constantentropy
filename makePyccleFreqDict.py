#This script is almost the same as pyccleFreq.py, but instead of outputting the bits for each word in a sentence, it produces a frequency dictionary for PYCCLE with word and bits on each line for every unique word in the text.

#Because PYCCLE is so big, I initially did this using the ECCO portion alone, in /CurrentLx/OldNorse/gentdigs/pyccle-ecco-all.tag . It also works with the ppceme and ppcmbe after they are convered to the same format as the pyccle pos-tagged texts, which they are in allPpcemePpcmbe.tabbed.pos

import sys,collections,contractions,string
import numpy as np


#This converts the one-word-plus-tag-per-line format to one sentence per line. The word-tag pairs are separated by an empty line, which is why this is able to keep the sentences on separate lines (it outputs a newline at the end of each sentence when it hits the empty line.

lines = sys.stdin.readlines()

intermed = open("pyccle.txt","w")


for line in lines:
    fields = line.split("\t")
    if len(fields) > 1:
        if fields[1] == "$":
            w = fields[0].replace("'s","\b's") #unsplits possessives in pyccle to match ppche4, even though i'd much rather split them in the latter
        else:
            w = fields[0].replace("—"," ").replace("$","") #replace that long dash thing with a space and dollar with nothing
    else:
        w = fields[0].replace("—"," ").replace("$","") #replace that long dash thing with a space and dollar with nothing
    intermed.write("%s " % w)

intermed.close()

#debug
sys.stderr.write("We got up to closing the intermediate file the first time\n\n\n")
lines = ""

#lines = open("pyccle.txt","r").readlines()
intermed = open("pyccle.txt","r")
text = intermed.read()
intermed.close()


#PART I: cleaning
#first, clean the string: expand contractions, strip punctuation and capitalisation
s=contractions.fix(text)
cleaned=s.translate(str.maketrans('', '', string.punctuation)).lower()
words = cleaned.split()

wordcount = collections.Counter(words)
total = len(words)

for word in wordcount:
    prob = float(wordcount[word]/total)
    bit = np.log2(1/prob)
    sys.stdout.write("%s\t%s\n" % (word,bit))


#for l in lines:
    #PART I: cleaning
    #first, clean the string: expand contractions, strip punctuation and capitalisation
#    s=contractions.fix(l)
#    cleaned=s.translate(str.maketrans('', '', string.punctuation)).lower()
#    linewords = cleaned.split()

#    ii = 0
#    while ii < len(linewords):
#        prob = float(wordcount[linewords[ii]]/len(words))
#        bit = np.log2(1/prob)
#        if ii == (len(linewords)-1):
#            sys.stdout.write("%s\n" % (bit))
#        else:
#            sys.stdout.write("%s," % (bit))
#        ii=ii+1
