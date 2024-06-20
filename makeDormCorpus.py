#Uses eccoPpcemePpcmbeFreqDict.txt to take ppche corpus pos files, and output a file with info content values, calibrated DORM, and IDs

import sys,string,re,urllib.request, contractions

#dormUidoFunctions = urllib.request.urlopen("https://raw.githubusercontent.com/rbailes/DORM-UIDO/master/uidodorm.py").read()

#tempFile = open("dormUidoTemp.py","w")

#tempFile.write(dormUidoFunctions)

#tempFile.close()

from dormUidoTemp import dorm,uido,getDORM,infovec,uidoraw



tokens = sys.stdin.read().split("\n\n") #In ppche4 pos files, tokens are separated by an empty line

#word-tag pairs consisting only of codes, punctuation, or IDs. The ID will be stored, and the rest will be skipped from further processing.
badStuff = re.compile("(^[\<\{].*)|(.*/ID$)|(^[%s]/[%s])" % (string.punctuation,string.punctuation))



wordTag = re.compile("(.*)/(.*)")

#Get a list of words for each token, which consists of word and pos tag on each line



#Create a dictionary with info content for each word:

freqDictFile = open("eccoPpcemeMbeFreqDict.txt","r")
freqDictLines = freqDictFile.readlines()

freqDict = {}

for l in freqDictLines:
    wordFreq = l.split()
    freqDict[wordFreq[0]] = float(wordFreq[1])


for tok in tokens:
    lineWords = tok.split()
    if len(lineWords) <= 1: #skip the token if the token only consists of a code or page number or some such
        continue
    id = wordTag.search(lineWords[len(lineWords)-1]).group(1) #grabs token ID
    #create a list of info content values for the words in the line
    infoVector = []
    for wt in lineWords:
        #Skips codes and IDs, after we've stored the ID above
        if badStuff.search(wt) == None:
            word = wordTag.search(wt).group(1)
            #Clean punctuation and stuff out
            word = word.translate(str.maketrans('', '', string.punctuation)).lower()
            if word in freqDict:
                infoVector.append(float(freqDict[word]))
            else:
                infoVector.append(25.83)

    
    #calculate calibrated DORM
    calDorm = float(dorm(infoVector,correct=True)-dorm(uidoraw(infoVector)))

    sys.stdout.write("%s\t%s\n" % (calDorm,id))

    #debug
    for thing in infoVector:
        sys.stderr.write("%s " % thing)
    sys.stderr.write("%s\n\n" % id)
    
    #    if line == "\n":
#        sys.stdout.write("\n\n")
