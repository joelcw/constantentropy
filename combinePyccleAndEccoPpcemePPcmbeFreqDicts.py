#30Aug2024
#Uses eccoPpcemePpcmbeFreqDict.txt and the pyccle freq dict from Seth. Converts the latter to bits, and combines them such that if a freq exists in the pyccle one, that gets priority; otherwise, the freq is taken from eccoPpcemePpcmbeFreqDict.txt. This will be then fed into makeDormDict.py and then addDormstoCodes

import sys,string,re
import numpy as np

#Create a dictionary with info content for each word for eccoPpcemeMbeFreqDict.txt:

eccoEtcFreqDictFile = open("eccoPpcemeMbeFreqDict.txt","r")
eccoEtcFreqDictLines = eccoEtcFreqDictFile.readlines()

eccoEtcFreqDict = {}

for l in eccoEtcFreqDictLines:
    wordFreq = l.split()
    eccoEtcFreqDict[wordFreq[0]] = float(wordFreq[1])

#Create a dictionary with info content for each word for sethFreqDict.txt:

sethFreqDictFile = open("sethFreqDict.txt","r")
sethFreqDictLines = sethFreqDictFile.readlines()

sethFreqDict = {}

#In addition to storing these, they must be converted from raw occurrences to bits, with total words = 1529672290
#Note that we are leaving punctuation in the counts for now, but this could be a mistake

total = 1529672290

for l in sethFreqDictLines:
    wordFreq = l.split()
    prob = float(wordFreq[1]/total)
    bit = np.log2(1/prob)
    sethFreqDict[wordFreq[0]] = float(bit)

    
#combine the dictionaries by appending all items that are in eccoEtc but not in sethFreqDict to sethFreqDict

for item in eccoEtcFreqDict:
    if item not in sethFreqDict:
        sethFreqDict[item] = eccoEtcFreqDict[item]

#output in the same format as earlier

outfile = open("combinedEeboEccoPpcemeMbeFreqDict.txt","w")

for word in sethFreqDict:
    outfile.write("%s\t%s\n" % (word,bit))

outfile.close()
