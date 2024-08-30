#30Aug2024
#Uses eccoPpcemePpcmbeFreqDict.txt and the pyccle freq dict from Seth. Converts the latter to bits, and combines them such that if a freq exists in the pyccle one, that gets priority; otherwise, the freq is taken from eccoPpcemePpcmbeFreqDict.txt. This will be then fed into makeDormDict.py and then addDormstoCodes

import sys,string,re


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

for l in sethFreqDictLines:
    wordFreq = l.split()
    sethFreqDict[wordFreq[0]] = float(wordFreq[1])

