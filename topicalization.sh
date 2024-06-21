#15 Nov 2022

CS="java -classpath /home/joelcw/CurrentLx/OldNorse/CS_2.003.04.jar csearch/CorpusSearch"

pceec="/home/joelcw/CurrentLx/OldNorse/hnps/pceec/psd/*.psd"

midfre="/home/joelcw/CurrentLx/OldNorse/MiddleFrench/*.psd"

icepahc="/home/joelcw/icecorpus/finished/*.psd"

ycoe="/home/joelcw/CurrentLx/OldNorse/historicalsyntaxcourse/YCOE/*.psd"

ppcmbe="/home/joelcw/CurrentLx/OldNorse/ppche4/PPCMBE2-RELEASE-1/corpus/psd/*.psd"
ppcme2="/home/joelcw/CurrentLx/OldNorse/ppche4/PPCME2-RELEASE-4/corpus/psd/*.psd"
ppceme="/home/joelcw/CurrentLx/OldNorse/ppche4/PPCEME-RELEASE-3/corpus/psd/*/*.psd"

tychobrahe="/home/joelcw/CurrentLx/OldNorse/tychobrahe/*psd.txt"


#Run Study on PPCHE


rm outputs/ipmat-obj-sbj-vfin.eb.cod
$CS ipmat-obj-sbj-vfin.c $ppceme $ppcmbe -out outputs/ipmat-obj-sbj-vfin.eb.cod

rm outputs/ipmat-obj-sbj-vfin.eb.cod.ooo
$CS codes.q outputs/ipmat-obj-sbj-vfin.eb.cod

echo "Done with query on ppche"

python3 fixIdFormat.eng.py < outputs/ipmat-obj-sbj-vfin.eb.cod.ooo > outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.tmp
mv outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.tmp outputs/ipmat-obj-sbj-vfin.eb.cod.ooo

echo "Fixed codes"

python3 mergeMetaPpcheGeneral.py < outputs/ipmat-obj-sbj-vfin.eb.cod.ooo > outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.tmp1
mv outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.tmp1 outputs/ipmat-obj-sbj-vfin.eb.cod.ooo

echo "added metadata, done with study on ppche"


#Add Dorms from dormCorpus based on the ID code for each observation

python3 addDormsToCodes.py < outputs/ipmat-obj-sbj-vfin.eb.cod.ooo > outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.tmp1
cp outputs/ipmat-obj-sbj-vfin.eb.cod.ooo outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.old
mv outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.tmp1 outputs/ipmat-obj-sbj-vfin.eb.cod.ooo.dorm

echo "added dorms"
