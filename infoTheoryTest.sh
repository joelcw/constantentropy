CS="java -classpath ~/CurrentLx/OldNorse/CS_2.003.04.jar csearch/CorpusSearch"

pceec="~/CurrentLx/OldNorse/hnps/pceec/psd/*.psd"

midfre="~/CurrentLx/OldNorse/MiddleFrench/*.psd"

icepahc="~/icecorpus/finished/*.psd"

ycoe="~/CurrentLx/OldNorse/historicalsyntaxcourse/YCOE/*.psd"

ppcmbe="~/CurrentLx/OldNorse/ppche4/PPCMBE2-RELEASE-1/corpus/psd/*.psd"
ppcme2="~/CurrentLx/OldNorse/ppche4/PPCME2-RELEASE-4/corpus/psd/*.psd"
ppceme="~/CurrentLx/OldNorse/ppche4/PPCEME-RELEASE-3/corpus/psd/*/*.psd"

tychobrahe="/Users/yoelw/CurrentLx/OldNorse/tychobrahe/*psd.txt"

#Run Study on PPCHE


rm infoTheoryTest.ymeb.cod
$CS infoTheoryTest.c $ycoe $ppcme2 $ppceme $ppcmbe -out infoTheoryTest.ymeb.cod

rm infoTheoryTest.ymeb.cod.ooo
$CS codes.q outputs/infoTheoryTest.ymeb.cod

"Done with study on ppche"

python fixIdFormat.py < outputs/infoTheoryTest.ymeb.cod.ooo > outputs/infoTheoryTest.ymeb.cod.ooo.tmp
mv outputs/infoTheoryTest.ymeb.cod.ooo.tmp outputs/infoTheoryTest.ymeb.cod.ooo

echo "Fixed codes"

rm outputs/infoTheoryTest.ice.nodates.cod
$CS infoTheoryTest.c $icepahc -out outputs/infoTheoryTest.ice.nodates.cod

echo "done with study on IcePaHC"

$CS dateGenre.ice.c outputs/infoTheoryTest.ice.nodates.cod -out outputs/infoTheoryTest.ice.cod

echo "done with dating IcePaHC data"

rm outputs/infoTheoryTest.ice.cod.ooo
$CS codes.q outputs/infoTheoryTest.ice.cod

"Done with study on icepahc"

python fixIdFormat.py < outputs/infoTheoryTest.ice.cod.ooo > outputs/infoTheoryTest.ice.cod.ooo.tmp
mv outputs/infoTheoryTest.ice.cod.ooo.tmp outputs/infoTheoryTest.ice.cod.ooo

echo "Fixed codes"

Rscript myovvo_plotsonly.R
