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
$CS codes.q infoTheoryTest.ymeb.cod

"Done with study on ppche"

python fixIdFormat.py < infoTheoryTest.ymeb.cod.ooo > infoTheoryTest.ymeb.cod.ooo.tmp
mv infoTheoryTest.ymeb.cod.ooo.tmp infoTheoryTest.ymeb.cod.ooo

echo "Fixed codes"

rm infoTheoryTest.ice.nodates.cod
$CS infoTheoryTest.c $icepahc -out infoTheoryTest.ice.nodates.cod

echo "done with study on IcePaHC"

$CS dateGenre.ice.c infoTheoryTest.ice.nodates.cod -out infoTheoryTest.ice.cod

echo "done with dating IcePaHC data"

rm infoTheoryTest.ice.cod.ooo
$CS codes.q infoTheoryTest.ice.cod

"Done with study on icepahc"

python fixIdFormat.py < infoTheoryTest.ice.cod.ooo > infoTheoryTest.ice.cod.ooo.tmp
mv infoTheoryTest.ice.cod.ooo.tmp infoTheoryTest.ice.cod.ooo

echo "Fixed codes"

Rscript myovvo_plotsonly.R
