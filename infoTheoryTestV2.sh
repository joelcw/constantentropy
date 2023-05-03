CS="java -classpath /home/joelcw/CurrentLx/OldNorse/CS_2.003.04.jar csearch/CorpusSearch"

icepahc="/home/joelcw/icecorpus/finished/*.psd"

rm outputs/infoTheoryTestV2.ice.nodates.cod
$CS infoTheoryTestV2.c $icepahc -out outputs/infoTheoryTestV2.ice.nodates.cod

echo "done with query on IcePaHC"

$CS dateGenreV2.ice.c outputs/infoTheoryTestV2.ice.nodates.cod -out outputs/infoTheoryTestV2.ice.raw.cod

echo "done with dating IcePaHC data"

#Some things are commented out below so that we can make a raw version with full IDs that Anton can use

rm outputs/infoTheoryTestV2.ice.raw.cod.ooo
$CS codes.q outputs/infoTheoryTestV2.ice.raw.cod

echo "Done with study on icepahc"

python fixIdFormat.ice.py < outputs/infoTheoryTestV2.ice.raw.cod.ooo > outputs/infoTheoryTestV2.ice.cod.ooo.tmp
mv outputs/infoTheoryTestV2.ice.treeIDandIDfixed.cod.ooo outputs/infoTheoryTestV2.ice.treeIDandIDfixed.cod.ooo.old
mv outputs/infoTheoryTestV2.ice.cod.ooo.tmp outputs/infoTheoryTestV2.ice.treeIDandIDfixed.cod.ooo

echo "Fixed codes"

#Rscript myovvo_plotsonly.R
