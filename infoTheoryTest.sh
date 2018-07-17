CS="java -classpath ~/CurrentLx/OldNorse/CS_2.003.04.jar csearch/CorpusSearch"

pceec="/Users/yoelw/CurrentLx/OldNorse/hnps/pceec/psd/*.psd"

midfre="/Users/yoelw/CurrentLx/OldNorse/MiddleFrench/*.psd"

icepahc="/Users/yoelw/icecorpus/finished/*.psd"

ycoe="/Users/yoelw/CurrentLx/OldNorse/historicalsyntaxcourse/YCOE/*.psd"

ppcmbe="~/CurrentLx/OldNorse/ppche4/PPCMBE2-RELEASE-1/corpus/psd/*.psd"
ppcme2="~/CurrentLx/OldNorse/ppche4/PPCME2-RELEASE-4/corpus/psd/*.psd"
ppceme="~/CurrentLx/OldNorse/ppche4/PPCEME-RELEASE-3/corpus/psd/*/*.psd"

tychobrahe="/Users/yoelw/CurrentLx/OldNorse/tychobrahe/*psd.txt"

#Run Study on PPCHE


rm myovvo.ymeb.cod
$CS myovvo.ymeb.c $ycoe $ppcme2 $ppceme $ppcmbe -out myovvo.ymeb.cod

rm myovvo.ymeb.cod.ooo
$CS codes.q myovvo.ymeb.cod

Rscript myovvo_plotsonly.R
