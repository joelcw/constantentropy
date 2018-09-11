//This query just finds the date for IcePaHC texts. There's definitely a more efficient way of doing this, but I don't feel like creating a metadata file right now.

node: IP*
define: ../CurrentLx/OldNorse/verbtopic.def
coding_query:

// time period
5: {
	\1150: (*1150* inID)
	\1210: (*1210* inID)
	\1250: (*1250* inID)
	\1260: (*1260* inID)
	\1270: (*1270* inID)
	\1275: (*1275* inID)
	\1300: (*1300* inID)
	\1310: (*1310* inID)
	\1325: (*1325* inID)
	\1350: (*1350* inID)
	\1400: (*1400* inID)
	\1450: (*1450* inID)
	\1475: (*1475* inID)
	\1480: (*1480* inID)
	\1525: (*1525* inID)
	\1540: (*1540* inID)
	\1593: (*1593* inID)
	\1611: (*1611* inID)
	\1628: (*1628* inID)
	\1630: (*1630* inID)
	\1650: (*1650* inID)
	\1659: (*1659* inID)
	\1661: (*1661* inID)
	\1675: (*1675* inID)
	\1680: (*1680* inID)
	\1720: (*1720* inID)
	\1750: (*1750* inID)
	\1725: (*1725* inID)
	\1745: (*1745* inID)
	\1790: (*1790* inID)
	\1791: (*1791* inID)
	\1830: (*1830* inID)
	\1835: (*1835* inID)
	\1850: (*1850* inID)
	\1859: (*1859* inID)
	\1861: (*1861* inID)
	\1882: (*1882* inID)
	\1883: (*1883* inID)
	\1888: (*1888* inID)
	\1902: (*1902* inID)
	\1907: (*1907* inID)
	\1908: (*1908* inID)
	\1920: (*1920* inID)
	\1985: (*1985* inID)
	\2008: (*2008* inID)
      z:  ELSE

}
/*Below is not necessary if print_ID is used in codes.q, so I've commented it out

7: {
firstgrammar: (*1150.FI* inID) 
homiliubok: (*1150.HO* inID)
jartein: (*1210.JA* inID)
thorlakur: (*1210.TH* inID)
sturlunga: (*1250.ST* inID)
thetubrot: (*1250.TH* inID)
jomsvikingar: (*1260* inID)
gragas: (*1270* inID)
morkin: (*1275* inID)
alexander: (*1300* inID)
grettir: (*1310* inID)
arni: (*1325* inID)
bandamenn1350: (*1350.BA* inID)
finnbogi: (*1350.FI* inID)
marta: (*1350.MA* inID)
gunnar: (*1400.GUNNAR.* inID)
gunnar2: (*1400.GUNNAR2* inID)
viglundur: (*1400.VI* inID)
bandamenn1450: (*1450.BA* inID)
ectorssaga: (*1450.EC* inID)
judit: (*1450.JU* inID)
vilhjalmur: (*1450.VI* inID)
aevintyri: (*1475* inID)
jarlmann: (*1480* inID)
erasmus: (*1525.ER* inID)
georgius: (*1525.GE* inID)
ntacts: (*1540.NTACTS* inID)
ntjohn: (*1540.NTJOHN* inID)
eintal: (*1593* inID)
okur: (*1611* inID)
olafuregils: (*1628* inID)
gerhard: (*1630* inID)
illugi: (*1650* inID)
pislarsaga: (*1659* inID)
indiafari: (*1661* inID)
armann: (*1675.AR* inID)
magnus: (*1675.MA* inID)
modars: (*1675.MO* inID)
skalholt: (*1680* inID)
vidalin: (*1720* inID)
biskupasogur: (*1725* inID)
klim: (*1745* inID)
fimmbraedra: (*1790* inID)
jonsteingrims: (*1791* inID)
hellismenn: (*1830* inID)
jonasedli: (*1835* inID)
piltur: (*1850* inID)
hugvekjur: (*1859* inID)
orrusta: (*1861* inID)
torfhildur: (*1882* inID)
voggur: (*1883* inID)
grimur: (*1888.GR* inID)
vordraumur: (*1888.VOR* inID)
fossar: (*1902* inID)
leysing: (*1907* inID)
ofurefli: (*1908* inID)
arin: (*1920* inID)
margsaga: (*1985.MA* inID)
sagan: (*1985.SAGAN* inID)
mamma: (*2008.MA* inID)
ofsi: (*2008.OF* inID)
        xxxtext: ELSE
}
*/
6: {
	nar: (*NAR-* inID)
	rel: (*REL-* inID)
	bio: (*BIO-* inID)
	sci: (*SCI-* inID)
	law: (*LAW-* inID)
	xxxgenre: ELSE
}
