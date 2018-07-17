//This query looks for OV and VO clauses, where T is occupied by a finite auxiliary, and codes for object type and subject type to see how the relative informational content (i.e. surprisal), affects the OV/VO effect by clause type.

//****NOTE: clauses with traces in object position are currently excluded. Extracted subjects could in theory be a relevant subject type, so they are coded separately, but they may be a mix of fairly informational (in subject questions) and very predictable (in relative clauses). Coding by clause type should also be done, but this will be in a separate query for sub clauses only. Matrix Qs and sub Qs with inversion are separated from embedded Qs below.

node: IP*
//add_to_ignore: \**
define: ../CurrentLx/OldNorse/verbtopic.def
coding_query:


// 1: ov
// 0: vo
1: {
 \1: (IP* idoms object)
           AND (IP* idoms finite_aux)
    AND (IP* iDoms nonfin_mainverb)  AND (finite_aux precedes object) AND (object precedes nonfin_mainverb)

	\0: (IP* idoms object)
           AND (IP* idoms finite_aux)
    AND (IP* iDoms nonfin_mainverb) AND (finite_aux precedes nonfin_mainverb) AND (nonfin_mainverb precedes object)

	z: ELSE
}


// MAT vs direct Qs vs various SUB clause types
2: {
	mat: (IP-MAT* idoms object)


/*         indirectq:   (CP-QUE* iDoms IP-SUB*) AND (CP-QUE* iDoms C)
     cpthat:   (CP-THT* idoms IP-SUB*) AND (CP-THT* iDoms C)
    cpdeg:   (CP-DEG* idoms IP-SUB*) AND (CP-DEG* iDoms C)
     cpadv:	  (CP-ADV* idoms IP-SUB*) AND (CP-ADV* iDoms C)
     cprel:   (CP-REL*|CP-FRL* idoms IP-SUB*) AND (CP-REL*|CP-FRL* iDoms C)
    cpcar:   (CP-CAR* idoms IP-SUB*) AND (CP-CAR* iDoms C)
    cpcmp:   (CP-CMP* idoms IP-SUB*) AND (CP-CMP* idoms C)
*/
    	sub: (IP-SUB* idoms object)


	z: ELSE
}

//object status: pron, negative/quantified obj, positive

3:{
 pronobj: (IP* idoms object) AND (object idomsonly PRO*)
 qobj: (IP* idoms object) AND (object idoms Q*|NUM*|NEG|ONE*)
  posobj: (IP* idoms object) AND (object doms any_nominal|CONJ*|N*|D*)
 z: ELSE
  
}

4: {
     pronsbj: (IP* idoms NP-SBJ|NP-NOM) AND (NP-SBJ|NP-NOM idomsonly PRO*)
     nomsbj: (IP* idoms NP-SBJ|NP-NOM) AND (NP-SBJ|NP-NOM doms any_nominal|CONJ*|N*|D*)
    z: ELSE
    }



// time period
5: {


// first line sets aside translations and archaic texts
      \0:  (AUTHNEW*|AUTHOLD*|BOETHEL*|ERV-*|NEWCOME-*|PURVER-*|TYNDNEW*|TYNDOLD*|CMAYEN*|CMBRUT3*|CMLAMB* inID)


//Rough time periods for YCOE texts (Old English)

    \800:  (codocu1* inID)
    \850: (codocu2*|cobede*|coboeth*|cocura*|colaece*|colawaf*|colawafint*|coorosiu*|coprefcura* inID)
      \950:  (coalex*|coblick*|cochad*|cochronA*|codocu3*|codocu4*|cogregdC*|cogregdH*|colacnu*|comart3*|comarvel*|coquadru* inID)
    \1000:  (coaelhom*|coaelive*|coapollo*|cobenrul*|cobyrhtf*|cocathom1*|cocathom2*|codocu3*|coepigen*|colaw1cn*|colaw2cn*|colaw5atr*|colaw6atr*|colawnorthu*|colsigef*|colwstan1*|colwstan2*|cootest*|coprefcath1*|coprefcath2*|coprefgen*|copreflives*|cotempo*|cowsgosp* inID)
    \1050:  (coadrian*|cochronE*|codicts*|coinspolD*|colawger*|colsigewZ*|colwsigeXa*|comargaC*|cowulf* inID) 
    \1100: (colawwllad*|coleofri*|cosolsat1* inID)


//No dates for ppche because I'll use metadata

    NA: ELSE
}

