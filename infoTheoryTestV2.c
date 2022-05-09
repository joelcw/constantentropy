//This query looks for OV and VO clauses, where T is occupied by a finite auxiliary, and codes for V2 and non-V2 clauses where an adjunct XP is fronted or otherwise present

//****NOTE: clauses with traces in object position are currently excluded (see coding column 3). 

node: IP-MAT*

define: ../CurrentLx/OldNorse/verbtopic.def
coding_query:


// 1: ov
// 0: vo

1: {
 \1: (IP-MAT* idoms object) 
           AND (IP-MAT* idoms finite_aux)
    AND (IP-MAT* iDoms nonfin_mainverb)  AND (finite_aux precedes object) AND (object precedes nonfin_mainverb) 

	\0: (IP-MAT* idoms object) 
           AND (IP-MAT* idoms finite_aux)
    AND (IP-MAT* iDoms nonfin_mainverb) AND (finite_aux precedes nonfin_mainverb) AND (nonfin_mainverb precedes object) 

	z: ELSE
}

//V2 with adjunct fronting, or non-V2 with adjunct but no fronting
2: {
 Sinv: (IP-MAT* idoms NP-SBJ*) 
           AND (IP-MAT* idoms finite_aux)
    AND (IP-MAT* iDoms nonfin_mainverb)  AND (finite_aux precedes NP-SBJ*) AND (NP-SBJ* precedes nonfin_mainverb) AND (IP-MAT* idoms ADVP*|CP-ADV*|NP-AD*|PP*) AND (ADVP*|CP-ADV*|NP-AD*|PP* precedes finite_aux) 

    nonSinv: (IP-MAT* idoms NP-SBJ*) 
           AND (IP-MAT* idoms finite_aux)
    AND (IP-MAT* iDoms nonfin_mainverb) AND (finite_aux precedes nonfin_mainverb) AND (NP-SBJ* precedes finite_aux) AND (IP-MAT* idoms ADVP*|CP-ADV*|NP-AD*|PP*) 

	z: ELSE
}


/*// MAT vs Qs with inversion vs other SUB clauses
2: {
	mat: (IP-MAT* idoms object) 
    //CONJ has to be excluded because the C is up a level in those cases:
    invq: (IP-SUB* idoms object) AND (IP-SUB* hassister !C|CONJ*)

    	sub: (IP-SUB* idoms object) 

        z: ELSE

*/
    
/*         indirectq:   (CP-QUE* iDoms IP-SUB*) AND (CP-QUE* iDoms C)
     cpthat:   (CP-THT* idoms IP-SUB*) AND (CP-THT* iDoms C)
    cpdeg:   (CP-DEG* idoms IP-SUB*) AND (CP-DEG* iDoms C)
     cpadv:	  (CP-ADV* idoms IP-SUB*) AND (CP-ADV* iDoms C)
     cprel:   (CP-REL*|CP-FRL* idoms IP-SUB*) AND (CP-REL*|CP-FRL* iDoms C)
    cpcar:   (CP-CAR* idoms IP-SUB*) AND (CP-CAR* iDoms C)
    cpcmp:   (CP-CMP* idoms IP-SUB*) AND (CP-CMP* idoms C)

}

*/

//object status: pron, negative/quantified obj, positive. Qobj is put in the query before pronobj because it will appear before the pronobj in icelandic in rare cases like: Spyr-spyrja hirðmaður$-hirðmaður $inn-hinn ef-ef hann-hann vildi-vilja nökkverju-nokkur bæta-bæta honum-hann .-. (1275.MORKIN.NAR-HIS,.1294)

3:{

    qobj: (IP-MAT* idoms object) AND (object idoms Q*|NUM*|NEG|ONE*)
    pronobj: (IP-MAT* idoms object) AND (object idomsonly PRO*)
    posobj: (IP-MAT* idoms object) AND (object doms any_nominal|CONJ*|N*|D*)

    //Note that object traces will be in this later category and should be excluded:
    z: ELSE
  
    }

//Subject status, including trace subjects ; these will behave differently in dif construction types: in rel clauses, the last over thing was the comp or the wh-word, i.e. low info, or the head word with a zero comp, which is higher info (but english only, not ice); in Qs, the preceding thing is a wh-word, so low info. Note: subject must precede objects, or else all of the above is different, so no postposed subjects are considered here (including ambiguous quirky passives in early ice)

4: {
  
 gapsbj: ((IP-MAT* idoms NP-SBJ|NP-NOM) AND (NP-SBJ|NP-NOM idomsonly \**) AND (IP-MAT* idoms object) AND (NP-SBJ|NP-NOM precedes object)) OR (IP-*-*|IP-*=* idoms !NP-SBJ|NP-NOM)
     
     pronsbj: (IP-MAT* idoms NP-SBJ|NP-NOM) AND (NP-SBJ|NP-NOM idomsonly PRO*) AND (IP-MAT* idoms object) AND (NP-SBJ|NP-NOM precedes object)
    
     nomsbj: (IP-MAT* idoms NP-SBJ|NP-NOM) AND (NP-SBJ|NP-NOM doms any_nominal|CONJ*|N*|D*) AND (IP-MAT* idoms object) AND (NP-SBJ|NP-NOM precedes object)
    
    z: ELSE
    }



/*
// time period
5: {


// first line sets aside translations and archaic texts; note that the last two are in IcePaHC, but you may want to put these back in (and/or exlude other weird icepahc texts too)
      \0:  (AUTHNEW*|AUTHOLD*|BOETHEL*|ERV-*|NEWCOME-*|PURVER-*|TYNDNEW*|TYNDOLD*|CMAYEN*|CMBRUT3*|CMLAMB*|NTACTS*|NTJOHN* inID)


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

*/
