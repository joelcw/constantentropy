//topicalization over time
nodes_only:t
remove_nodes:t
add_to_ignore: \**
define:../CurrentLx/OldNorse/verbtopic.def
node: IP-MAT*
coding_query:


// 1: fronting
// 0: no fronting
1: {
 \1: (IP-MAT* idoms finite_verb) AND (IP-MAT* idoms [1]object) AND (IP-MAT* idoms NP-NOM*|NP-SBJ*) AND ([1]object precedes NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* precedes finite_verb) AND (IP-MAT* idoms !NP-OB2)
      
    \0: (IP-MAT* idoms finite_verb) AND (IP-MAT* idoms [1]object) AND (IP-MAT* idoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* precedes finite_verb) AND (finite_verb precedes [1]object) AND (IP-MAT* idoms !NP-OB2)

	z: ELSE
}




//object status: pronominal, demonstrative, other nominals

2:{

    pronobj: (IP-MAT* idoms object) AND (object idomsonly PRO*)

    demobj: ((IP-MAT* idoms object) AND (object idomsonly D))
    OR ((IP-MAT* idoms object) AND (object idoms D) AND (D idoms [Tt]h[iey]s.*|\+[TtdD][iey]s.*|\+[TtdD][ea]t.*|[Tt]h[ae]t.*|y.*[st].*))

    nomobj: (IP-MAT* idoms object) AND (object doms any_nominal|CONJ*|N*|D*)

    z: ELSE
  
}

//Subject status:  pronominal, demonstrative, other nominals

3: {
  pronsbj: (IP-MAT* idoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* idomsonly PRO*)

    demsbj: ((IP-MAT* idoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* idomsonly D))
    OR ((IP-MAT* idoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* idoms D) AND (D idoms [Tt]h[iey]s.*|\+[TtdD][iey]s.*|\+[TtdD][ea]t.*|[Tt]h[ae]t.*|y.*[st].*))

    nomsbj: (IP-MAT* idoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* doms any_nominal|CONJ*|N*|D*)
    
    z: ELSE
    }
