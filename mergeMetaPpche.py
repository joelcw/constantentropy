#11Sept2018
#Script takes the output of CS extracting codes, and adds dates for ppche texts from the file metaDataPPCHE.txt. It reads the metadata by text id and date into a dictionary, strips dates of initial "a", "c", then matches text id, and adds date if there is one, no date if there is not.

import sys,string,re

codes = 
