echo $1 |public_mm/bin/metamap12 -IT |tr "\0" " " | sed -f nds.sed 
