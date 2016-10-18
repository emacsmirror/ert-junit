#!/bin/bash
#
total=$2
index=$1

errorcounter() {
	(( errcount++ ))
}

trap errorcounter ERR

: ${EVMS:="23.4 24.1 24.2 24.3 24.4 24.5 25.1 git-snapshot"}
evms=$(echo $EVMS | tr ' ' '\n' | awk "NR % $total == $index")
for ever in $evms; do
	evm install --use emacs-${ever}-travis
	make EMACS=evm-emacs
done

exit ${errcount:-0}
