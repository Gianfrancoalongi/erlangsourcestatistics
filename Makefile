

all:
	erlc -o ebin -I include/ src/*.erl test/*.erl

test: all
	erl -pa ebin/ -eval 'eunit:test(ess), init:stop().'

PROJ=/local/scratch/etxpell/proj
reg:
	\cp -fv src/*.erl \
	${PROJ}/sgc/tools/erlangsourcestatistics/src/
	make ess -C ${PROJ}/sgc/src/sgc/reg
	\cp -fv ${PROJ}/sgc/src/sgc/reg/res.* ~/dev_patches

sgc:
	\cp -fv src/*.erl \
	${PROJ}/sgc/tools/erlangsourcestatistics/src/
	make ess -C ${PROJ}/sgc/src
	\cp -fv ${PROJ}/sgc/src/reg/res.* ~/dev_patches

