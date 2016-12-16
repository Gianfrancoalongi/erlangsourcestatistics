

all:
	erlc -o ebin -I include/ src/*.erl test/*.erl

test: all
	erl -pa ebin/ -eval 'eunit:test(ess), init:stop().'

PROJ=/local/scratch/etxpell/proj
reg:
	mkdir -p ${PROJ}/sgc/tools/erlangsourcestatistics/src/
	mkdir -p ${PROJ}/sgc/tools/erlangsourcestatistics/include/
	\cp -fv src/*.erl \
	${PROJ}/sgc/tools/erlangsourcestatistics/src/
	\cp -fv include/*.hrl \
	${PROJ}/sgc/tools/erlangsourcestatistics/src/
	make ess -C ${PROJ}/sgc/src/sgc/reg
	\cp -fv ${PROJ}/sgc/src/sgc/reg/res.* ~/dev_patches

sgc:
	\cp -fv src/*.erl \
	${PROJ}/sgc/tools/erlangsourcestatistics/src/
	make ess -C ${PROJ}/sgc/src
	\cp -fv ${PROJ}/sgc/src/res.* ~/dev_patches

