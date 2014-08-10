.SUFFIXES: .erl .beam .yrl

EBIN = ../ebin

.erl.beam:
	erlc -W -o ${EBIN} $< 

MODS = db my_db_gen my_db_gen_sup my_db_gen_app

all: compile

compile: ${MODS:%=%.beam} 

clean:
	rm -rf ${EBIN}/*.beam ${EBIN}/erl_crash.dump 
