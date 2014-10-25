all : arbiter.beam  factory.beam  myArbiter.beam  myLists.beam  robotFunctions.beam\
robotUtils.beam  test.beam

%.beam : %.erl
	erl -compile $<
	
clean : 
	rm -f *.beam *~
