all : arbiter.beam  factory.beam  myArbiter.beam myArbiter_tests.beam\
myLists.beam  myLists_tests.beam robotFunctions.beam robotUtils.beam  test.beam\
myRobot.beam myRobot_tests.beam

%.beam : %.erl
	erl -compile $<
	
clean : 
	rm -f *.beam *~
