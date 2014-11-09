all : arbiter.beam  factory.beam  myArbiter.beam myArbiter_tests.beam\
myLists.beam  myLists_tests.beam robotFunctions.beam robotUtils.beam  test.beam\
myRobot.beam myRobot_tests.beam\
allTests.beam

%.beam : %.erl
	erl -compile $<

test:
	erl -run allTests test

clean : 
	rm -f *.beam *.dump *~
