all : arbiter.beam  factory.beam  myArbiter.beam myArbiter_tests.beam\
myLists.beam  myLists_tests.beam robotFunctions.beam robotUtils.beam  test.beam\
myRobot.beam myRobot_tests.beam main.beam ui.beam\
allTests.beam

%.beam : %.erl
	erl -compile $<

test:
	erl -noshell -s allTests test -s init stop 2>/dev/null

start:
	erl -setcookie asimov -name bob@foo.bar -run main start 2> main.log

ui:
	erl -setcookie asimov -name alice@abc.def -run ui start

clean : 
	rm -f *.beam *.dump *~ *.log
