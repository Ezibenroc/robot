all : arbiter.beam  factory.beam  myArbiter.beam myArbiter_tests.beam\
myLists.beam  myLists_tests.beam robotFunctions.beam robotUtils.beam  test.beam\
myRobot.beam myRobot_tests.beam main.beam ui.beam\
allTests.beam

%.beam : %.erl
	erl -compile $<

test:
	erl -noshell -s allTests test -s init stop 2>/dev/null

start_simulation:
	erl -run main start 2> main.log

start_simulation_ui:
	erl -setcookie asimov -name bob@foo.bar -run main start 2> main.log

start_ui:
	erl -setcookie asimov -name alice@abc.def -run ui start

help:
	@echo "make"
	@echo "make start_simulation"
	@echo "make start_simulation_ui"
	@echo "make start_ui"
	@echo "make test"

clean : 
	rm -f *.beam *.dump *~ *.log
