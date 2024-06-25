RUN_CMD=dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate

.PHONY: example-euler example-gcd example-division example-counter example-factorial example-isqrt example-pi

example-euler:
	$(RUN_CMD) examples/euler.crn -p -s 600000 -n "e"

example-gcd:
	$(RUN_CMD) examples/gcd.crn -p -s 70000 -n "a;b" -i "a0=32;b0=12"

example-division:
	$(RUN_CMD) examples/division.crn -p -s 150000 -n "a;b;q;r" -i "a0=20;b0=3"

example-counter:
	$(RUN_CMD) examples/counter.crn -p -s 500000 -n "c" -i "c0=3"

example-factorial: 
	$(RUN_CMD) examples/factorial.crn -p -s 100000 -n "f" -i "f0=5"

example-isqrt:
	$(RUN_CMD) examples/isqrt.crn -p -s 70000 -n "z;zpow;out" -i "n0=10"

example-pi:
	$(RUN_CMD) examples/pi.crn -p -s 100000 -n "pi"
