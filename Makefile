example-euler:
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/euler.crn -p -s 600000 -n "e"

example-gcd:
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/gcd.crn -p -s 70000 -n "a;b" -i "a0=32;b0=12"

example-division:
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/division.crn -p -s 150000 -n "a;b;q;r" -i "a0=20;b0=3"

example-counter:
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/counter.crn -p -s 500000 -n "c" -i "c0=3"

example-factorial: 
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/factorial.crn -p -s 100000 -n "f" -i "f0=5"

example-isqrt:
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/isqrt.crn -p -s 70000 -n "z;zpow;out" -i "n0=10"

example-pi:
	dotnet run --project src/crnccrun/crnccrun.fsproj -- simulate examples/pi.crn -p -s 100000 -n "pi"
