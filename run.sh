echo "For all python scripts, you can specify arguments before pressing Enter"
echo -n "Press enter to generate execution traces: "
read a
python exec$1.py $a
echo -n "Press to generate test cases for HOL: "
read a
python casegen.py $a
echo -n  "Press to run tests on HOL: "
read a
rm tcs*.txt
python testrun.py $a
echo "Testing complete!"
