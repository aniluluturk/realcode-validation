echo 'val mismatches = ' > tests.sml
sed -e 's/^.*\(\[(.*)\]\).*cycles:mod=\([0-9]*\),mea=\([0-9]*\)   \([^ ]*\)   .*$/ (\1,\2,\3,[\4])::/ ; s/\(0x[0-9A-Fa-f]*\)w/\1/g' < Fail_Mismatch >> tests.sml
echo '[];' >> tests.sml
echo 'val successes = ' >> tests.sml
sed -e 's/^.*\(\[(.*)\]\).*   \(0x[^ ]*w\)   .*$/ (\1,[\2])::/ ; s/\(0x[0-9A-Fa-f]*\)w/\1/g' < Success >> tests.sml
echo '[];' >> tests.sml
