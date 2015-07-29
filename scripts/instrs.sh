echo 'val instrs = ' > instrs.sml
sed -e 's/^.*\(\[(.*)\]\).*$/ \1::/' < Success >> instrs.sml
echo '[];' >> instrs.sml
