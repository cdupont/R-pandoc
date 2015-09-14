# one plot with default name
pandoc -t html test1.md --filter R-pandoc -o test1.html -s

# one plot with specific name
pandoc -t html test2.md --filter R-pandoc -o test2.html -s

# two plots
pandoc -t html test3.md --filter R-pandoc -o test3.html -s

# one plot with code echo
pandoc -t html test4.md --filter R-pandoc -o test4.html -s
