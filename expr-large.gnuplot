set datafile separator ","
set terminal epslatex
set output "expr-large.tex"
set xrange [0:10000]
set yrange [0:40000]
f(x) = a * x + b
fit f(x) 'output/benchmark-expr.csv' using 2:3 via a, b
plot 'output/benchmark-expr.csv' using 2:3 title '(\ring runtime (ms); \ringexp runtime (ms))', f(x) title sprintf("%f * x", a)
