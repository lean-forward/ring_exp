set datafile separator ","
set terminal png
set output "output/expr-small.png"
set xrange[1:600]
set yrange[1:1000]
f(x) = a * x
fit f(x) 'output/benchmark-expr.csv' using 2:3 via a
plot 'output/benchmark-expr.csv' using 2:3 title '(\ring runtime (ms); \ringexp runtime (ms))', f(x) title sprintf("%f * x", a)
