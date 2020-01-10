set datafile separator ","
set xrange[1:1000]
set yrange[1:1600]
f(x) = a * x + b
fit f(x) 'output/benchmark_expr.csv' using 2:3 via a, b
plot 'output/benchmark_expr.csv' using 2:3, f(x)
