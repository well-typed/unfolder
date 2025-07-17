set terminal png
set output "superimpose.png"
plot "points.data", 5 + 2 * x
