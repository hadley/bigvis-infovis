library(raster)

alt <- getData('alt', country='NZ')
slope <- terrain(alt[[1]], opt='slope')
aspect <- terrain(alt[[1]], opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)

plot(alt[[1]], col = grey(80:0/100))
plot(hill, col = grey(0:100/100))
plot(slope, col = grey(80:0/100))
