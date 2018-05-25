library(reticulate)
library(tidyverse)
library(stringr)
library(imager)
main <- import_main()
cv2 <- import("cv2", as = "cv2")

source_python("./inst/HoughLines.py")

chunk_edges <- list.files("inst/processed/slices/", pattern = "_edge", full.names = T)
tmp <- lapply(chunk_edges[1:100], calcHoughLines)

hist(tmp[[1]][,,2])

res <- map_df(1:length(tmp), ~data.frame(img=.x, link=chunk_edges[.x], thetas=tmp[[.x]][,,2]))
ggplot(data = res) + geom_density(aes(x = thetas)) + facet_wrap(~img)

plot(load.image(chunk_edges[1]))
