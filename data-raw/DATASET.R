## code to prepare `DATASET` dataset goes here
barrier<-read.csv2("~/Documents/GitHub/ilab_202122/outfile_barrier.csv")
usethis::use_data(barrier, overwrite = TRUE)
