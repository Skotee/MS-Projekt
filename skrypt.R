dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

# Szeregi rozdzielcze
dane_sklepu1_vec <- c(dane_sklepu_1[[1]])
sort(dane_sklepu1_vec)

dane_sklepu2_vec <- c(dane_sklepu_2[[1]])
sort(dane_sklepu2_vec)

cat("Œrednia sklepu 1: ", mean(dane_sklepu1_vec))

cat("Œrednia sklepu 2: ", mean(dane_sklepu2_vec))
