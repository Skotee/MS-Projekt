moda <- function(vec) {
  ux <- unique(vec)
  ux[which.max(tabulate(match(vec, ux)))]
}

dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

# Szeregi rozdzielcze
dane_sklepu1_vec <- c(dane_sklepu_1[[1]])
sort(dane_sklepu1_vec)

dane_sklepu2_vec <- c(dane_sklepu_2[[1]])
sort(dane_sklepu2_vec)

cat("Œrednia sklepu 1: ", mean(dane_sklepu1_vec))
cat("Kwantyl 0.25 sklepu 1:", quantile(dane_sklepu1_vec, 0.25))
cat("Mediana sklepu 1: ", median(dane_sklepu1_vec))
cat("Kwantyl 0.75 sklepu 1:", quantile(dane_sklepu1_vec, 0.75))
cat("Odchylenie standardowe sklepu 1: ", sd(dane_sklepu1_vec))
cat("Wariancja sklepu 1: ", var(dane_sklepu1_vec))
cat("Dominanta sklepu 1: ", moda(dane_sklepu1_vec))

cat("Œrednia sklepu 2: ", mean(dane_sklepu2_vec))
cat("Kwantyl 0.25 sklepu 2:", quantile(dane_sklepu2_vec, 0.25))
cat("Mediana sklepu 2: ", median(dane_sklepu2_vec))
cat("Kwantyl 0.75 sklepu 2:", quantile(dane_sklepu2_vec, 0.75))
cat("Odchylenie standardowe sklepu 2: ", sd(dane_sklepu2_vec))
cat("Wariancja sklepu 2: ", var(dane_sklepu2_vec))
cat("Dominanta sklepu 2: ", moda(dane_sklepu2_vec))