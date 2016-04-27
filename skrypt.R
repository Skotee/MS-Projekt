moda <- function(vec) {
  ux <- unique(vec)
  return(ux[which.max(tabulate(match(vec, ux)))])
}

rozstep <- function(vec)
{
  result <- max(vec) - min(vec)
  return(result)
}

cwiartkowe <- function(q1, q3)
{
  result <- (q3 - q1) / 2
  return(result)
}

wspolczynnik_zmiennosci <- function(odch, sred)
{
  v <- (odch / sred) * 100
  return(v)
}

moment_centralny <- function(nr, vec, sr)
{
  ile <- length(vec)
  m <- sum((vec - sr) ^ nr) / ile
  return(m)
}

kurtoza <- function(vec, sr, odch)
{
  k <- moment_centralny(4, vec, sr) / (odch ^ 4)
  return(k)
}

eksces <- function(kurt)
{
  eks <- kurt - 3
  return(eks)
}

wspolczynnik_asymetrii <- function(vec, sr, odch)
{
  k <- moment_centralny(3, vec, sr) / (odch ^ 3)
  return(k)  
}

dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

# Szeregi rozdzielcze
dane_sklepu1_vec <- c(dane_sklepu_1[[1]])
sort(dane_sklepu1_vec)

dane_sklepu2_vec <- c(dane_sklepu_2[[1]])
sort(dane_sklepu2_vec)

sklep1_sr <- mean(dane_sklepu1_vec)
sklep1_q1 <- quantile(dane_sklepu1_vec, 0.25)
sklep1_q3 <- quantile(dane_sklepu1_vec, 0.75)
sklep1_moda <- moda(dane_sklepu1_vec)
sklep1_odch <- sd(dane_sklepu1_vec)
sklep1_kurt <- kurtoza(dane_sklepu1_vec, sklep1_sr, sklep1_odch)

sklep2_sr <- mean(dane_sklepu2_vec)
sklep2_q1 <- quantile(dane_sklepu2_vec, 0.25)
sklep2_q3 <- quantile(dane_sklepu2_vec, 0.75)
sklep2_moda<- moda(dane_sklepu2_vec)
sklep2_odch <- sd(dane_sklepu2_vec)
sklep2_kurt <- kurtoza(dane_sklepu2_vec, sklep2_sr, sklep2_odch)

cat("Sklep 1:")
cat("Srednia: ", sklep1_sr)
cat("Kwartyl 0.25:", sklep1_q1)
cat("Kwartyl 0.75:", sklep1_q3)
cat("Mediana: ", median(dane_sklepu1_vec))
cat("Odchylenie standardowe nieobciazone: ", sklep1_odch)
cat("Odchylenie cwiartkowe: ", cwiartkowe(sklep1_q1,sklep1_q3))
cat("Wariancja nieobciazona: ", var(dane_sklepu1_vec))
cat("Dominanta: ", sklep1_moda)
cat("Rozstep: ", rozstep(dane_sklepu1_vec))
cat("Wspolczynnik zmiennosci: ", wspolczynnik_zmiennosci(sklep1_sr, sklep1_odch), "%")
cat("Wspolczynnik asymetrii: ", wspolczynnik_asymetrii(dane_sklepu1_vec, sklep1_sr, sklep1_odch))
cat("Kurtoza: ", sklep1_kurt)
cat("Eksces: ", eksces(sklep1_kurt))

cat("Sklep 2:")
cat("Srednia: ", sklep2_sr)
cat("Kwartyl 0.25 sklepu 2:", sklep2_q1 )
cat("Kwartyl 0.75 sklepu 2:", sklep2_q3)
cat("Mediana: ", median(dane_sklepu2_vec))
cat("Odchylenie standardowe nieobciazone: ", sklep2_odch)
cat("Odchylenie cwiartkowe: ", cwiartkowe(sklep2_q1,sklep2_q3))
cat("Wariancja nieobciazona: ", var(dane_sklepu2_vec))
cat("Dominanta: ", sklep2_moda)
cat("Rozstep: ", rozstep(dane_sklepu2_vec))
cat("Wspolczynnik zmiennosci: ", wspolczynnik_zmiennosci(sklep2_sr, sklep2_odch), "%")
cat("Wspolczynnik asymetrii: ", wspolczynnik_asymetrii(dane_sklepu2_vec, sklep2_sr, sklep2_odch))
cat("Kurtoza: ", sklep2_kurt)
cat("Eksces: ", eksces(sklep2_kurt))