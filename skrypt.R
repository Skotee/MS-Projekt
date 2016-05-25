wariancja_rozdzielczy <- function(mids, counts, sred)
{
  war = sum((mids-sred)^2*counts)/sum(counts)
  
  return(war)
}

kwartyl_rozdzielczy <- function(breaks, counts, kwartyl)
{
  poz_kwar <- sum(counts)
  if(poz_kwar%%2)
  {
    poz_kwar = (poz_kwar+1)*kwartyl
  }
  else
  {
    poz_kwar = poz_kwar*kwartyl
  }
  
  licz_skumul <- cumsum(counts)
  szuk_przedzial <- findInterval(poz_kwar, licz_skumul)
  
  med = breaks[szuk_przedzial]+(poz_kwar-licz_skumul[szuk_przedzial-1])*((breaks[szuk_przedzial+1]-breaks[szuk_przedzial])/counts[szuk_przedzial])
  
  return(med)
}

srednia_rozdzielczy <- function(counts, mids)
{
  srednia = sum(mids*counts)/sum(counts)
  
  return(srednia)
}

moda_rozdzielczy <- function(counts, breaks)
{
  szuk = max(counts)
  n = 0
  
  for(i in counts)
  {
    n = n + 1
    if(i == szuk)
    {
      break()
    }
  }
  
  moda = breaks[n]+(((szuk-counts[n-1])/((szuk-counts[n-1])+(szuk-counts[n+1])))*(breaks[n+1]-breaks[n]))
  
  return(moda)
}

wariancja_obciazona <- function(vec, sr)
{
  ile <- length(vec)
  w <- sum((vec - sr)^2) / ile
  return(w)
}

# war - wariancja obci¹¿ona
odchylenie_obciazone <- function(war)
{
  o <- sqrt(war)
  return(o)
}

moda <- function(vec)
{
  ux <- unique(vec)
  
  if(length(ux) == length(vec))
  {
    return("Brak dominanty")
  }
  else
  {
    wek_ilosci_powtorzen <- tabulate(match(vec, ux))
    czy_zduplikowane <- duplicated(wek_ilosci_powtorzen, incomparables = FALSE, fromLast = TRUE)
    poz_mody <- which.max(wek_ilosci_powtorzen)
    
    if(czy_zduplikowane[poz_mody])
    {
      return("Brak dominanty")
    }
    else
    {
      return(ux[poz_mody])
    }
  }
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

przecietne_od_sredniej <- function(vec, sr)
{
  ile <- length(vec)
  m <- sum(abs(vec - sr)) / ile
  return(m)
}

przecietne_od_mediany <- function(vec, med)
{
  ile <- length(vec)
  m <- sum(abs(vec - med)) / ile
  return(m)
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

wspolczynnik_skosnosci <- function(vec, sr, odch)
{
  ile <- length(vec)
  s <- (ile * sum((vec - sr)^3)) / ((ile - 1) * (ile - 2) * (odch)^4)
  return(s)
}

# sr - srednia proby
# odch - odchylenie standardowe próby
# wsp_ufnosci - poziom ufnoœci podany w treœci
przedzial_sredniej <- function(vec, sr, odch, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("Niepoprawny poziom ufnoœci.")
    
  ile <- length(vec)
  
  if(ile < 1)
    return("Nie podano ¿adnych danych.")
  
  # Poziom istotnoœci
  alfa <- 1 - wsp_ufnosci
  
  # Ró¿ne wzory na podstawie iloœci danych - http://wm.pollub.pl/files/77/content/files/3097_estymacja_przedzialowa.pdf
  if(ile <= 30)
  {
    # Model II - nieznane odchylenie populacji
    # qt - kwantyl rozk³adu t-Studenta
    wart_t_lub_u <- qt(1 - (alfa / 2), ile - 1) * (odch / sqrt(ile - 1))
    
  }
  else
  {
    # Model III - nieznane odchylenie populacji
    # qnorm - kwantyl rozk³adu normalnego
    wart_t_lub_u <- qnorm(1 - (alfa / 2)) * (odch / sqrt(ile))
  }
  
  przedzial <- c(sr - wart_t_lub_u, sr + wart_t_lub_u)
  return(przedzial)
}

# war - wariancja próby
przedzial_odchylenia <- function(vec, war, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("Niepoprawny poziom ufnosci.")
  
  ile <- length(vec)
  
  if(ile < 1)
    return("Nie podano zadnych danych.")
  
  # Poziom istotnoœci
  alfa <- 1 - wsp_ufnosci
 
  # Model I - nieznane odchylenie populacji
  # qchisq - kwantyl rozk³adu chi-kwadrat 
  ns <- ile * war
  przedzial <- c(ns / qchisq(1 - (alfa / 2), ile - 1), ns / qchisq(alfa / 2, ile - 1))
  
  # Przedzia³ jest dla wariancji, wiêc pierwiastkujemy go
  return(sqrt(przedzial))
}

dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

dane_sklepu1_vec <- c(dane_sklepu_1[[1]])
sort(dane_sklepu1_vec)

dane_sklepu2_vec <- c(dane_sklepu_2[[1]])
sort(dane_sklepu2_vec)

sklep1_hist <- hist(dane_sklepu1_vec)
sklep2_hist <- hist(dane_sklepu2_vec)

# Szeregi szczegolowe
sklep1_sr <- mean(dane_sklepu1_vec)
sklep1_med <- median(dane_sklepu1_vec)
sklep1_q1 <- quantile(dane_sklepu1_vec, 0.25)
sklep1_q3 <- quantile(dane_sklepu1_vec, 0.75)
sklep1_moda <- moda(dane_sklepu1_vec)
sklep1_war <- wariancja_obciazona(dane_sklepu1_vec, sklep1_sr)
sklep1_odch <- odchylenie_obciazone(sklep1_war)
sklep1_odch_nieob <- sd(dane_sklepu1_vec)
sklep1_kurt <- kurtoza(dane_sklepu1_vec, sklep1_sr, sklep1_odch)

sklep2_sr <- mean(dane_sklepu2_vec)
sklep2_med <- median(dane_sklepu2_vec)
sklep2_q1 <- quantile(dane_sklepu2_vec, 0.25)
sklep2_q3 <- quantile(dane_sklepu2_vec, 0.75)
sklep2_moda<- moda(dane_sklepu2_vec)
sklep2_war <- wariancja_obciazona(dane_sklepu2_vec, sklep2_sr)
sklep2_odch <- odchylenie_obciazone(sklep2_war)
sklep2_odch_nieob <- sd(dane_sklepu2_vec)
sklep2_kurt <- kurtoza(dane_sklepu2_vec, sklep2_sr, sklep2_odch)

#Szeregi rozdzielcze
sklep1_med_r <- kwartyl_rozdzielczy(sklep1_hist$breaks, sklep1_hist$counts, 0.5)
sklep1_sr_r <- srednia_rozdzielczy(sklep1_hist$counts, sklep1_hist$mids)
sklep1_q1_r <- kwartyl_rozdzielczy(sklep1_hist$breaks, sklep1_hist$counts, 0.25)
sklep1_q3_r <- kwartyl_rozdzielczy(sklep1_hist$breaks, sklep1_hist$counts, 0.75)
sklep1_moda_r <- moda_rozdzielczy(sklep1_hist$counts, sklep1_hist$breaks)
sklep1_war_r <- wariancja_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r)


sklep2_med_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.5)
sklep2_sr_r <- srednia_rozdzielczy(sklep2_hist$counts, sklep2_hist$mids)
sklep2_med_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.25)
sklep2_med_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.75)
sklep2_moda_r <- moda_rozdzielczy(sklep2_hist$counts, sklep2_hist$breaks)
sklep2_war_r <- wariancja_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r)

#zad 3 i 4

sklep1_przedzial_sredniej <- przedzial_sredniej(dane_sklepu1_vec, sklep1_sr, sklep1_odch, 0.95)
sklep1_przedzial_odchylenia <- przedzial_odchylenia(dane_sklepu1_vec, sklep1_war, 0.95)

sklep2_przedzial_sredniej <- przedzial_sredniej(dane_sklepu2_vec, sklep2_sr, sklep2_odch, 0.95)
sklep2_przedzial_odchylenia <- przedzial_odchylenia(dane_sklepu2_vec, sklep2_war, 0.95)

cat("Sklep 1:")
cat("Srednia: ", sklep1_sr)
cat("Kwartyl 0.25:", sklep1_q1)
cat("Kwartyl 0.75:", sklep1_q3)
cat("Mediana: ", sklep1_med)
cat("Odchylenie standardowe nieobciazone: ", sklep1_odch_nieob)
cat("Odchylenie standardowe obciazone: ", sklep1_odch)
cat("Odchylenie cwiartkowe: ", cwiartkowe(sklep1_q1,sklep1_q3))
cat("Odchylenie przecietne od sredniej: ", przecietne_od_sredniej(dane_sklepu1_vec, sklep1_sr))
cat("Odchylenie przecietne od mediany: ", przecietne_od_mediany(dane_sklepu1_vec, sklep1_med))
cat("Wariancja nieobciazona: ", var(dane_sklepu1_vec))
cat("Wariancja obciazona: ", sklep1_war)
cat("Dominanta: ", sklep1_moda)
cat("Rozstep: ", rozstep(dane_sklepu1_vec))
cat("Wspolczynnik zmiennosci: ", wspolczynnik_zmiennosci(sklep1_sr, sklep1_odch), "%")
cat("Wspolczynnik asymetrii: ", wspolczynnik_asymetrii(dane_sklepu1_vec, sklep1_sr, sklep1_odch))
cat("Wspolczynnik skosnosci: ", wspolczynnik_skosnosci(dane_sklepu1_vec, sklep1_sr, sklep1_odch))
cat("Kurtoza: ", sklep1_kurt)
cat("Eksces: ", eksces(sklep1_kurt))
cat("Przedzial sredniej: (", sklep1_przedzial_sredniej, ")")
cat("Przedzial odchylenia: (", sklep1_przedzial_odchylenia, ")")

cat("Sklep 2:")
cat("Srednia: ", sklep2_sr)
cat("Kwartyl 0.25 sklepu 2:", sklep2_q1 )
cat("Kwartyl 0.75 sklepu 2:", sklep2_q3)
cat("Mediana: ", sklep2_med)
cat("Odchylenie standardowe nieobciazone: ", sklep2_odch_nieob)
cat("Odchylenie standardowe obciazone: ", sklep2_odch)
cat("Odchylenie przecietne od sredniej: ", przecietne_od_sredniej(dane_sklepu2_vec, sklep2_sr))
cat("Odchylenie przecietne od mediany: ", przecietne_od_mediany(dane_sklepu2_vec, sklep2_med))
cat("Odchylenie cwiartkowe: ", cwiartkowe(sklep2_q1,sklep2_q3))
cat("Wariancja nieobciazona: ", var(dane_sklepu2_vec))
cat("Wariancja obciazona: ", sklep1_war)
cat("Dominanta: ", sklep2_moda)
cat("Rozstep: ", rozstep(dane_sklepu2_vec))
cat("Wspolczynnik zmiennosci: ", wspolczynnik_zmiennosci(sklep2_sr, sklep2_odch), "%")
cat("Wspolczynnik asymetrii: ", wspolczynnik_asymetrii(dane_sklepu2_vec, sklep2_sr, sklep2_odch))
cat("Wspolczynnik skosnosci: ", wspolczynnik_skosnosci(dane_sklepu2_vec, sklep2_sr, sklep2_odch))
cat("Kurtoza: ", sklep2_kurt)
cat("Eksces: ", eksces(sklep2_kurt))
cat("Przedzial sredniej: (", sklep2_przedzial_sredniej, ")")
cat("Przedzial odchylenia: (", sklep2_przedzial_odchylenia, ")")