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

# Obciazona - dzielimy przez n, nieobciazona - przez (n - 1)
wariancja_obciazona <- function(vec, sr)
{
  ile <- length(vec)
  w <- sum((vec - sr)^2) / ile
  return(w)
}

# war - wariancja obciążona
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
# wsp_ufnosci - poziom ufności podany w treści
przedzial_sredniej <- function(vec, sr, odch, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("niepoprawny poziom ufności")
    
  ile <- length(vec)
  
  if(ile < 1)
    return("nie podano żadnych danych")
  
  # Poziom istotności
  alfa <- 1 - wsp_ufnosci
  
  # Różne wzory na podstawie ilości danych - http://wm.pollub.pl/files/77/content/files/3097_estymacja_przedzialowa.pdf
  if(ile <= 30)
  {
    # Model II - nieznane odchylenie populacji, mała próba
    # qt - kwantyl rozkładu t-Studenta
    wart_t_lub_u <- qt(1 - (alfa / 2), ile - 1) * (odch / sqrt(ile - 1))
    
  }
  else
  {
    # Model III - nieznane odchylenie populacji, duża próba
    # qnorm - kwantyl rozkładu normalnego
    wart_t_lub_u <- qnorm(1 - (alfa / 2)) * (odch / sqrt(ile))
  }
  
  przedzial <- c(sr - wart_t_lub_u, sr + wart_t_lub_u)
  return(przedzial)
}

# war - wariancja próby
przedzial_odchylenia <- function(vec, war, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("niepoprawny poziom ufności")
  
  ile <- length(vec)
  
  if(ile < 1)
    return("nie podano żadnych danych")
  
  # Poziom istotności
  alfa <- 1 - wsp_ufnosci
 
  if(ile <= 30)
  {
    # Model I - nieznane odchylenie populacji, mała próba
    # qchisq - kwantyl rozkładu chi-kwadrat 
    ns <- ile * war
    przedzial <- c(ns / qchisq(1 - (alfa / 2), ile - 1), ns / qchisq(alfa / 2, ile - 1))
    
    # Przedział jest dla wariancji, więc pierwiastkujemy go
    przedzial <- sqrt(przedzial)
  }
  else
  {
    # Model II - nieznane odchylenie populacji, duża próba
    odch <- sqrt(war)
    wart_u <- qnorm(1 - (alfa / 2)) / sqrt(2 * ile)
    przedzial <- c(odch / (1 + wart_u), odch / (1 - wart_u))
  }

  return(przedzial)
}

# przedzial - wektor przedzialu zawierajacy dwa elementy
# wart_do_porownania - wartość do porównania precyzji, np. wyliczona średnia
precyzja_wzgledna <- function(przedzial, wart_do_porownania)
{
  ile <- length(przedzial)
  
  if(ile != 2)
    return("niepoprawny wektor przedziału")
  
  # Odejmujemy dolną wartość przedziału od górnej
  blad_maksymalny <- (przedzial[2] - przedzial[1]) / 2
  precyzja <- (blad_maksymalny / wart_do_porownania) * 100
  
  if(precyzja < 5)
    return(sprintf("%f%% jest mniejsze od 5%%, więc możemy bezpiecznie stwierdzić, że istnieją podstawy do uogólnienia", precyzja))
  else if(precyzja < 10)
    return(sprintf("%f%% jest między 5%% a 10%%, więc możemy stwierdzić, że istnieją podstawy do uogólnienia, jednak musimy pozostać ostrożni", precyzja))
  else
    return(sprintf("%f%% jest większe od 10%%, więc należy odrzucić tezę, że istnieją podstawy do uogólnienia", precyzja))
}

# vec/sr/war/war_nieob - wektor/średnia/wariancja obciążona/ wariancja nieobciążona prób 1 i 2
# alfa - poziom istotności podany w treści (alfa)
# Można również użyć t.test(dane_sklepu1_vec, dane_sklepu2_vec, alternative="greater", var.equal=[TRUE|FALSE], conf.level = 0.95)
test_dwoch_srednich <- function(vec1, vec2, sr1, sr2, war1, war2, war1_nieob, war2_nieob, alfa)
{
  ile1 <- length(vec1)
  ile2 <- length(vec2)
  ile1bez1 <- ile1 - 1
  ile2bez1 <- ile2 - 1
  
  # Najpierw testujemy równość wariancji populacji
  # H0 - są równe
  # H1 - wariancja pierwszej jest mniejsza (gdyż tak wskazują wariancje prób)
  # Korzystamy ze statystyki F-Snedecora
  f <- war1_nieob / war2_nieob
  wart_f <- qf(1 - alfa, ile1 - 1, ile2 - 1)
  
  # Gdy wartość statystyki jest większa od górnej granicy przedziału, nie mamy podstaw do odrzucenia hipotezy
  if(f >= -wart_f)
  {
    ile1i2bez2 <- ile1 + ile2 - 2
    
    # Test t-Studenta dla grup niezależnych o równej wariancji - http://www-users.mat.umk.pl/~gemini/2mieStat/2012/testy_teoria.pdf
    t <- (sr1 - sr2) / sqrt(((war1 * ile1bez1 + war2 * ile2bez1) * (ile1 + ile2)) / (ile1i2bez2 * ile1 * ile2))
    
    # Liczba stopni swobody przy odczycie - n1 + n2 - 2
    wart_t <- qt(1 - alfa, ile1i2bez2)
    
    # Gdy wartość jest mniejsza od dolnej granicy przedziału, nie ma podstaw do odrzucenia hipotezy
    if(t <= wart_t)
      h <- "Wartość nie należy do przedziału - przyjmujemy hipotezę zerową - średnie są równe."
    else
      h <- "Wartość należy do przedziału - odrzucamy hipotezę zerową - średnia pierwszego sklepu jest większa."
    
    return(sprintf("równe odchylenia populacji, więc: statystyka t = %f, przedział <%f, ∞). %s", t, wart_t, h))
  }
  else
  {
    war_ile1 <- war1 / ile1
    war_ile2 <- war2 / ile2
    
    # Statystyka Cochrana-Coxa, gdyż odchylenia są różne
    C <- (sr1 - sr2) / sqrt(war_ile1 + war_ile2)
    
    # Wzór Cochrana-Coxa
    wart_C <- (war_ile1 * qt(1 - alfa, ile1bez1) + war_ile2 * qt(1 - alfa, ile2bez1)) / (war_ile1 + war_ile2)
    
    if(C <= wart_C)
      h <- "Wartość nie należy do przedziału - przyjmujemy hipotezę zerową - średnie są równe."
    else
      h <- "Wartość należy do przedziału - odrzucamy hipotezę zerową - średnia pierwszego sklepu jest większa."
    
    return(sprintf("różne odchylenia populacji, więc: statystyka C = %f, przedział <%f, ∞). %s", C, wart_C, h))
  }
}

# Wczytanie danych z plików
dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

# Przekształcenie ich do posortowanych wektorów
dane_sklepu1_vec <- c(dane_sklepu_1[[1]])
sort(dane_sklepu1_vec)

dane_sklepu2_vec <- c(dane_sklepu_2[[1]])
sort(dane_sklepu2_vec)

# Histogramy
sklep1_hist <- hist(dane_sklepu1_vec)
sklep2_hist <- hist(dane_sklepu2_vec)

# Szeregi szczegółowe
sklep1_sr <- mean(dane_sklepu1_vec)
sklep1_med <- median(dane_sklepu1_vec)
sklep1_q1 <- quantile(dane_sklepu1_vec, 0.25)
sklep1_q3 <- quantile(dane_sklepu1_vec, 0.75)
sklep1_moda <- moda(dane_sklepu1_vec)
sklep1_war <- wariancja_obciazona(dane_sklepu1_vec, sklep1_sr)
sklep1_war_nieob <- var(dane_sklepu1_vec)
sklep1_odch <- odchylenie_obciazone(sklep1_war)
sklep1_odch_nieob <- sd(dane_sklepu1_vec)
sklep1_kurt <- kurtoza(dane_sklepu1_vec, sklep1_sr, sklep1_odch)

sklep2_sr <- mean(dane_sklepu2_vec)
sklep2_med <- median(dane_sklepu2_vec)
sklep2_q1 <- quantile(dane_sklepu2_vec, 0.25)
sklep2_q3 <- quantile(dane_sklepu2_vec, 0.75)
sklep2_moda<- moda(dane_sklepu2_vec)
sklep2_war <- wariancja_obciazona(dane_sklepu2_vec, sklep2_sr)
sklep2_war_nieob <- var(dane_sklepu2_vec)
sklep2_odch <- odchylenie_obciazone(sklep2_war)
sklep2_odch_nieob <- sd(dane_sklepu2_vec)
sklep2_kurt <- kurtoza(dane_sklepu2_vec, sklep2_sr, sklep2_odch)

# Szeregi rozdzielcze
sklep1_med_r <- kwartyl_rozdzielczy(sklep1_hist$breaks, sklep1_hist$counts, 0.5)
sklep1_sr_r <- srednia_rozdzielczy(sklep1_hist$counts, sklep1_hist$mids)
sklep1_q1_r <- kwartyl_rozdzielczy(sklep1_hist$breaks, sklep1_hist$counts, 0.25)
sklep1_q3_r <- kwartyl_rozdzielczy(sklep1_hist$breaks, sklep1_hist$counts, 0.75)
sklep1_moda_r <- moda_rozdzielczy(sklep1_hist$counts, sklep1_hist$breaks)
sklep1_war_r <- wariancja_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r)

sklep2_med_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.5)
sklep2_sr_r <- srednia_rozdzielczy(sklep2_hist$counts, sklep2_hist$mids)
sklep2_q1_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.25)
sklep2_q3_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.75)
sklep2_moda_r <- moda_rozdzielczy(sklep2_hist$counts, sklep2_hist$breaks)
sklep2_war_r <- wariancja_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r)

# Zad 3

sklep1_przedzial_sredniej <- przedzial_sredniej(dane_sklepu1_vec, sklep1_sr, sklep1_odch, 0.95)
#sklep1_przedzial_odchylenia <- przedzial_odchylenia(dane_sklepu1_vec, sklep1_war, 0.95)
sklep1_precyzja_wzgledna <- precyzja_wzgledna(sklep1_przedzial_sredniej, sklep1_sr)

# Zad 4

#sklep2_przedzial_sredniej <- przedzial_sredniej(dane_sklepu2_vec, sklep2_sr, sklep2_odch, 0.95)
sklep2_przedzial_odchylenia <- przedzial_odchylenia(dane_sklepu2_vec, sklep2_war, 0.95)
sklep2_precyzja_wzgledna <- precyzja_wzgledna(sklep2_przedzial_odchylenia, sklep2_odch)

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
cat("Wariancja nieobciazona: ", sklep1_war_nieob)
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
cat("Precyzja względna: ", sklep1_precyzja_wzgledna)

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
cat("Wariancja nieobciazona: ", sklep2_war_nieob)
cat("Wariancja obciazona: ", sklep2_war)
cat("Dominanta: ", sklep2_moda)
cat("Rozstep: ", rozstep(dane_sklepu2_vec))
cat("Wspolczynnik zmiennosci: ", wspolczynnik_zmiennosci(sklep2_sr, sklep2_odch), "%")
cat("Wspolczynnik asymetrii: ", wspolczynnik_asymetrii(dane_sklepu2_vec, sklep2_sr, sklep2_odch))
cat("Wspolczynnik skosnosci: ", wspolczynnik_skosnosci(dane_sklepu2_vec, sklep2_sr, sklep2_odch))
cat("Kurtoza: ", sklep2_kurt)
cat("Eksces: ", eksces(sklep2_kurt))
cat("Przedzial sredniej: (", sklep2_przedzial_sredniej, ")")
cat("Przedzial odchylenia: (", sklep2_przedzial_odchylenia, ")")
cat("Precyzja względna: ", sklep2_precyzja_wzgledna)

# Zad 5
cat("Czy na poziomie istotności 0.05 można twierdzić, że wartość miesięcznych wydatków, na jedną osobę, na pieczywo i produkty zbożowe są większe dla klientów pierwszego marketu (sformułować i zweryfikować odpowiednią hipotezę)?")
cat("H0 - m1 = m2")
cat("H1 - m1 > m2")
cat("Wynik testu t-Welcha: ", test_dwoch_srednich(dane_sklepu1_vec, dane_sklepu2_vec, sklep1_sr, sklep2_sr, sklep1_war, sklep2_war, sklep1_war_nieob, sklep2_war_nieob, 0.05))