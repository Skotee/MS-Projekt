#Funkcja liczaca wariancje obciazona/nieobciazona szeregu rozdzielczego
#Do funkcji kolejno przekazujemy wektory srodkow przedzialow i liczebnosci przedzialow oraz srednia
#Zmienna ob sluzy do wyboru czy chcemy policzy� wariancje obciazona (1) lub nieobciazona (0)
wariancja_rozdzielczy <- function(mids, counts, sred, ob)
{
  if(ob)
  {
    war = sum((mids-sred)^2*counts)/sum(counts)
  }
  else
  {
    war = sum((mids-sred)^2*counts)/(sum(counts)-1)
  }
  
  return(war)
}

#Funkcja liczaca pierwszy i trzeci kwartyl oraz mediane szeregu rozdzielczego
#Do funkcji kolejno przekazujemy wektory granic przedzialow i liczebnosci przedzialow
#Zmienna kwartyl sluzy do wyboru ktory kwartyl chcemy policzyc - 0.25 dla pierwszego, 0.5 dla mediany, 0.75 dla trzeciego
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

#Funkcja liczaca srednia szeregu rozdzielczego
#Do funkcji kolejno przekazujemy wektory liczebnosci i srodkow przedzialow
srednia_rozdzielczy <- function(counts, mids)
{
  srednia = sum(mids*counts)/sum(counts)
  
  return(srednia)
}

#Funkcja liczaca dominante szeregu rozdzielczego
#Do funkcji kolejno przekazujemy wektory liczebnosci i granic przedzialow
moda_rozdzielczy <- function(counts, breaks)
{
  szuk = max(counts)
  n = which.max(counts)
  
  moda = breaks[n]+(((szuk-counts[n-1])/((szuk-counts[n-1])+(szuk-counts[n+1])))*(breaks[n+1]-breaks[n]))
  
  return(moda)
}

#Funkcja liczaca odchylenie przecietne od sredniej/mediany w szeregu rozdzielczym
#Do funkcji kolejno przekazujemy wektory srodkow i liczebnosci przedzialow
#W zaleznosci od tego, czy przekazemy srednia czy mediane, mozemy policzyc odpowiednie odchylenie przecietne
przecietne_rozdzielczy <- function(mids, counts, x)
{
  odch = sum(abs((mids - x)*counts))/sum(counts)
  
  return(odch)
}

#Funkcja liczaca kurtoze w szeregu rozdzielczym
#Do funkcji kolejno przekazujemy wektory srodkow i liczebnosci przedzialow oraz srednia i odchylenie standardowe obciazone
kurtoza_rozdzielczy <- function(mids, counts, sred, odch)
{
  kurt = (sum(((mids-sred)^4)*counts)/sum(counts))/(odch)^4
  
  return(kurt)
}

#Funkcja liczaca wspolczynnik asymetrii w szeregu rozdzielczym
#Do funkcji kolejno przekazujemy wektory srodkow i liczebnosci przedzialow oraz srednia i odchylenie standardowe obciazone
wspolczynnik_asymetrii_rozdzielczy <- function(mids, counts, sred, odch)
{
  asym = (sum(((mids-sred)^3)*counts)/sum(counts))/(odch)^3
  
  return(asym)
}

# Obciazona - dzielimy przez n, nieobciazona - przez (n - 1)
wariancja_obciazona <- function(vec, sr)
{
  ile <- length(vec)
  w <- sum((vec - sr)^2) / ile
  return(w)
}

# war - wariancja obciazona
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
# odch - odchylenie standardowe proby
# wsp_ufnosci - poziom ufnosci podany w tresci
przedzial_sredniej <- function(vec, sr, odch, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("niepoprawny poziom ufnosci")
    
  ile <- length(vec)
  
  if(ile < 1)
    return("nie podano zadnych danych")
  
  # Poziom istotnosci
  alfa <- 1 - wsp_ufnosci
  
  # Rozne wzory na podstawie ilosci danych - http://wm.pollub.pl/files/77/content/files/3097_estymacja_przedzialowa.pdf
  if(ile <= 30)
  {
    # Model II - nieznane odchylenie populacji, mala proba
    # qt - kwantyl rozkladu t-Studenta
    wart_t_lub_u <- qt(1 - (alfa / 2), ile - 1) * (odch / sqrt(ile - 1))
    
  }
  else
  {
    # Model III - nieznane odchylenie populacji, duza proba
    # qnorm - kwantyl rozkladu normalnego
    wart_t_lub_u <- qnorm(1 - (alfa / 2)) * (odch / sqrt(ile))
  }
  
  przedzial <- c(sr - wart_t_lub_u, sr + wart_t_lub_u)
  return(przedzial)
}

# war - wariancja proby
przedzial_odchylenia <- function(vec, war, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("niepoprawny poziom ufnosci")
  
  ile <- length(vec)
  
  if(ile < 1)
    return("nie podano zadnych danych")
  
  # Poziom istotnosci
  alfa <- 1 - wsp_ufnosci
 
  if(ile <= 30)
  {
    # Model I - nieznane odchylenie populacji, mala proba
    # qchisq - kwantyl rozkladu chi-kwadrat 
    ns <- ile * war
    przedzial <- c(ns / qchisq(1 - (alfa / 2), ile - 1), ns / qchisq(alfa / 2, ile - 1))
    
    # Przedzial jest dla wariancji, wiec pierwiastkujemy go
    przedzial <- sqrt(przedzial)
  }
  else
  {
    # Model II - nieznane odchylenie populacji, duza proba
    odch <- sqrt(war)
    wart_u <- qnorm(1 - (alfa / 2)) / sqrt(2 * ile)
    przedzial <- c(odch / (1 + wart_u), odch / (1 - wart_u))
  }

  return(przedzial)
}

# przedzial - wektor przedzialu zawierajacy dwa elementy
# wart_do_porownania - wartosc do porownania precyzji, np. wyliczona srednia
precyzja_wzgledna <- function(przedzial, wart_do_porownania)
{
  ile <- length(przedzial)
  
  if(ile != 2)
    return("niepoprawny wektor przedzialu")
  
  # Odejmujemy dolna wartosc przedzialu od gornej
  blad_maksymalny <- (przedzial[2] - przedzial[1]) / 2
  precyzja <- (blad_maksymalny / wart_do_porownania) * 100
  
  if(precyzja < 5)
    return(sprintf("%f%% jest mniejsze od 5%%, wiec mozemy bezpiecznie stwierdzic, ze istnieja podstawy do uogolnienia", precyzja))
  else if(precyzja < 10)
    return(sprintf("%f%% jest miedzy 5%% a 10%%, wiec mozemy stwierdzic, ze istnieja podstawy do uogolnienia, jednak musimy pozostac ostrozni", precyzja))
  else
    return(sprintf("%f%% jest wieksze od 10%%, wiec nalezy odrzucic teze, ze istnieja podstawy do uogolnienia", precyzja))
}

# vec/sr/war/war_nieob - wektor/srednia/wariancja obciazona/ wariancja nieobciazona prob 1 i 2
# alfa - poziom istotnosci podany w tresci (alfa)
# Mozna rowniez uzyc t.test(dane_sklepu1_vec, dane_sklepu2_vec, alternative="greater", var.equal=[TRUE|FALSE], conf.level = 0.95)
test_dwoch_srednich <- function(vec1, vec2, sr1, sr2, war1, war2, war1_nieob, war2_nieob, alfa)
{
  ile1 <- length(vec1)
  ile2 <- length(vec2)
  ile1bez1 <- ile1 - 1
  ile2bez1 <- ile2 - 1
  
  # Najpierw testujemy rownosc wariancji populacji
  # H0 - sa rowne
  # H1 - wariancja pierwszej jest mniejsza (gdyz tak wskazuja wariancje prob)
  # Korzystamy ze statystyki F-Snedecora
  f <- war1_nieob / war2_nieob
  wart_f <- qf(1 - alfa, ile1 - 1, ile2 - 1)
  
  # Gdy wartosc statystyki jest wieksza od gornej granicy przedzialu, nie mamy podstaw do odrzucenia hipotezy
  if(f >= -wart_f)
  {
    ile1i2bez2 <- ile1 + ile2 - 2
    
    # Test t-Studenta dla grup niezaleznych o rownej wariancji - http://www-users.mat.umk.pl/~gemini/2mieStat/2012/testy_teoria.pdf
    t <- (sr1 - sr2) / sqrt(((war1 * ile1bez1 + war2 * ile2bez1) * (ile1 + ile2)) / (ile1i2bez2 * ile1 * ile2))
    
    # Liczba stopni swobody przy odczycie - n1 + n2 - 2
    wart_t <- qt(1 - alfa, ile1i2bez2)
    
    # Gdy wartosc jest mniejsza od dolnej granicy przedzialu, nie ma podstaw do odrzucenia hipotezy
    if(t <= wart_t)
      h <- "Wartosc nie nalezy do przedzialu - przyjmujemy hipoteze zerowa - srednie sa rowne."
    else
      h <- "Wartosc nalezy do przedzialu - odrzucamy hipoteze zerowa - srednia pierwszego sklepu jest wieksza."
    
    return(sprintf("rowne odchylenia populacji, wiec: statystyka t = %f, przedzial <%f, ∞). %s", t, wart_t, h))
  }
  else
  {
    war_ile1 <- war1 / ile1
    war_ile2 <- war2 / ile2
    
    # Statystyka Cochrana-Coxa, gdyz odchylenia sa rozne
    C <- (sr1 - sr2) / sqrt(war_ile1 + war_ile2)
    
    # Wzor Cochrana-Coxa
    wart_C <- (war_ile1 * qt(1 - alfa, ile1bez1) + war_ile2 * qt(1 - alfa, ile2bez1)) / (war_ile1 + war_ile2)
    
    if(C <= wart_C)
      h <- "Wartosc nie nalezy do przedzialu - przyjmujemy hipoteze zerowa - srednie sa rowne."
    else
      h <- "Wartosc nalezy do przedzialu - odrzucamy hipoteze zerowa - srednia pierwszego sklepu jest wieksza."
    
    return(sprintf("rozne odchylenia populacji, wiec: statystyka C = %f, przedzial <%f, ∞). %s", C, wart_C, h))
  }
}

# Wczytanie danych z plikow
dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

# Przeksztalcenie ich do posortowanych wektorow
dane_sklepu1_vec <- c(dane_sklepu_1[[1]])
sort(dane_sklepu1_vec)

dane_sklepu2_vec <- c(dane_sklepu_2[[1]])
sort(dane_sklepu2_vec)

# Histogramy
sklep1_hist <- hist(dane_sklepu1_vec)
sklep2_hist <- hist(dane_sklepu2_vec)

# Szeregi szczegolowe
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
sklep1_war_ob_r <- wariancja_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r, 1)
sklep1_war_nob_r <- wariancja_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r, 0)
sklep1_odch_ob_r <- sqrt(sklep1_war_ob_r)
sklep1_odch_nob_r <- sqrt(sklep1_war_nob_r)
sklep1_odch_cwr_r <- cwiartkowe(sklep1_q1_r, sklep1_q3_r)
sklep1_odch_sr_r <- przecietne_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r)
sklep1_odch_med_r <- przecietne_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_med_r)
sklep1_rozstep_r <- rozstep(sklep1_hist$breaks)
sklep1_wsp_zmien_r <- wspolczynnik_zmiennosci(sklep1_sr_r, sklep1_odch_ob_r)
sklep1_wsp_asym_r <- wspolczynnik_asymetrii_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r, sklep1_odch_ob_r)
#wspolczynnik skosnosci
sklep1_kurt_r <- kurtoza_rozdzielczy(sklep1_hist$mids, sklep1_hist$counts, sklep1_sr_r, sklep1_odch_ob_r)
sklep1_eksc_r <- eksces(sklep1_kurt_r)

sklep2_med_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.5)
sklep2_sr_r <- srednia_rozdzielczy(sklep2_hist$counts, sklep2_hist$mids)
sklep2_q1_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.25)
sklep2_q3_r <- kwartyl_rozdzielczy(sklep2_hist$breaks, sklep2_hist$counts, 0.75)
sklep2_moda_r <- moda_rozdzielczy(sklep2_hist$counts, sklep2_hist$breaks)
sklep2_war_ob_r <- wariancja_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r, 1)
sklep2_war_nob_r <- wariancja_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r, 0)
sklep2_odch_ob_r <- sqrt(sklep2_war_ob_r)
sklep2_odch_nob_r <- sqrt(sklep2_war_nob_r)
sklep2_odch_cwr_r <- cwiartkowe(sklep2_q1_r, sklep2_q3_r)
sklep2_odch_sr_r <- przecietne_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r)
sklep2_odch_med_r <- przecietne_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_med_r)
sklep2_rozstep_r <- rozstep(sklep2_hist$breaks)
sklep2_wsp_zmien_r <- wspolczynnik_zmiennosci(sklep2_sr_r, sklep2_odch_ob_r)
sklep2_wsp_asym_r <- wspolczynnik_asymetrii_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r, sklep2_odch_ob_r)
#wspolczynnik skosnosci
sklep2_kurt_r <- kurtoza_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r, sklep2_odch_ob_r)
sklep2_eksc_r <- eksces(sklep2_kurt_r)

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
cat("Precyzja wzgledna: ", sklep1_precyzja_wzgledna)

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
cat("Precyzja wzgledna: ", sklep2_precyzja_wzgledna)

# Zad 5
cat("Czy na poziomie istotnosci 0.05 mozna twierdzic, ze wartosc miesiecznych wydatkow, na jedna osobe, na pieczywo i produkty zbozowe sa wieksze dla klientow pierwszego marketu (sformulowac i zweryfikowac odpowiednia hipoteze)?")
cat("H0 - m1 = m2")
cat("H1 - m1 > m2")
cat("Wynik testu: ", test_dwoch_srednich(dane_sklepu1_vec, dane_sklepu2_vec, sklep1_sr, sklep2_sr, sklep1_war, sklep2_war, sklep1_war_nieob, sklep2_war_nieob, 0.05))