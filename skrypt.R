skosnosc <- function(sred, med, odch)
{
  skosn = 3*(sred-med)/odch
  
  return(skosn)
}

#Funkcja tworzaca wektor przedziałów dla histogramu
#Do funkcji przekazujemy wektor danych
przedzialy_histogramu <- function(vec)
{
  x = round(sqrt(length(vec)))
  war_min <- min(vec)
  
  szer = (max(vec) - war_min)/x
  
  
  vector <- c(war_min)
  
  for (i in 1:x)
  {
    vector <- c(vector, (war_min+(szer*i)))
  }
  
  return(vector)
}

#Funkcja liczaca wariancje obciazona/nieobciazona szeregu rozdzielczego
#Do funkcji kolejno przekazujemy wektory srodkow przedzialow i liczebnosci przedzialow oraz srednia
#Zmienna ob sluzy do wyboru czy chcemy policzyc wariancje obciazona (1) lub nieobciazona (0)
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
  
  if(szuk_przedzial==1)
  {
    med = breaks[szuk_przedzial]+(poz_kwar)*((breaks[szuk_przedzial+1]-breaks[szuk_przedzial])/counts[szuk_przedzial])
  }
  else
  {
    med = breaks[szuk_przedzial]+(poz_kwar-licz_skumul[szuk_przedzial-1])*((breaks[szuk_przedzial+1]-breaks[szuk_przedzial])/counts[szuk_przedzial])
  }
  
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

# war - wariancja obciążona
odchylenie_obciazone <- function(war)
{
  o <- sqrt(war)
  return(o)
}

moda <- function(vec)
{
  ux <- unique(vec) #tworzy wektor wartosci wystepujacych tylko raz
  
  if(length(ux) == length(vec))
  {
    return("Brak")
  }
  else
  {
    wek_ilosci_powtorzen <- tabulate(match(vec, ux)) #liczy ilosc powtorzen wszystkich elementow
    czy_zduplikowane <- duplicated(wek_ilosci_powtorzen, incomparables = FALSE, fromLast = TRUE)
    poz_mody <- which.max(wek_ilosci_powtorzen)
    
    if(czy_zduplikowane[poz_mody])
    {
      return("Brak")
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

# sr - srednia proby
# odch - odchylenie standardowe próby
# wsp_ufnosci - poziom ufności podany w treści
przedzial_sredniej <- function(vec, sr, odch, wsp_ufnosci)
{
  if(wsp_ufnosci > 1 || wsp_ufnosci < 0)
    return("niepoprawny poziom ufnosci")
  
  ile <- length(vec)
  
  if(ile < 1)
    return("nie podano zadnych danych")
  
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
    return("niepoprawny poziom ufnosci")
  
  ile <- length(vec)
  
  if(ile < 1)
    return("nie podano zadnych danych")
  
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
    return("niepoprawny wektor przedzialu")
  
  # Odejmujemy dolną wartość przedziału od górnej
  blad_maksymalny <- (przedzial[2] - przedzial[1]) / 2
  precyzja <- (blad_maksymalny / wart_do_porownania) * 100
  
  if(precyzja < 5)
    return(sprintf("%f%% jest mniejsze od 5%%, wiec mozemy bezpiecznie stwierdzic, ze istnieje podstawy do uogolnienia", precyzja))
  else if(precyzja < 10)
    return(sprintf("%f%% jest miedzy 5%% a 10%%, wiec mozemy stwierdzic, ze istnieja podstawy do uogolnienia, jednak musimy pozostac ostrozni", precyzja))
  else
    return(sprintf("%f%% jest wieksze od 10%%, wiec nalezy odrzucic teze, ze istnieja podstawy do uogolnienia", precyzja))
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
  # H1 - wariancje są różne
  # Korzystamy ze statystyki F-Snedecora
  f <- war1_nieob / war2_nieob
  wart_f <- qf(1 - alfa / 2, ile1 - 1, ile2 - 1)
  
  # Gdy wartość statystyki jest większa od górnej granicy przedziału, nie mamy podstaw do odrzucenia hipotezy, w przeciwnym wypadku odrzucamy
  if(f < wart_f)
  {
    ile1i2bez2 <- ile1 + ile2 - 2
    
    # Test t-Studenta dla grup niezależnych o równej wariancji - http://www-users.mat.umk.pl/~gemini/2mieStat/2012/testy_teoria.pdf
    t <- (sr1 - sr2) / sqrt(((war1 * ile1bez1 + war2 * ile2bez1) * (ile1 + ile2)) / (ile1i2bez2 * ile1 * ile2))
    
    # Liczba stopni swobody przy odczycie - n1 + n2 - 2
    wart_t <- qt(1 - alfa, ile1i2bez2)
    
    # Gdy wartość jest mniejsza od dolnej granicy przedziału, nie ma podstaw do odrzucenia hipotezy
    if(t <= wart_t)
      h <- "Wartosc nie nalezy do przedzialu - przyjmujemy hipoteze zerowa - wartości przeciętne sa rowne."
    else
      h <- "Wartosc nalezy do przedzialu - odrzucamy hipoteze zerowa - wartość przeciętna pierwszego sklepu jest wieksza."
    
    return(sprintf("rowne odchylenia populacji, wiec: statystyka t = %f, przedzial <%f, ∞). %s", t, wart_t, h))
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
      h <- "Wartosc nie nalezy do przedzialu - przyjmujemy hipoteze zerowa - wartości przeciętne sa rowne."
    else
      h <- "Wartosc nalezy do przedzialu - odrzucamy hipoteze zerowa - wartość przeciętna pierwszego sklepu jest wieksza."
    
    return(sprintf("rozne odchylenia populacji, wiec: statystyka C = %f, przedzial <%f, ∞). %s", C, wart_C, h))
  }
}

test_kolmogorowa <- function(vec, sr, odch)
{
  ile <- length(vec)
  
  # F0(vec i)
  dystr_rozk_norm <- pnorm(vec, sr, odch)
  
  # Fn(vec i)
  dystr_emp <- 1:ile / ile
  
  # Wartość statystyki ze wzoru
  wart_kryt <- 0.881 / sqrt(ile)
  
  # Wartość statystyki
  dn = max(abs(dystr_emp - dystr_rozk_norm))
  
  if(dn <= wart_kryt || dn >= 1)
    return("brak podstaw do odrzucenia hipotezy zerowej - rozkład jest normalny.")
  else
    return("odrzucamy hipotezę zerową - rozkład nie jest normalny.")
}

# Wczytanie danych z plików
dane_sklepu_1 <- read.table("sklep1.txt", header=F, dec=",")
dane_sklepu_2 <- read.table("sklep2.txt", header=F, dec=",")

# Przekształcenie ich do posortowanych wektorów
dane_sklepu1_vec <- sort(c(dane_sklepu_1[[1]]))
dane_sklepu2_vec <- sort(c(dane_sklepu_2[[1]]))

# Histogramy
sklep1_hist <- hist(dane_sklepu1_vec, breaks = przedzialy_histogramu(dane_sklepu1_vec))
sklep2_hist <- hist(dane_sklepu2_vec, breaks = przedzialy_histogramu(dane_sklepu2_vec))

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
sklep1_cwart <- cwiartkowe(sklep1_q1,sklep1_q3)
sklep1_przec_sred <- przecietne_od_sredniej(dane_sklepu1_vec, sklep1_sr)
sklep1_przec_med <- przecietne_od_mediany(dane_sklepu1_vec, sklep1_med)
sklep1_rozstep <- rozstep(dane_sklepu1_vec)
sklep1_wsp_zmien <- wspolczynnik_zmiennosci(sklep1_sr, sklep1_odch)
sklep1_wsp_asym <- wspolczynnik_asymetrii(dane_sklepu1_vec, sklep1_sr, sklep1_odch)
sklep1_wsp_skos <- skosnosc(sklep1_sr, sklep1_med, sklep1_odch)
sklep1_kurt <- kurtoza(dane_sklepu1_vec, sklep1_sr, sklep1_odch)
sklep1_eksc <- eksces(sklep1_kurt)


sklep2_sr <- mean(dane_sklepu2_vec)
sklep2_med <- median(dane_sklepu2_vec)
sklep2_q1 <- quantile(dane_sklepu2_vec, 0.25)
sklep2_q3 <- quantile(dane_sklepu2_vec, 0.75)
sklep2_moda<- moda(dane_sklepu2_vec)
sklep2_war <- wariancja_obciazona(dane_sklepu2_vec, sklep2_sr)
sklep2_war_nieob <- var(dane_sklepu2_vec)
sklep2_odch <- odchylenie_obciazone(sklep2_war)
sklep2_odch_nieob <- sd(dane_sklepu2_vec)
sklep2_cwart <- cwiartkowe(sklep2_q1,sklep2_q3)
sklep2_przec_sred <- przecietne_od_sredniej(dane_sklepu2_vec, sklep2_sr)
sklep2_przec_med <- przecietne_od_mediany(dane_sklepu2_vec, sklep2_med)
sklep2_rozstep <- rozstep(dane_sklepu2_vec)
sklep2_wsp_zmien <- wspolczynnik_zmiennosci(sklep2_sr, sklep2_odch)
sklep2_wsp_asym <- wspolczynnik_asymetrii(dane_sklepu2_vec, sklep2_sr, sklep2_odch)
sklep2_wsp_skos <- skosnosc(sklep2_sr, sklep2_med, sklep2_odch)
sklep2_kurt <- kurtoza(dane_sklepu2_vec, sklep2_sr, sklep2_odch)
sklep2_eksc <- eksces(sklep2_kurt)

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
sklep1_skosn <- skosnosc(sklep1_sr_r, sklep1_med_r, sklep1_odch_ob_r)
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
sklep2_skosn <- skosnosc(sklep2_sr_r, sklep2_med_r, sklep2_odch_ob_r)
sklep2_kurt_r <- kurtoza_rozdzielczy(sklep2_hist$mids, sklep2_hist$counts, sklep2_sr_r, sklep2_odch_ob_r)
sklep2_eksc_r <- eksces(sklep2_kurt_r)

# Zad 2

wynik_kolmogorowa_1 <- test_kolmogorowa(dane_sklepu2_vec, sklep2_sr, sklep2_odch)
wynik_kolmogorowa_2 <- test_kolmogorowa(dane_sklepu2_vec, sklep2_sr, sklep2_odch)

# Zad 3

sklep1_przedzial_sredniej <- przedzial_sredniej(dane_sklepu1_vec, sklep1_sr, sklep1_odch, 0.95)
sklep1_precyzja_wzgledna <- precyzja_wzgledna(sklep1_przedzial_sredniej, sklep1_sr)

# Zad 4

sklep2_przedzial_odchylenia <- przedzial_odchylenia(dane_sklepu2_vec, sklep2_war, 0.95)
sklep2_precyzja_wzgledna <- precyzja_wzgledna(sklep2_przedzial_odchylenia, sklep2_odch)

# Zad 5
wynik_testu <- test_dwoch_srednich(dane_sklepu1_vec, dane_sklepu2_vec, sklep1_sr, sklep2_sr, sklep1_war, sklep2_war, sklep1_war_nieob, sklep2_war_nieob, 0.05)


#formatowanie

#zaokraglanie liczb, aby zmiescily sie w odpowiednich kolumnach
# Szeregi szczegółowe
sklep1_sr <- round(sklep1_sr,3)
sklep1_med <- round(sklep1_med,3)
if(sklep1_moda != "Brak") sklep1_moda <- round(sklep1_moda,3)
sklep1_q1 <- round(sklep1_q1,3)
sklep1_q3 <- round(sklep1_q3,3)
sklep1_war <- round(sklep1_war,3)
sklep1_war_nieob <- round(sklep1_war_nieob,3)
sklep1_odch <- round(sklep1_odch,3)
sklep1_odch_nieob <- round(sklep1_odch_nieob,3)
sklep1_cwart <- round(sklep1_cwart,3)
sklep1_przec_sred <- round(sklep1_przec_sred,3)
sklep1_przec_med <- round(sklep1_przec_med,3)
sklep1_rozstep <- round(sklep1_rozstep,3)
sklep1_wsp_zmien <- round(sklep1_wsp_zmien,3)
sklep1_wsp_asym <- round(sklep1_wsp_asym,3)
sklep1_wsp_skos <- round(sklep1_wsp_skos,3)
sklep1_kurt <- round(sklep1_kurt,3)
sklep1_eksc <- round(sklep1_eksc,3)


sklep2_sr <- round(sklep2_sr,3)
sklep2_med <- round(sklep2_med,3)
if(sklep2_moda != "Brak") sklep2_moda<- round(sklep2_moda,3)
sklep2_q1 <- round(sklep2_q1,3)
sklep2_q3 <- round(sklep2_q3,3)
sklep2_war <- round(sklep2_war,3)
sklep2_war_nieob <- round(sklep2_war_nieob,3)
sklep2_odch <- round(sklep2_odch,3)
sklep2_odch_nieob <-round(sklep2_odch_nieob,3)
sklep2_cwart <- round(sklep2_cwart,3)
sklep2_przec_sred <- round(sklep2_przec_sred,3)
sklep2_przec_med <- round(sklep2_przec_med,3)
sklep2_rozstep <- round(sklep2_rozstep,3)
sklep2_wsp_zmien <- round(sklep2_wsp_zmien,3)
sklep2_wsp_asym <- round(sklep2_wsp_asym,3)
sklep2_wsp_skos <-round(sklep2_wsp_skos,3)
sklep2_kurt <- round(sklep2_kurt,3)
sklep2_eksc <- round(sklep2_eksc,3)

# Szeregi rozdzielcze
sklep1_sr_r <- round(sklep1_sr_r,3)
sklep1_med_r <- round(sklep1_med_r,3)
sklep1_moda_r <- round(sklep1_moda_r,3)
sklep1_q1_r <- round(sklep1_q1_r,3)
sklep1_q3_r <- round(sklep1_q3_r,3)
sklep1_war_ob_r <- round(sklep1_war_ob_r,3)
sklep1_war_nob_r <- round(sklep1_war_nob_r,3)
sklep1_odch_ob_r <- round(sklep1_odch_ob_r,3)
sklep1_odch_nob_r <- round(sklep1_odch_nob_r,3)
sklep1_odch_cwr_r <- round(sklep1_odch_cwr_r,3)
sklep1_odch_sr_r <- round(sklep1_odch_sr_r,3)
sklep1_odch_med_r <- round(sklep1_odch_med_r,3)
sklep1_rozstep_r <- round(sklep1_rozstep_r,3)
sklep1_wsp_zmien_r <- round(sklep1_wsp_zmien_r,3)
sklep1_wsp_asym_r <- round(sklep1_wsp_asym_r,3)
sklep1_skosn <-  round(sklep1_skosn,3)
sklep1_kurt_r <- round(sklep1_kurt_r,3)
sklep1_eksc_r <- round(sklep1_eksc_r,3)

sklep2_sr_r <- round(sklep2_sr_r ,3)
sklep2_med_r <- round(sklep2_med_r,3)
sklep2_moda_r <- round(sklep2_moda_r,3)
sklep2_q1_r <- round(sklep2_q1_r,3)
sklep2_q3_r <- round(sklep2_q3_r,3)
sklep2_war_ob_r <- round(sklep2_war_ob_r,3)
sklep2_war_nob_r <- round(sklep2_war_nob_r,3)
sklep2_odch_ob_r <- round(sklep2_odch_ob_r,3)
sklep2_odch_nob_r <- round(sklep2_odch_nob_r,3)
sklep2_odch_cwr_r <- round(sklep2_odch_cwr_r,3)
sklep2_odch_sr_r <- round(sklep2_odch_sr_r,3)
sklep2_odch_med_r <- round(sklep2_odch_med_r,3)
sklep2_rozstep_r <- round(sklep2_rozstep_r,3)
sklep2_wsp_zmien_r <- round(sklep2_wsp_zmien_r,3)
sklep2_wsp_asym_r <- round(sklep2_wsp_asym_r,3)
sklep2_skosn <-  round(sklep2_skosn,3)
sklep2_kurt_r <- round(sklep2_kurt_r,3)
sklep2_eksc_r <- round(sklep2_eksc_r,3)

#tworzenie ramki danych
zad1opis <- c("srednia                            ",
              "mediana                            ",
              "moda/dominanta                     ",
              "Kwartyl pierwszy                   ",
              "Kwartyl trzeci                     ",
              "Wariancja obciazona                ",
              "Wariancja nieobciazona             ",
              "Odchylenie standardowe obciazone   ",
              "Odchylenie standardowe nieobciazone",
              "Odchylenie cwiartkowe              ",
              "Odchylenie przecietne od sredniej  ",
              "Odchylenie przecietne od mediany   ",
              "Rozstep                            ",
              "Wspolczynnik zmiennosci            ",
              "Wspolczynnik asymetrii             ",
              "Skosnosc                           ",
              "Kurtoza                            ",
              "Eksces                             ")

#wektory tworzace kolumny
S1_szczeg <- c(sklep1_sr ,sklep1_med ,sklep1_moda ,sklep1_q1 ,
               sklep1_q3 ,sklep1_war ,sklep1_war_nieob ,sklep1_odch ,
               sklep1_odch_nieob ,sklep1_cwart ,sklep1_przec_sred ,
               sklep1_przec_med ,sklep1_rozstep ,paste(sklep1_wsp_zmien, "%") ,
               sklep1_wsp_asym ,sklep1_wsp_skos ,sklep1_kurt ,sklep1_eksc )


S2_szczeg <- c(sklep2_sr ,sklep2_med ,sklep2_moda,sklep2_q1 ,sklep2_q3 ,
               sklep2_war ,sklep2_war_nieob ,sklep2_odch ,sklep2_odch_nieob ,
               sklep2_cwart ,sklep2_przec_sred ,sklep2_przec_med ,
               sklep2_rozstep ,paste(sklep2_wsp_zmien, "%") ,sklep2_wsp_asym ,
               sklep2_wsp_skos ,sklep2_kurt ,sklep2_eksc )

S1_rozdz <- c(sklep1_sr_r ,sklep1_med_r ,sklep1_moda_r ,sklep1_q1_r ,
              sklep1_q3_r ,sklep1_war_ob_r ,sklep1_war_nob_r ,
              sklep1_odch_ob_r ,sklep1_odch_nob_r ,sklep1_odch_cwr_r ,
              sklep1_odch_sr_r ,sklep1_odch_med_r ,sklep1_rozstep_r ,
              paste(sklep1_wsp_zmien_r, "%") ,sklep1_wsp_asym_r,sklep1_skosn,sklep1_kurt_r ,sklep1_eksc_r)


S2_rozdz <- c(sklep2_sr_r ,sklep2_med_r ,sklep2_moda_r , paste(sklep2_q1_r) ,
              sklep2_q3_r ,sklep2_war_ob_r ,sklep2_war_nob_r ,
              sklep2_odch_ob_r ,sklep2_odch_nob_r , paste(sklep2_odch_cwr_r),
              sklep2_odch_sr_r ,sklep2_odch_med_r ,sklep2_rozstep_r ,
              paste(sklep2_wsp_zmien_r, "%") ,sklep2_wsp_asym_r,sklep2_skosn,sklep2_kurt_r ,sklep2_eksc_r)



zad1 <- data.frame(opis = zad1opis, S1_szczeg, S2_szczeg,S1_rozdz,S2_rozdz)

zad2 <-  c(paste("Wynik testu Kolmogorowa - Lillieforse'a dla zestawu danych sklepu 1 (H0 - podane dane mają rozkład normalny H1 - nie mają): ",  wynik_kolmogorowa_1),
           paste("Wynik testu Kolmogorowa - Lillieforse'a dla zestawu danych sklepu 2 (H0 - podane dane mają rozkład normalny H1 - nie mają): ",  wynik_kolmogorowa_2))

zad3 <-  c(paste("Przedzial sredniej: (", sklep1_przedzial_sredniej[1]," , ", sklep1_przedzial_sredniej[2], ")"),
           paste("Precyzja wzgledna: ", sklep1_precyzja_wzgledna))

zad4 <-  c(paste( "Przedzial odchylenia: (" , sklep2_przedzial_odchylenia[1] , " , " , sklep2_przedzial_odchylenia[2] ,  ")"),
  paste("Precyzja wzgledna: ", sklep2_precyzja_wzgledna))

zad5 <-  c("Czy na poziomie istotnosci 0.05 mozna twierdzic, ze wartosc miesiecznych wydatkow, na jedna osobe, na pieczywo i produkty zbozowe sa wieksze dla klientow pierwszego marketu (sformulowac i zweryfikowac odpowiednia hipoteze)?",
           "H0 - m1 = m2",
           "H1 - m1 > m2",
           paste("Wynik testu: ", wynik_testu))


zad1
zad2
zad3
zad4
zad5