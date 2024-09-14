install.packages("Hmisc")
library(Hmisc)

install.packages("dplyr")
library(dplyr)

install.packages("https://cran.r-project.org/src/contrib/Archive/ggpubr/ggpubr_0.2.4.tar.gz", repo=NULL, type="source")
install.packages("ggpubr")
library(ggpubr)

install.packages("car")
library(car)

install.packages("FSA")
library(FSA)

# 1) poradzenie z brakiem danych

dane <- read.csv2("przykladoweDane-Projekt.csv", sep = ";")
grupy <- as.vector(unique(dane$grupa))

kolumny <- c(1:ncol(dane))
ramka_charakterystyk <- data.frame(grupa = character(), kolumna = character(), min = numeric(), max = numeric(), mean = numeric(), sd = numeric(), var = numeric(), median = numeric())
rozklady <- data.frame(kolumna = character(), grupa = character(), statistic = numeric(), p.value = numeric())
wariancje <- data.frame(kolumna = character(), p.value = numeric())

for(i in grupy) {
  for(j in kolumny) {
    if(is.numeric(dane[, j])) {
      if(sum(is.na(dane[dane$grupa == i, j])) > 0) {
        dane[dane$grupa == i, j] <- impute(dane[dane$grupa == i, j], mean)
        cat(paste("\n", "Grupa", i, "posiada wartości puste w kolumnie", colnames(dane)[j], "- zastąpiono średnią dla tej grupy."))
      }
      if(colnames(dane)[j] != "wiek") {
        box_stats <- boxplot.stats(dane[dane$grupa == i, j])
        if(length(box_stats$out) != 0) cat(paste("\n", "Grupa", i, "w kolumnie", colnames(dane)[j], "posiada wartości odstające:", box_stats$out))
      }
    } else next
  }
}

# 2) charakterystyka grup

dane$grupa <- ordered(dane$grupa, levels = c("KONTROLA", "CHOR1", "CHOR2"))

pdf("Wykresy-charakterystyka.pdf")

for(j in kolumny) {
  if(is.numeric(dane[[j]])) {
    nazwa_kolumny <- colnames(dane)[j]
    
    # Statystyka opisowa
    podsumowanie_kolumny <- dane %>%
      group_by(grupa) %>%
      summarise(
        kolumna = nazwa_kolumny,
        min = round(min(get(nazwa_kolumny), na.rm = TRUE), 2),
        max = round(max(get(nazwa_kolumny), na.rm = TRUE), 2),
        mean = round(mean(get(nazwa_kolumny), na.rm = TRUE), 2),
        sd = round(sd(get(nazwa_kolumny), na.rm = TRUE), 2),
        var = round(var(get(nazwa_kolumny), na.rm = TRUE), 2),
        median = round(median(get(nazwa_kolumny), na.rm = TRUE), 2),
        .groups = "keep"
      )
    ramka_charakterystyk <- bind_rows(ramka_charakterystyk, podsumowanie_kolumny)
    
    boxplot(dane[[j]] ~ dane$grupa,
            xlab = "Grupa",
            ylab = nazwa_kolumny,
            main = "Charakterystyka statystyczna"
    )
    
    # Rozklad normalny
    for(i in grupy) {
      shapiro_result <- shapiro.test(dane[dane$grupa == i, j])
      rozklad_kolumny <- data.frame(
        kolumna = nazwa_kolumny,
        grupa = i,
        statistic = shapiro_result$statistic,
        p.value = shapiro_result$p.value
      )
      rozklady <- bind_rows(rozklady, rozklad_kolumny)
    }
    
    plotg <- ggdensity(dane, x = nazwa_kolumny,
                       color = "grupa", fill = "grupa",
                       palette = c("#99cc00", "#660099", "#0047b3"),
                       ylab = "gęstość",
                       xlab = nazwa_kolumny,
                       main = "Rozklad danych dla podanego parametru"
    ) + facet_wrap(~ grupa, scales = "free")
    
    print(plotg)
    
    # Jednorodność wariancji
    levene_test <- leveneTest(get(nazwa_kolumny) ~ grupa, data = dane)
    pvalue_wariancja <- levene_test$"Pr(>F)"[1]
    wariancja_kolumny <- data.frame(
      kolumna = nazwa_kolumny,
      p.value = pvalue_wariancja
    )
    wariancje <- bind_rows(wariancje, wariancja_kolumny)
  }
}
dev.off()
cat(paste("\n", "Zapisano plik z wykresami (Wykresy-charakterystyka.pdf) w katalogu roboczym"))

cat(paste("\n", "Wykaz charakterystyk opisowych określony dla każdej grupy i kategorii:", "\n"))
print(ramka_charakterystyk)

cat(paste("\n", "Wyniki zgodności danych z rozkładem normalnym (test Shapiro-Wilka) określony dla każdej grupy i kategorii:", "\n"))
print(rozklady)

cat(paste("\n", "Wykaz wariancji (test Levene'a):", "\n"))
print(wariancje)

# 3) Porownanie grup

czy_rozklad_normalny <- data.frame(kolumna = character(), rozklad_normalny = logical())

for(k in unique(rozklady$kolumna)) {
  kolumna_rozklady <- rozklady[rozklady$kolumna == k, ]
  pvalues <- kolumna_rozklady$p.value
  
  if(all(pvalues > 0.05)) {
    czy_rozklad_normalny <- bind_rows(czy_rozklad_normalny, data.frame(kolumna = k, rozklad_normalny = TRUE))
  } else {
    czy_rozklad_normalny <- bind_rows(czy_rozklad_normalny, data.frame(kolumna = k, rozklad_normalny = FALSE))
  }
}

czy_jednorodna_wariancja <- data.frame(kolumna = character(), jednorodna_wariancja = logical())

for(w in unique(wariancje$kolumna)) {
  pvalue <- wariancje$p.value[wariancje$kolumna == w]
  
  if(all(pvalue > 0.05)) {
    czy_jednorodna_wariancja <- bind_rows(czy_jednorodna_wariancja, data.frame(kolumna = w, jednorodna_wariancja = TRUE))
  } else {
    czy_jednorodna_wariancja <- bind_rows(czy_jednorodna_wariancja, data.frame(kolumna = w, jednorodna_wariancja = FALSE))
  }
}

test_anova <- list()
test_kruskala <- list()

for(i in czy_rozklad_normalny$kolumna) {
  if(czy_rozklad_normalny$rozklad_normalny[czy_rozklad_normalny$kolumna == i] == TRUE) {
    if(czy_jednorodna_wariancja$jednorodna_wariancja[czy_jednorodna_wariancja$kolumna == i] == TRUE) {
      test_anova <- append(test_anova, i)
    } else {
      test_kruskala <- append(test_kruskala, i)
    }
  } else {
    test_kruskala <- append(test_kruskala, i)
  } 
}

for(i in test_anova) {
  cat(paste("*******************************************************", "\n"))
  
  aov_wynik <- aov(dane[[i]] ~ grupa, data = dane)
  pvalue <- summary(aov_wynik)[[1]][1, "Pr(>F)"]
  
  if(pvalue < 0.05) {
    tukey_wynik <- TukeyHSD(aov_wynik)
    cat(paste(pvalue, "< 0.05 - są różnice między grupami w kolumnie", i, "\n"))
    print(tukey_wynik$grupa)
    cat(paste("Wartosc p adj < 0.05 oznacza istotna roznice pomiedzy podanymi grupami", "\n", "Wykonano testem Tukeya.", "\n"))
  } else {
    cat(paste("Pr(>F) =", pvalue, "\n", "Brak istotnych różnic w kolumnie", i, "znalezione przy użyciu testu ANOVA.\n"))
  }
}

for(i in test_kruskala) {
  cat(paste("*******************************************************", "\n"))
  
  kruskal_result <- kruskal.test(dane[[i]] ~ grupa, data = dane)
  pvalue <- kruskal.test(dane[[i]] ~ grupa, data = dane)$p.value
  
  if(pvalue < 0.05) {
    dunn_wynik <- dunnTest(dane[[i]], dane$grupa)
    cat(paste(pvalue, "< 0.05 - są różnice między grupami w kolumnie", i, "\n"))
    print(dunn_wynik)
    cat(paste("Wartosc P.adj < 0.05 oznacza istotna roznice pomiedzy podanymi grupami.", "\n", "Wykonano testem Dunna.", "\n"))
  } else {
    cat(paste("P-value =", pvalue, "\n", "Brak istotnych różnic w kolumnie", i, "znalezione przy użyciu testu Kruskala-Wallisa.\n"))
  }
}

# 4) Analiza korelacji

ramka_korelacji <- data.frame(grupa = character(), porownywana_para = character(), p.value = numeric(), r = numeric(), korelacja = character(), sila_korelacji = character(), metoda = character())

jaka_korelacja <- function(r) {
  if(r > 0) {
    return("dodatnia")
  } else if(r == 0) {
    return("brak korelacji")
  } else if(r < 0) {
    return("ujemna")
  }
}

sila_korelacji <- function(r) {
  if(-1 < r && r <= -0.7) {
    return("bardzo silna ujemna")
  } else if(-0.7 < r && r <= -0.5) {
    return("silna ujemna")
  } else if(-0.5 < r && r <= -0.3) {
    return("srednia ujemna")
  } else if(-0.3 < r && r <= -0.2) {
    return("slaba ujmena")
  } else if(-0.2 < r && r < 0.2) {
    return("brak korelacji")
  } else if(0.2 <= r && r < 0.3) {
    return("slaba dodatnia")
  } else if(0.3 <= r && r < 0.5) {
    return("srednia dodatnia")
  } else if(0.5 <= r && r < 0.7) {
    return("silna dodatnia")
  } else if(0.7 <= r && r < 1) {
    return("bardzo silna dodatnia")
  } else {
    return("nieprawidlowa wartosc korelacji")
  }
}

pary_pearson <- combn(unlist(test_anova), 2)
pary_spearman <- combn(unlist(test_kruskala), 2)

pdf("Wykresy-korelacja.pdf")

for(g in grupy) {
  for(p in 1:ncol(pary_pearson)) {
    p1 <- pary_pearson[1, p]
    p2 <- pary_pearson[2, p]
    wynik <- cor.test(dane[dane$grupa == g, p1], dane[dane$grupa == g, p2], method = "pearson")
    pvalue <- wynik$p.value
    
    if(pvalue < 0.05) {
      kor <- wynik$estimate
      jaka <- jaka_korelacja(kor)
      sila <- sila_korelacji(kor)
    
      podsumowanie_iteracji <- data.frame(
      grupa = g, 
      porownywana_para = paste(p1, "+", p2), 
      p.value = pvalue, 
      r = kor, 
      korelacja = jaka, 
      sila_korelacji = sila,
      metoda = "Pearsona",
      stringsAsFactors = FALSE)
    } else {
      podsumowanie_iteracji <- data.frame(
        grupa = g, 
        porownywana_para = paste(p1, "+", p2), 
        p.value = pvalue, 
        r = 0, 
        korelacja = "brak", 
        sila_korelacji = "brak",
        metoda = "Pearsona",
        stringsAsFactors = FALSE)
    }
    
    ramka_korelacji <- rbind(ramka_korelacji, podsumowanie_iteracji)
  }
}

for(p in 1:ncol(pary_pearson)) {
  p1 <- pary_pearson[1, p]
  p2 <- pary_pearson[2, p]
  
  gg <- ggscatter(dane, x = p1, y = p2, 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              color = "grupa", fill = "grupa",
              palette = c("#99cc00", "#660099", "#0047b3"),
              ylab = p1, 
              xlab = p2,
              main = "Wykres korelacji metoda Pearsona"
    )+ facet_wrap(~ grupa, scales = "free")
    
    print(gg)
}

for(g in grupy) {
  for(p in 1:ncol(pary_spearman)) {
    p1 <- pary_spearman[1, p]
    p2 <- pary_spearman[2, p]
    wynik <- cor.test(dane[dane$grupa == g, p1], dane[dane$grupa == g, p2], method = "spearman")
    pvalue <- wynik$p.value
    
    if(pvalue < 0.05) {
      kor <- wynik$estimate
      jaka <- jaka_korelacja(kor)
      sila <- sila_korelacji(kor)
      
      podsumowanie_iteracji <- data.frame(
        grupa = g, 
        porownywana_para = paste(p1, "+", p2), 
        p.value = pvalue, 
        r = kor, 
        korelacja = jaka, 
        sila_korelacji = sila,
        metoda = "Spearmana",
        stringsAsFactors = FALSE)
    } else {
      podsumowanie_iteracji <- data.frame(
        grupa = g, 
        porownywana_para = paste(p1, "+", p2), 
        p.value = pvalue, 
        r = 0, 
        korelacja = "brak", 
        sila_korelacji = "brak",
        metoda = "Spearmana",
        stringsAsFactors = FALSE)
    }
    
    ramka_korelacji <- rbind(ramka_korelacji, podsumowanie_iteracji)
  }
}

for(p in 1:ncol(pary_spearman)) {
  p1 <- pary_spearman[1, p]
  p2 <- pary_spearman[2, p]
  
  gg <- ggscatter(dane, x = p1, y = p2, 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "grupa", fill = "grupa",
              palette = c("#99cc00", "#660099", "#0047b3"),
              ylab = p1, 
              xlab = p2,
              main = "Wykres korelacji metoda Spearmana"
    ) + facet_wrap(~ grupa, scales = "free")
    
    print(gg)
}

dev.off()
cat(paste("\n", "Aby wyswietlic wykresy zapisano plik z wykresami (Wykresy-korelacja.pdf) w katalogu roboczym"))


cat(paste("Wykaz korelacji"))
print(ramka_korelacji)

cat(paste("Istotne korelacje"))
istotne_korelacje <- ramka_korelacji[ramka_korelacji$korelacja != "brak", ]
print(istotne_korelacje)




