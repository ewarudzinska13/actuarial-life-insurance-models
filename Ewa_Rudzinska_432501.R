#ROZKŁAD WYKŁADNICZY

#1. #funkcja przezycia dla rozkladu wykladniczego


# Wczytanie potrzebnej biblioteki
library(ggplot2)

# Definicja funkcji przeżycia
survival_function <- function(t, mu) {
  return(exp(-mu * t))
}

# Zdefiniowanie zakresu wartości t
t_values <- seq(0, 100, by = 1)

# Obliczenie prawdopodobieństw przeżycia dla różnych wartości mu
mu_values <- c(0.03, 0.05, 0.08)
survival_data <- data.frame()

for(mu in mu_values) {
  survival_data <- rbind(survival_data, data.frame(t = t_values, Survival = survival_function(t_values, mu), Rate = as.factor(mu)))
}

# Stworzenie wykresu funkcji przeżycia
ggplot(survival_data, aes(x = t, y = Survival, color = Rate)) +
  geom_line() +
  labs(title = "Funkcja przeżycia 0-latka dla różnych wartości parametru mu", x = "Wiek", y = "Funkcja przeżycia", color = "Wartość parametru (mu)") +
  theme_minimal()


#prawdopodobieństwo przeżycia dla x latka przez kolejne 10 lat 

# Definiowanie zakresu wieku
wiek <- seq(0, 100, by = 1)

# Trzy różne wartości parametru MI
mi_values <- c(0.03, 0.05, 0.08)
t <- 10  # Okres, przez który obliczamy przeżycie

# Obliczenie funkcji przeżycia dla stałego t i różnych wartości MI
przezycie_data <- data.frame()
for (mi in mi_values) {
  przezycie <- exp(-mi * t)
  przezycie_data <- rbind(przezycie_data, data.frame(wiek, Przezycie = rep(przezycie, length(wiek)), Mi = rep(mi, length(wiek))))
}

# Rysowanie wykresu
ggplot(przezycie_data, aes(x = wiek, y = Przezycie, color = as.factor(Mi))) +
  geom_line() +
  labs(title = "Prawdopodobieństwo przeżycia przez 10 lat w zależności od obecnego wieku",
       x = "Obecny wiek x",
       y = "Prawdopodobieństwo przeżycia przez 10 lat",
       color = "Wartość parametru mu") +
  theme_minimal()




##########################################################################################################


# 2.JSN - wykladniczy dla roznych mu

library(ggplot2)

# Definicja funkcji dla nowego wzoru
calculate_value <- function(age, mu, i) {
  return(mu / (log(1 + i) + mu))
}

# Stała stopa procentowa
i <- 0.05
# Zakres wieku od 0 do 100
age_range <- 0:100
# Wartości mu
mu_values <- c(0.03, 0.05, 0.08)

# Przygotowanie ramki danych do wykresu
value_data <- data.frame()

for(mu in mu_values) {
  value_data <- rbind(value_data, data.frame(Age = age_range, 
                                             Value = sapply(age_range, calculate_value, mu = mu, i = i), 
                                             Mu = as.factor(mu)))
}

# Tworzenie wykresu liniowego z wartościami dla różnych mu
ggplot(value_data, aes(x = Age, y = Value, color = Mu)) +
  geom_line() +
  labs(title = "Wartość JSN w zależności od wieku dla różnych wartości mu",
       x = "Wiek",
       y = "JSN",
       color = "Wartość mu") +
  theme_minimal()





#JSN - WYKŁADNICZY W ZALEŻNOŚCI OD WIEKU dla i=0.01, i=0.03, i=0.05
# Definicja funkcji dla wzoru
calculate_value <- function(age, i, mu) {
  return(mu / (log(1 + i) + mu))
}

# Stała wartość mu
mu <- 0.05
# Zakres wieku od 0 do 100
age_range <- 0:100
# Wartości stopy procentowej i
i_values <- c(0.03, 0.05, 0.08)

# Przygotowanie ramki danych do wykresu
value_data <- data.frame()

for(i in i_values) {
  # Obliczenie wartości dla danego i i całego zakresu wieku
  value_data <- rbind(value_data, data.frame(Age = age_range, 
                                             Value = sapply(age_range, calculate_value, i = i, mu = mu), 
                                             InterestRate = as.factor(i)))
}

# Tworzenie wykresu liniowego z wartościami dla różnych i
ggplot(value_data, aes(x = Age, y = Value, color = InterestRate)) +
  geom_line() +
  labs(title = "Wartość JSN w zależności od wieku dla różnych stóp procentowych i",
       x = "Wiek",
       y = "Wartość JSN",
       color = "Stopa procentowa i") +
  theme_minimal()




#JSN dla x=20 i różnych i 
library(ggplot2)

# Definicja funkcji JSN
jsn_function <- function(i, mu) {
  return(mu / (log(1 + i) + mu))
}

# Stała wartość mu
mu <- 0.05

# Zakres stóp procentowych od 0 do 0.2 z krokiem 0.005
i_values <- seq(0, 0.2, by = 0.005)

# Obliczenie wartości JSN dla danego wieku i różnych wartości stopy procentowej
jsn_values <- sapply(i_values, jsn_function, mu = mu)

# Stworzenie ramki danych do wykresu
data <- data.frame(InterestRate = i_values, JSN = jsn_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = JSN)) +
  geom_line() +
  labs(title = "Wartość JSN dla wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość JSN") +
  theme_minimal()



#JSN- WYKŁADNICZY W ZALEŻNOŚCI OD OKRESU UBEZPIECZENIA, dla różnych i 

# Definicja funkcji JSN
jsn_function <- function(n, mu, i) {
  delta <- log(1 + i)
  jsn <- (mu * (1 - exp(-n * (delta + mu))) / (delta + mu))
  return(jsn)
}

# Stała wartość mu
mu <- 0.05

# Zakres okresu ubezpieczenia n
n_values <- seq(1, 30, by = 1) # dla okresu od 1 do 30 lat

# Zakres stóp procentowych i
i_values <- c(0.03, 0.05, 0.08)

# Przygotowanie ramki danych do wykresu
data <- expand.grid(InsurancePeriod = n_values, InterestRate = i_values)
data$JSN <- mapply(jsn_function, n = data$InsurancePeriod, mu = mu, i = data$InterestRate)

# Generowanie wykresu
ggplot(data, aes(x = InsurancePeriod, y = JSN, color = as.factor(InterestRate))) +
  geom_line() +
  labs(title = "Wartość JSN dla rozkładu wykładniczego w zależności od okresu ubezpieczenia",
       x = "Okres ubezpieczenia n (w latach)",
       y = "Wartość JSN",
       color = "Stopa procentowa i") +
  theme_minimal()

####################################################################################################################
# 2.RENTA - wykladniczy dla roznych mu

# Definicja funkcji dla nowego wzoru
calculate_value <- function(age, mu, i) {
  return(1/ (log(1 + i) + mu))
}

# Stała stopa procentowa
i <- 0.05
# Zakres wieku od 0 do 100
age_range <- 0:100
# Wartości mu
mu_values <- c(0.03, 0.05, 0.08)

# Przygotowanie ramki danych do wykresu
value_data <- data.frame()

for(mu in mu_values) {
  value_data <- rbind(value_data, data.frame(Age = age_range, 
                                             Value = sapply(age_range, calculate_value, mu = mu, i = i), 
                                             Mu = as.factor(mu)))
}

# Tworzenie wykresu liniowego z wartościami dla różnych mu
ggplot(value_data, aes(x = Age, y = Value, color = Mu)) +
  geom_line() +
  labs(title = "Wartość renty w zależności od wieku dla różnych wartości mu",
       x = "Wiek",
       y = "Renta",
       color = "Wartość mu") +
  theme_minimal()


#RENTA - WYKŁADNICZY W ZALEŻNOŚCI OD WIEKU dla i=0.01, i=0.03, i=0.05
# Definicja funkcji dla wzoru
calculate_value <- function(age, i, mu) {
  return(1 / (log(1 + i) + mu))
}

# Stała wartość mu
mu <- 0.05
# Zakres wieku od 0 do 100
age_range <- 0:100
# Wartości stopy procentowej i
i_values <- c(0.03, 0.05, 0.08)

# Przygotowanie ramki danych do wykresu
value_data <- data.frame()

for(i in i_values) {
  # Obliczenie wartości dla danego i i całego zakresu wieku
  value_data <- rbind(value_data, data.frame(Age = age_range, 
                                             Value = sapply(age_range, calculate_value, i = i, mu = mu), 
                                             InterestRate = as.factor(i)))
}

# Tworzenie wykresu liniowego z wartościami dla różnych i
ggplot(value_data, aes(x = Age, y = Value, color = InterestRate)) +
  geom_line() +
  labs(title = "Wartość w zależności od wieku dla różnych stóp procentowych i",
       x = "Wiek",
       y = "Renta",
       color = "Stopa procentowa i") +
  theme_minimal()



#RENTA dla x=20 i różnych i 


# Definicja funkcji JSN
renta_function <- function(i, mu) {
  return(1 / (log(1 + i) + mu))
}

# Stała wartość mu
mu <- 0.05

# Zakres stóp procentowych od 0 do 0.2 z krokiem 0.005
i_values <- seq(0, 0.2, by = 0.005)

# Obliczenie wartości JSN dla danego wieku i różnych wartości stopy procentowej
renta_values <- sapply(i_values, renta_function, mu = mu)

# Stworzenie ramki danych do wykresu
data <- data.frame(InterestRate = i_values, renta = renta_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = renta)) +
  geom_line() +
  labs(title = "Wartość renty dla wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość renty") +
  theme_minimal()




#RENTA - WYKŁADNICZY W ZALEŻNOŚCI OD OKRESU UBEZPIECZENIA

# Definicja funkcji wartości renty
rent_value <- function(age, mu, i) {
  delta <- log(1 + i)
  return((1 - exp(-age * (delta + mu))) / (delta + mu))
}

# Stała wartość mu
mu <- 0.05

# Zakres wieku
age_range <- seq(0, 100, by = 1)

# Wartości stopy procentowej i
i_values <- c(0.03, 0.05, 0.08)

# Przygotowanie ramki danych do wykresu
data <- expand.grid(Age = age_range, InterestRate = i_values)
data$RentValue <- mapply(rent_value, age = data$Age, mu = mu, i = data$InterestRate)

# Generowanie wykresu z liniami dla każdej wartości stopy procentowej
ggplot(data, aes(x = Age, y = RentValue, group = InterestRate, color = as.factor(InterestRate))) +
  geom_line() +
  labs(title = "Wartość renty w zależności od wieku dla różnych stóp procentowych",
       x = "Wiek",
       y = "Wartość renty",
       color = "Stopa procentowa i") +
  theme_minimal()


##########################################################################################
#WARTOŚĆ SKŁADKI DLA ROZKŁADU WYKŁADNICZEGO W ZALEŻNOŚCI OD MU


# Definicja funkcji JSN
jsn_function <- function(mu, i) {
  delta <- log(1 + i)
  return(mu / (delta + mu))
}

# Definicja funkcji renty
rent_function <- function(mu, i) {
  delta <- log(1 + i)
  return(1 / (delta + mu))
}

# Stała wartość stopy procentowej i
i <- 0.05

# Zakres wartości mu
mu_values <- seq(0.01, 1, by = 0.01)

# Obliczenie wartości JSN i renty dla różnych wartości mu
jsn_values <- sapply(mu_values, jsn_function, i = i)
rent_values <- sapply(mu_values, rent_function, i = i)

# Obliczenie składki jako stosunek JSN do renty
premium_values <- jsn_values / rent_values

# Stworzenie ramki danych do wykresu
data <- data.frame(MortalityRate = mu_values, Premium = premium_values)

# Generowanie wykresu
ggplot(data, aes(x = MortalityRate, y = Premium)) +
  geom_line() +
  labs(title = "Wartość składki dla różnych wartości mu przy i = 0.05",
       x = "Intensywność śmiertelności mu",
       y = "Wartość składki") +
  theme_minimal()


#WARTOŚĆ OKRESOWEJ SKŁADKI 20-LETNIEJ NA ŻYCIE I DOŻYCIE DLA ROZKŁADU WYKŁADNICZEGO W ZALEŻNOŚCI OD WIEKU

# Stałe wartości
mu <- 0.05
i <- 0.05
n <- 20 # okres ubezpieczenia to 20 lat
delta <- log(1 + i)

# Definicja funkcji dla JSN okresowej i renty okresowej
jsn_okresowa <- function(mu, n, delta) {
  return((mu * (1 - exp(-n * (delta + mu))) / (delta + mu)) + exp(-n * (delta + mu)))
}

renta_okresowa <- function(mu, n, delta) {
  return((1 - exp(-n * (delta + mu))) / (delta + mu))
}

# Zakres wieku
age_range <- seq(0, 100, by = 1)

# Obliczenie wartości JSN okresowej i renty okresowej dla każdego wieku
jsn_values <- sapply(age_range, function(age) jsn_okresowa(mu, n, delta))
renta_values <- sapply(age_range, function(age) renta_okresowa(mu, n, delta))

# Obliczenie składki okresowej dla każdego wieku
skladka_okresowa_values <- jsn_values / renta_values

# Stworzenie ramki danych do wykresu
data <- data.frame(Age = age_range, PeriodicPremium = skladka_okresowa_values)

# Generowanie wykresu
ggplot(data, aes(x = Age, y = PeriodicPremium)) +
  geom_line() +
  labs(title = "Wartość składki okresowej dla 20-letniego okresu ubezpieczenia w zależności od wieku",
       x = "Wiek ubezpieczonego",
       y = "Wartość składki okresowej") +
  theme_minimal()


#WARTOŚĆ OKRESOWEJ SKŁADKI 20-LETNIEJ NA ŻYCIE I DOŻYCIE DLA ROZKŁADU WYKŁADNICZEGO W ZALEŻNOŚCI OD STOPY PROCENTOWEJ
library(ggplot2)

# Stałe wartości
mu <- 0.05
n <- 20 # okres ubezpieczenia to 20 lat

# Zakres stóp procentowych
i_values <- seq(0.01, 0.15, by = 0.005)

# Definicja funkcji dla JSN okresowej i renty okresowej
jsn_okresowa <- function(i, mu, n) {
  delta <- log(1 + i)
  return((mu * (1 - exp(-n * (delta + mu))) / (delta + mu)) + exp(-n * (delta + mu)))
}

renta_okresowa <- function(i, mu, n) {
  delta <- log(1 + i)
  return((1 - exp(-n * (delta + mu))) / (delta + mu))
}

# Obliczenie wartości JSN okresowej i renty okresowej dla różnych wartości i
jsn_values <- sapply(i_values, jsn_okresowa, mu = mu, n = n)
renta_values <- sapply(i_values, renta_okresowa, mu = mu, n = n)

# Obliczenie składki okresowej dla każdego i
skladka_okresowa_values <- jsn_values / renta_values

# Stworzenie ramki danych do wykresu
data <- data.frame(InterestRate = i_values, PeriodicPremium = skladka_okresowa_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = PeriodicPremium)) +
  geom_line() +
  labs(title = "Wartość składki okresowej dla 20-letniego okresu ubezpieczenia w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość składki okresowej") +
  theme_minimal()


#WARTOŚĆ OKRESOWEJ SKŁADKI N-LETNIEJ NA ŻYCIE I DOŻYCIE DLA ROZKŁADU WYKŁADNICZEGO W ZALEŻNOŚCI OD OKRESU UBEZPIECZENIA, DLA i = 0.03, 0.05, 0.08

# Stała wartość mu
mu <- 0.05

# Wartości stopy procentowej i
i_values <- c(0.03, 0.05, 0.08)

# Zakres okresu ubezpieczenia n
n_values <- seq(1, 50, by = 1)  # przykład dla okresu od 1 do 50 lat

# Definicja funkcji dla JSN okresowej i renty okresowej
jsn_okresowa <- function(n, mu, i) {
  delta <- log(1 + i)
  return((mu * (1 - exp(-n * (delta + mu))) / (delta + mu)) + exp(-n * (delta + mu)))
}

renta_okresowa <- function(n, mu, i) {
  delta <- log(1 + i)
  return((1 - exp(-n * (delta + mu))) / (delta + mu))
}

# Obliczenie wartości JSN okresowej i renty okresowej dla każdej kombinacji n i i
data <- expand.grid(n = n_values, i = i_values)
data$JSN <- mapply(jsn_okresowa, n = data$n, mu = mu, i = data$i)
data$Renta <- mapply(renta_okresowa, n = data$n, mu = mu, i = data$i)

# Obliczenie składki okresowej
data$PeriodicPremium <- data$JSN / data$Renta

# Przekształcenie stopy procentowej na czynnik, aby uzyskać różne kolory na wykresie
data$i <- as.factor(data$i)

# Generowanie wykresu
ggplot(data, aes(x = n, y = PeriodicPremium, group = i, color = i)) +
  geom_line() +
  labs(title = "Wartość składki okresowej w zależności od okresu ubezpieczenia dla różnych stóp procentowych",
       x = "Okres ubezpieczenia n (w latach)",
       y = "Wartość składki okresowej",
       color = "Stopa procentowa i") +
  theme_minimal()



###################################################################################################
##############################    ROKŁAD DE MOIVRE'A ##############################################
###################################################################################################

#funkcja przeżycia w zależności od wieku 
# Definicja funkcji przeżycia dla rozkładu de Moivre'a
survival_function <- function(x, w = 100) {
  if (x < 0 || x > w) {
    stop("Wiek powinien być w zakresie od 0 do ", w)
  }
  return(1 - (x / w))
}

# Tworzenie wektora wieków od 0 do 100
ages <- 0:100

# Obliczenie prawdopodobieństwa przeżycia dla każdego wieku
survival_probs <- sapply(ages, survival_function)

# Tworzenie wykresu
plot(ages, survival_probs, type = "l", col = "blue",
     xlab = "Wiek", ylab = "Prawdopodobieństwo przeżycia",
     main = "Funkcja przeżycia dla rozkładu de Moivre'a")



#funkacja przezycia dla rozkladu de Moivre'a, dla x latka przez 10 lat w zalezności od omega
library(ggplot2)

# Definicja funkcji przeżycia według rozkładu de Moivre'a
survival_function <- function(x, t, omega) {
  if (x + t > omega) {
    return(0)
  } else {
    return((omega - x - t) / (omega - x))
  }
}

# Wartości omega
omega_values <- c(90, 100, 110)

# Zakres wieku x od 0 do 100
x_values <- seq(0, 100, by = 1)
t <- 10  # Ile lat osoba ma przeżyć

# Przygotowanie danych do wykresu
data <- data.frame()
for (omega in omega_values) {
  survival_data <- sapply(x_values, survival_function, t = t, omega = omega)
  data <- rbind(data, data.frame(Age = x_values, Survival = survival_data, Omega = omega))
}

# Generowanie wykresu
ggplot(data, aes(x = Age, y = Survival, color = as.factor(Omega), group = Omega)) +
  geom_line() +
  scale_color_discrete(name = "Maksymalny\nwiek życia ω") +
  labs(title = "Prawdopodobieństwo przeżycia dla t=10 według rozkładu de Moivre'a",
       x = "Wiek osoby (x)",
       y = "Prawdopodobieństwo przeżycia do wieku x+10") +
  theme_minimal()


#JSN
# Funkcja JSN (składka jednorazowa) dla rozkładu de Moivre'a
jsn_de_moivre <- function(x, w, delta) {
  (1 - exp(-(w - x) * delta)) / ((w - x) * delta)
}

# Definiowanie zakresu wieku
wiek <- seq(0, 100, by = 1)

# Maksymalny wiek w populacji
w <- 100

# Parametr delta
delta <- 0.05

# Obliczanie JSN dla rozkładu de Moivre'a
jsn_data <- data.frame(Wiek = wiek, JSN = jsn_de_moivre(wiek, w, delta))

# Rysowanie wykresu
ggplot(jsn_data, aes(x = Wiek, y = JSN)) +
  geom_line() +
  labs(title = "JSN dla rozkładu de Moivre'a",
       x = "Wiek osoby",
       y = "Wartość JSN") +
  theme_minimal()



#JSN - W ZALEŻNOŚCI OD WIEKU dla i=0.03, i=0.05, i=0.01

library(ggplot2)
library(tidyr)

# Funkcja JSN (składka jednorazowa) dla rozkładu de Moivre'a z uwzględnieniem delta
jsn_de_moivre_z_delta <- function(x, w, delta) {
  (1 - exp(-(w - x) * delta)) / ((w - x) * delta)
}

# Parametry
wiek <- seq(0, 100, by = 1)  # Zakres wieku
w <- 100  # Maksymalny wiek w populacji

# Trzy różne stopy procentowe (i)
i_values <- c(0.01, 0.03, 0.05)

# Obliczanie JSN dla różnych stóp procentowych
jsn_data <- data.frame(Wiek = wiek)
for (i in i_values) {
  delta <- log(1 + i)
  jsn_data[paste0("JSN_i", i * 100)] <- jsn_de_moivre_z_delta(wiek, w, delta)
}

# Przekształcenie danych do formatu długiego
jsn_data_long <- gather(jsn_data, key = "Stopa", value = "JSN", -Wiek)

# Mapowanie etykiet dla stóp procentowych
labels_map <- setNames(paste0(i_values * 100, "%"), paste0("JSN_i", i_values * 100))

# Rysowanie wykresu
ggplot(jsn_data_long, aes(x = Wiek, y = JSN, color = Stopa)) +
  geom_line() +
  labs(title = "JSN dla rozkładu de Moivre'a z różnymi stopami procentowymi",
       x = "Wiek osoby",
       y = "Wartość JSN") +
  scale_color_manual(values = c("blue", "red", "green"),
                     name = "Stopa procentowa",
                     labels = labels_map[unique(jsn_data_long$Stopa)]) +
  theme_minimal() +
  theme(legend.position = "bottom")


#JSN dla 20-latka, w=100 w zależności od i 
# Stałe wartości
x <- 20
omega <- 100

# Zakres stóp procentowych
i_values <- seq(0.01, 0.15, by = 0.005)

# Obliczanie wartości JSN dla każdej stopy procentowej
jsn_values <- sapply(i_values, function(i) {
  delta <- log(1 + i)
  return((1 - exp(-delta * (omega - x))) / (delta * (omega - x)))
})

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, JSN = jsn_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = JSN)) +
  geom_line() +
  labs(title = "JSN dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Jednorazowa składka netto (JSN)") +
  theme_minimal()




#JSN na życie w zależności od okresu ubezpieczenia, dla x=20 i i=0.01, 0.03, 0.05

# Stałe wartości
x <- 20
omega <- 100
i_values <- c(0.01, 0.03, 0.05)

# Zakres okresu ubezpieczenia n
n_values <- seq(1, 80, by = 1)  # Zakładamy, że okres ubezpieczenia może wynosić od 1 do 80 lat

# Obliczanie wartości JSN dla każdej kombinacji stopy procentowej i i okresu n
jsn_data <- expand.grid(n = n_values, i = i_values)
jsn_data$delta <- log(1 + jsn_data$i)
jsn_data$JSN <- (1 - exp(-jsn_data$n * jsn_data$delta)) / ((omega - x) * jsn_data$delta)

# Tworzenie wykresu
ggplot(jsn_data, aes(x = n, y = JSN, group = as.factor(i), color = as.factor(i))) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "JSN w zależności od okresu ubezpieczenia n dla osoby w wieku 20 lat",
       x = "Okres ubezpieczenia n (w latach)",
       y = "Jednorazowa składka netto (JSN)") +
  theme_minimal()




#RENTA
#RENTA - W ZALEŻNOŚCI OD WIEKU dla i=0.01, i=0.03, i=0.05


# Funkcja renty bezterminowej dla rozkładu de Moivre'a w zależności od i 
renta_bezterminowa_de_moivre <- function(x, w, i) {
  delta <- log(1 + i)
  ((1 - (1 - exp(-delta * (omega - x))) / ((omega - x) * delta)) * (1 / delta))
}

# Parametry
w <- 100    # Maksymalny wiek w populacji
stopy_procentowe <- c(0.01, 0.03, 0.05)  # Trzy różne stopy procentowe
kolory <- c("blue", "red", "green")  # Kolory dla linii

# Zakres wieku
wieki <- seq(0, w, by = 1)

# Inicjalizacja pustej macierzy na wyniki
wyniki <- matrix(NA, nrow = length(wieki), ncol = length(stopy_procentowe))

# Obliczanie wartości renty bezterminowej dla różnych stóp procentowych
for (j in 1:length(stopy_procentowe)) {
  wyniki[, j] <- renta_bezterminowa_de_moivre(wieki, w, stopy_procentowe[j])
}

# Rysowanie wykresu
plot(wieki, wyniki[, 1], type = "l", col = kolory[1], xlab = "Wiek osoby", ylab = "Wartość renty",
     main = "Renta bezterminowa dla rozkładu de Moivre'a", ylim = c(0, max(wyniki, na.rm = TRUE)),
     xlim = c(0, max(wieki)), lwd = 2)
for (j in 2:length(stopy_procentowe)) {
  lines(wieki, wyniki[, j], col = kolory[j], lwd = 2)
}

# Dodawanie legendy
legend("topright", legend = paste("Stopa procentowa =", stopy_procentowe), col = kolory, lty = 1, lwd = 2, cex = 1.2)


#RENTA BEZTERMINOWA dla x=20 i różnych i 
# Stałe wartości
x <- 20
omega <- 100

# Zakres stóp procentowych
i_values <- seq(0.01, 0.15, by = 0.005)

# Funkcja do obliczania wartości renty
calculate_annuity <- function(i, omega, x) {
  delta <- log(1 + i)
  return((1 - (1 - exp(-delta * (omega - x))) / ((omega - x) * delta)) * (1 / delta))
}

# Obliczanie wartości renty dla każdej stopy procentowej
annuity_values <- sapply(i_values, calculate_annuity, omega = omega, x = x)

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, Annuity = annuity_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = Annuity)) +
  geom_line() +
  labs(title = "Wartość renty dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość renty") +
  theme_minimal()



# Funkcja renty okresowej dla rozkładu de Moivre'a dla różnego okresu ubezpieczenia

renta_okresowa_de_moivre <- function(n, x, w, i) {
  delta <- log(1 + i)
  ((1 - (1 - exp(-delta * n)) / ((omega - x) * delta) + (n / (omega - x) - 1) * exp(-n * delta)) * (1 / delta)) 
}

# Parametry
w <- 100    # Maksymalny wiek w populacji
i <- 0.05   # Stała stopa procentowa
x <- 50     # Wiek osoby
okresy <- seq(0, 50, by = 1)  # Zakres okresów

# Inicjalizacja pustej macierzy na wyniki
wyniki <- rep(NA, length(okresy))

# Obliczanie wartości okresowej renty dla różnych okresów
for (j in 1:length(okresy)) {
  wyniki[j] <- renta_okresowa_de_moivre(okresy[j], x, w, i)
}

# Rysowanie wykresu
plot(okresy, wyniki, type = "l", col = "blue", xlab = "Okres (n)", ylab = "Wartość okresowej renty",
     main = "Okresowa Renta dla rozkładu de Moivre'a (i=0.05)",
     ylim = c(0, max(wyniki, na.rm = TRUE)), xlim = c(0, max(okresy)), lwd = 2)


##########################################################################################
#SKŁADKA BEZTERMINOWA w zależności od wieku dla róznych i 
# Funkcja JSN dla rozkładu de Moivre'a

jsn_de_moivre <- function(x, w, i) {
  delta <- log(1 + i)
  (1 - exp(-delta * (w - x))) / ((w - x) * delta)
}

# Funkcja renty bezterminowej dla rozkładu de Moivre'a
renta_bezterminowa_de_moivre <- function(x, w, i) {
  delta <- log(1 + i)
  (1 - (1 - exp(-delta * (w - x))) / ((w - x) * delta)) * 1 / delta
}

# Parametry
w <- 100    # Maksymalny wiek w populacji
wieki_osoby <- seq(0, w, by = 1)  # Zakres wieku osoby
stopy_procentowe <- c(0.01, 0.03, 0.05)  # Trzy różne stopy procentowe
kolory <- c("blue", "red", "green")  # Kolory dla linii

# Inicjalizacja pustej macierzy na wyniki
wyniki <- matrix(NA, nrow = length(wieki_osoby), ncol = length(stopy_procentowe))

# Obliczanie składek dla różnych stóp procentowych
for (j in 1:length(stopy_procentowe)) {
  for (x in 1:length(wieki_osoby)) {
    jsn <- jsn_de_moivre(wieki_osoby[x], w, stopy_procentowe[j])
    renta <- renta_bezterminowa_de_moivre(wieki_osoby[x], w, stopy_procentowe[j])
    wyniki[x, j] <- jsn / renta
  }
}

# Rysowanie wykresu
plot(wieki_osoby, wyniki[, 1], type = "l", col = kolory[1], xlab = "Wiek osoby", ylab = "Składka (JSN/Renta)",
     ylim = c(0, max(wyniki, na.rm = TRUE)), xlim = c(0, max(wieki_osoby)), lwd = 2)
lines(wieki_osoby, wyniki[, 2], col = kolory[2], lwd = 2)
lines(wieki_osoby, wyniki[, 3], col = kolory[3], lwd = 2)
axis(1, at=seq(0, 100, by=10))  # Dodawanie podziałki na osi x

# Dodawanie legendy
legend("topright", legend = paste("Stopa procentowa =", stopy_procentowe), col = kolory, lty = 1, lwd = 2, cex = 1.2)

#WARTOŚĆ OKRESOWEJ SKŁADKI 20-LETNIEJ NA ŻYCIE I DLA ROZKŁADU DE'MOIVRE'A W ZALEŻNOŚCI OD WIEKU

library(ggplot2)

# Stałe wartości
omega <- 100
n <- 20
i_values <- c(0.01, 0.03, 0.05)

# Zakres wieku x od 0 do (omega - n)
x_values <- seq(0, omega - n, by = 1)

# Funkcja do obliczania JSN i renty
calculate_values <- function(x, i, n, omega) {
  delta <- log(1 + i)
  jsn <- (1 - exp(-n * delta)) / ((omega - x) * delta)
  renta <- (1 - (1 - exp(-delta * n)) / ((omega - x) * delta) + (n / (omega - x) - 1) * exp(-n * delta)) * (1 / delta)
  return(c(JSN = jsn, Renta = renta))
}

# Przygotowanie danych do wykresu
data <- expand.grid(Age = x_values, InterestRate = i_values)
data_values <- mapply(calculate_values, x = data$Age, i = data$InterestRate, MoreArgs = list(n = n, omega = omega), SIMPLIFY = FALSE)
data <- cbind(data, do.call(rbind, data_values))

# Obliczenie składki jako stosunku JSN do renty
data$Premium <- data$JSN / data$Renta

# Przekształcenie stopy procentowej na czynnik, aby uzyskać różne kolory na wykresie
data$InterestRate <- as.factor(data$InterestRate)

# Generowanie wykresu
ggplot(data, aes(x = Age, y = Premium, group = InterestRate, color = InterestRate)) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "Wartość składki w zależności od wieku",
       x = "Wiek (x)",
       y = "Wartość składki") +
  theme_minimal()



#WARTOŚĆ OKRESOWEJ SKŁADKI 20-LETNIEJ NA ŻYCIE DLA ROZKŁADU DE MOIVRE'A W ZALEŻNOŚCI OD STOPY PROCENTOWEJ (X=20)

# Stałe wartości
x <- 20
omega <- 100
n <- 20

# Zakres stóp procentowych
i_values <- seq(0.01, 0.2, by = 0.001)

# Funkcja do obliczania JSN i renty
calculate_values <- function(i, n, x, omega) {
  delta <- log(1 + i)
  jsn <- (1 - exp(-n * delta)) / ((omega - x) * delta)
  renta <- (1 - (1 - exp(-delta * n)) / ((omega - x) * delta) + (n / (omega - x) - 1) * exp(-n * delta)) * (1 / delta)
  return(jsn / renta)
}

# Obliczenie wartości składki dla każdej stopy procentowej
premium_values <- sapply(i_values, calculate_values, n = n, x = x, omega = omega)

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, Premium = premium_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = Premium)) +
  geom_line() +
  labs(title = "Wartość składki dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość składki") +
  theme_minimal()


#WARTOŚĆ OKRESOWEJ SKŁADKI N-LETNIEJ NA ŻYCIE  DLA ROZKŁADU DE MOIVRE'A W ZALEŻNOŚCI OD OKRESU UBEZPIECZENIA, DLA i = 0.01, 0.03, 0.05

library(ggplot2)

# Stałe wartości
x <- 20
omega <- 100
n <- 20

# Zakres stóp procentowych
i_values <- seq(0.01, 0.2, by = 0.001)

# Funkcja do obliczania JSN i renty
calculate_values <- function(i, n, x, omega) {
  v <- 1 / (1 + i)
  jsn <- (v / (omega - x)) * (1 - v^(n + 1)) / (1 - v)
  renta <- (1 - jsn) / (i / (1 + i))
  return(jsn / renta)
}

# Obliczenie wartości składki dla każdej stopy procentowej
premium_values <- sapply(i_values, calculate_values, n = n, x = x, omega = omega)

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, Premium = premium_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = Premium)) +
  geom_line() +
  labs(title = "Wartość składki dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość składki") +
  theme_minimal()


####################
#rozkład de Moivre'a - podejście dyskretne

###################################################################################################
##############################    ROKŁAD DE MOIVRE'A ##############################################
###################################################################################################

#JSN - W ZALEŻNOŚCI OD WIEKU dla i=0.03, i=0.05, i=0.08

# Wartości omega i stopy procentowe
omega <- 100
i_values <- c(0.01, 0.03, 0.05)

# Funkcja do obliczania JSN
calculate_jsn <- function(x, i, omega) {
  v <- 1 / (1 + i)
  return((1 / (omega - x)) * (v / (1 - v)) * (1 - v^(omega - x)))
}

# Zakres wieku x
x_values <- seq(0, omega - 1, by = 1)

# Przygotowanie danych do wykresu
data <- expand.grid(Age = x_values, InterestRate = i_values)
data$JSN <- mapply(calculate_jsn, data$Age, data$InterestRate, MoreArgs = list(omega = omega))

# Przekształcenie stopy procentowej na czynnik
data$InterestRate <- as.factor(data$InterestRate)

# Generowanie wykresu
ggplot(data, aes(x = Age, y = JSN, group = InterestRate, color = InterestRate)) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "Jednorazowa składka netto (JSN) w zależności od wieku",
       x = "Wiek (x)",
       y = "Jednorazowa składka netto (JSN)") +
  theme_minimal()


#JSN dla 20-latka, w=100 w zależności od i 

library(ggplot2)

# Stałe wartości
x <- 20
omega <- 100

# Zakres stóp procentowych
i_values <- seq(0.01, 0.15, by = 0.001)

# Obliczanie JSN dla każdej stopy procentowej
jsn_values <- sapply(i_values, function(i) {
  v <- 1 / (1 + i)
  return((1 / (omega - x)) * (v / (1 - v)) * (1 - v^(omega - x)))
})

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, JSN = jsn_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = JSN)) +
  geom_line() +
  labs(title = "Jednorazowa składka netto (JSN) dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "JSN") +
  theme_minimal()



#JSN na życie w zależności od okresu ubezpieczenia, dla x=20 i i=0.01, 0.03, 0.05?


# Stałe wartości
x <- 20
omega <- 100

# Wartości stopy procentowej i
i_values <- c(0.01, 0.03, 0.05)

# Zakres okresu ubezpieczenia n
n_values <- seq(1, omega - x, by = 1)  # Zakładamy, że okres ubezpieczenia może wynosić od 1 do (omega - x) lat

# Funkcja do obliczania JSN
calculate_jsn <- function(n, i, x, omega) {
  v <- 1 / (1 + i)
  jsn <- (v / (omega - x)) * ((1 - v^(n+1)) / (1 - v))
  return(jsn)
}

# Przygotowanie danych do wykresu
data <- expand.grid(n = n_values, i = i_values)
data$JSN <- mapply(calculate_jsn, data$n, data$i, MoreArgs = list(x = x, omega = omega))

# Przekształcenie stopy procentowej na czynnik, aby uzyskać różne kolory na wykresie
data$i <- as.factor(data$i)

# Generowanie wykresu
ggplot(data, aes(x = n, y = JSN, group = i, color = i)) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "JSN na życie w zależności od okresu ubezpieczenia dla osoby w wieku 20 lat",
       x = "Okres ubezpieczenia n (w latach)",
       y = "Jednorazowa składka netto (JSN)") +
  theme_minimal()


#RENTA
#wyznaczamy z jedynki aktuarialnej 
#RENTA - W ZALEŻNOŚCI OD WIEKU dla i=0.01, i=0.03, i=0.05
library(ggplot2)

# Stałe wartości
omega <- 100
i_values <- c(0.01, 0.03, 0.05)

# Zakres wieku x
x_values <- seq(0, omega, by = 1)

# Funkcja do obliczania JSN i renty
calculate_annuity <- function(x, i, omega) {
  v <- 1 / (1 + i)
  jsn <- (1 / (omega - x)) * (v / (1 - v)) * (1 - v^(omega - x))
  annuity <- (1 - jsn) / (i / (1 + i))
  return(annuity)
}

# Przygotowanie danych do wykresu dla każdej stopy procentowej
data <- data.frame(Age = rep(x_values, times = length(i_values)),
                   InterestRate = factor(rep(i_values, each = length(x_values))),
                   Annuity = unlist(lapply(i_values, function(i) sapply(x_values, calculate_annuity, i = i, omega = omega))))

# Generowanie wykresu
ggplot(data, aes(x = Age, y = Annuity, group = InterestRate, color = InterestRate)) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "Wartość renty w zależności od wieku dla różnych stóp procentowych",
       x = "Wiek ubezpieczonego (x)",
       y = "Wartość renty") +
  theme_minimal()



#RENTA BEZTERMINOWA dla x=20 i różnych i 
library(ggplot2)

# Stałe wartości
x <- 20
omega <- 100

# Zakres stóp procentowych
i_values <- seq(0.01, 0.15, by = 0.001)

# Funkcja do obliczania JSN i renty
calculate_annuity <- function(i, x, omega) {
  v <- 1 / (1 + i)
  jsn <- (1 - (v^(omega - x))) / ((omega - x) * i)
  annuity <- (1 - jsn) * ((1 + i) / i)
  return(annuity)
}

# Obliczanie wartości renty dla każdej stopy procentowej
annuity_values <- sapply(i_values, calculate_annuity, x = x, omega = omega)

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, Annuity = annuity_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = Annuity)) +
  geom_line() +
  labs(title = "Wartość renty dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość renty") +
  theme_minimal()



# Funkcja renty okresowej dla rozkładu de Moivre'a dla różnego okresu ubezpieczenia

# Stałe wartości
x <- 20
omega <- 100
i_values <- c(0.01, 0.03, 0.05)

# Zakres okresu ubezpieczenia n
n_values <- seq(1, omega - x, by = 1)  # od 1 do (omega - x)

# Obliczenie JSN i renty dla danego n i i
calculate_renta <- function(n, i, x, omega) {
  v <- 1 / (1 + i)
  jsn <- v / (omega - x) * (1 - v^(n+1)) / (1 - v)
  renta <- (1 - jsn) / (i / (1 + i))
  return(renta)
}

# Przygotowanie danych do wykresu dla każdej stopy procentowej
data <- expand.grid(n = n_values, i = i_values)
data$Renta <- mapply(calculate_renta, data$n, data$i, MoreArgs = list(x = x, omega = omega))

# Przekształcenie stopy procentowej na czynnik, aby uzyskać różne kolory na wykresie
data$i <- as.factor(data$i)

# Generowanie wykresu
ggplot(data, aes(x = n, y = Renta, group = i, color = i)) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "Wartość renty w zależności od okresu ubezpieczenia n dla osoby w wieku 20 lat",
       x = "Okres ubezpieczenia n (w latach)",
       y = "Wartość renty") +
  theme_minimal()

##########################################################################################
#SKŁADKA BEZTERMINOWA w zależności od wieku dla róznych i
# Funkcje obliczeniowe
library(ggplot2)
library(tidyr)

# Funkcje obliczeniowe
jsn_de_moivre <- function(x, omega, i) {
  v <- 1 / (1 + i)
  (1 / (omega - x)) * (v / (1 - v)) * (1 - v^(omega - x))
}

renta_bezterminowa_de_moivre <- function(jsn, i) {
  (1 - jsn) / (i / (1 + i))
}

# Parametry
omega <- 100    # Maksymalny wiek w populacji
wieki_osoby <- seq(0, omega, by = 1)  # Zakres wieku osoby
stopy_procentowe <- c(0.01, 0.03, 0.05)  # Trzy różne stopy procentowe
kolory <- c("blue", "red", "green")  # Kolory dla linii

# Inicjalizacja pustej macierzy na wyniki
wyniki <- matrix(NA, nrow = length(wieki_osoby), ncol = length(stopy_procentowe))

# Obliczanie składek dla różnych stóp procentowych
for (j in 1:length(stopy_procentowe)) {
  for (x in wieki_osoby) {
    jsn <- jsn_de_moivre(x, omega, stopy_procentowe[j])
    renta <- renta_bezterminowa_de_moivre(jsn, stopy_procentowe[j])
    wyniki[x + 1, j] <- jsn / renta
  }
}

# Rysowanie wykresu
plot(wieki_osoby, wyniki[, 1], type = "l", col = kolory[1], xlab = "Wiek osoby", ylab = "Składka",
     ylim = c(0, max(wyniki, na.rm = TRUE)), xlim = c(0, max(wieki_osoby)), lwd = 2)
for (j in 2:length(stopy_procentowe)) {
  lines(wieki_osoby, wyniki[, j], col = kolory[j], lwd = 2)
}

# Dodawanie legendy
legend("topright", legend = paste("i =", stopy_procentowe * 100, "%"), col = kolory, lty = 1, lwd = 2, cex = 0.8)


#WARTOŚĆ OKRESOWEJ SKŁADKI 20-LETNIEJ NA ŻYCIE I DLA ROZKŁADU DE'MOIVRE'A W ZALEŻNOŚCI OD WIEKU
library(ggplot2)

# Stałe wartości
omega <- 100
n <- 20
i_values <- c(0.01, 0.03, 0.05)

# Zakres wieku x od 0 do (omega - n)
x_values <- seq(0, omega - n, by = 1)

# Funkcja do obliczania JSN i renty
calculate_values <- function(x, i, n, omega) {
  v <- 1/(1 + i)
  jsn <- (v / (omega - x) * (1 - v^(n+1)) / (1 - v))
  renta <-  (1 - jsn) / (i / (1 + i))
  return(c(JSN = jsn, Renta = renta))
}

# Przygotowanie danych do wykresu
data <- expand.grid(Age = x_values, InterestRate = i_values)
data_values <- mapply(calculate_values, x = data$Age, i = data$InterestRate, MoreArgs = list(n = n, omega = omega), SIMPLIFY = FALSE)
data <- cbind(data, do.call(rbind, data_values))

# Obliczenie składki jako stosunku JSN do renty
data$Premium <- data$JSN / data$Renta

# Przekształcenie stopy procentowej na czynnik, aby uzyskać różne kolory na wykresie
data$InterestRate <- as.factor(data$InterestRate)

# Generowanie wykresu
ggplot(data, aes(x = Age, y = Premium, group = InterestRate, color = InterestRate)) +
  geom_line() +
  scale_color_discrete(name = "Stopa procentowa i") +
  labs(title = "Wartość składki w zależności od wieku",
       x = "Wiek (x)",
       y = "Wartość składki") +
  theme_minimal()


#WARTOŚĆ OKRESOWEJ SKŁADKI 20-LETNIEJ NA ŻYCIE I DOŻYCIE DLA ROZKŁADU DE MOIVRE'A W ZALEŻNOŚCI OD STOPY PROCENTOWEJ (X=20)
# Stałe wartości
library(ggplot2)

# Stałe wartości
x <- 20
omega <- 100
n <- 20

# Zakres stóp procentowych
i_values <- seq(0.01, 0.2, by = 0.001)

# Funkcja do obliczania JSN i renty
calculate_values <- function(i, n, x, omega) {
  v <- 1 / (1 + i)
  jsn <- (v / (omega - x)) * (1 - v^(n + 1)) / (1 - v)
  renta <- (1 - jsn) / (i / (1 + i))
  return(jsn / renta)
}

# Obliczenie wartości składki dla każdej stopy procentowej
premium_values <- sapply(i_values, calculate_values, n = n, x = x, omega = omega)

# Przygotowanie ramki danych
data <- data.frame(InterestRate = i_values, Premium = premium_values)

# Generowanie wykresu
ggplot(data, aes(x = InterestRate, y = Premium)) +
  geom_line() +
  labs(title = "Wartość składki dla osoby w wieku 20 lat w zależności od stopy procentowej",
       x = "Stopa procentowa i",
       y = "Wartość składki") +
  theme_minimal()


#WARTOŚĆ OKRESOWEJ SKŁADKI N-LETNIEJ NA ŻYCIE I DOŻYCIE DLA ROZKŁADU DE MOIVRE'A W ZALEŻNOŚCI OD OKRESU UBEZPIECZENIA, DLA i = 0.01, 0.03, 0.05
library(ggplot2)

# Stałe wartości
x <- 20
omega <- 100

# Zakres n (liczba lat)
n_values <- 1:60

# Stopy procentowe
i_values <- c(0.01, 0.03, 0.05)

# Funkcja do obliczania JSN i renty
calculate_values <- function(n, x, omega, i) {
  v <- 1 / (1 + i)
  jsn <- (v / (omega - x)) * (1 - v^(n + 1)) / (1 - v)
  renta <- (1 - jsn) / (i / (1 + i))
  return(jsn / renta)
}

# Przygotowanie ramki danych
data <- expand.grid(N = n_values, InterestRate = i_values)
data$Premium <- mapply(calculate_values, data$N, x, omega, data$InterestRate)

# Generowanie wykresu
ggplot(data, aes(x = N, y = Premium, color = as.factor(InterestRate))) +
  geom_line() +
  scale_color_manual(values = c("blue", "red", "green"),
                     name = "Stopa procentowa i",
                     labels = paste0(i_values * 100, "%")) +
  labs(title = "Wartość składki dla osoby w wieku 20 lat w zależności od n i stopy procentowej",
       x = "Liczba lat n",
       y = "Stosunek JSN do Renty") +
  theme_minimal()


####################



