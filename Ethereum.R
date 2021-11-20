### Projekt ###

# Przewidywanie ceny Ethereum w USD

# Zobaczmy jak dane wyglądają na wykresie

ETH <- read.csv(file='ETH.csv')
head(ETH)
average = ETH[,7]
average.ts <- ts(average, start = 1, frequency = 1)
plot.ts(average.ts, main = "Wartość ETH",xlab = "Dni",ylab = "USD", col = "black")

# z powodu dużej ilości danych skupimy się na krótszym przedziale czasu

average.ts <- ts(average[c(848:892)], start = 1, frequency = 1)
plot.ts(average.ts, main = "Wartość ETH",xlab = "Dni",ylab = "USD", col = "black")

# Wyznaczymy w którym dniu wartość ETH była najwyższa

max(average)
which.max(average)
dane <- ETH[,c(1,7)]
dane[848,] # początek przedziału
dane[892,] # maksymalna wartość

# wartości dla dni: 2017-12-01, 2018-01-04, 2018-01-14

przedział <- dane[c(848, 882, 892),]

# Będziemy pracować na przedziale od 2017-12-01 do 2018-01-04 (35 dni)

dane <- dane[c(848:882),2]
length(dane)

wektor <- c(1:35)
length(wektor)

# Spróbujemy przewidzieć maksymalną wartość ETH w dniu 2018-01-14,
# wartość teoretyczna dla argumentu 45.

# (1) regresja wykładnicza

model_1 <- lm(log10(dane)~wektor)
c = 10^coef(model_1)
wynik_1 <- c[1]*c[2]^45
wynik_1

x <- c(1:45)
curve(c[1]*c[2]^x, add = T, col = "blue") 
# niebieska krzywa na wykresie

### błąd ###

błąd_1 <- abs(max(average)-wynik_1)
błąd_1

# (2) regresja liniowa

model_2 <- lm(dane~wektor)
sum(resid(model_2)^2)
wynik_2 <- predict(model_2, data.frame(wektor=45))
wynik_2

curve(predict(model_2, data.frame(wektor=x)), add = T, col = "green") 
# zielona linia na wykresie

### błąd ###

błąd_2 <- abs(max(average)-wynik_2)
błąd_2

# (3) regresja wieloraka

model_3 <- lm(dane~wektor+I(wektor^2)+I(wektor^3))
sum(resid(model_3)^2)
wynik_3 <- predict(model_3, data.frame(wektor=45))
wynik_3

curve(predict(model_3, data.frame(wektor=x)), add = T, col = "red")
# czerwona krzywa na wykresie

### błąd ###

błąd_3 <- abs(max(average)-wynik_3)
błąd_3

### Wnioski ###

# Jak widać niebieska krzywa najbardziej zbliżyła się do wartości maksymalnej

wyniki <- as.table(setNames(c(błąd_1, błąd_2, błąd_3), c(1:3)))
wyniki

# Najmniejszy błąd otrzymaliśmy używając regresji wykładniczej i wyniósł on 248.74 USD
min(wyniki)

# Wykres dla kolejnych dni (te same regresje):

average.ts <- ts(average[c(848:920)], start = 1, frequency = 1)
plot.ts(average.ts, main = "Wartość ETH",xlab = "Dni",ylab = "USD", col = "black")
curve(c[1]*c[2]^x, add = T, col = "blue")
curve(predict(model_2, data.frame(wektor=x)), add = T, col = "green") 
curve(predict(model_3, data.frame(wektor=x)), add = T, col = "red")

# Po dniu 2018-01-14 wartość ETH zaczęła drasycznie spadać 
# (co możemy zaobserwować na wykresie) czego przyczyną była
# chęć regulacji rynku kryptowalut przez rządy wielu krajów w tym USA.
# Warto zauważyć, że każda z linii przetnie czarną linię, która reprezentuje
# realną wartość ETH.
# Modele te będą jednak mało użyteczne w skali długoterminowej ponieważ trudno
# przewidzeć wpływ efektów losowych na kurs kryptowalut.
