# haetaan data netistä
uciCar <- read.table('http://win-vector.com/dfiles/car.data.csv', sep = ',', header = TRUE)

# datan tutkiskelua

#tyyppi
class(uciCar)

# yhteenveto
summary(uciCar)
str(uciCar)
#inspect(uciCar)

# koko
dim(uciCar)

d <- read.table(paste('http://archive.ics.uci.edu/ml/', 'machine-learning-databases/statlog/german/german.data', sep = ''), stringsAsFactors = F, header = F)

#slicing
print(d[1:3,]) #kaikki sarakkeet
print(d[,1]) #kaikki rowit
print(d[1,1]) #rivi 1 sarake 1
print(d[2, 2:3]) # rivi 2 sarakkeet 2-3

# sarakkeiden nimet
colnames(uciCar)

colnames(d) <- c('Status.of.existing.checking.account',
                 'Duration.in.month',  'Credit.history', 'Purpose',
                 'Credit.amount', 'Savings account/bonds',
                 'Present.employment.since',
                 'Installment.rate.in.percentage.of.disposable.income',
                 'Personal.status.and.sex', 'Other.debtors/guarantors',
                 'Present.residence.since', 'Property', 'Age.in.years',
                 'Other.installment.plans', 'Housing',
                 'Number.of.existing.credits.at.this.bank', 'Job',
                 'Number.of.people.being.liable.to.provide.maintenance.for',
                 'Telephone', 'foreign.worker', 'Good.Loan')

colnames(d)


mapping <- list(
  'A40' = 'car (new)',
  'A41' = 'car (used)',
  'A42' = 'car (furniture/equipment)',
  'A43' = 'car (radio/television)',
  'A44' = 'car (domestic appliances)',
  'A45' = 'repairs',
  'A46' = 'education', 
  'A47' = '(vacation - does not exist?)',
  'A48' = 'retraining',
  'A49' = 'business'
  )

# as. funktiot tyypin muunnoksiin
for (i in 1:(dim(d)) [2]) {
  if(class(d[,i]) == 'character') {
    d[,i] <- as.factor(as.character(mapping[d[,i]]))
  }
}

# dollari korvaa hakasulkeet
t <- table(d$Purpose, d$Good.Loan)
t

# hox. yksi vai kaksi hakasulkua kun on kyseessä taulukot

# ladataan kirjasto
library(RJDBC)
library(RMySQL)
# voidaan käyttää myös require(), mutta library on varmempi

# ota yhteys MySQL-tietokantaan
mydb <- dbConnect(MySQL(), user = "root", password = "Passw0rd", dbname = "world", host = "172.31.17.68")

# listaa tablet tietokannassa
dbListTables(mydb)

# listaa fieldit
field <- dbListFields(mydb, "city")

# luo query
q <- dbGetQuery(mydb, "SELECT * FROM city;")

# luo dataframe tuloksista
tulokset_df <- data.frame(q)



setwd("C:/Users/mikkok/Desktop")
custdata <- read.table("custdata.tsv", header = T, sep = "\t")
summary(custdata)
summary(custdata$income)

install.packages("ggplot2")
library(ggplot2)

ggplot(custdata) + geom_histogram(aes(x = age), binwidth = 5, fill = "gray")

install.packages("scales")
library(scales)

# hox. selvitä densityn merkitys
ggplot(custdata) + geom_density( aes(x = income)) + scale_x_continuous(labels = dollar)

ggplot(custdata) + geom_bar(aes(x = marital.stat), fill="gray")

statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef) <- c("state.of.res", "count")
summary(statef)
statef <- transform(statef, state.of.res = reorder(state.of.res, count))
summary(statef)

ggplot(statef) + geom_bar(aes(x = state.of.res, y = count), stat = "identity", fill= "gray") + coord_flip() + theme(axis.text.y = element_text(size = rel(0.8)))

x <- runif(100)
y <- x^2 + 0.2*x
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) + geom_line()
