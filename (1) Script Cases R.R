
#Author: Larissa Souza 
#Date: 27/10/2023

#------------------- Cases: Importing Data in R ------------------------------------------

# Importing data sets:

# 1) R file (.RData) extension
# 2) Data related to the academic performance of 2,000 individuals
# Data source: Fávero & Belfiore (2017, Chapter 16)

load("(2) performance_student_school.RData")

# 3) Excel file (using readxl - tidyverse)
# Monthly closing prices of 4 stocks

library(readxl)
price <- read_excel("(2) stock_prices.xlsx")

# 4) CSV format
# World Bank data on GDP growth
# Data source: https://databank.worldbank.org/

gdp_countries <- read.csv("(2) gdp_countries.csv",
                          sep = ",",
                          dec = ".")

# Additional arguments in this function were:
# The separator (,) and the indication of decimals (.)

# 5) Collecting data directly from links
# Commercial dollar exchange rate (buying and selling) - daily rates
# Source: Central Bank of Brazil

dollar <- read.csv("https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoDolarPeriodo(dataInicial=@dataInicial,dataFinalCotacao=@dataFinalCotacao)?@dataInicial='01-01-2020'&@dataFinalCotacao='12-31-2022'&$top=10000&$format=text/csv&$select=cotacaoCompra,cotacaoVenda,dataHoraCotacao",
                  sep = ",")

# Arguments are: the link and the variable separator (,)


# 6) Importing an SPSS file (.sav)
# The data contains grades of 100 students in 4 different subjects
# Data source: Fávero & Belfiore (2017, Chapter 10)


#------------------- Data Manipulation and Queries -----------------------------------

# Academic performance database of students

load("(2) performance_student_school.RData") # If already loaded, not necessary

View(performance_student_school)

# Look only at the initial part of the database (first 10 rows)

head(performance_student_school, n = 10)

# Names of available variables

names(performance_student_school)

# Number of variables and observations in the database

nrow(performance_student_school) # observations (rows)
ncol(performance_student_school) # variables (columns)
dim(performance_student_school) # both together (rows, columns)

# Structure of the database

str(performance_student_school)

# Specification of a variable
# data_object_name$variable

performance_student_school$performance # Only print the variable

study_hours <- performance_student_school$hours # Storing in a vector

# To remove an object from the environment, it can be done as follows:

rm(study_hours)

# It is possible to locate information from the database as follows:
# The first argument is the row number, and after the comma, the column position

# What is the value of academic performance (variable in the 3rd column) of the 1st student?
performance_student_school[1 , 3]

# What are the values of all variables for the 5th student?
performance_student_school[5 , ]

# What are the values of all variables for students 7 to 12?
performance_student_school[c(7:12), ]

# What are the observations for the variable in the 6th column (school type)?
performance_student_school[ , 6]

# In the above case, it could also be indicated by the variable name
performance_student_school[ , "priv"]

# Reorganizing the order of variables
reorganize <- performance_student_school[ , c(2,5,3,4,1,6)]

# Store only the student (id), performance, and hours variables
data_part <- performance_student_school[ , c("student", "performance", "hours")]

# The variables "hours" and "experience time" can be excluded
new_data_1 <- performance_student_school[ , -c(4,5)]

# Assuming that rows 476 to 522 will not be analyzed
new_data_2 <- performance_student_school[ -c(476:522), ]

# Filtering observations using operators:
# Some useful operators for filtering:

"== equal"
"> greater"
">= greater or equal"
"< less"
"<= less or equal"
"!= not equal"
"& indicates and"
"| indicates or"

performance_student_school[performance_student_school$performance > 50,]

performance_student_school[performance_student_school$priv == "public",]

performance_student_school[performance_student_school$priv == "public" &
                        performance_student_school$performance <= 20,]

performance_student_school[performance_student_school$school != "A",]

select_schools <- performance_student_school[performance_student_school$school == "C" |
                                             performance_student_school$school == "H",]

# Database of GDP by countries

gdp_countries <- read.csv("(2) gdp_countries.csv",
                       sep = ",",
                       dec = ".")

# Exclude variables that will not be used
# At the same time, exclude the last lines (267 to 271)

gdp_countries <- gdp_countries[-c(267:271),-c(2,4)]

# Change variable names

names <- c("year","countries_regions","gdp_per_capita_var","total_gdp_var")
names(gdp_countries) <- names

# Create new variables containing the values

gdp_countries$gdp_per_capita_adjusted <- as.numeric(gdp_countries$gdp_per_capita_var)
gdp_countries$total_gdp_adjusted <- as.numeric(gdp_countries$total_gdp_var)

# The "as.numeric" function was used together

# A detail: note that "warnings" appeared - this is not an error, it is information!

# Finally, we can generate some descriptive statistics of the two variables

summary(gdp_countries$gdp_per_capita_adjusted)
summary(gdp_countries$total_gdp_adjusted)



#------------------- Data Visualization ---------------------------------------------

# Package "ggplot2"

# "ggplot2" is a package for creating graphics that is part of the tidyverse
# (https://ggplot2.tidyverse.org/)
# ggplot2 was installed when installing the tidyverse. Now, just load it.

library(ggplot2) # Note: if tidyverse is already loaded, no need to load it again.

# In addition to ggplot2, the following are other useful packages:

install.packages(c("plotly","reshape2","ggrepel","rgl","car","sf","esquisse"))

options(rgl.debug = TRUE)

library(plotly)
library(reshape2)
library(ggrepel)
library(rgl)
library(car)
library(sf)
library(esquisse)

# --- Bar Chart ----------------------------------------------------------------------

# Qualitative variable, investor profile

load("(2) perfil_investidor.RData")

# As it is a categorical variable, let's create a bar chart (geom_bar)
# In this case, the chart will show the count in each category of the variable
# Note that in "aes" (aesthetics), the variable of interest is entered as an argument

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil))

# We could change the presentation order by rearranging the levels of the variable

perfil_investidor$perfil <- factor(perfil_investidor$perfil,
                                   levels = c("Conservative", 
                                              "Moderate", 
                                              "Aggressive"))

# The new chart would be:

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil))

# As indicated, many of the additional arguments are adjustments and formatting
# Addition of new legends to the chart

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil)) +
  labs(title = "Investor Profiles",
       subtitle = "Bank X",
       x = "Investor Profile",
       y = "Quantity",
       caption = "Period: 2020")

# Changing the color of the bars (argument "fill")

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), fill = "blue") +
  labs(title = "Investor Profiles",
       subtitle = "Bank X",
       x = "Investor Profile",
       y = "Quantity",
       caption = "Period: 2020")

# The possible colors available in the base language can be consulted

colours()

# Adding borders to the bars (argument "color")

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  labs(title = "Investor Profiles",
       subtitle = "Bank X",
       x = "Investor Profile",
       y = "Quantity",
       caption = "Period: 2020")

# Changing the background of the chart (theme)

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  labs(title = "Investor Profiles",
       subtitle = "Bank X",
       x = "Investor Profile",
       y = "Quantity",
       caption = "Period: 2020") +
  theme_dark()

# Adding more than one geometry to the chart
# Adding text (geom_text) containing the values that were counted

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", vjust = 2) +
  labs(title = "Investor Profiles",
       subtitle = "Bank X",
       x = "Investor Profile",
       y = "Quantity",
       caption = "Period: 2020") +
  theme_light()

# Presenting the chart with inverted axes

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", hjust = -1) +
  labs(title = "Investor Profiles",
       subtitle = "Bank X",
       x = "Investor Profile",
       y = "Quantity",
       caption = "Period: 2020") +
  coord_flip() +
  theme_light()

# --- Histogram ---------------------------------------------------------------

# Creating the histogram of student grades

load("(2) desempenho_aluno_escola.RData")

# Basic chart (geom_histogram)

ggplot(data = desempenho_aluno_escola) +
  geom_histogram(aes(x = desempenho))

# Adding some formatting

ggplot(data = desempenho_aluno_escola) +
  geom_histogram(aes(x = desempenho), color = "black", fill = "light green") +
  labs(x = "School Performance",
       y = "Frequency") +
  theme_bw()

# Formatting the number of bars presented

ggplot(data = desempenho_aluno_escola) +
  geom_histogram(aes(x = desempenho), color = "black", fill = "light green", bins = 50) +
  labs(x = "School Performance",
       y = "Frequency") +
  theme_bw()

# --- Scatter Plot --------------------------------------------------------------

# Scatter plot of points

load("(2) atlas_ambiental.RData")

# Starting with the basic chart (geom_point)
# Specification of the variables on the x and y axes in "aes"

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade))

# As there are variables on both axes, add other variables:

# In the form of point size ("size")

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, size = idade))

# In the form of point colors ("color")

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, size = idade, color = favel < 6))

# In the form of point shape ("shape")

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, 
                 size = idade, color = favel < 6, 
                 shape = mortalidade > 18)) +
  labs(title = "Indicators of Districts in the Municipality of São Paulo",
       x = "Income",
       y = "Education") +
  theme_bw()

# It is also possible to plot fitted values on the chart

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, 
                 size = idade, color = favel < 6, 
                 shape = mortalidade > 18)) +
  geom_smooth(aes(x = renda, y = escolaridade), method = "loess", se = FALSE) +
  labs(title = "Indicators of Districts in the Municipality of São Paulo",
       x = "Income",
       y = "Education") +
  theme_bw()

# --- Line Chart ------------------------------------------------------------------

# Creating a line chart (geom_line) for data over time

library(readxl)
preco_acao <- read_excel("(2) precos_acao.xlsx")

# As we have 4 stocks in the database, let's implement the following chart
# Note that we will separate each company by the color of the chart

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao))

# Let's format the chart a bit more

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  labs(x = "Reference Month",
       y = "Closing Quotation",
       title = "Historical Series of Stocks",
       color = "Company") +
  theme_classic()

# In addition, let's add points with the values of the series (geom_point)

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  geom_point(aes(x = data, y = preco, color = acao)) +
  labs(x = "Reference Month",
       y = "Closing Quotation",
       title = "Historical Series of Stocks",
       color = "Company") +
theme_classic()

# Adding labels for points within the chart becomes unfeasible

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  geom_point(aes(x = data, y = preco, color = acao)) +
  geom_text(aes(x = data, y = preco, label = preco), size = 3, vjust = 2, angle = 45) + 
  labs(x = "Reference Month",
       y = "Closing Quotation",
       title = "Historical Series of Stocks",
       color = "Company") +
theme_classic()

# To make the chart more interactive, use ggplotly
# Simply point the mouse cursor to the point to see information

ggplotly(
ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  geom_point(aes(x = data, y = preco, color = acao)) +
  labs(x = "Reference Month",
       y = "Closing Quotation",
       title = "Historical Series of Stocks",
       color = "Company") +
  theme_classic()
)


# --- Boxplot ----------------------------------------------------------------------

# The boxplot presents position measures of variables
# Minimum, maximum, 1st quartile, median, and 3rd quartile
# Distribution of data in variables and any univariate outliers

# Presentation of the boxplot of a single variable

load("(2) atlas_ambiental.RData")

var_boxplot <- atlas_ambiental[,c(1,3)]

ggplot(var_boxplot) +
  geom_boxplot(aes(y = renda), fill = "gray", color = "blue") +
  labs(x = "Income",
       y = "Values")

# Let's make it more informative with ggplotly

ggplotly(
ggplot(var_boxplot) +
  geom_boxplot(aes(y = renda), fill = "gray", color = "blue") +
  labs(x = "Income",
       y = "Values")
)

#---------------------------------------------------------------------------------

