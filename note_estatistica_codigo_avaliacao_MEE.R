###############################################################################################
#                        Modelagem de equa??es estruturais
###############################################################################################
##### Lavaan modeling
library(lavaan); library(semPlot)
################### Exerc?cio 1 ##################
dat <- read.csv2("dataset/dados_moda_sustentavel.csv", sep = ";", fileEncoding = "latin1"); str(dat)
levels(dat$ESCOLARIDADE) <- c( 0,1,2,3,4,5,6) 
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Ensino Fundamental Incompleto'] <- 0
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Ensino Fundamental Completo'] <- 1
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Ensino Médio Incompleto'] <- 2
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Ensino Médio Completo'] <- 3
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Ensino Superior Incompleto'] <- 4
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Ensino Superior Completo'] <- 5
dat$ESCOLARIDADE[dat$ESCOLARIDADE == 'Mestrado'] <- 6

dat$ESCOLARIDADE <- as.numeric(dat$ESCOLARIDADE)

levels(dat$IDADE) <- c(0,1,2,3,4) 
dat$IDADE[dat$IDADE == '17 a 20'] <- 0
dat$IDADE[dat$IDADE == '21 a 24'] <- 1
dat$IDADE[dat$IDADE == '25 a 30'] <- 2
dat$IDADE[dat$IDADE == '31 a 35'] <- 3
dat$IDADE[dat$IDADE == '36 ou mais'] <- 4

dat$IDADE <- as.numeric(dat$IDADE)

levels(dat$GENERO) <- c(0,1)
dat$GENERO[dat$GENERO == 'Feminino'] <- 0
dat$GENERO[dat$GENERO == 'Masculino'] <- 1

levels(dat$RENDA) <- c(0,1,2,3) 
dat$RENDA[dat$RENDA == 'Até 4 salários mínimos (R$ 3.816)'] <- 0
dat$RENDA[dat$RENDA == 'De 4 a 10 salários mínimos (R$ 3.816 a 9.540)'] <- 1
dat$RENDA[dat$RENDA == 'De 10 a 20 salários mínimos (R$ 9.540 a 19.080)'] <- 2
dat$RENDA[dat$RENDA == 'Mais de 20 salários mínimos (a cima de R$ 19.080)'] <- 3

dat$RENDA <- as.numeric(dat$RENDA)
# Step 1: Specify model
mod <- '
  CMS  =~ Q6 + Q7 + Q8 
  CMMA =~ Q9 + Q10 + Q11 + Q26
  ACS  =~ Q16 + Q17 + Q18 + Q26 + Q41
  RS   =~ Q20 + Q21 + Q22
  SP   =~ Q24 + Q25 + Q26 + Q8
  PE   =~ Q26 + Q27 + Q28
  RM   =~ Q32 + Q33 + Q34
  A    =~ Q35 + Q36 + Q37 + Q38
  IC   =~ Q39 + Q40 + Q41
  A    ~  CMMA + ACS + RS + PE + RM
  IC   ~ A + IDADE + ESCOLARIDADE + RENDA 
'
# Step 2: Estimate model
mod.fit <- sem(mod, data=dat, std.lv = TRUE);summary(mod.fit, fit.measures = TRUE)

# Step 3: Extract results
standardizedSolution(mod.fit, type = 'std.all')

# Step 4: Request Modification Indices
summary(mod.fit, modindices = TRUE)

# Standardized parameters:
semPlot::semPaths(mod.fit, "std", edge.label.cex = 1.5, curvePivot = TRUE, intercepts = FALSE)
