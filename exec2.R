
#LISTA 6 (5 pontos) – MEDIDAS RESUMO
aula1 <- read.csv2("dataset/aula1.csv")

colnames(aula1) =  c("idade","sexo","altura","peso","areconh","natsp","satiCel","timefut","satban","fuma")

#1) Para o conjunto de dados Questionário. Fazer em Excel.
#a. (2.0) Para as variáveis Idade, Altura e Peso. Calcular

#    i. Média, 
    data.frame ( mean(aula1$idade), mean(aula1$altura), mean(aula1$peso))
    
#    ii. Mediana, 
    data.frame ( median(aula1$idade), median(aula1$altura), median(aula1$peso))
    
#    iii. Quartis, 
    data.frame ( quantile(aula1$idade), quantile(aula1$altura), quantile(aula1$peso))
    
#    iv. Decil 4 e 7, 
    result = data.frame (quantile(aula1$idade, probs = c(0.4, 0.7)),
                        quantile(aula1$altura, probs = c(0.4, 0.7)),
                        quantile(aula1$peso, probs = c(0.4, 0.7)))
    rownames(result) <- c("idade.decil","altura.decil", "peso.decil")
    
#    v. Percentis 33, 64 e 79, 
    result = data.frame ( quantile(aula1$idade, probs = c(0.33, 0.64, 0.79)),
                        quantile(aula1$altura, probs = c(0.33, 0.64, 0.79),
                        quantile(aula1$peso, probs = c(0.33, 0.64, 0.79)))
    rownames(result) <- c("idade.percent","altura.percent", "peso.percent")
    
#    vi. Variância amostral,
    n = length(aula1[,1])
    result = data.frame ( var(aula1$idade) * ((n-1)/n), var(aula1$peso) * ((n-1)/n), var(aula1$altura) * ((n-1)/n))
    rownames(results) <- c("idade.var","altura.var", "peso.var")
    
#    vii. Desvio padrão amostral, 
    n = length(aula1[,1])
    result = data.frame ( sd(aula1$idade) * ((n-1)/n), sd(aula1$peso) * ((n-1)/n), sd(aula1$altura) * ((n-1)/n))
    rownames(result) = c("idade.sd","altura.sd", "peso.sd")
    
#    viii. Amplitude, 
    max(aula1$idade) - min(aula1$idade)
    max(aula1$peso) - min(aula1$peso)
    max(aula1$altura) - min(aula1$altura)
    
#    ix. Amplitude interquartil e 
    quantile(aula1$idade, probs = 0.75) - quantile(aula1$idade, probs = 0.25)
    quantile(aula1$peso, probs = 0.75) - quantile(aula1$peso, probs = 0.25)
    quantile(aula1$altura, probs = 0.75) - quantile(aula1$altura, probs = 0.25)
    
#    x. Coeficiente de variação.
     !!!!!!!!!!!!(aula1$idade - mean (aula1$idade))/sd(aula1$idade)
    
#b. (1.0) Para as variáveis Idade, Altura e Peso. Calcular a correlação 2 a 2 e interpretar esses valores.
    cor (aula1[,c(1,3,4)])
    
#c. (1.0) Para as variáveis Idade, Altura e Peso, desenhar o box-plot e interpretar (se seu Excel não permitir desenhar, fazer em R)
    boxplot (aula1[,c(1,3,4)])
    
#d. (1.0) Para as variáveis Time de Futebol e satisfação com a operadora de celular, indicar a moda.
    
    statmod <- function(x) {
        z <- table(as.vector(x))
        names(z)[z == max(z)]
    }
    
    statmod(aula1$timefut)
    statmod(aula1$satiCel)

#LISTA 7 (10 pontos) – MEDIDAS RESUMO – R – RH_empresa
    exec_1_1 <- read.csv2("dataset/exec_1_1.csv")
#1) (10 pontos) Para o conjunto de dados RH_empresa. Fazer em R
#a. (1.0) Para as variáveis Nível de satisfação, avaliação, número do projeto, média de horas trabalhadas e tempo de trabalho. Calcular mínimo, Q1, média, mediana, Q3 e máximo.
    summary(data.frame(exec_1_1$nivel_satisfacao, exec_1_1$num_projeto,exec_1_1$media_horas_mensais,exec_1_1$tempo_trabalho ))
    
#b. (1.0) Para as variáveis Nível de satisfação, avaliação, número do projeto, média de horas trabalhadas e tempo de trabalho. Calcular variância e desvio padrão amostral, amplitude e amplitude interquartil.
    var(exec_1_1$nivel_satisfacao)
    var(exec_1_1$num_projeto)
    var(exec_1_1$media_horas_mensais)
    var(exec_1_1$tempo_trabalho)
    
    quantile(exec_1_1$nivel_satisfacao, probs = 0.75) - quantile(exec_1_1$nivel_satisfacao, probs = 0.25)
    quantile(exec_1_1$num_projeto, probs = 0.75) - quantile(exec_1_1$num_projeto, probs = 0.25)
    quantile(exec_1_1$media_horas_mensais, probs = 0.75) - quantile(exec_1_1$media_horas_mensais, probs = 0.25)
    quantile(exec_1_1$tempo_trabalho, probs = 0.75) - quantile(exec_1_1$tempo_trabalho, probs = 0.25)
    
    max(exec_1_1$nivel_satisfacao) - min(exec_1_1$nivel_satisfacao)
    max(exec_1_1$num_projeto) - min(exec_1_1$num_projeto)
    max(exec_1_1$media_horas_mensais) - min(exec_1_1$media_horas_mensais) 
    max(exec_1_1$tempo_trabalho) - min(exec_1_1$tempo_trabalho)
    
    n = length(aula1[,1])
    result = data.frame ( sd(exec_1_1$nivel_satisfacao) * ((n-1)/n),
                          sd(exec_1_1$num_projeto) * ((n-1)/n),
                          sd(exec_1_1$media_horas_mensais) * ((n-1)/n),
                          sd(exec_1_1$tempo_trabalho) * ((n-1)/n))
    colnames(result) = c("nivel_satisfacao.sd","num_projeto.sd", "media_horas_mensais.sd", "tempo_trabalho.sd")
    result
    
#c. (2.0) Faça um gráfico de dispersão entre Nível de satisfação e avaliação e calcule a correlação entre elas e interprete.
    
    plot(exec_1_1$nivel_satisfacao ~ exec_1_1$avaliacao)
    cor (exec_1_1$nivel_satisfacao, exec_1_1$avaliacao)
    
#d. (2.0) Para cada faixa salarial, calcule a média para o nível de satisfação.
    
    tapply(exec_1_1$nivel_satisfacao, exec_1_1$nivel_salario, mean)
    aggregate(exec_1_1$nivel_satisfacao, list(exec_1_1$nivel_salario), mean)
    
#e. (2.0) Desenhe o box-plot para horas trabalhadas e interprete.
    
    boxplot(exec_1_1$media_horas_mensais)
    summary(exec_1_1$media_horas_mensais)
    
#f. (2.0) Desenhe um box-plot para avaliação avaliando de acordo para os que já saíram do trabalho e para os que não saíram e interprete.
    
    boxplot(exec_1_1$avaliacao ~exec_1_1$saida_trabalho)

#LISTA 8 (10 pontos) – MEDIDAS RESUMO – R – FIFA2017
    
    exec_1_2 <- read.csv2("dataset/exec_1_2.csv")
    
#1) (10 pontos) Para o conjunto de dados FIFA2017. Fazer em R
#a.(2.0) Para as variáveis tempo de clube, nota, altura, peso e idade. Calcular mínimo, Q1, média, mediana, Q3 e máximo.
    
    summary(data.frame(exec_1_2$tempo_clube_meses, exec_1_2$nota, exec_1_2$altura, exec_1_2$peso, exec_1_2$idade))
    
#b.(2.0) Para as variáveis tempo de clube, nota, altura, peso e idade. Calcular variância e desvio padrão amostral, amplitude e amplitude interquartil.
    
    var(exec_1_2$tempo_clube_meses)
    var(exec_1_2$nota)
    var(exec_1_2$altura)
    var(exec_1_2$peso)
    var(exec_1_2$idade)
    
    quantile(exec_1_2$tempo_clube_meses, probs = 0.75) - quantile(exec_1_2$tempo_clube_meses, probs = 0.25)
    quantile(exec_1_2$nota, probs = 0.75) - quantile(exec_1_2$nota, probs = 0.25)
    quantile(exec_1_2$altura, probs = 0.75) - quantile(exec_1_2$altura, probs = 0.25)
    quantile(exec_1_2$peso, probs = 0.75) - quantile(exec_1_2$peso, probs = 0.25)
    quantile(exec_1_2$idade, probs = 0.75) - quantile(exec_1_2$idade, probs = 0.25)
    
    max(exec_1_2$tempo_clube_meses) - min(exec_1_2$tempo_clube_meses)
    max(exec_1_2$nota) - min(exec_1_2$nota)
    max(exec_1_2$altura) - min(exec_1_2$altura) 
    max(exec_1_2$peso) - min(exec_1_2$peso)
    max(exec_1_2$idade) - min(exec_1_2$idade)
    
    n = length(aula1[,1])
    result = data.frame ( sd(exec_1_2$tempo_clube_meses) * ((n-1)/n),
                          sd(exec_1_2$nota) * ((n-1)/n),
                          sd(exec_1_2$altura) * ((n-1)/n),
                          sd(exec_1_2$peso) * ((n-1)/n),
                          sd(exec_1_2$idade) * ((n-1)/n))
    colnames(result) = c("tempo_clube_meses.sd","nota.sd", "altura.sd", "peso.sd", "idade.sd")
    result
    
#c.(2.0) Faça um gráfico de dispersão entre peso e altura e calcule a correlação entre elas e interprete.

    plot(exec_1_1$nivel_satisfacao ~ exec_1_1$avaliacao)
    cor (exec_1_1$nivel_satisfacao, exec_1_1$avaliacao)

#d.(2.0) Para cada classificação skill, calcule a média para (Dica: usar tapply)
    
#i.    Controle de bola
    
    tapply(exec_1_2$controle_bola, exec_1_2$classificacao_skill, mean)
    aggregate(exec_1_2$controle_bola, list(exec_1_2$classificacao_skill), mean)
    
#ii.    Drible
    
    tapply(exec_1_2$drible, exec_1_2$classificacao_skill, mean)
    
#iii.    Goleiro_posição.
    
    tapply(exec_1_2$goleiro_posicao, exec_1_2$classificacao_skill, mean)
    
#e. (2.0) Desenhe um box-plot para nota avaliando segundo a perna do jogador e interprete.
    
    boxplot(exec_1_2$perna_preferencia)
