#--------------------Exercicio1------------------------
#---------------------LetraA--------------------------

a <- (10:30)

#---------------------LetraB-------------------------

b <- (30:10)

#--------------------LetraC--------------------------

c <- c(a,b)


#-------------------Exercicio2-----------------------
#---------------------LetraA-------------------------

vet <- seq(2, 8, by = 2)
vet10 <- rep(vet,10)
vet10

#---------------------LetraB--------------------------

vet11 <- rep(vet, length.out = 41)
vet11

#-------------------Exercicio3------------------------
#---------------------LetraA--------------------------

soma1 <- 0

for (n in 20:30){
  soma1 <- soma1 + ((n**2) + (4*n))  
}
soma1

#---------------------LetraB--------------------------

soma2 <- 0

for (n in 10:20){
  soma2 <- soma2 + (((3**n) / n) + ((2**n) / (n**2)))
}
soma2

#-------------------Exercicio4------------------------

bolas <- sample(1:100, size = 40, replace = TRUE)
bolas

#---------------------LetraA--------------------------

bolasPar <- bolas[bolas %% 2 == 0]
length(bolasPar)

#---------------------LetraB--------------------------

bolasMaiorQue70 <- bolas[bolas > 70]
length(bolasMaiorQue70)

#---------------------LetraC--------------------------

posImpares <- which(bolas %% 2 != 0)
posImpares

#--------------------Exercicio5------------------------

simularArremessos <- function(){
  tentativas <- 0
  nroObservacoes4 <- 0
  
  while(nroObservacoes4 < 2){
    dado <- sample(1:6, 1)
    
    tentativas <- tentativas + 1
    
    if(dado == 4){
      nroObservacoes4 <- nroObservacoes4 + 1
    }
  }
  return(tentativas)
}
qtdade <- simularArremessos()

cat("Foram necessárias", qtdade, "vezes para observar 2 vezes o número 4.")

#--------------------Exercicio6------------------------

quantidades <- c()

for(i in 1:10000){
  quantidades <- c(quantidades,simularArremessos())
}

cat("A média de vezes necessárias para ter duas observações do número 4 é ", mean(quantidades))

#--------------------Exercicio7------------------------

fibonacci <- function(n){
  for(i in 1:n){
    print(fibonacci1(i))
  }
}

fibonacci1 <- function(n){
  if(n == 1 | n == 2){
    return(1);
  }
  else{
    return(fibonacci1(n-2) + fibonacci1(n-1))
  }
}

#--------------------Exercicio8------------------------

amgSecreto <- function() {
  
  participantes <- c("Michael","Dwight","Jim","Kevin","Creed")
  
  sucessos <- 0 
  
  
  for(i in 1:100000){
    
    falha <- 0
    
    embaralha <- sample(x = participantes, replace = FALSE, size = 5)
    sorteio <- sample(x = participantes, replace = FALSE, size = 5)
    
    for(k in 1:5) {
      if(embaralha[k] == sorteio[k]) {
        falha <- falha + 1
      }
    }
    
    if(falha == 0) {
      sucessos <- sucessos + 1
    }
  
  }
  
  cat("Numero de sucessos:",sucessos)
  cat("\nNumero de falhas:",i-sucessos)
  cat("\nProporção de acertos:",((sucessos/i)*100),"%\nProporção de erros:",100-((sucessos/i)*100),"%")
  
}
  
amgSecreto()

#--------------------Exercicio9------------------------  

jogoCraps <- function() {
  
  vitorias <- 0
  derrotas <- 0
  
  for(i in 1:100000) {
    
    dado1 <- sample(x = 1:6, replace = FALSE, size = 2)
    
    if(sum(dado1) == (7 || 11)) {
      vitorias <- vitorias + 1
    } 
    else if(sum(dado1) == (2 || 3 || 12)) {
      derrotas <- derrotas + 1
      vitorias <- vitorias + 0
      
    } else {
      
      while(1) {
        dado2 <- sample(x = 1:6, replace = FALSE, size = 2)
        if(sum(dado2) == 7) {
          derrotas <- derrotas + 1
          vitorias <- vitorias + 0
          break;
        }
        if(sum(dado2) == sum(dado1)) {
          vitorias <- vitorias + 1
          break;
        }
      }
    }
  }
  
  cat("Quantidade de vitórias:",vitorias,"\nProporção de vitórias:",vitorias/i,"\nPorcentagem:",(vitorias/i)*100,"%")
  
  cat("\nQuantidade de derrotas:",derrotas,"\nProporção de derrotas:",derrotas/i,"\nPorcentagem:",(derrotas/i)*100,"%")
  
}

jogoCraps()

#--------------------Exercicio10------------------------ 
#----------------------LetraA---------------------------

lukeSkywalker <- function(L){
  if(L >= 20){
    return(1)
  }
  if(L <= 0){
    return(0)
  }
  
  moeda <- sample(1:2 , 1)
  
  if(moeda == 1){
    return(lukeSkywalker(L-1))
  }
  else{
    return(lukeSkywalker(L+1))
  }
}

#----------------------LetraB---------------------------

StarWars <- function(L){
  vitoriasDerrotas <- c()
  for(i in 1:10000){
    vitoriasDerrotas <- c(vitoriasDerrotas,lukeSkywalker(L))
  }
  return(vitoriasDerrotas)
}

#----------------------LetraC---------------------------
library(ggplot2)

dados <- function(){
  temp <- c()
  for(L in 1:19){
    temp <- c(temp,mean(StarWars(L)))
  }
  return(temp)
}

grafico <- dados()
valoresL <- 1:19  


df <- data.frame(L = valoresL, Media = grafico)


ggplot(df, aes(x = L, y = Media)) +
  geom_col(fill = "steelblue") +
  labs(title = "Média de Vitórias/Derrotas por Valor Inicial L",
       x = "Valor Inicial L",
       y = "Média de Vitórias/Derrotas") +
  theme_minimal()

#--------------------Exercicio11------------------------ 

passos <- c("L", "R", "U", "D")


#--------------------Letra A------------------------ 

caminhoLink <- function(){
  link <- c(0,0)
  for(i in 1:8) {
    zelda <- sample(x = passos, replace = TRUE, size = 1)
    
    if(zelda == "L") {
      link[2] = link[2] - 1
    }
    
    if(zelda == "R") {
      link[2] = link[2] + 1
    }
    
    if(zelda == "U") {
      link[1] = link[1] + 1
    }
    
    if(zelda == "D") {
      link[1] = link[1] - 1
    }
    
    
  }

  return(link)
}

#--------------------Letra B------------------------

countOrigem <- 0
qtd <- 10000

for(i in 1:qtd){
  camLink <- caminhoLink()
  if(camLink[1] == 0 && camLink[2] == 0){
    countOrigem <- countOrigem + 1
  }
}
proporcao <- countOrigem/qtd

#Essa proporção significa que as chances de, após 8 passos, Link voltar para a origem são bem baixas, o que faz sentido, visto que a cada passo ele tem 25% de chance de ir para qualquer uma das 4 direções.

#--------------------Letra C------------------------

caminhoLinkNvezes <- function(N){
  link <- c(0,0)
  if(N == 0) return(link)
  for(i in 1:N) {
    zelda <- sample(x = passos, replace = TRUE, size = 1)
    
    if(zelda == "L") {
      link[2] = link[2] - 1
    }
    
    if(zelda == "R") {
      link[2] = link[2] + 1
    }
    
    if(zelda == "U") {
      link[1] = link[1] + 1
    }
    
    if(zelda == "D") {
      link[1] = link[1] - 1
    }
    
    
  }
  return(link)
}

proporcaoLink <- function(N){
  if(N %% 2 == 1) print("N não pode ser um número ímpar")
  else{
    qtd <- 10000
    countOrigem <- 0
    
    for(i in 1:qtd){
      camLink <- caminhoLinkNvezes(N)
      if(camLink[1] == 0 && camLink[2] == 0){
        countOrigem <- countOrigem + 1
      }
    }
    return(countOrigem/qtd)
  }
}

proporcaoLink(1000)

#------------------Exercicio12------------------

steven <- c(0,1,0)
garnit <- c(0,0,1)

# cara = 1 coroa = 0

sorteio <- c(sample(x = 0:1, size = 3, replace = TRUE))
sorteio <- as.numeric(sorteio) #permite utilizar identical() para comparar os vetores

# Para jogar uma única vez
JogoStevenGarnit <- function(vSteven, vGarnit) {
  while((vGarnit < 1) && (vSteven < 1)) {
    if(identical(steven,sorteio)) {
      print("Steven ganhou!")
      vSteven <- vSteven + 1
      return ("steven")
    } else if(identical(garnit,sorteio)) {
        print("Garnit ganhou!")
        vGarnit <- vGarnit + 1
        return ("garnit")
    }
  
    aux <- c(sample(x = 0:1, size = 1))
    aux <- as.numeric(aux)
  
    sorteio[1] = sorteio[2]
    sorteio[2] = sorteio[3]
    sorteio[3] = aux[1]
  }
}

teste <- c(JogoStevenGarnit(0,0))

# 10 mil vezes

steven <- c(0,1,0)
garnit <- c(0,0,1)

resultados <- c()

for(j in 1:10000) {
  sorteio <- c(sample(x = 0:1, size = 3, replace = TRUE))
  sorteio <- as.numeric(sorteio)
  resultados[j] <- c(JogoStevenGarnit(0,0))
}
mean("garnit" == resultados)

print("Ao rodar o teste 10mil vezes (múltiplas vezes para ter certeza), podemos observar que Garnit tem a maior média e/ou porcentagem de vitórias, ou seja, ela ganha a maior parte das vezes, isso é devido à sequência 0,0,1 escolhido por Garnit, ser mais fácil de ocorrer")

#------------------Exercicio13------------------

assassinatos <- read.table(file = "dados.txt", header = TRUE, sep = ";")

#--------------------Letra A--------------------
library(ggplot2)

ggplot(data = assassinatos, aes(x = Genero, fill = Genero))+
  geom_bar()+
  labs(title ="Assassinatos por Gênero", y = "Quantidade", x = "Gênero", fill = "Gênero")+
  scale_fill_discrete(labels = c("Women" = "mulher", "Men" = "homem"))+
  scale_x_discrete(labels=c("Women" = "mulher", "Men" = "homem"))+
  theme_minimal()

#O gráfico demonstra que a preferência do assassino era por alvos mulheres

#--------------------Letra B--------------------

ggplot(data = assassinatos, aes(x = Idade))+
  geom_histogram(bins = 8, color = "black",fill = "lightblue")+
  labs(title = "Assassinatos por Idade", y = "Quantidade")+
  theme_minimal()
#Com este gráfico podemos perceber que a maioria dos assassinatos se concentrava em idosos

homensMortos <- assassinatos[assassinatos$Genero == "Men",]

ggplot(data = homensMortos, aes(x = Idade))+
  geom_histogram(bins = 8, color = "black",fill = "lightblue")+
  labs(title = "Assassinatos por Idade - Homens", y = "Quantidade")+
  theme_minimal()

mulheresMortas <- assassinatos[assassinatos$Genero == 'Women',]


ggplot(data = mulheresMortas, aes(x = Idade))+
  geom_histogram(bins = 8, color = "black",fill = "lightblue")+
  labs(title = "Assassinatos por Idade - Mulheres", y = "Quantidade")+
  theme_minimal()

#Com esta análise, vemos que tanto para homens quanto para mulheres assassinados há maior frequência de idosos
#--------------------Letra C--------------------

ggplot(data = assassinatos, aes(y = Idade))+
  geom_boxplot(fill = "lightblue")+
  labs(title = "Boxplot de Assassinatos por Idade")+
  theme_minimal()

#O boxplot indica que a maioria dos assassinatos eram em idosos, com a mediana sendo por volta de 76 anos, e os casos menores que 60 anos são outliers

#--------------------Letra D--------------------

assassinatos$LocalDaMorte <- as.factor(assassinatos$LocalDaMorte)

ggplot(data = assassinatos, aes(x = LocalDaMorte))+
  labs(title = "Local da Morte dos Assassinados", y = "Quantidade", x = "Local")+
  scale_x_discrete(labels = c("Nursing home" = "Lar de Idosos", "Own home" = "Própria residência"))+
  geom_bar(fill = "lightblue", color = "black")+
  theme_minimal()

#O gráfico indica que a maioria dos assassinatos ocorreu na própria residência da vítima

#--------------------Letra E--------------------

ggplot(data = assassinatos, mapping = aes(x = AnoDaMorte))+
  geom_bar(fill = "lightgreen", color = "black")+
  theme_minimal()+
  labs(title = "Assassinatos de Harold Shipman por Ano", y = "Quantidade", x = "Ano")

#A maior quantidade de assassinatos de Shipman ocorreu nos anos 90, que também foi o final de sua "carreira"

#--------------------Letra F--------------------

#Harold Shipman foi um assassino em série com um perfil de alvo claro: mulheres idosas, as quais ele assassinava principalmente na residência da vítima. Os assassinatos em série começaram em "poucas" quantidades por volta de 1975 e foram progressivamente aumentando, chegando no auge em cerca de 1997. As mortes em hospitais e lares de idosos podem indicar algumas coisas: por Shipman ser um médico, sua profissão pode ter servido de pretexto para entrar nesses lugares, onde cometia seus crimes, ou também pode signficar que alguns dos alvos que foram assassinados foram levados ao hospital com os ferimentos cometidos por Harold. Os lares de idosos também confirmam mais uma vez a clara preferência do assassino em fazer de vítimas os idosos.

#------------------Exercicio14------------------

macakes <- read.table(file = "primatas.txt", header = TRUE, sep = ":")

library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)

#------------------LetraA------------------

summary(macakes)

#------------------LetraB------------------

ggplot(data = macakes, aes(x = especie, fill = especie))+
  geom_bar(color = "#2e2c2c") +
  labs(title = "Quantidade de cada espécie no conjunto 🐒", x = "Espécie", y = "Quantidade", fill = "Espécie") +
  scale_x_discrete(labels = c("bonobo" = "Bonobo", "chimpanze" = "Chimpanzé")) +
  scale_fill_discrete(labels = c("bonobo" = "Bonobo", "chimpanze" = "Chimpanzé")) +
  theme_minimal()

ggplot(data = macakes, aes(x = especie, fill = genero))+
  geom_bar(color = "#2e2c2c") +
  labs(title = "Frequência de machos e fêmeas em cada espécie 🐒", x = "Espécie", y = "Quantidade", fill = "Gênero") +
  scale_x_discrete(labels = c("bonobo" = "Bonobo", "chimpanze" = "Chimpanzé")) +
  scale_fill_discrete(labels = c("femea" = "Fêmea", "macho" = "Macho")) +
  theme_classic()

#------------------LetraC------------------

chimpanzes <- macakes[macakes$especie == 'chimpanze',]
bonobos <- macakes[macakes$especie == 'bonobo',]

ggplot(data = bonobos, aes(x = peso, y = altura, col = genero))+
  geom_point() +
  labs(title = "Comparação entre fêmeas e machos dos bonobos 🐵")+
  theme_minimal()

ggplot(data = chimpanzes, aes(x = peso, y = altura, col = genero))+
  geom_point() +
  labs(title = "Comparação entre fêmeas e machos dos chimpanzés 🐵")+
  theme_minimal()

#------------------LetraD------------------

macakeMacho <- macakes[macakes$genero == 'macho',]
macakeFemea <- macakes[macakes$genero == 'femea',]

#removendo colunas desnecessárias
macakeMacho <- macakeMacho[,-c(2)]
macakeFemea <- macakeFemea[,-c(2)]

ggplot(data = macakeMacho, aes(x = peso, y = altura, col = especie)) +
  geom_point()+
  labs(title = "Comparação entre machos dos bonobos e chimpanzés 🙊")+
  theme_minimal()


ggplot(data = macakeFemea, aes(x = peso, y = altura, col = especie))+
  geom_point()+
  labs(title = "Comparação entre fêmeas dos bonobos e chimpanzés 🙊") +
  theme_minimal()

#------------------LetraE------------------

print("A partir da análise dos itens anteriores, em relação ao conjunto de dados em si, podemos afirmar que a proporção entre a quantidade de chimpanzés e bonobos é de 50/50, ou seja, metade do conjunto é de chimpanzés, e a outra metade é de bonobos. Na mesma ótica, também é possível afirmar, que nos dois conjuntos, o número de machos e fêmeas é igual, e, assim como as espécies, eles estão igualmente distribuidos em 50/50. Em relação às diferenças entre os gêneros dentro das espécies, podemos observar que para os chimpanzés, temos machos mais altos e com maior peso, já para os bonobos, observamos quase o mesmo padrão, no entanto, dessa vez a diferença entre os dois fica um pouco menos acentuada, e temos algumas fêmeas com a mesma altura que alguns machos, mas o peso continua maior para os machos. De forma geral, temos machos mais altos e mais pesados. E finalmente, comparando os generos das espécies entre si, temos que os chimpanzés tendem a ter machos um pouco mais altos (pois a altura é a mesma em alguns casos) e definitivamente mais pesados, já para as fêmeas temos a tendência de uma altura maior nos bonobos (apesar de novamente os dois conjuntos apresentarem intersecções nesse atributo), mas, de forma geral, os chimpanzés [fêmeas] apresentam um peso maior.")

#------------------LetraF------------------

n <- round(0.8*nrow(macakes))

indices_treino <- sample(1:nrow(macakes), size = n, replace = FALSE)

treino <- macakes[indices_treino,]
teste <- macakes[-indices_treino,]

arvore <- rpart(formula = especie ~. , data = treino, method = "class")
rpart.plot(arvore, extra = 101)
previsao.arvore <- predict(arvore, newdata = teste, type = "class")
mean(previsao.arvore == teste$especie)


