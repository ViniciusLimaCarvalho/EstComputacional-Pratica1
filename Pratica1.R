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

#--------------------Letra C------------------------

caminhoLinkNvezes <- function(N){
  link <- c(0,0)
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

proporcaoLink(100)
