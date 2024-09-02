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
}

jogoCraps()

vitorias
derrotas





