#Projeto 2 estatistica
#Arthur Romaguera Lima - arl3
#Vitor Miranda - vmm

#Q1 Descarregue o arquivo .csv da planilha e imprima o dataframe obtido exatamente do jeito que ele se encontra.
df <- read.csv(file="ET586-Projeto2/detalhes-albuns.csv", header=TRUE, sep=",", encoding = "UTF-8")
print("Q1:")
print(df)

#Q2 Encontre a média, o desvio padrão e a moda das vendas do total de álbuns.
#Média
#usando a função existente no R "mean", calculamos a média da 5ª coluna (qtd de albuns vendidos)
media = mean(df[[5]])
print("Q2: Media:")
print(media)

#Desvio Padrão
#usando a função existente no R "sd", calculamos o desvio padrão da 5ª coluna (qtd de albuns vendidos)
devP = sd(df[[5]])
print("Q2: Desvio Padrão:")
print(devP)

#Moda
#calculamos as ocorrência de cada valor e retornamos a maior quantidade (moda)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda = getmode(df[[5]])
print("Q2: moda:")
print(moda)

#Q3 Faça uma função que retorna os nomes dos artistas que lançaram álbuns nos dois anos (ou seja, o mesmo artista lançou um ou mais álbuns em 2018 e em 2019).
albunsxartistas <- function() {
  #vetores com artistas que laçaram álbuns em 2018 e 2019
  v2018 = df[df$Ano == 2018,][["Artista"]]
  v2019 = df[df$Ano == 2019,][["Artista"]]

  artistas = vector()
  #varre o vetor v2018 e verifica se o artista está contido no vetor 2019
  for (i in v2018){
    if (i %in% v2019) {
       artistas = c(artistas, i)
    }
  }
  return (unique(artistas))
}
print("Q3:")
print("Artistas que lançaram albums em 2018 e 2019:")
print(albunsxartistas())

#Q4Faça uma função que retorne qual artista possui o menor desvio padrão nas vendas.
artistaMenorDesvio <- function() {
  # cria dataframe com as frequencias de cada artista, eliminando os que aparecem apenas 1 vez
  freq = as.data.frame(table(df["Artista"]))
  freq = freq[(freq$Freq > 1),]
  
  #cria um vetor para armazenar os DP de cada artista
  vDP = vector()
  # armazena no vetor
  for (artist in as.vector(freq[[1]])) {
    vDP = c(vDP, sd(df[df$Artista == artist,][[5]]))
  }
  #cria um novo dataframe com o artista e seu DP
  dfDP = data.frame(freq[1], vDP)
  
  #nomeia colunas para ordenar pelo desvio padrao
  colnames(dfDP)[1] = "Artista"
  colnames(dfDP)[2] = "DesvioPadrao"
  dfDP = dfDP[order(dfDP$DesvioPadrao),]
  #retorna o artista da primeira posicao do novo dataframe
  return(as.character(dfDP[[1,1]]))
}
print("Q4:")
print(artistaMenorDesvio())

#Q5 Faça uma função que retorne o nome do álbum que mais vendeu e o que menos vendeu ao dar um ano de lançamento (retorne também o nome dos artistas correspondentes a cada álbum).
albunsxano <- function(ano) {

  vendasMax = max(df[df$Ano == ano,][[5]])
  vendasMin = min(df[df$Ano == ano,][[5]])

  albumMax = df[df[5] == vendasMax,]["Album"]
  albumMin = df[df[5] == vendasMin,]["Album"]

  bandaMax = df[df[[5]] == vendasMax,]["Artista"]
  bandaMin = df[df[5] == vendasMin,]["Artista"]

  max = merge(albumMax, bandaMax)
  min = merge(albumMin, bandaMin)
  
  return(rbind(max, min))
}
print("Q5:")
print(albunsxano(2018))

#Q6 Faça uma função que retorne uma lista com os artistas que só apareceram uma vez na planilha, indicando também o ano que cada um aparece.
artistas1v = function(){
  #Usamos table pra contar as ocorrencias e depois removi as maiores que 1
  freq = as.data.frame(table(df["Artista"]))
  freq = freq[!(freq$Freq > 1),]
  
  colnames(freq)[1] = "Artista"

  Ano = vector()
  
  #adicionando o Ano que o Artista apareceu
  for(i in freq[[1]]){ 
    Ano = c(Ano, df[df$Artista == i,][["Ano"]])
  }
  return(data.frame(freq[1], Ano))
}

print("Q6:")
print(artistas1v())

#Q7 Faça um dataframe com as colunas EMPRESA, NÚMERO DE ARTISTAS que mostra quantos artistas cada empresa possui (lembrando que um mesmo artista pode aparecer mais de uma vez na planilha).
#Coloque em ordem crescente do número de artistas.

#cria um vetor para armazenar as empresas e outro para a quatidade de artistas
empresas = unique(as.vector(df[[4]]))
qtdArtistas = vector()
# armazena no vetor as quantidades de artistas de cada empresa
for (empresa in empresas) {
  qtdArtistas = c(qtdArtistas, nrow(unique(df[df$Empresa == empresa,]["Artista"])))
}

#cria um novo dataframe com a empresa e a quantidade de artistas
empresasDF = data.frame(empresas, qtdArtistas)
#nomeia colunas para ordenar pela quantidade de artistas crescente
colnames(empresasDF)[1] = "Empresa"
colnames(empresasDF)[2] = "QuantidadeArtistas"

empresasDF = empresasDF[order(empresasDF$QuantidadeArtistas),]
print(empresasDF)


#Q8 Elabore uma função que retorna os 3 artistas que mais aparecem na planilha, para em seguida fazer um dataframe desses artistas com as colunas ARTISTA, TOTAL DE VENDAS onde a última coluna deve ser a soma de todas as vendas do artista. Faça por ordem decrescente de vendas.

maxArtistas = function(){
  #calcula a frequência de cada artista
  freq = as.data.frame(table(df["Artista"]))
  #ordena freq pela frequência decrescente (-freq)
  freq = freq[order(-freq$Freq),]
  #retorna os 3 primeiros artistas que mais aparecem
  return(freq[1:3,])
}
#armazena o retorno da funcao maxArtistas na variavel 3 maiores
tresMaiores = maxArtistas()
#calcula a soma das vendas dos 3 artistas retornados acima
TotalVendas = c(sum(df[df$Artista == tresMaiores[1,1],][[5]]), sum(df[df$Artista == tresMaiores[2,1],][[5]]), sum(df[df$Artista == tresMaiores[3,1],][[5]]))

vectArtistas = tresMaiores[1]

dfV = data.frame(vectArtistas, TotalVendas)
dfV = dfV[order(-dfV$TotalVendas),]

colnames(dfV)[1] = "Artista"
colnames(dfV)[2] = "Total de Vendas"

print("Q8:")
print(dfV)


#Q9 Elabore uma função que retorna o nome do álbum que mais vendeu de cada empresa. 
#Por fim, faça um dataframe com as colunas EMPRESA, ARTISTA, ÁLBUM, VENDAS que mostra o álbum mais vendido de cada empresa, 
#por ordem decrescente de vendas.


albumxempresa = function(){
  empresas = unique(as.vector(df[[4]]))
  albuns = vector()
#armazena no vetor as quantidades de artistas de cada empresa
  for(empresa in empresas){
    maisvendido = max(df[df$Empresa == empresa,][[5]]) 

    albuns = c(albuns, as.vector(df[df$Qnt..de.Albuns.Vendidos == maisvendido, 2]))
  }
    
  empresas_albuns = data.frame(empresas, albuns)

  colnames(empresas_albuns)[1] = "Empresa"
  colnames(empresas_albuns)[2] = "Album mais vendido"

  
  return(empresas_albuns)
}

empresas = unique(as.vector(df[[4]]))
df10 = data.frame()
for(empresa in empresas){

  maisvendido = max(df[df$Empresa == empresa,][[5]])

  df10 = rbind(df10, df[df$Qnt..de.Albuns.Vendidos == maisvendido,])

}

print(df10)

print(albumxempresa())

#Q10 Faça uma função que ao receber o nome de uma empresa, cria um histograma onde mostra a frequência de álbuns lançados pela empresa de acordo com o ano. Não esqueça de dar um título e fazer ele de forma colorida, facilitando a visualização.
frequenciaAlbuns <- function(empresa) {
  print(df[df$Empresa == empresa,])
  barplot(table(df[df$Empresa == empresa,]$Ano),
          #titulo do histograma
          main=paste("frequência de álbuns lançados pela empresa ", empresa),
          # label eixo x
          xlab="Ano",
          # label eixo y
          ylab = "Frequência",
          # vetor de cores
          col= colors()[grep("red",colors())]
  )
}
frequenciaAlbuns('JYP')
