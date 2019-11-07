#Projeto 2 estatistica
#Arthur Romaguera Lima - arl3
#Vitor Miranda - vmm

#Q1 Descarregue o arquivo .csv da planilha e imprima o dataframe obtido exatamente do jeito que ele se encontra.

df <- read.csv(file="/Users/art/Documents/ET586-Projeto2/ET586-Projeto2/detalhes-albuns.csv", header=TRUE, sep=",", encoding = "UTF-8")

print(df)


#Q2 Encontre a média, o desvio padrão e a moda das vendas do total de álbuns.

#Média
#usando a função existente no R "mean", calculamos a média da 5ª coluna (qtd de albuns vendidos)
media = mean(df[[5]])
print(media)

#Desvio Padrão
#usando a função existente no R "sd", calculamos o desvio padrão da 5ª coluna (qtd de albuns vendidos)
devP = sd(df[[5]])
print(devP)

#Moda
#calculamos as ocorrência de cada valor e retornamos a maior quantidade (moda)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda = getmode(df[[5]])
print(moda)



#Q3 Faça uma função que retorna os nomes dos artistas que lançaram álbuns nos dois anos (ou seja, o mesmo artista lançou um ou mais álbuns em 2018 e em 2019).

albunsxartistas <- function() {

  v2018 = df[df$Ano == 2018,][["Artista"]]
  v2019 = df[df$Ano == 2019,][["Artista"]]

  artistas = vector()

  for (i in v2018){
    if (i %in% v2019) {
       artistas = c(artistas, i)
    }
  }
  return (unique(artistas))
}

print("Artistas que lançaram albums em 2018 e 2019:")
print(albunsxartistas())

#Q4Faça uma função que retorne qual artista possui o menor desvio padrão nas vendas.


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

print(albunsxano(2018))

#Q6 Faça uma função que retorne uma lista com os artistas que só apareceram uma vez na planilha, indicando também o ano que cada um aparece.


#Q7 Faça um dataframe com as colunas EMPRESA, NÚMERO DE ARTISTAS que mostra quantos artistas cada empresa possui (lembrando que um mesmo artista pode aparecer mais de uma vez na planilha). Coloque em ordem crescente do número de artistas.


#Q8 Elabore uma função que retorna os 3 artistas que mais aparecem na planilha, para em seguida fazer um dataframe desses artistas com as colunas ARTISTA, TOTAL DE VENDAS onde a última coluna deve ser a soma de todas as vendas do artista. Faça por ordem decrescente de vendas.


#Q9 Elabore uma função que retorna o nome do álbum que mais vendeu de cada empresa. Por fim, faça um dataframe com as colunas EMPRESA, ARTISTA, ÁLBUM, VENDAS que mostra o álbum mais vendido de cada empresa, por ordem decrescente de vendas.


#Q10 Faça uma função que ao receber o nome de uma empresa, cria um histograma onde mostra a frequência de álbuns lançados pela empresa de acordo com o ano. Não esqueça de dar um título e fazer ele de forma colorida, facilitando a visualização.

