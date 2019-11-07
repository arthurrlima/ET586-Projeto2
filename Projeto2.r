#Projeto 2 estatistica
#Arthur Romaguera Lima - arl3
#Vitor Miranda - vmm

#Q1 Descarregue o arquivo .csv da planilha e imprima o dataframe obtido exatamente do jeito que ele se encontra.

df <- read.csv(file="detalhes-albuns.csv", header=TRUE, sep=",", encoding = "UTF-8")

print(df)


#Q2 Encontre a média, o desvio padrão e a moda das vendas do total de álbuns.

#Média

media = mean(df[[5]])
print(media)

#Desvio Padrão

devP = sd(df[[5]])
print(devP)

#Moda
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda = getmode(df[[5]])
print(moda)



#Q3 Faça uma função que retorna os nomes dos artistas que lançaram álbuns nos dois anos (ou seja, o mesmo artista lançou um ou mais álbuns em 2018 e em 2019).


#Q4Faça uma função que retorne qual artista possui o menor desvio padrão nas vendas.


#Q5 Faça uma função que retorne o nome do álbum que mais vendeu e o que menos vendeu ao dar um ano de lançamento (retorne também o nome dos artistas correspondentes a cada álbum).


#Q6 Faça uma função que retorne uma lista com os artistas que só apareceram uma vez na planilha, indicando também o ano que cada um aparece.


#Q7 Faça um dataframe com as colunas EMPRESA, NÚMERO DE ARTISTAS que mostra quantos artistas cada empresa possui (lembrando que um mesmo artista pode aparecer mais de uma vez na planilha). Coloque em ordem crescente do número de artistas.


#Q8 Elabore uma função que retorna os 3 artistas que mais aparecem na planilha, para em seguida fazer um dataframe desses artistas com as colunas ARTISTA, TOTAL DE VENDAS onde a última coluna deve ser a soma de todas as vendas do artista. Faça por ordem decrescente de vendas.


#Q9 Elabore uma função que retorna o nome do álbum que mais vendeu de cada empresa. Por fim, faça um dataframe com as colunas EMPRESA, ARTISTA, ÁLBUM, VENDAS que mostra o álbum mais vendido de cada empresa, por ordem decrescente de vendas.


#Q10 Faça uma função que ao receber o nome de uma empresa, cria um histograma onde mostra a frequência de álbuns lançados pela empresa de acordo com o ano. Não esqueça de dar um título e fazer ele de forma colorida, facilitando a visualização.

