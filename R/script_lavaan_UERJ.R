library(psych)
library(lavaan)



### Análise Fatorial Confirmatória ###
# carregar o banco de dados #
data_exemplo<- read.table("banco_exemplo_thompson.txt", header = F, sep='\t')
names(data_exemplo)<-c("id", 'grupo', "it1" , "it3" , "it4", "it5", "it6", 
                       "it7" , "it8", "it9", "it10", "it11", "it12")

# exemplo 1#
exemplo1<-'f1 =~ it1 + it2 + it3 + it4' #configura o modelo
cfa_ex1<- cfa(model = exemplo1, data=data_exemplo) #roda o modelo
summary(cfa_ex1) # output com o resumo do modelo
summary(cfa_ex1, fit.measures=TRUE) # output com indicadores de ajuste
summary(cfa_ex1, fit.measures=TRUE, standardized=TRUE) # output com parametros padronizados


# exemplo 2#
exemplo2<-'f1 =~ it1 + it2' #configura o modelo
cfa_ex2<- cfa(model = exemplo2, data=data_exemplo) #roda o modelo
summary(cfa_ex2, fit.measures=TRUE) # output com indicadores de ajuste


# exemplo 3#
exemplo3<-'f1 =~ it1 + it2 + it3 + it4
           f2 =~ it5 + it6 + it7 + it8 
           f3 =~ it9 + it10 + it11 + it12' #configura o modelo
cfa_ex3<- cfa(model = exemplo3, data=data_exemplo) #roda o modelo
summary(cfa_ex3, fit.measures=TRUE) # output com indicadores de ajuste


# exemplo 3B
modindices(cfa_ex3) # solicita indices de modificacao


# exemplo 3C
summary(cfa_ex1, standardized=TRUE) # parametros padronizados


# exemplo 4
d
  



#gabarito exercicio Lavaan

# buscar o banco
bancoExerc<- read.table("banco_exemplo_cat.txt", header = T, sep='\t')

#configurar o modelo
exercicio<- 'f1 =~ i1 + i2 + i3 + i4
             f2 =~ i5 + i6 + i7 + i8
             f3 =~ i9 + i10 + i11 + i12'

#rodar o modelo
cfa_exerc<- cfa(model = exercicio, data=bancoExerc, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_exerc, fit.measures=TRUE, standardized=TRUE)


#passo 2, recofigurar o modelo retirando o item 6 (belo)
#configurar o modelo
exercicioV2<- 'f1 =~ i1 + i2 + i3 + i4
             f2 =~ i5 + i7 + i8
             f3 =~ i9 + i10 + i11 + i12
              i1 | t1 + t2 + t3 + t4
              i2 | t1 + t2 + t3 + t4
              i3 | t1 + t2 + t3 + t4
              i4 | t1 + t2 + t3 + t4
              i5 | t1 + t2 + t3 + t4
              i7 | t1 + t2 + t3 + t4
              i8 | t1 + t2 + t3 + t4
              i9 | t1 + t2 + t3 + t4
              i10 | t1 + t2 + t3 + t4
              i11 | t1 + t2 + t3 + t4
              i12 | t1 + t2 + t3 + t4'

#rodar o modelo
cfa_exercV2<- cfa(model = exercicioV2, data=bancoExerc, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_exercV2, fit.measures=TRUE, standardized=TRUE)
modindices(cfa_exercV2)



#passo 3, recofigurar o modelo incluindo um cross-loading
#configurar o modelo
exercicioV3<- 'f1 =~ i1 + i2 + i3 + i4
               f2 =~ i5 + i7 + i8
               f3 =~ i9 + i10 + i11 + i12 + i1
              i1 | t1 + t2 + t3 + t4
              i2 | t1 + t2 + t3 + t4
              i3 | t1 + t2 + t3 + t4
              i4 | t1 + t2 + t3 + t4
              i5 | t1 + t2 + t3 + t4
              i7 | t1 + t2 + t3 + t4
              i8 | t1 + t2 + t3 + t4
              i9 | t1 + t2 + t3 + t4
              i10 | t1 + t2 + t3 + t4
              i11 | t1 + t2 + t3 + t4
              i12 | t1 + t2 + t3 + t4'

#rodar o modelo
cfa_exercV3<- cfa(model = exercicioV3, data=bancoExerc, ordered = T, estimator = "DWLS") #informar que os itens sao categoricos ordinais
summary(cfa_exercV3, fit.measures=TRUE, standardized=TRUE) 
modindices(cfa_exercV3)



