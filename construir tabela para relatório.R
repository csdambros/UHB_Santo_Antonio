brutos<-read.csv('DADOSbrutos.csv')


Modulo.sigla<-c('IB','IP','JP','JI','TE')

tabela1=tabela[,0]

head(tabela1)
head(tabela)
head(brutos)


tabela1$data<-''
tabela1$Munic�pio<-brutos$Munic�pio[match(Modulo.sigla[tabela$M�dulo.Grid],brutos$M�dulo)]
tabela1$Campanha<-9
tabela1$M�dulo<-Modulo.sigla[tabela$M�dulo.Grid]
tabela1$Transecto<-tabela$Trilha
tabela1$Parcela<-paste('P',as.integer(as.factor(tabela$Parcela)),sep='')
tabela1$Ponto<-tabela$Parcela
tabela1$'Tipo de Parcela'<-'Terrestre'
tabela1$'Coordenadas UTM (X)'<-brutos$Coordenadas.UTM..X.[match(Modulo.sigla[tabela$M�dulo.Grid],brutos$M�dulo)]
tabela1$'Coordenadas UTM (Y)'<-brutos$Coordenadas.UTM..Y.[match(Modulo.sigla[tabela$M�dulo.Grid],brutos$M�dulo)]
tabela1$Ordem<-'Isoptera'
tabela1$Fam�lia<-tabela$Fam�lia
tabela1$Subfam�lia<-tabela$Subfam�lia
tabela1$Esp�cie<-tabela$Taxon
tabela1$'Tipo Amostragem'<-''
tabela1$'N�m. Ind. (Col�nias/Encontros)'<-1
tabela1$'N�m.ID (campo)'<-''
tabela1$'Institui��o para Tombamento'<-'INPA'
tabela1$'Tombo'<-''
tabela1$'OBS'<-''
tabela1$'G�nero'<-tabela$Genero
tabela1$'Ep�teto espec�fico'<-unlist(data.frame(strsplit(gsub("_"," ",as.character(tabela$Taxon)),"mes "))[2,])


write.csv(tabela1,file='SAE_Dados_Brutos_cupins_9CP.csv',row.names=FALSE)





