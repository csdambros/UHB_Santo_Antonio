brutos<-read.csv('DADOSbrutos.csv')


Modulo.sigla<-c('IB','IP','JP','JI','TE')

tabela1=tabela[,0]

head(tabela1)
head(tabela)
head(brutos)


tabela1$data<-''
tabela1$Município<-brutos$Município[match(Modulo.sigla[tabela$Módulo.Grid],brutos$Módulo)]
tabela1$Campanha<-9
tabela1$Módulo<-Modulo.sigla[tabela$Módulo.Grid]
tabela1$Transecto<-tabela$Trilha
tabela1$Parcela<-paste('P',as.integer(as.factor(tabela$Parcela)),sep='')
tabela1$Ponto<-tabela$Parcela
tabela1$'Tipo de Parcela'<-'Terrestre'
tabela1$'Coordenadas UTM (X)'<-brutos$Coordenadas.UTM..X.[match(Modulo.sigla[tabela$Módulo.Grid],brutos$Módulo)]
tabela1$'Coordenadas UTM (Y)'<-brutos$Coordenadas.UTM..Y.[match(Modulo.sigla[tabela$Módulo.Grid],brutos$Módulo)]
tabela1$Ordem<-'Isoptera'
tabela1$Família<-tabela$Família
tabela1$Subfamília<-tabela$Subfamília
tabela1$Espécie<-tabela$Taxon
tabela1$'Tipo Amostragem'<-''
tabela1$'Núm. Ind. (Colônias/Encontros)'<-1
tabela1$'Núm.ID (campo)'<-''
tabela1$'Instituição para Tombamento'<-'INPA'
tabela1$'Tombo'<-''
tabela1$'OBS'<-''
tabela1$'Gênero'<-tabela$Genero
tabela1$'Epíteto específico'<-unlist(data.frame(strsplit(gsub("_"," ",as.character(tabela$Taxon)),"mes "))[2,])


write.csv(tabela1,file='SAE_Dados_Brutos_cupins_9CP.csv',row.names=FALSE)





