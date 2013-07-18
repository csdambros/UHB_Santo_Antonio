SAE_cupins<-read.csv(paste(WDIR,"/SAE_cupins.csv",sep=""),sep=",",row.names=1)

SAE_cupins$Campanha[-grep("[0-9]{2}",SAE_cupins$Campanha)]<- paste(0,SAE_cupins$Campanha[-grep("[0-9]{2}",SAE_cupins$Campanha)],sep="")

head(SAE_cupins)

cupins.geral<-read.csv(paste(WDIR,"/Isoptera.csv",sep=""))
comparacao.nomes<-read.csv(paste(WDIR,"/names.csv",sep=""),sep="\t")

names.match<-match(SAE_cupins$Especie,comparacao.nomes$Thiago)

SAE_cupins$Especie_orig<-SAE_cupins$Especie

SAE_cupins$Especie<-as.character(SAE_cupins$Especie)

SAE_cupins$Especie<-ifelse(is.na(names.match),SAE_cupins$Especie,as.character(comparacao.nomes$Renato)[names.match])

SAE_cupins$Num._Ind._.Colonias.Encontros.<-1

#SAE_cupins[SAE_cupins==""]<-NA

write.csv(SAE_cupins,file=paste(WDIR,"/SAE_cupins_cor_names.csv",sep=""))


#cupins.1.8[grep("Spin",cupins.1.8$Especie),]
#cupins.geral[grep("Syntermes_t",cupins.geral$Taxon),]

SAE_cupins[Modulo=="JP"&Transecto=="T2"&Po,]

levels(as.factor(Ponto[Modulo=="TE"&Transecto=="T2"]))

source("Relatorio_geral.R")






 p <- ggplot(data.frame(scores(SAE_cupins_NMDS)), aes(x=NMDS1, y=NMDS2, label=rownames(scores(SAE_cupins_NMDS))))

p + geom_text(fontface=3)
     p + geom_text(aes(fontface=am+1))
     p + geom_text(aes(family=c("serif", "mono")[am+1]))
     


