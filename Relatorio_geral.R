source("WDIR.R")
 
SAE_cupins<-read.csv(paste(WDIR,"/SAE_cupins_cor_names.csv",sep=""),row.names=1)

ambiente<-read.csv(paste(WDIR,"/dados_ambientais.csv",sep=""),row.names=1)


#isoptera.geral <- read.csv(paste(WDIR,"/Isoptera.csv",sep=""))
#write.csv(SAE_cupins,file=paste(WDIR,"/ver.csv",sep=""))

#Remover os apico

SAE_cupins<-SAE_cupins[SAE_cupins$Subfamilia!="Apicotermitinae",]

SAE_cupins$Campanha[-grep("[0-9]{2}",SAE_cupins$Campanha)]<- paste(0,SAE_cupins$Campanha[-grep("[0-9]{2}",SAE_cupins$Campanha)],sep="")

attach(SAE_cupins)

#detach(SAE_cupins)

#Campanha<-as.ordered(Campanha)

#colnames(SAE_cupins)
#levels(as.factor(paste(Modulo,Transecto)))


SAE_cupins_encontros.modulo<- by(SAE_cupins,paste(Familia,Subfamilia,sep=","),function(x)tapply(x$Num._Ind._.Colonias.Encontros.,list(x$Especie,x$Modulo),sum))


SAE_cupins_encontros.modulo_sub<-
    lapply(1:length(SAE_cupins_encontros.modulo),
           function(i){
               x<-SAE_cupins_encontros.modulo[[i]]
               if(is.null(x)){NULL}
               else{
                   x[is.na(x)]<-0;
                   data.frame("Familia,Subfamilia"=names(SAE_cupins_encontros.modulo)[i],x)[rowSums(x)>0,]                
               }
           }
           )


SAE_cupins_encontros.modulo_sub2<-do.call(rbind,SAE_cupins_encontros.modulo_sub)
colnames(SAE_cupins_encontros.modulo_sub2)<-gsub("\\.","\\,",colnames(SAE_cupins_encontros.modulo_sub2))

write.csv(SAE_cupins_encontros.modulo_sub2,file="Tabelas_output/Encontros.por.modulo.xls",quote=FALSE)

####

SAE_cupins_encontros.campanha<- by(SAE_cupins,paste(Familia,Subfamilia,sep=","),function(x)tapply(x$Num._Ind._.Colonias.Encontros.,list(x$Especie,factor(x$Campanha,levels=levels(as.factor(Campanha)))),sum))

#factor(Campanha[1:10],levels=levels(as.factor(Campanha)))

SAE_cupins_encontros.campanha_sub<-
    lapply(1:length(SAE_cupins_encontros.campanha),
           function(i){
               x<-SAE_cupins_encontros.campanha[[i]]
               if(is.null(x)){NULL}
               else{
                   x[is.na(x)]<-0;
                   data.frame("Familia,Subfamilia"=names(SAE_cupins_encontros.campanha)[i],x)[rowSums(x)>0,]
                               }
           }
           )


SAE_cupins_encontros.campanha_sub2<-do.call(rbind,SAE_cupins_encontros.campanha_sub)

colnames(SAE_cupins_encontros.campanha_sub2)<-gsub("\\.","\\,",colnames(SAE_cupins_encontros.campanha_sub2))


write.csv(SAE_cupins_encontros.campanha_sub2,file="Tabelas_output/Encontros.por.campanha.xls",quote=FALSE)


#######


############################################

SAE_cupins_short<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="-"),Especie),sum)
SAE_cupins_short_var <- ambiente[match(rownames(SAE_cupins_short),rownames(ambiente)),]

SAE_cupins_short_by_modulo<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="-"),Especie,paste(Modulo,Transecto,sep="-")),sum,simplify=TRUE)

SAE_cupins_short_by_campanha<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="-"),Especie,Campanha),sum,simplify=TRUE)
#SAE_cupins_short_by_campanha_var <- lapply(SAE_cupins_short_by_campanha,function(x) ambiente[match(rownames(x),rownames(ambiente)),])

SAE_cupins_short_by_campanha_modulo<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="-"),Especie,paste(Campanha,Modulo,Transecto,sep="-")),sum,simplify=TRUE)
#SAE_cupins_short_by_campanha_modulo_var <- lapply(SAE_cupins_short_by_campanha_modulo,function(x) ambiente[match(rownames(x),rownames(ambiente)),])

#SAE_cupins_short_by_campanha_modulo<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Transecto,Ponto),Especie,paste(Campanha,Modulo,Transecto)),sum,simplify=TRUE)

SAE_cupins_short[is.na(SAE_cupins_short)]<-0

SAE_cupins_short_by_modulo[is.na(SAE_cupins_short_by_modulo)]<-0

SAE_cupins_short_by_campanha[is.na(SAE_cupins_short_by_campanha)]<-0

SAE_cupins_short_by_campanha_modulo[is.na(SAE_cupins_short_by_campanha_modulo)]<-0


SAE_cupins_short_by_modulo<-lapply(seq_len(dim(SAE_cupins_short_by_modulo)[3]),function(i) SAE_cupins_short_by_modulo[,,i])
SAE_cupins_short_by_modulo<-lapply(SAE_cupins_short_by_modulo,function(x){x<-data.frame(x);x[rowSums(x)>0,]})

SAE_cupins_short_by_modulo_var <- lapply(SAE_cupins_short_by_modulo,function(x) ambiente[match(rownames(x),rownames(ambiente)),])

SAE_cupins_short_by_campanha<-lapply(seq_len(dim(SAE_cupins_short_by_campanha)[3]),function(i) SAE_cupins_short_by_campanha[,,i])
SAE_cupins_short_by_campanha<-lapply(SAE_cupins_short_by_campanha,function(x){x<-data.frame(x);x[rowSums(x)>0,]})
SAE_cupins_short_by_campanha_var <- lapply(SAE_cupins_short_by_campanha,function(x) ambiente[match(rownames(x),rownames(ambiente)),])


SAE_cupins_short_by_campanha_modulo<-lapply(seq_len(dim(SAE_cupins_short_by_campanha_modulo)[3]),function(i) SAE_cupins_short_by_campanha_modulo[,,i])
SAE_cupins_short_by_campanha_modulo<-lapply(SAE_cupins_short_by_campanha_modulo,function(x){x<-data.frame(x);x[rowSums(x)>0,]})
SAE_cupins_short_by_campanha_modulo_var <- lapply(SAE_cupins_short_by_campanha_modulo,function(x) ambiente[match(rownames(x),rownames(ambiente)),])


#lapply(SAE_cupins_short_by_campanha_modulo_var,nrow)


names(SAE_cupins_short_by_modulo) <- levels(as.factor(paste(Modulo,Transecto,sep="-")))
#str(SAE_cupins_short_by_modulo)

names(SAE_cupins_short_by_campanha) <- levels(as.factor(Campanha))
#str(SAE_cupins_short_by_campanha)
 
names(SAE_cupins_short_by_campanha_modulo) <- levels(as.factor(paste(Campanha,Modulo,Transecto,sep="-")))


#str(SAE_cupins_short_by_campanha_modulo)


######################################################

# Aqui começam as análises

########################################################

library(vegan)
library(ggplot2)
library(reshape)

# Abundancia

#Numero estimado de especies
est.rich <- specpool(SAE_cupins_short)

#Curva de acumulo de especies
accum <- specaccum(SAE_cupins_short)

SAE_cupins_rarefacao <- data.frame(rich=accum$richness,site=accum$sites,sd.l=accum$richness-1.96*accum$sd,sd.u=accum$richness+1.96*accum$sd,id="Total")

print(ggplot(SAE_cupins_rarefacao,aes(site,rich))+
    geom_point()+geom_line(data=SAE_cupins_rarefacao)+
    geom_ribbon(data=SAE_cupins_rarefacao,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+
    facet_wrap(~id)+
    ylab("Número de espécies")+xlab("Número de amostras"))

dev.print(device = cairo_ps, file="Figuras/rarefacao.eps")
dev.copy2pdf(file="Figuras/rarefacao.geral.pdf")
dev.off()


###

SAE_cupins_NMDS<-metaMDS(vegdist(SAE_cupins_short,"horn"))

SAE_cupins_short_var[c("NMDS1","NMDS2")]<-data.frame(scores(SAE_cupins_NMDS))

ggplot(data=SAE_cupins_short_var,aes(NMDS1,NMDS2,fill=Altitude))+geom_point(pch=21)+geom_text(cex=3,aes(NMDS1,NMDS2,label=Modulo), hjust = 1.3,vjust=1.3)

summary(lm(NMDS2~Altitude+P+N,data=decostand(SAE_cupins_short_var[,-c(1:3)],"standardize",na.rm=TRUE)))

#Plot  rarefaction curve for each module (all time)
#Plota curvas de rarefacao para cada módulo (todas as campanhas)

# Numero estimado de especies

est.rich_modulo <- do.call(rbind,lapply(SAE_cupins_short_by_modulo,function(x)specpool(x[rowSums(x)>0,])))
       
#Curvas de acumulo de especies

var_modulo<-lapply(
    seq_len(length(SAE_cupins_short_by_modulo)),
       function(i){
       x<- SAE_cupins_short_by_modulo[[i]]   
       accum<-specaccum(x)
       SAE_cupins_NMDS_s<-scores(metaMDS(vegdist(x,"horn")))     
       data.frame(rich=accum$richness,site=accum$sites,sd.l=accum$richness-1.96*accum$sd,
                  sd.u=accum$richness+1.96*accum$sd,SAE_cupins_NMDS_s,ambiente[match(rownames(x),rownames(ambiente)),],id=names(SAE_cupins_short_by_modulo)[i])})
       

SAE_cupins_var_by_modulo<-do.call(rbind,var_modulo)

###

by(SAE_cupins_var_by_modulo,SAE_cupins_var_by_modulo$id,function(x){
    summary(lm(NMDS1~rich,data=x))})

##

print(ggplot(SAE_cupins_var_by_modulo,aes(site,rich))+
    geom_point()+geom_line(data=SAE_cupins_rarefacao_by_modulo)+
    geom_ribbon(data=SAE_cupins_var_by_modulo,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+
    facet_wrap(~id)+
    ylab("Número de espécies")+xlab("Número de amostras"))

dev.print(device = cairo_ps, file="Figuras/rarefacao.geral.modulos.eps")
dev.copy2pdf(file="Figuras/rarefacao.geral.modulos.pdf")
dev.off()


# Composicao de especies por modulo

ggplot(data=SAE_cupins_var_by_modulo,aes(NMDS1,NMDS2,fill=Altitude,symbols=Modulo))+geom_point(pch=21)+geom_text(cex=3,aes(NMDS1,NMDS2,label=Modulo), hjust = 1.3,vjust=1.3)+facet_wrap(~Modulo)

ggplot(data=SAE_cupins_var_by_modulo,aes(rich,NMDS1,size=rich,fill=Modulo))+geom_point(pch=21)+geom_text(cex=3,aes(rich,NMDS1,label=""), hjust = 1.3,vjust=1.3)+facet_grid(Transecto~Modulo)

summary(lm(NMDS2~Altitude+P+N,data=decostand(SAE_cupins_short_var[,-c(1:3)],"standardize",na.rm=TRUE)))


#################

est.rich_campanha <- do.call(rbind,lapply(SAE_cupins_short_by_campanha,function(x)specpool(x)))

#######

var_campanha<-lapply(
    seq_len(length(SAE_cupins_short_by_campanha)),
       function(i){
       x<- SAE_cupins_short_by_campanha[[i]]   
       accum<-specaccum(x)
       SAE_cupins_NMDS_s<-scores(metaMDS(vegdist(x,"horn")))     
       data.frame(rich=accum$richness,site=accum$sites,sd.l=accum$richness-1.96*accum$sd,
                  sd.u=accum$richness+1.96*accum$sd,SAE_cupins_NMDS_s,
                  ambiente[match(rownames(x),rownames(ambiente)),],id=names(SAE_cupins_short_by_campanha)[i],campanha=strsplit(names(SAE_cupins_short_by_campanha[i]),"-")[[1]][1])})
       

SAE_cupins_var_by_campanha<-do.call(rbind,var_campanha)


##

by(SAE_cupins_var_by_campanha,SAE_cupins_var_by_campanha$id,function(x){
    summary(lm(NMDS1~rich,data=x))})

##

print(ggplot(SAE_cupins_var_by_campanha,aes(site,rich))+
    geom_point()+geom_line(data=SAE_cupins_var_by_campanha)+
    geom_ribbon(data=SAE_cupins_var_by_campanha,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+
    facet_wrap(~id)+
    ylab("Número de espécies")+xlab("Número de amostras"))

dev.print(device = cairo_ps, file="Figuras/rarefacao.geral.campanhas.eps")
dev.copy2pdf(file="Figuras/rarefacao.geral.campanhas.pdf")
dev.off()


###########################
# Por campanha ou módulo

# Numero de especies estimado por campanha em cada modulo
est.rich_campanha_modulo<-do.call(rbind,lapply(SAE_cupins_short_by_campanha_modulo,function(x)specpool(x)))

#####Curvas de acumulo de especies

var_campanha_modulo<-lapply(
    seq_len(length(SAE_cupins_short_by_campanha_modulo)),
       function(i){
       x<- SAE_cupins_short_by_campanha_modulo[[i]]   
       accum<-specaccum(x)
       SAE_cupins_NMDS_s<-scores(metaMDS(vegdist(x,"horn")))     
       data.frame(rich=accum$richness,site=accum$sites,sd.l=accum$richness-1.96*accum$sd,
                  sd.u=accum$richness+1.96*accum$sd,by.mod=SAE_cupins_NMDS_s,ambiente[match(rownames(x),rownames(ambiente)),],
                  id=names(SAE_cupins_short_by_campanha_modulo)[i],
                  campanha=strsplit(names(SAE_cupins_short_by_campanha_modulo[i]),"-")[[1]][1])})


SAE_cupins_var_by_campanha_modulo<-do.call(rbind,var_campanha_modulo)

campanha_modulo<-do.call(rbind,strsplit(as.character(SAE_cupins_var_by_campanha_modulo$id),"-"))

especies_campanha_modulo<-do.call(rbind,SAE_cupins_short_by_campanha_modulo)

SAE_cupins_campanha_modulo_NMDS<-metaMDS(vegdist(especies_campanha_modulo,"horn"))

SAE_cupins_var_by_campanha_modulo<-data.frame(SAE_cupins_var_by_campanha_modulo,scores(SAE_cupins_campanha_modulo_NMDS))


##

by(SAE_cupins_var_by_campanha_modulo,SAE_cupins_var_by_campanha_modulo$id,function(x){
    summary(lm(by.mod.NMDS1~rich,data=x))})

##

#Cada grafico, um painel com todas as localidades em uma campanha 

for(i in levels(as.factor(campanha_modulo[,1]))){
    
    x<-SAE_cupins_var_by_campanha_modulo[campanha_modulo[,1]==i,]   

    print(ggplot(x,aes(site,rich))+geom_point()+geom_line(data=x)+
        geom_ribbon(data=x,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+facet_wrap(~id)+
            ylab("Número de espécies")+xlab("Número de amostras"))
    
    dev.print(device = cairo_ps,
              file=paste("Figuras/","rarefacao.","campanha.", i,".eps",sep=""))
    dev.copy2pdf(file=paste("Figuras/","rarefacao.","campanha.", i,".pdf",sep=""))
    dev.off()

}

#Cada grafico, um painel com uma localidade em todas as campanhas

for(i in levels(as.factor(paste(campanha_modulo[,2],campanha_modulo[,3])))){
    
    x<-SAE_cupins_var_by_campanha_modulo[paste(campanha_modulo[,2],campanha_modulo[,3])==i,]

    print(ggplot(x,aes(site,rich))+geom_point()+geom_line(data=x)+
        geom_ribbon(data=x,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+facet_wrap(~id)+
            ylab("Número de espécies")+xlab("Número de amostras"))
    
    dev.print(device = cairo_ps,
              file=paste("Figuras/","rarefacao.","modulo.", i,".eps",sep=""))
    dev.copy2pdf(file=paste("Figuras/","rarefacao.","modulo.", i,".pdf",sep=""))
    dev.off()

}

#############################
# Grafico da riqueza estimada em cada modulo por campanha
# Exatamente como no exemplo das normas em pdf

campanha_modulo2<-data.frame(do.call(rbind,strsplit(names(SAE_cupins_short_by_campanha_modulo),"-")))

#campanha_modulo2$X1<-as.ordered(campanha_modulo2$X1)
summary(campanha_modulo2)

est.rich.split_m_c<-data.frame(est.rich_campanha_modulo,campanha_modulo2)

print(ggplot(data=est.rich.split_m_c,aes(X1,jack1))+ geom_point()+
geom_errorbar(data=est.rich.split_m_c,aes(ymin=jack1-1.96*jack1.se,ymax=jack1+1.96*jack1.se),width=0.3)+
facet_wrap(~X2+X3)+ylab("Riqueza estimada (Jacknife 1)")+xlab("Campanha"))

dev.print(device = cairo_ps,
file=paste("Figuras/","riq.est.jack1.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","riq.est.jack1.pdf",sep=""))
dev.off()

#### Tabela com a riqueza estimada por varios índices (todas as combinacoes possiveis)

est.rich.all<-rbind(est.rich,
est.rich_modulo,
est.rich_campanha,
est.rich_campanha_modulo)

write.csv(round(est.rich.all,2),file="Tabelas_output/Riqueza.estimada.xls")

################

# Diversidade Beta

jost.diver<-function(x){

    gamma.d<-1/sum((colSums(x)/sum(x))^2)
    alpha.d<-1/mean(rowSums((x/rowSums(x))^2))
    beta.d <- gamma.d/alpha.d

    resu <- data.frame(alpha=alpha.d,beta=beta.d,gamma=gamma.d)
    resu
}


# Unico numero para cada modulo (Jost 2007)

jost.diversity_modulo<-do.call(rbind,lapply(SAE_cupins_short_by_modulo,jost.diver))
jost.diversity_modulo <- data.frame(jost.diversity_modulo,Modulo=rownames(jost.diversity_modulo))

print(ggplot(melt(jost.diversity_modulo,id="Modulo"),aes(Modulo,1-1/value,fill=variable))+geom_point(pch=21,cex=3)+ylab("Diversidade (Gini-Simpson)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_simpson.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_simpson.pdf",sep=""))
dev.off()


print(ggplot(melt(jost.diversity_modulo,id="Modulo"),aes(Modulo,value,fill=variable))+geom_point(pch=21,cex=3)+ylab("Diversidade (Número efetivo de espécies)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_hill.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_hill.pdf",sep=""))
dev.off()


################

jost.diversity_campanha<-do.call(rbind,lapply(SAE_cupins_short_by_campanha,jost.diver))
jost.diversity_campanha <- data.frame(jost.diversity_campanha,Campanha=rownames(jost.diversity_campanha))

jost.diversity_campanha_long<-melt(jost.diversity_campanha,id="Campanha")

print(ggplot(jost.diversity_campanha_long,aes(Campanha,1-1/value,group=variable,color=variable))+geom_point()+geom_line()+ylab("Diversidade (Gini-Simpson)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_simpson_campanha.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_simpson_campanha.pdf",sep=""))
dev.off()

print(ggplot(jost.diversity_campanha_long,aes(Campanha,value,group=variable,color=variable))+geom_point()+geom_line()+ylab("Diversidade (Número efetivo de espécies)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_hill_campanha.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_hill_campanha.pdf",sep=""))
dev.off()


###################











