source("WDIR.R")
 
SAE_cupins<-read.csv(paste(WDIR,"/sae_cupins.csv",sep=""),row.names=1)

ambiente<-read.csv(paste(WDIR,"/dados_ambientais.csv",sep=""),row.names=1)


#isoptera.geral <- read.csv(paste(WDIR,"/Isoptera.csv",sep=""))
#write.csv(SAE_cupins,file=paste(WDIR,"/ver.csv",sep=""))

#Remover os apico

SAE_cupins<-SAE_cupins[SAE_cupins$Subfamilia!="Apicotermitinae",]

#SAE_cupins$Campanha.char[-grep("[0-9]{2}",SAE_cupins$Campanha)]<-	paste(0,SAE_cupins$Campanha[-grep("[0-9]{2}",SAE_cupins$Campanha)],sep="")

range=1:max(SAE_cupins$Campanha) #mudar se não quiser considerar todas as campanhas
#range=1:10

SAE_cupins <- data.frame(SAE_cupins[!is.na(match(SAE_cupins$Campanha,range)),])

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

write.csv(SAE_cupins_encontros.modulo_sub2,file="Tabelas_output/Encontros.por.modulo.csv",quote=FALSE)

####

SAE_cupins_encontros.campanha<- by(SAE_cupins,paste(Familia,Subfamilia,sep=","),function(x)tapply(x$Num._Ind._.Colonias.Encontros.,list(x$Especie,factor(x$Campanha,levels=range)),sum))

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


write.csv(SAE_cupins_encontros.campanha_sub2,file="Tabelas_output/Encontros.por.campanha.csv",quote=FALSE)

#########


levels.mc<-paste(expand.grid(levels(Modulo),range)[,1],
expand.grid(levels(Modulo),range)[,2],sep="_")


SAE_cupins_encontros.modulo.campanha<- by(SAE_cupins,paste(Familia,Subfamilia,sep=","),function(x)tapply(x$Num._Ind._.Colonias.Encontros.,list(x$Especie,factor(paste(x$Modulo,x$Campanha,sep="_"),levels=levels.mc)),sum))


SAE_cupins_encontros.modulo.campanha_sub<-
	lapply(1:length(SAE_cupins_encontros.modulo.campanha),
		function(i){
			x<-SAE_cupins_encontros.modulo.campanha[[i]]
			if(is.null(x)){NULL}
			else{
				x[is.na(x)]<-0;
				data.frame("Familia,Subfamilia"=names(SAE_cupins_encontros.modulo.campanha)[i],x)[rowSums(x)>0,]                
			}
		}
	)

SAE_cupins_encontros.modulo.campanha_sub2<-do.call(rbind,SAE_cupins_encontros.modulo.campanha_sub)

SAE_cupins_encontros.modulo.campanha_sub2<-SAE_cupins_encontros.modulo.campanha_sub2[,c(TRUE,colSums(SAE_cupins_encontros.modulo.campanha_sub2[,-1])>0)]

colnames(SAE_cupins_encontros.modulo.campanha_sub2)<-gsub("\\.","\\,",colnames(SAE_cupins_encontros.modulo.campanha_sub2))

write.csv(SAE_cupins_encontros.modulo.campanha_sub2,file="Tabelas_output/Encontros.por.modulo.e.campanha.xls",quote=FALSE)


#######
#sum(SAE_cupins_encontros.modulo.campanha_sub2[,-1])

############################################

SAE_cupins_short<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="_"),Especie),sum)
SAE_cupins_short_var <- ambiente[match(rownames(SAE_cupins_short),rownames(ambiente)),]

SAE_cupins_short_by_modulo<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="_"),Especie,paste(Modulo,sep="_")),sum,simplify=TRUE)

SAE_cupins_short_by_campanha<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="_"),Especie,Campanha),sum,simplify=TRUE)
#SAE_cupins_short_by_campanha_var <- lapply(SAE_cupins_short_by_campanha,function(x) ambiente[match(rownames(x),rownames(ambiente)),])

SAE_cupins_short_by_campanha_modulo<-tapply(Num._Ind._.Colonias.Encontros.,list(paste(Modulo,Transecto,Ponto,sep="_"),Especie,paste(Campanha,Modulo,sep="_")),sum,simplify=TRUE)
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


names(SAE_cupins_short_by_modulo) <- levels(as.factor(paste(Modulo,sep="_")))
#str(SAE_cupins_short_by_modulo)

names(SAE_cupins_short_by_campanha) <- range
#str(SAE_cupins_short_by_campanha)
 
names(SAE_cupins_short_by_campanha_modulo) <- sort(factor(unique(paste(Modulo,Campanha,sep="_")),levels=levels.mc))


#str(SAE_cupins_short_by_campanha_modulo)


######################################################

# Aqui começam as análises

########################################################

library(vegan)
library(ggplot2)
library(reshape)
library(lme4)
library(BiodiversityR)


# Abundancia

#abund_c_m <- unlist(lapply(SAE_cupins_short_by_campanha_modulo,function(x)sum(as.matrix(x))))
abund_c_m <- unlist(lapply(SAE_cupins_short_by_campanha_modulo,function(x)sum(x)))

#Numero estimado de especies
est.rich <- specpool(SAE_cupins_short)

#Curva de acumulo de especies
accum <- specaccum(SAE_cupins_short)


SAE_cupins_geral <- data.frame(rich=accum$richness,site=accum$sites,sd.l=accum$richness-1.96*accum$sd,sd.u=accum$richness+1.96*accum$sd,id="Total")

theme_new <- theme_set(theme_bw())

theme_new <- theme_update(axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(colour = "black",size=16),
                  axis.title.y = element_text(colour = "black",angle=90,size = 16),
                  panel.background = element_rect(fill="white"),
                  plot.background = element_rect(fill="white"))


# Curva de acumulo de esp?cies total

print(ggplot(SAE_cupins_geral,aes(site,rich))+
    geom_point()+geom_line(data=SAE_cupins_geral)+
    geom_ribbon(data=SAE_cupins_geral,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+
    facet_wrap(~id)+
    ylab("N?mero de espécies")+xlab("Número de amostras"))

dev.print(device = cairo_ps, file="Figuras/rarefacao.eps")
dev.copy2pdf(file="Figuras/rarefacao.geral.pdf")
dev.off()


# rank de distribuicao das abundancias

hist.abund <- data.frame(abund=sort(colSums(SAE_cupins_short[,colSums(SAE_cupins_short)>0]),decreasing = T),names=1:sum(colSums(SAE_cupins_short)>0))

print(ggplot(data.frame(ab=rep(hist.abund$names,hist.abund$abund)),aes(ab))+geom_bar()+xlab("Rank de espécies")+ylab("Frequência"))

rank <- rankabundance(SAE_cupins_short)

print(ggplot(data.frame(rank),aes(rank,abundance+1))+geom_point()+scale_y_log10()+xlab("Rank de espécies")+ylab("Frequência (log)")+geom_smooth(stat = "smooth"))

dev.print(device = cairo_ps, file="Figuras/rank.abundancias.eps")
dev.copy2pdf(file="Figuras/rank.abundancias.pdf")
dev.off()

###

hist.abund_by_campanha_temp<- lapply(SAE_cupins_short_by_campanha,rankabundance)
hist.abund_by_campanha_temp2<- lapply(1:length(hist.abund_by_campanha_temp),function(i){data.frame(hist.abund_by_campanha_temp[[i]],id=i)})
hist.abund_by_campanha <- do.call(rbind,hist.abund_by_campanha_temp2)
hist.abund_by_campanha <- hist.abund_by_campanha[hist.abund_by_campanha$abundance>0,]

print(ggplot(hist.abund_by_campanha,aes(rank,abundance+1))+geom_point()+scale_y_log10()+xlab("Rank de espécies")+ylab("Frequência (log)")+geom_smooth(method="loess")+facet_wrap(~id))

dev.print(device = cairo_ps, file="Figuras/rank.abundancias_campanhas.eps")
dev.copy2pdf(file="Figuras/rank.abundancias_campanhas.pdf")
dev.off()


for(i in 1:max(hist.abund_by_campanha$id)){

temp<-hist.abund_by_campanha[hist.abund_by_campanha$id==i,]
    
print(ggplot(temp,aes(rank,abundance+1))+geom_point()+scale_y_log10(breaks = round(seq(min(temp$abundance), max(temp$abundance), by = 2),1))+xlab("Rank de esp?cies")+ylab("Frequ?ncia")+geom_smooth(method="loess"))

dev.print(device = cairo_ps, file=paste("Figuras/rank.abundancias_modulo",i,".eps",sep = ""))
dev.copy2pdf(file=paste("Figuras/rank.abundancias_modulo",i,".pdf",sep = ""))
dev.off()

}



###

hist.abund_by_modulo_temp<- lapply(SAE_cupins_short_by_modulo,rankabundance)
hist.abund_by_modulo_temp2<- lapply(1:length(hist.abund_by_modulo_temp),function(i){data.frame(hist.abund_by_modulo_temp[[i]],id=names(SAE_cupins_short_by_modulo)[i])})
hist.abund_by_modulo <- do.call(rbind,hist.abund_by_modulo_temp2)
hist.abund_by_modulo <- hist.abund_by_modulo[hist.abund_by_modulo$abundance>0,]

print(ggplot(hist.abund_by_modulo,aes(rank,abundance+1))+geom_point()+scale_y_log10()+xlab("Rank de esp?cies")+ylab("Frequ?ncia (log)")+geom_smooth(method="loess")+facet_wrap(~id))

dev.print(device = cairo_ps, file="Figuras/rank.abundancias_modulos.eps")
dev.copy2pdf(file="Figuras/rank.abundancias_modulos.pdf")
dev.off()

###

hist.abund_by_campanha_modulo_temp<- lapply(SAE_cupins_short_by_campanha_modulo,rankabundance)
hist.abund_by_campanha_modulo_temp2<- lapply(1:length(hist.abund_by_campanha_modulo_temp),function(i){data.frame(hist.abund_by_campanha_modulo_temp[[i]],id=names(SAE_cupins_short_by_campanha_modulo)[i])})
hist.abund_by_campanha_modulo <- do.call(rbind,hist.abund_by_campanha_modulo_temp2)
hist.abund_by_campanha_modulo <- hist.abund_by_campanha_modulo[hist.abund_by_campanha_modulo$abundance>0,]

for (i in levels(hist.abund_by_campanha_modulo$id)){

temp<-hist.abund_by_campanha_modulo[hist.abund_by_campanha_modulo$id==i,]

print(ggplot(temp,aes(rank,abundance+1))+geom_point()+scale_y_log10(breaks = round(seq(min(temp$abundance), max(temp$abundance), by = 1),1))+xlab("Rank de esp?cies")+ylab("Frequ?ncia")+geom_smooth(method="loess"))

dev.print(device = cairo_ps, file=paste("Figuras/rank.abundancias_",i,".eps",sep = ""))
dev.copy2pdf(file=paste("Figuras/rank.abundancias_",i,".pdf",sep = ""))
dev.off()

}



####



###

SAE_cupins_NMDS<-metaMDS(vegdist(SAE_cupins_short,"horn"))

SAE_cupins_short_var<-data.frame(freq=rowSums(SAE_cupins_short),rich=rowSums(SAE_cupins_short>0),SAE_cupins_short_var,scores(SAE_cupins_NMDS))

SAE_cupins_short_var_std<-data.frame(SAE_cupins_short_var[,!sapply(SAE_cupins_short_var,is.numeric)],decostand(SAE_cupins_short_var[,sapply(SAE_cupins_short_var,is.numeric)],"standardize",na.rm=TRUE))


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

##

print(ggplot(SAE_cupins_var_by_modulo,aes(site,rich))+
    geom_point()+geom_line(data=SAE_cupins_var_by_modulo)+
    geom_ribbon(data=SAE_cupins_var_by_modulo,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+
    facet_wrap(~id)+
    ylab("Número de espécies")+xlab("Número de amostras"))

dev.print(device = cairo_ps, file="Figuras/rarefacao.geral.modulos.eps")
dev.copy2pdf(file="Figuras/rarefacao.geral.modulos.pdf")
dev.off()


###


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
                  ambiente[match(rownames(x),rownames(ambiente)),],id=names(SAE_cupins_short_by_campanha)[i],campanha=strsplit(names(SAE_cupins_short_by_campanha[i]),"_")[[1]][1])})
       

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

### Por campanha - uma campanha em cada imagem separadamente

for (i in levels(SAE_cupins_var_by_campanha$id)){

temp<-SAE_cupins_var_by_campanha[SAE_cupins_var_by_campanha$id==i,]

print(ggplot(temp,aes(site,rich))+
    geom_point()+geom_line(data=temp)+
    geom_ribbon(data=temp,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+
    facet_wrap(~id)+
    ylab("N?mero de esp?cies")+xlab("N?mero de amostras"))

dev.print(device = cairo_ps, file=paste("Figuras/rarefacao.geral.campanha.",i,".eps",sep=""))
dev.copy2pdf(file=paste("Figuras/rarefacao.geral.campanha.",i,".pdf",sep=""))
dev.off()

}

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
                  campanha=strsplit(names(SAE_cupins_short_by_campanha_modulo[i]),"_")[[1]][1])})


SAE_cupins_var_by_campanha_modulo<-do.call(rbind,var_campanha_modulo)

campanha_modulo<-do.call(rbind,strsplit(as.character(SAE_cupins_var_by_campanha_modulo$id),"_"))

especies_campanha_modulo<-do.call(rbind,SAE_cupins_short_by_campanha_modulo)

SAE_cupins_campanha_modulo_NMDS<-metaMDS(vegdist(especies_campanha_modulo,"horn"))

SAE_cupins_var_by_campanha_modulo<-data.frame(SAE_cupins_var_by_campanha_modulo,scores(SAE_cupins_campanha_modulo_NMDS))


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

levels(as.factor(paste(campanha_modulo[,2])))

sort(factor(unique(paste(Modulo,Campanha,sep="_")),levels=levels.mc))

for(i in range){
    
    x<-SAE_cupins_var_by_campanha_modulo[paste(campanha_modulo[,2])==i,]

    print(ggplot(x,aes(site,rich))+geom_point()+geom_line(data=x)+
        geom_ribbon(data=x,aes(ymin=sd.l,ymax=sd.u),alpha=0.3)+facet_wrap(~id)+
            ylab("Número de espécies")+xlab("Número de amostras"))
    
    dev.print(device = cairo_ps,
              file=paste("Figuras/","rarefacao.","modulo.", i,".eps",sep=""))
    dev.copy2pdf(file=paste("Figuras/","rarefacao.","modulo.", i,".pdf",sep=""))
    dev.off()

}

#############################
# Grafico da abundância e riqueza estimada em cada modulo por campanha
# Exatamente como no exemplo das normas em pdf

campanha_modulo2<-data.frame(do.call(rbind,strsplit(names(SAE_cupins_short_by_campanha_modulo),"_")))

#campanha_modulo2$X1<-as.ordered(campanha_modulo2$X1)
summary(campanha_modulo2)

est.rich.split_m_c<-data.frame(est.rich_campanha_modulo,campanha_modulo2,abund_c_m)

###Abundancia - modulo e campanha

print(ggplot(data=est.rich.split_m_c,aes(X1,abund_c_m))+ geom_bar(stat="bin")+
facet_wrap(~X2)+ylab("Abundância")+xlab("Campanha"))

dev.print(device = cairo_ps,
file=paste("Figuras/","abund.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","abund.pdf",sep=""))
dev.off()

### Abundancia - campanha (um grafico por campanha)

for(i in levels(campanha_modulo2[,1])){  
print(ggplot(data=est.rich.split_m_c[campanha_modulo2[,1]==i,],aes(X2,abund_c_m))+ geom_bar(stat="bin")+ylab("Abundância")+xlab("Módulo"))   
dev.print(device = cairo_ps,
file=paste("Figuras/","abund.","camp-",i,".eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","abund.","camp-",i,".pdf",sep=""))
dev.off()
}


### Abundancia - modulo (um grafico por modulo)

for(i in levels(campanha_modulo2[,2])){    
print(ggplot(data=est.rich.split_m_c[campanha_modulo2[,2]==i,],aes(X1,abund_c_m))+ geom_bar(stat="bin")+ylab("Abundância")+xlab("Módulo"))
dev.print(device = cairo_ps,
file=paste("Figuras/","abund.","mod-",i,".eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","abund.","mod-",i,".pdf",sep=""))
dev.off()
}

#levels.mc.restrict<-sort(factor(unique(paste(Modulo,Campanha,sep="_")),levels=levels.mc))


### Riqueza

est.rich.split_m_c$X2<-factor(est.rich.split_m_c$X2,levels=range)

print(ggplot(data=est.rich.split_m_c,aes(X1,jack1))+ geom_point()+
geom_errorbar(data=est.rich.split_m_c,aes(ymin=jack1-1.96*jack1.se,ymax=jack1+1.96*jack1.se),width=0.3)+
facet_wrap(~X2)+ylab("Riqueza estimada (Jacknife 1)")+xlab("Campanha"))

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

print(ggplot(melt(jost.diversity_modulo,id="Modulo",variable_name = "Diversidade"),aes(Modulo,1-1/value,fill=Diversidade))+geom_point(pch=21,cex=3)+ylab("Diversidade (Gini-Simpson)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_simpson.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_simpson.pdf",sep=""))
dev.off()

print(ggplot(melt(jost.diversity_modulo,id="Modulo",variable_name = "Diversidade"),aes(Modulo,value,fill=Diversidade))+geom_point(pch=21,cex=3)+ylab("Diversidade (Número efetivo de espécies)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_hill.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_hill.pdf",sep=""))
dev.off()


################

jost.diversity_campanha<-do.call(rbind,lapply(SAE_cupins_short_by_campanha,jost.diver))
jost.diversity_campanha <- data.frame(jost.diversity_campanha,Campanha=factor(rownames(jost.diversity_campanha),levels=range))

jost.diversity_campanha_long<-melt(jost.diversity_campanha,id="Campanha",variable_name = "Diversidade")

print(ggplot(jost.diversity_campanha_long,aes(Campanha,1-1/value,group=Diversidade,color=Diversidade))+geom_point()+geom_line()+ylab("Diversidade (Gini-Simpson)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_simpson_campanha.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_simpson_campanha.pdf",sep=""))
dev.off()

print(ggplot(jost.diversity_campanha_long,aes(Campanha,value,group=Diversidade,color=Diversidade))+geom_point()+geom_line()+ylab("Diversidade (Número efetivo de espécies)"))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_hill_campanha.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_hill_campanha.pdf",sep=""))
dev.off()

###################

################

jost.diversity_campanha_modulo<-do.call(rbind,lapply(SAE_cupins_short_by_campanha_modulo,jost.diver))

jost.diversity_campanha_modulo <- data.frame(jost.diversity_campanha_modulo,campanha_modulo2)

jost.diversity_campanha_modulo_long<-melt(jost.diversity_campanha_modulo,variable_name = "Diversidade")

jost.diversity_campanha_modulo_long$X2<-factor(jost.diversity_campanha_modulo_long$X2,levels=range)

print(ggplot(jost.diversity_campanha_modulo_long,aes(X1,1-1/value,group=Diversidade,color=Diversidade))+geom_point()+geom_line()+ylab("Diversidade (Gini-Simpson)")+xlab("Campanha")+facet_wrap(~X2))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_simpson_campanha_modulo.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_simpson_campanha_modulo.pdf",sep=""))
dev.off()

print(ggplot(jost.diversity_campanha_modulo_long,aes(X1,value,group=Diversidade,color=Diversidade))+geom_point()+geom_line()+ylab("Diversidade (Número efetivo de espécies)")+xlab("Campanha")+facet_wrap(~X2))

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_hill_campanha_modulo.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_hill_campanha_modulo.pdf",sep=""))
dev.off()

###################
#Efeitos sazonais e temporais

#Cada modulo foi considerado uma unidade, pois nao ha dados
#suficientes para analisar cada parcela por campanha como unidade

#A media das variaveis ambientais foi calculada para cada modulo (nao parcela)
#e utilizada como variavel preditora

jost.diversity_campanha_modulo$season <- as.factor(c("Seca","Chuvosa")[as.numeric(jost.diversity_campanha_modulo$X1)%%2+1])

jost.diversity_campanha_modulo_long$season <- as.factor(c("Seca","Chuvosa")[as.numeric(jost.diversity_campanha_modulo_long[,1])%%2+1])

mean_var_modulo<-apply(ambiente[,sapply(ambiente,is.numeric)],2,function(x){
tapply(x,ambiente$Modulo,mean,na.rm=TRUE)})

SAE_cupins_c_m_var <- data.frame(est.rich.split_m_c,jost.diversity_campanha_modulo,
mean_var_modulo[match(est.rich.split_m_c$X2,rownames(mean_var_modulo)),])

ggplot(SAE_cupins_c_m_var,aes(season,1-1/gamma))+geom_point()+
    geom_boxplot()+
    ylab("Diversidade (Gini-Simpson)")+
    xlab("Estação")+
    facet_wrap(~X2)

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_simpson_modulo_estacao.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_simpson_modulo_estacao.pdf",sep=""))
dev.off()

ggplot(SAE_cupins_c_m_var,aes(season,gamma))+geom_point()+
    geom_boxplot()+
    ylab("Diversidade (Número efetivo de espécies)")+
    xlab("Estação")+
    facet_wrap(~X2)

dev.print(device = cairo_ps,
file=paste("Figuras/","Diversidade_particionada_hill_modulo_estacao.eps",sep=""))
dev.copy2pdf(file=paste("Figuras/","Diversidade_particionada_hill_modulo_estacao.pdf",sep=""))
dev.off()

summary(aov(jack1~season+X2,data=SAE_cupins_c_m_var))
summary(aov(jack1~season,data=SAE_cupins_c_m_var))

summary(aov(Species~season+X2,data=SAE_cupins_c_m_var))
summary(aov(Species~season,data=SAE_cupins_c_m_var))

summary(aov(gamma~season+X2,data=SAE_cupins_c_m_var))
summary(aov(gamma~season,data=SAE_cupins_c_m_var))

tapply(SAE_cupins_c_m_var$gamma,SAE_cupins_c_m_var$season,mean)

########

########

summary(freq_glm<-glm(SAE_cupins_short_var$freq~0+Altitude+P+N,data=SAE_cupins_short_var_std,family=poisson))

#R squared
cor(SAE_cupins_short_var$freq[!is.na(SAE_cupins_short_var$Altitude)],predict(freq_glm,type="link"))


summary(rich_glm<-glm(SAE_cupins_short_var$rich~0+Altitude+P+N,data=SAE_cupins_short_var_std,family=poisson))
cor(SAE_cupins_short_var$rich[!is.na(SAE_cupins_short_var$Altitude)],predict(rich_glm,type="link"))


summary(lm(NMDS1~Altitude+P+N,data=SAE_cupins_short_var_std))
  

SAE_cupins_RDA<-rda(SAE_cupins_short[!is.na(SAE_cupins_short_var$Altitude),]~Altitude+P+N,data=SAE_cupins_short_var_std[!is.na(SAE_cupins_short_var$Altitude),],scale=TRUE
)

anova(SAE_cupins_RDA,by="terms")


find_hull <- function(x) x[chull(x$NMDS1, x$NMDS2), ]

hulls <- ddply(SAE_cupins_short_var, "Modulo", find_hull)

print(ggplot(data=SAE_cupins_short_var,aes(NMDS1,NMDS2,fill=Modulo,col=Modulo))+geom_point(pch=21)+geom_text(cex=3,aes(NMDS1,NMDS2,label=Modulo), hjust = 1.3,vjust=1.3)+geom_polygon(data=hulls,fill=NA))

dev.print(device = cairo_ps, file="Figuras/NMDS1.NMDS2.geral.modulos.eps")
dev.copy2pdf(file="Figuras/NMDS1.NMDS2.geral.modulos.geral.modulos.pdf")
dev.off()


# Composicao de especies por modulo

ggplot(data=SAE_cupins_var_by_modulo,aes(NMDS1,NMDS2,fill=Altitude,symbols=Modulo))+geom_point(pch=21)+geom_text(cex=3,aes(NMDS1,NMDS2,label=Modulo), hjust = 1.3,vjust=1.3)+facet_wrap(~Modulo)

ggplot(data=SAE_cupins_var_by_modulo,aes(Altitude,rich))+geom_point()+geom_text(cex=3,aes(rich,NMDS1,label=""), hjust = 1.3,vjust=1.3)+facet_grid(~Modulo)


# Composicao de especies por campanha

#head(SAE_cupins_var_by_campanha_modulo)

#hulls2 <- ddply(SAE_cupins_var_by_campanha_modulo, "Modulo", find_hull)

#ggplot(data=SAE_cupins_var_by_campanha_modulo,aes(by.mod.NMDS1,by.mod.NMDS2,fill=Altitude))+geom_point(pch=21,fill=Modulo)+geom_text(cex=3,aes(by.mod.NMDS1,by.mod.NMDS2,label=Modulo), hjust = 1.3,vjust=1.3)+facet_wrap(~campanha)

#ggplot(data=SAE_cupins_var_by_modulo,aes(Altitude,rich))+geom_point()+geom_text(cex=3,aes(rich,NMDS1,label=""), hjust = 1.3,vjust=1.3)+facet_grid(~Modulo)

##

paraponcho<-SAE_cupins_short[!is.na(SAE_cupins_short_var$Altitude),]
paraponchovar<-SAE_cupins_short_var[!is.na(SAE_cupins_short_var$Altitude),]

poncho3.2(paraponcho[,colSums(paraponcho)>0]>0,gradient=paraponchovar$NMDS1,border=2,lwd=0,lty.lines=3)

plot(hclust(vegdist(t(sapply(SAE_cupins_short_by_campanha,colSums)),"horn")),xlab="Campanha",ylab="Similaridade",main="Dendrograma - Anlálise de agrupamentos",)

dev.print(device = cairo_ps, file="Figuras/Cluster.geral.campanhas.eps")
dev.copy2pdf(file="Figuras/Cluster.geral.geral.campanhas.pdf")
dev.off()

################
################
################
