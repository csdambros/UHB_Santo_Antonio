################# Script - Análises Rondônia
# Criado por RAZ em XX Abril 2013
# Modificado por CSD em 06 Abril 2013

############

library(vegan)
source("http://dl.dropbox.com/s/lx2o5p1g79s69qd/PIE.R?token_hash=AAFCn_pb496kcaZN-XePhLDh-Eno_9v2lRnckuzwO6FRiQ&dl=1")

############

ambiente<-read.csv('dados ambientais.csv')
tabela<-read.csv("Planilha_UHE_CAM._1.csv")
ncol(tabela)
nrow(tabela)
head(ambiente)

ta_local<-tabela[,"Módulo.Grid"]
ta_parcela<-tabela[,"Parcela"]
ta_especie<-tabela[,"Taxon"]
MTP=paste(ta_local,ta_parcela)

local.names<-data.frame(strsplit(levels(factor(MTP)),' '))[1,]
local.numbers<-tapply(ta_local,MTP,function(x)x[1])

ta_abundancia<-tapply(rep(1,nrow(tabela)),c,sum)
ta_abundancia<-ifelse(is.na(ta_abundancia),0,ta_abundancia)

ta_abundancia.modulo<-tapply(rep(1,nrow(tabela)),list(ta_local,ta_especie),sum)
ta_abundancia.modulo<-ifelse(is.na(ta_abundancia.modulo),0,ta_abundancia.modulo)


ta_presença=ifelse(ta_abundancia>0,1,0)
tab_presença<-colSums(ta_presença)

write.csv(ta_abundancia.modulo,file='Tabela abundância.csv')

#max(ta_abundancia)

####### Abundância por módulo

barplot(rowSums(ta_abundancia.modulo),names.arg=c('I. Búfalo','I. Pedra','J. Paraná','M. Novo','Teotônio'),col=1,ylim=c(0,70),
ylab='Abundância',xlab='Módulo')
box()

####### Riqueza por módulo

barplot(rowSums(ifelse(ta_abundancia.modulo>0,1,0)),names.arg=c('I. Búfalo','I. Pedra','J. Paraná','M. Novo','Teotônio'),col=1,ylim=c(0,50),
ylab='Riqueza',xlab='Módulo')
box()


####### Estimação de riqueza

specpool(ta_presença)

####### Curvas de acumulo de espécie

by(ta_presença,local.numbers,function(x){specpool(x)})

plot(specaccum(ta_presença),ci.type="line",ci.lty=2,ylab="Espécies",xlab="Parcelas")

#######

par(mfrow=c(3,2))

for(i in 1:max(local.numbers)){

plot(specaccum(ta_presença[local.numbers==i,]),
,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Parcelas",main=levels(factor(ta_local))[i])
}

##########################

beta.local<-by(ta_presença,local.numbers,function(x){(1/simpson(colSums(x)))/(1/mean(apply(x,1,simpson)))})

barplot(beta.local,names.arg=c('I. Búfalo','I. Pedra','J. Paraná','M. Novo','Teotônio'),col=1,ylim=c(0,1),
ylab='Diversidade beta (Simpson)',xlab='Módulo')
box()

###########################

NMDS<-metaMDS(vegdist(ta_presença,'jaccard'),k=2)

plot(scores(NMDS),type='n')
text(scores(NMDS),levels(factor(ta_local))[local.numbers],cex=0.4)

ordihull(NMDS,local.numbers)
ordiellipse(NMDS,local.numbers)

########################### Matriz de similaridade

d<-vegdist(ta_abundancia.modulo,'jaccard')
plot(d)
plot(hclust(d),xlab="",ylab="",main="Dendograma")

###########################


#Aqui terminou o que o Cristian fez

########################### 




Abundancia<-sort(tab_presença,decreasing=TRUE)
barplot(Abundancia,,axisnames=F,xlab="Rank de espécies",ylab="Frequência de ocorência")

PCA1=
prcomp(tab_presença)
PCA1$x


################## Riqueza e Abundancia###############
library(vegan)

taxon<-tabela[,"Taxon"]
riqueza<-tapply(rep(1,nrow(tabela)), taxon,sum)
nrow(riqueza)
sort(riqueza,decreasing=TRUE)

familia<-tabela[,"Família"]
modulo<-tabela[,"Módulo.Grid"]

MTP=paste(modulo,taxon)
riqueza.mod<-tapply(rep(1,nrow(tabela)),list(taxon,modulo),sum)
riqueza.mod[riqueza.mod>0]=1
riqueza_mod<-ifelse(is.na(riqueza.mod),0,riqueza.mod)

colSums(riqueza_mod)########## Riqueza por módulo############

################# Tabela de Riqueza por modulo ##################
subfami<-tabela[,"Subfamília"]
MTP=paste(familia,subfami,taxon)

tab_riq_mod<-tapply(rep(1,nrow(tabela)),list(MTP,modulo),sum)
tab_riq_mod<-ifelse(is.na(tab_riq_mod),0,tab_riq_mod)
tab_riq_mod

sub<-tabela[,"Subparcela"]

###################Curva de acumulo de especie modulos##############

tabela<-read.csv("Planilha_UHE_CAM._1.csv")
M_modulo<-tabela[,"Módulo.Grid"]
M_taxon<-tabela[,"Taxon"]
M_parcela<-tabela[,"Parcela"]
M_lado<-tabela[,"Lado"]
MTP=paste(M_parcela,M_modulo,M_lado)
M_presença<-tapply(rep(1,nrow(tabela)),list(MTP,M_taxon),sum)
M_presença[M_presença>0]=1
M_presença<-ifelse(is.na(M_presença),0,M_presença)
M_nome=specaccum(M_presença)
plot(M_nome,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Subparcelas")
specpool(M_presença)

####################Curva de acumulo de especie bufalo######################

library(vegan)
bufalo<-read.csv("Bufalo.csv")
B_modulo<-bufalo[,"Módulo.Grid"]
B_taxon<-bufalo[,"Taxon"]
B_parcela<-bufalo[,"Parcela"]
B_lado<-bufalo[,"Lado"]
MTP=paste(B_lado,B_parcela,B_modulo)
B_presença<-tapply(rep(1,nrow(bufalo)),list(MTP,B_taxon),sum)
B_presença[B_presença>0]=1
B_presença<-ifelse(is.na(B_presença),0,B_presença)
B_nome=specaccum(B_presença)
plot(B_nome,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Subparcelas em IB")
specpool(B_presença)

##################Curva de acumulo de especie pedra ###################3333

pedra<-read.csv("pedra.csv")
P_modulo<-pedra[,"Módulo.Grid"]
P_taxon<-pedra[,"Taxon"]
P_parcela<-pedra[,"Parcela"]
P_lado<-pedra[,"Lado"]
MTP=paste(P_lado,P_parcela,P_modulo)
P_presença<-tapply(rep(1,nrow(pedra)),list(MTP,P_taxon),sum)
P_presença[P_presença>0]=1
P_presença<-ifelse(is.na(P_presença),0,P_presença)
P_nome=specaccum(P_presença)
plot(P_nome,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Subparcelas em IP")
specpool(P_presença)

######################Curva de acumulo de especie parana #############

parana<-read.csv("parana.csv")
p_modulo<-parana[,"Módulo.Grid"]
p_taxon<-parana[,"Taxon"]
p_parcela<-parana[,"Parcela"]
p_lado<-parana[,"Lado"]
MTP=paste(p_lado,p_parcela,p_modulo)
p_presença<-tapply(rep(1,nrow(parana)),list(MTP,p_taxon),sum)
p_presença[p_presença>0]=1
p_presença<-ifelse(is.na(p_presença),0,p_presença)
p_nome=specaccum(p_presença)
plot(p_nome,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Subparcelas em JP")
specpool(p_presença)

#######################Curva de acumulo de especie M.novo##############

novo<-read.csv("novo.csv")
n_modulo<-novo[,"Módulo.Grid"]
n_taxon<-novo[,"Taxon"]
n_parcela<-novo[,"Parcela"]
n_lado<-novo[,"Lado"]
MTP=paste(n_lado,n_parcela,n_modulo)
n_presença<-tapply(rep(1,nrow(novo)),list(MTP,n_taxon),sum)
n_presença[n_presença>0]=1
n_presença<-ifelse(is.na(n_presença),0,n_presença)
n_nome=specaccum(n_presença)
plot(n_nome,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Subparcelas em MN")
specpool(n_presença)

########################Curva de acumulo de especie teotonio ###############

teotonio<-read.csv("teotonio.csv")
t_modulo<-teotonio[,"Módulo.Grid"]
t_taxon<-teotonio[,"Taxon"]
t_parcela<-teotonio[,"Parcela"]
t_lado<-teotonio[,"Lado"]
MTP=paste(t_lado,t_parcela,t_modulo)
t_presença<-tapply(rep(1,nrow(teotonio)),list(MTP,t_taxon),sum)
t_presença[t_presença>0]=1
t_presença<-ifelse(is.na(t_presença),0,t_presença)
t_nome=specaccum(t_presença)
plot(t_nome,ci.type="line",ci.lty=2,ylab="Espécies",xlab="Subparcelas em TE")
specpool(t_presença)

par(mfrow=c(3,2))

###################### Indice de Similaridade ##################

pedra
pe_local<-pedra[,"Módulo.Grid"]
pe_parcela<-pedra[,"Parcela"]
pe_especie<-pedra[,"Taxon"]
MTP=paste(pe_local,pe_parcela)
pe_presença<-tapply(rep(1,nrow(pedra)),list(MTP,pe_especie),sum)
pe_presença=ifelse(is.na(pe_presença),0,pe_presença)
ped_presença<-colSums(pe_presença)

bufalo
bu_local<-bufalo[,"Módulo.Grid"]
bu_parcela<-bufalo[,"Parcela"]
bu_especie<-bufalo[,"Taxon"]
MTP=paste(bu_local,bu_parcela)
bu_presença<-tapply(rep(1,nrow(bufalo)),list(MTP,bu_especie),sum)
bu_presença=ifelse(is.na(bu_presença),0,bu_presença)
buf_presença<-colSums(bu_presença)

parana
pa_local<-parana[,"Módulo.Grid"]
pa_parcela<-parana[,"Parcela"]
pa_especie<-parana[,"Taxon"]
MTP=paste(pa_local,pa_parcela)
pa_presença<-tapply(rep(1,nrow(parana)),list(MTP,pa_especie),sum)
pa_presença=ifelse(is.na(pa_presença),0,pa_presença)
par_presença<-colSums(pa_presença)

novo
no_local<-novo[,"Módulo.Grid"]
no_parcela<-novo[,"Parcela"]
no_especie<-novo[,"Taxon"]
MTP=paste(no_local,no_parcela)
no_presença<-tapply(rep(1,nrow(novo)),list(MTP,no_especie),sum)
no_presença=ifelse(is.na(no_presença),0,no_presença)
nov_presença<-colSums(no_presença)

teotonio
te_local<-teotonio[,"Módulo.Grid"]
te_parcela<-teotonio[,"Parcela"]
te_especie<-teotonio[,"Taxon"]
MTP=paste(te_local,te_parcela)
te_presença<-tapply(rep(1,nrow(teotonio)),list(MTP,te_especie),sum)
te_presença=ifelse(is.na(te_presença),0,te_presença)
teo_presença<-colSums(te_presença)

