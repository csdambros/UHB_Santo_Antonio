################# Script - An�lises Rond�nia
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

ta_local<-tabela[,"M�dulo.Grid"]
ta_parcela<-tabela[,"Parcela"]
ta_especie<-tabela[,"Taxon"]
MTP=paste(ta_local,ta_parcela)

local.names<-data.frame(strsplit(levels(factor(MTP)),' '))[1,]
local.numbers<-tapply(ta_local,MTP,function(x)x[1])

ta_abundancia<-tapply(rep(1,nrow(tabela)),c,sum)
ta_abundancia<-ifelse(is.na(ta_abundancia),0,ta_abundancia)

ta_abundancia.modulo<-tapply(rep(1,nrow(tabela)),list(ta_local,ta_especie),sum)
ta_abundancia.modulo<-ifelse(is.na(ta_abundancia.modulo),0,ta_abundancia.modulo)


ta_presen�a=ifelse(ta_abundancia>0,1,0)
tab_presen�a<-colSums(ta_presen�a)

write.csv(ta_abundancia.modulo,file='Tabela abund�ncia.csv')

#max(ta_abundancia)

####### Abund�ncia por m�dulo

barplot(rowSums(ta_abundancia.modulo),names.arg=c('I. B�falo','I. Pedra','J. Paran�','M. Novo','Teot�nio'),col=1,ylim=c(0,70),
ylab='Abund�ncia',xlab='M�dulo')
box()

####### Riqueza por m�dulo

barplot(rowSums(ifelse(ta_abundancia.modulo>0,1,0)),names.arg=c('I. B�falo','I. Pedra','J. Paran�','M. Novo','Teot�nio'),col=1,ylim=c(0,50),
ylab='Riqueza',xlab='M�dulo')
box()


####### Estima��o de riqueza

specpool(ta_presen�a)

####### Curvas de acumulo de esp�cie

by(ta_presen�a,local.numbers,function(x){specpool(x)})

plot(specaccum(ta_presen�a),ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Parcelas")

#######

par(mfrow=c(3,2))

for(i in 1:max(local.numbers)){

plot(specaccum(ta_presen�a[local.numbers==i,]),
,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Parcelas",main=levels(factor(ta_local))[i])
}

##########################

beta.local<-by(ta_presen�a,local.numbers,function(x){(1/simpson(colSums(x)))/(1/mean(apply(x,1,simpson)))})

barplot(beta.local,names.arg=c('I. B�falo','I. Pedra','J. Paran�','M. Novo','Teot�nio'),col=1,ylim=c(0,1),
ylab='Diversidade beta (Simpson)',xlab='M�dulo')
box()

###########################

NMDS<-metaMDS(vegdist(ta_presen�a,'jaccard'),k=2)

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




Abundancia<-sort(tab_presen�a,decreasing=TRUE)
barplot(Abundancia,,axisnames=F,xlab="Rank de esp�cies",ylab="Frequ�ncia de ocor�ncia")

PCA1=
prcomp(tab_presen�a)
PCA1$x


################## Riqueza e Abundancia###############
library(vegan)

taxon<-tabela[,"Taxon"]
riqueza<-tapply(rep(1,nrow(tabela)), taxon,sum)
nrow(riqueza)
sort(riqueza,decreasing=TRUE)

familia<-tabela[,"Fam�lia"]
modulo<-tabela[,"M�dulo.Grid"]

MTP=paste(modulo,taxon)
riqueza.mod<-tapply(rep(1,nrow(tabela)),list(taxon,modulo),sum)
riqueza.mod[riqueza.mod>0]=1
riqueza_mod<-ifelse(is.na(riqueza.mod),0,riqueza.mod)

colSums(riqueza_mod)########## Riqueza por m�dulo############

################# Tabela de Riqueza por modulo ##################
subfami<-tabela[,"Subfam�lia"]
MTP=paste(familia,subfami,taxon)

tab_riq_mod<-tapply(rep(1,nrow(tabela)),list(MTP,modulo),sum)
tab_riq_mod<-ifelse(is.na(tab_riq_mod),0,tab_riq_mod)
tab_riq_mod

sub<-tabela[,"Subparcela"]

###################Curva de acumulo de especie modulos##############

tabela<-read.csv("Planilha_UHE_CAM._1.csv")
M_modulo<-tabela[,"M�dulo.Grid"]
M_taxon<-tabela[,"Taxon"]
M_parcela<-tabela[,"Parcela"]
M_lado<-tabela[,"Lado"]
MTP=paste(M_parcela,M_modulo,M_lado)
M_presen�a<-tapply(rep(1,nrow(tabela)),list(MTP,M_taxon),sum)
M_presen�a[M_presen�a>0]=1
M_presen�a<-ifelse(is.na(M_presen�a),0,M_presen�a)
M_nome=specaccum(M_presen�a)
plot(M_nome,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Subparcelas")
specpool(M_presen�a)

####################Curva de acumulo de especie bufalo######################

library(vegan)
bufalo<-read.csv("Bufalo.csv")
B_modulo<-bufalo[,"M�dulo.Grid"]
B_taxon<-bufalo[,"Taxon"]
B_parcela<-bufalo[,"Parcela"]
B_lado<-bufalo[,"Lado"]
MTP=paste(B_lado,B_parcela,B_modulo)
B_presen�a<-tapply(rep(1,nrow(bufalo)),list(MTP,B_taxon),sum)
B_presen�a[B_presen�a>0]=1
B_presen�a<-ifelse(is.na(B_presen�a),0,B_presen�a)
B_nome=specaccum(B_presen�a)
plot(B_nome,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Subparcelas em IB")
specpool(B_presen�a)

##################Curva de acumulo de especie pedra ###################3333

pedra<-read.csv("pedra.csv")
P_modulo<-pedra[,"M�dulo.Grid"]
P_taxon<-pedra[,"Taxon"]
P_parcela<-pedra[,"Parcela"]
P_lado<-pedra[,"Lado"]
MTP=paste(P_lado,P_parcela,P_modulo)
P_presen�a<-tapply(rep(1,nrow(pedra)),list(MTP,P_taxon),sum)
P_presen�a[P_presen�a>0]=1
P_presen�a<-ifelse(is.na(P_presen�a),0,P_presen�a)
P_nome=specaccum(P_presen�a)
plot(P_nome,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Subparcelas em IP")
specpool(P_presen�a)

######################Curva de acumulo de especie parana #############

parana<-read.csv("parana.csv")
p_modulo<-parana[,"M�dulo.Grid"]
p_taxon<-parana[,"Taxon"]
p_parcela<-parana[,"Parcela"]
p_lado<-parana[,"Lado"]
MTP=paste(p_lado,p_parcela,p_modulo)
p_presen�a<-tapply(rep(1,nrow(parana)),list(MTP,p_taxon),sum)
p_presen�a[p_presen�a>0]=1
p_presen�a<-ifelse(is.na(p_presen�a),0,p_presen�a)
p_nome=specaccum(p_presen�a)
plot(p_nome,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Subparcelas em JP")
specpool(p_presen�a)

#######################Curva de acumulo de especie M.novo##############

novo<-read.csv("novo.csv")
n_modulo<-novo[,"M�dulo.Grid"]
n_taxon<-novo[,"Taxon"]
n_parcela<-novo[,"Parcela"]
n_lado<-novo[,"Lado"]
MTP=paste(n_lado,n_parcela,n_modulo)
n_presen�a<-tapply(rep(1,nrow(novo)),list(MTP,n_taxon),sum)
n_presen�a[n_presen�a>0]=1
n_presen�a<-ifelse(is.na(n_presen�a),0,n_presen�a)
n_nome=specaccum(n_presen�a)
plot(n_nome,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Subparcelas em MN")
specpool(n_presen�a)

########################Curva de acumulo de especie teotonio ###############

teotonio<-read.csv("teotonio.csv")
t_modulo<-teotonio[,"M�dulo.Grid"]
t_taxon<-teotonio[,"Taxon"]
t_parcela<-teotonio[,"Parcela"]
t_lado<-teotonio[,"Lado"]
MTP=paste(t_lado,t_parcela,t_modulo)
t_presen�a<-tapply(rep(1,nrow(teotonio)),list(MTP,t_taxon),sum)
t_presen�a[t_presen�a>0]=1
t_presen�a<-ifelse(is.na(t_presen�a),0,t_presen�a)
t_nome=specaccum(t_presen�a)
plot(t_nome,ci.type="line",ci.lty=2,ylab="Esp�cies",xlab="Subparcelas em TE")
specpool(t_presen�a)

par(mfrow=c(3,2))

###################### Indice de Similaridade ##################

pedra
pe_local<-pedra[,"M�dulo.Grid"]
pe_parcela<-pedra[,"Parcela"]
pe_especie<-pedra[,"Taxon"]
MTP=paste(pe_local,pe_parcela)
pe_presen�a<-tapply(rep(1,nrow(pedra)),list(MTP,pe_especie),sum)
pe_presen�a=ifelse(is.na(pe_presen�a),0,pe_presen�a)
ped_presen�a<-colSums(pe_presen�a)

bufalo
bu_local<-bufalo[,"M�dulo.Grid"]
bu_parcela<-bufalo[,"Parcela"]
bu_especie<-bufalo[,"Taxon"]
MTP=paste(bu_local,bu_parcela)
bu_presen�a<-tapply(rep(1,nrow(bufalo)),list(MTP,bu_especie),sum)
bu_presen�a=ifelse(is.na(bu_presen�a),0,bu_presen�a)
buf_presen�a<-colSums(bu_presen�a)

parana
pa_local<-parana[,"M�dulo.Grid"]
pa_parcela<-parana[,"Parcela"]
pa_especie<-parana[,"Taxon"]
MTP=paste(pa_local,pa_parcela)
pa_presen�a<-tapply(rep(1,nrow(parana)),list(MTP,pa_especie),sum)
pa_presen�a=ifelse(is.na(pa_presen�a),0,pa_presen�a)
par_presen�a<-colSums(pa_presen�a)

novo
no_local<-novo[,"M�dulo.Grid"]
no_parcela<-novo[,"Parcela"]
no_especie<-novo[,"Taxon"]
MTP=paste(no_local,no_parcela)
no_presen�a<-tapply(rep(1,nrow(novo)),list(MTP,no_especie),sum)
no_presen�a=ifelse(is.na(no_presen�a),0,no_presen�a)
nov_presen�a<-colSums(no_presen�a)

teotonio
te_local<-teotonio[,"M�dulo.Grid"]
te_parcela<-teotonio[,"Parcela"]
te_especie<-teotonio[,"Taxon"]
MTP=paste(te_local,te_parcela)
te_presen�a<-tapply(rep(1,nrow(teotonio)),list(MTP,te_especie),sum)
te_presen�a=ifelse(is.na(te_presen�a),0,te_presen�a)
teo_presen�a<-colSums(te_presen�a)

