# Análises do artigo:
# DESEMPENHO UNIVERSITÁRIO NO BRASIL: 
# Estudo sobre desigualdade educacional com dados do Enade 2014

# Neylson Crepalde e Leonardo Silveira

# Carregando os pacotes necessarios. Se der uma mensagem de erro, use 
# install.packages("NOME-DO-PACOTE", dependencies=T) para cada um. Abs
library(car)
library(ggplot2)
library(lme4)
library(merTools)
library(lmerTest)
library(lmtest)
library(stargazer)
library(xtable)
library(descr)

########################
# Lendo os dados
# Baixe os microdados do Enade 2014 em http://portal.inep.gov.br/basica-levantamentos-acessar
# Coloque o arquivo microdados_enade_2014.csv na pasta Documentos
enade <- read.csv("microdados_enade_2014.csv", sep = ";", stringsAsFactors = T, na.strings = "")

# Variavel dependente
attach(enade)
summary(nt_ger)

# verificando a distribuicao
hist(nt_ger, main = "Nota Geral")
ggplot(data=enade, aes(x=nt_ger))+geom_histogram(col = "grey")+labs(x="Nota Geral", y="")+
  theme_bw(base_size = 24)
detach(enade)

#############################
# Centralizando a idade
enade$idade.cent <- enade$nu_idade-mean(enade$nu_idade)
summary(enade$idade.cent)
hist(enade$idade.cent)

#colocando NA's nas opcoes NAO SEI e NAO SE APLICA
for (i in 101:142){
  print(i)
  enade[,i] <- car::recode(enade[,i], "c(7,8)=NA")
}

# Arrumando algumas variáveis
enade$tp_sexo <- as.character(enade$tp_sexo)
enade$tp_sexo[enade$tp_sexo=="N"] <- NA
enade$tp_sexo <- as.factor(enade$tp_sexo)
summary(enade$tp_sexo)

enade$co_grupo <- as.factor(enade$co_grupo)
enade$co_ies <- as.factor(enade$co_ies)
enade$co_catad <- as.factor(enade$co_catad)
enade$co_regiao_curso <- as.factor(enade$co_regiao_curso)

# binarizando Categoria Administrativa
enade$cat <- enade$co_catad
enade$cat <- as.character(enade$cat)
enade$cat[enade$cat==93] <- "Pública"
enade$cat[enade$co_catad==116] <- "Pública"
enade$cat[enade$co_catad==10001] <- "Pública"
enade$cat[enade$co_catad==10002] <- "Pública"
enade$cat[enade$co_catad==10003] <- "Pública"
enade$cat[enade$co_catad==118] <- "Privada"
enade$cat[enade$co_catad==121] <- "Privada"
enade$cat[enade$co_catad==121] <- "Privada"
enade$cat[enade$co_catad==10004] <- "Privada"
enade$cat[enade$co_catad==10005] <- "Privada"
enade$cat[enade$co_catad==10006] <- "Privada"
enade$cat[enade$co_catad==10007] <- "Privada"
enade$cat[enade$co_catad==10008] <- "Privada"
enade$cat[enade$co_catad==10009] <- "Privada"
enade$cat <- as.factor(enade$cat)
summary(enade$cat)

# separando as variaveis de interesse -- curso, escola, idade, sexo, nota geral, todo o questionario do aluno e
# a idade centralizada que criamos
enade.qe <- enade[,c(2,3,8:10,65,75:142,156,157)]

musica <- 4301
arquitetura <- 21
cs_lic <- 5402
civil <- 5710
computacao <- 4004
historia <- 2402
pedagogia <- 2001

#######################################
# estatisticas descritivas

ggplot(data=enade.qe, aes(x=qe_i2, y=nt_ger))+geom_boxplot()+facet_grid(cat~tp_sexo)+
  labs(title="Enade 2014",x="Cor/Raça", y="Nota Geral")+
  scale_x_discrete(labels=c("Br","Neg","Par","Ama","Ind","NA"))+theme_bw(base_size = 16)

ggplot(data=enade.qe, aes(x=qe_i8, y=nt_ger))+geom_boxplot()+facet_grid(cat~tp_sexo)+
  labs(title="Enade 2014",x="Renda (categorias)", y="Nota Geral")+theme_bw(base_size = 16)

descr(enade$nt_ger) 
freq(enade$tp_sexo)
freq(enade$qe_i2) 
freq(enade$qe_i8) 
freq(enade$cat)

# gerando tabelas para LaTeX
#xtable(cbind(summary(enade$qe_i8)))
#stargazer(enade$nt_ger)

#######################################
#modelos lineares

modelo.geral <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                     qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                     qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                     qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                     qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                     qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                     qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                   data=enade.qe)
summary(modelo.geral)


modelo.musica <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                      qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                      qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                      qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                      qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                      qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                      qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                    data=enade.qe, subset=co_grupo==musica)

modelo.arquitetura <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                           qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                           qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                           qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                           qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                           qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                           qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso, 
                         data=enade.qe, subset=co_grupo==arquitetura)

modelo.cs_lic <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                      qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                      qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                      qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                      qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                      qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                      qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                    data=enade.qe, subset=co_grupo==cs_lic)

modelo.civil <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                     qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                     qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                     qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                     qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                     qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                     qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                   data=enade.qe, subset=co_grupo==civil)

modelo.computacao <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                          qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                          qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                          qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                          qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                          qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                          qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                        data=enade.qe, subset=co_grupo==computacao)

modelo.historia <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                        qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                        qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                        qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                        qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                        qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                        qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                      data=enade.qe, subset=co_grupo==historia)

modelo.pedagogia <- lm(nt_ger~tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                         qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                         qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                         qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                         qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                         qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                         qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                       data=enade.qe, subset=co_grupo==pedagogia)

# montando a tabela
#stargazer(modelo.geral, modelo.arquitetura, modelo.civil, modelo.computacao, modelo.cs_lic, 
#          modelo.historia, modelo.musica, modelo.pedagogia,
#          type = "text", no.space = T, omit.stat = c("ser", "f"),title = "Modelos de Regressão OLS",
#          column.labels = c("Geral","Arquitetura","Eng. Civil", "Computação", "Sociais(Lic)",
#                            "História(Lic)","Música(Lic)","Pedagogia(Lic)"),
#          dep.var.labels = "Nota Geral")


#################################################
# Montando os modelos multinivel

multi.geral <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                            qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                            qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                            qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                            qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                            qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                            qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                          data=enade.qe, REML = F)
#summary(multi.geral)
ICC.geral = var(multi.geral@u) / (var(multi.geral@u)+var(residuals(multi.geral)))


multi.musica <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                             qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                             qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                             qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                             qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                             qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                             qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                           data=enade.qe, subset=co_grupo==musica, REML = F)
ICC.musica = var(multi.musica@u) / (var(multi.musica@u)+var(residuals(multi.musica)))

multi.arquitetura <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                                  qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                                  qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                                  qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                                  qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                                  qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                                  qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso, 
                                data=enade.qe, subset=co_grupo==arquitetura, REML = F)
ICC.arquitetura = var(multi.arquitetura@u) / (var(multi.arquitetura@u)+var(residuals(multi.arquitetura)))

multi.cs_lic <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                             qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                             qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                             qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                             qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                             qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                             qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                           data=enade.qe, subset=co_grupo==cs_lic, REML = F)
ICC.cs_lic = var(multi.cs_lic@u) / (var(multi.cs_lic@u)+var(residuals(multi.cs_lic)))

multi.civil <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                            qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                            qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                            qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                            qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                            qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                            qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                          data=enade.qe, subset=co_grupo==civil, REML = F)
ICC.civil = var(multi.civil@u) / (var(multi.civil@u)+var(residuals(multi.civil)))

multi.computacao <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                                 qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                                 qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                                 qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                                 qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                                 qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                                 qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                               data=enade.qe, subset=co_grupo==computacao, REML = F)
ICC.computacao = var(multi.computacao@u) / (var(multi.computacao@u)+var(residuals(multi.computacao)))

multi.historia <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                               qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                               qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                               qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                               qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                               qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                               qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                             data=enade.qe, subset=co_grupo==historia, REML = F)
ICC.historia = var(multi.historia@u) / (var(multi.historia@u)+var(residuals(multi.historia)))

multi.pedagogia <- lme4::lmer(nt_ger~(1|co_ies)+tp_sexo+qe_i2+qe_i8+cat+qe_i1+qe_i3+qe_i4+qe_i5+qe_i6+qe_i7+qe_i9+qe_i10+
                                qe_i11+qe_i12+qe_i13+qe_i14+qe_i15+qe_i16+qe_i17+qe_i18+qe_i19+qe_i20+qe_i21+
                                qe_i22+qe_i23+qe_i24+qe_i25+qe_i26+qe_i27+qe_i28+qe_i29+qe_i30+qe_i31+qe_i32+
                                qe_i33+qe_i34+qe_i35+qe_i36+qe_i37+qe_i38+qe_i39+qe_i40+qe_i41+qe_i42+qe_i43+
                                qe_i44+qe_i45+qe_i46+qe_i47+qe_i48+qe_i49+qe_i50+qe_i51+qe_i52+qe_i53+qe_i54+
                                qe_i55+qe_i56+qe_i57+qe_i58+qe_i59+qe_i60+qe_i61+qe_i62+qe_i63+qe_i64+qe_i65+
                                qe_i66+qe_i67+qe_i68+idade.cent+co_regiao_curso,
                              data=enade.qe, subset=co_grupo==pedagogia, REML = F)
ICC.pedagogia = var(multi.pedagogia@u) / (var(multi.pedagogia@u)+var(residuals(multi.pedagogia)))

###############################
#plotando os ICC's
ICC = c(ICC.geral, ICC.arquitetura, ICC.civil, ICC.computacao,
        ICC.cs_lic, ICC.historia, ICC.musica, ICC.pedagogia)
ggplot(data=NULL, aes(x = ICC , y = c("Geral","Arquitetura","Civil","Computação","Ciências Sociais",
                                      "História","Música","Pedagogia")))+geom_point(size=2)+
  labs(x="ICC's",y="")+theme_light()

#plotando os interceptos aleatorios
plotREsim(REsim(multi.arquitetura))+labs(title="Arquitetura")+theme_bw(base_size = 20)
plotREsim(REsim(multi.civil))+labs(title="Eng. Civil")+theme_bw(base_size = 20)
plotREsim(REsim(multi.computacao))+labs(title="Ciências da Computação")+theme_bw(base_size = 20)
plotREsim(REsim(multi.cs_lic))+labs(title="Ciências Sociais (Lic)")+theme_bw(base_size = 20)
plotREsim(REsim(multi.historia))+labs(title="História (Lic)")+theme_bw(base_size = 20)
plotREsim(REsim(multi.musica))+labs(title="Música (Lic)")+theme_bw(base_size = 20)
plotREsim(REsim(multi.pedagogia))+labs(title="Pedagogia (Lic)")+theme_bw(base_size = 20)

# verificando a significancia dos random interecepts
rand(multi.arquitetura)
rand(multi.civil)
rand(multi.computacao)
rand(multi.cs_lic)
rand(multi.historia)
rand(multi.musica)
rand(multi.pedagogia)

# montando as tabelas dos hlms
#stargazer(multi.geral, multi.arquitetura, multi.civil, multi.computacao, multi.cs_lic, 
#          multi.historia, multi.musica, multi.pedagogia,
#          type = "text", no.space = T, omit.stat = "ll",title = "Modelos de Regressão Multinível",
#          column.labels = c("Geral", "Arquitetura","Eng. Civil", "Computação", "Sociais(Lic)",
#                            "História(Lic)","Música(Lic)","Pedagogia(Lic)"),
#         dep.var.labels = "Nota Geral")
