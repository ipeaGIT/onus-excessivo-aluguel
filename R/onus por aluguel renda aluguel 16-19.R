# Calculo do indicador de componente de deficit habitacional - onus excessivo por aluguel.
# autora: Vanessa Nadalin
# junho 2021

library(dplyr)
library(survey)
library(PNADcIBGE)


setwd("L:/# VANESSA GAPRIOTTI NADALIN #/aluguel/dados")


# dados serao carregados offline (arquivos de microdados nos downloads do IBGE)
#https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1

#S01017                              Este domicilio é: 3 Alugado
#S01019                              Qual foi o valor mensal do aluguel pago, ou que deveria ter sido pago, no mês de referência
#VD5004 Rendimento (efetivo) domiciliar (inclusive rendimentos em cartão/tíquete transporte ou alimentação) (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)
#V1022                Situacao do domicilio (1 urbana 2 rural)
#V1023              Tipo de área	1 Capital, 2 Resto da RM, 3	Resto da RIDE, excluindo a capital 4 Resto da UF

#valores dos salários mínimos nos anos 2016-2019
#2016=880
#2017=937
#2018=954
#2019=998

#### funcao que carrega dados, calcula indicador, areas de divulgacao e aplica plano amostral----

func_onus <- function(filename, sal_min) {
  
    pnad <- read_pnadc(paste0(filename, ".txt"),paste0(paste0("input_",filename), ".txt"))
    
    # familias que gastam mais de 30% da renda familiar com aluguel
    # domicilios urbanos, alugados e com renda familiar menor q 3 sm
    pnad$i_onus= if_else(((as.numeric(pnad$S01019)/as.numeric(pnad$VD5004))>0.3),"1","0")
    pnad$i_onus=if_else(as.numeric(pnad$VD5004)<sal_min*3,pnad$i_onus,"0")
    pnad$i_onus= if_else(pnad$S01017=="3",pnad$i_onus,NULL)
    pnad$i_onus= if_else(pnad$V1022=="1",pnad$i_onus,NULL)
    pnad$al_onus=if_else(pnad$i_onus=="1",pnad$S01019,NULL)
    pnad$r_dom_onus=if_else(pnad$i_onus=="1",pnad$VD5004,NULL)
    pnad$onus_onus=if_else((pnad$i_onus=="1"&pnad$r_dom_onus>0),(as.numeric(pnad$S01019)/as.numeric(pnad$VD5004)),NULL)
 
    
    # areas de divulgacao
    # alocando mun ride teresina e df como se fossem RM da capital da UF
    pnad$UF_RM=if_else(pnad$V1023=="3"&pnad$UF=="21","22",pnad$UF)
    pnad$UF_RM=if_else(pnad$V1023=="3"&pnad$UF=="31","53",pnad$UF)
    pnad$UF_RM=if_else(pnad$V1023=="3"&pnad$UF=="52","53",pnad$UF)
    pnad$V1023=if_else(pnad$V1023=="3","2",pnad$V1023)
    pnad$UF_RM <- as.factor(pnad$UF_RM)
    pnad$area_divulgacao <- as.factor(pnad$V1023)
    
    #plano amostral
    pnadc_plano <- pnadc_design(pnad)
    return(pnadc_plano)
}

# aplicando a funcao

plano_16 <- func_onus("PNADC_2016_visita1",880)
plano_17 <- func_onus("PNADC_2017_visita1",937)
plano_18 <- func_onus("PNADC_2018_visita1",954)
plano_19 <- func_onus("PNADC_2019_visita1",998)

##############calculando o total de onus por area de divulgacao-------------
ônus_aluguel_16 <- survey::svyby(~ i_onus,~area_divulgacao+UF_RM,subset(plano_16, V2005=="01"), svytotal, na.rm  =  TRUE)
ônus_aluguel_17 <- survey::svyby(~ i_onus,~area_divulgacao+UF_RM,subset(plano_17, V2005=="01"), svytotal, na.rm  =  TRUE)
ônus_aluguel_18 <- survey::svyby(~ i_onus,~area_divulgacao+UF_RM,subset(plano_18, V2005=="01"), svytotal, na.rm  =  TRUE)
ônus_aluguel_19 <- survey::svyby(~ i_onus,~area_divulgacao+UF_RM,subset(plano_19, V2005=="01"), svytotal, na.rm  =  TRUE)


 ###################################aluguel e renda domicilios em onus-----------

aluguel_16 <- survey::svyby(~ al_onus,~area_divulgacao+UF_RM,subset(plano_16, V2005=="01"), svymean, na.rm  =  TRUE)
aluguel_17 <- survey::svyby(~ al_onus,~area_divulgacao+UF_RM,subset(plano_17, V2005=="01"), svymean, na.rm  =  TRUE)
aluguel_18 <- survey::svyby(~ al_onus,~area_divulgacao+UF_RM,subset(plano_18, V2005=="01"), svymean, na.rm  =  TRUE)
aluguel_19 <- survey::svyby(~ al_onus,~area_divulgacao+UF_RM,subset(plano_19, V2005=="01"), svymean, na.rm  =  TRUE)


renda_16 <- survey::svyby(~ r_dom_onus,~area_divulgacao+UF_RM,subset(plano_16, V2005=="01"), svymean, na.rm  =  TRUE)
renda_17 <- survey::svyby(~ r_dom_onus,~area_divulgacao+UF_RM,subset(plano_17, V2005=="01"), svymean, na.rm  =  TRUE)
renda_18 <- survey::svyby(~ r_dom_onus,~area_divulgacao+UF_RM,subset(plano_18, V2005=="01"), svymean, na.rm  =  TRUE)
renda_19 <- survey::svyby(~ r_dom_onus,~area_divulgacao+UF_RM,subset(plano_19, V2005=="01"), svymean, na.rm  =  TRUE)

###média do ônus dos domicílios em ônus----

 o_16 <- survey::svyby(~ onus_onus,~area_divulgacao+UF_RM,subset(plano_16, V2005=="01"), svyquantile, quantiles=0.5,keep.var=FALSE, na.rm  =  TRUE)
 o_17 <- survey::svyby(~ onus_onus,~area_divulgacao+UF_RM,subset(plano_17, V2005=="01"), svyquantile, quantiles=0.5,keep.var=FALSE, na.rm  =  TRUE)
 o_18 <- survey::svyby(~ onus_onus,~area_divulgacao+UF_RM,subset(plano_18, V2005=="01"), svyquantile, quantiles=0.5,keep.var=FALSE, na.rm  =  TRUE)
 o_19 <- survey::svyby(~ onus_onus,~area_divulgacao+UF_RM,subset(plano_19, V2005=="01"), svyquantile, quantiles=0.5,keep.var=FALSE, na.rm  =  TRUE)

####consolidando e salvando as estimativas----------------
 df <- rbind(ônus_aluguel_16,ônus_aluguel_17,ônus_aluguel_18,ônus_aluguel_19)
 df <- df %>% mutate(pct=i_onus1/(i_onus0+i_onus1)) %>% 
   select(area_divulgacao, UF_RM, i_onus1,pct)
 al <- rbind(aluguel_16, aluguel_17, aluguel_18, aluguel_19 )
 al <- al[,3]
 r <- rbind(renda_16, renda_17, renda_18, renda_19)
 r <-  r[,3]
 o <- rbind(o_16, o_17, o_18, o_19)
 o <-  o[,3]
 ano <- rep(c(16,17,18,19), each=75)
  
dados <- cbind(df,al,r,o, ano)
  colnames(dados)[3] <- "total_onus"
  colnames(dados)[4] <- "pct_onus"
  colnames(dados)[5] <- "media_aluguel"
  colnames(dados)[6] <- "media_renda"
  colnames(dados)[7] <- "mediana_onus"
dados2 <- dados %>% tidyr::pivot_wider(id_cols=c(UF_RM, ano),names_from=area_divulgacao , values_from=c(total_onus,pct_onus,media_aluguel,media_renda,mediana_onus))
  
saveRDS(dados2,"onus.rds")
write.csv2(dados2, "onus.csv")
 