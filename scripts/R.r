library(readxl)
base <- read_excel("PDCI OD BICICLETAS.xlsx")

library(dplyr)
library("lavaan")
library("semPlot")
base = base %>% filter(Meio == 'BICICLETA')

base = base %>% mutate(Distancia_km = sqrt((Bairro_Destino_cent_x - Bairro_Origem_cent_x)^2 + (Bairro_Destino_cent_y - Bairro_Origem_cent_y)^2)/1000)

base = base %>% mutate(Distancia_cat =  factor(cut(Distancia_km, seq(0 , 18 , 2),right=FALSE),ordered = TRUE))

base = base %>%mutate(Ocupa��o = factor(Ocupa��o, levels = c( 'DESEMPREGADO', 'ESTUDANTE', 'AUT�NOMO','APOSENTADO', 'EMPREGADO / FUNCION�RIO'),ordered = TRUE))

base = base %>%mutate(Ocupa��o = ifelse(Ocupa��o =='DESEMPREGADO','DESEMPREGADO',ifelse(Ocupa��o == 'ESTUDANTE','ESTUDANTE','EMPREGADO / FUNCION�RIO / APOSENTADO / AUT�NOMO')))
base = base %>%mutate(Ocupa��o = factor(Ocupa��o, levels = c( 'DESEMPREGADO', 'ESTUDANTE', 'EMPREGADO / FUNCION�RIO / APOSENTADO / AUT�NOMO'),ordered = TRUE))

base = base %>% mutate(Sexo = ifelse(Sexo == 'Feminino' | Sexo == 'FEMININO', 'Feminino', ifelse(Sexo == 'Masculino' | Sexo == 'MASCULINO','Masculino',Sexo))) %>% 
  mutate(Sexo = factor(Sexo, levels = c('Feminino', 'Masculino'),ordered = FALSE))

base = base %>% mutate(Idade = ifelse(Idade <= 20, '0-20', ifelse(Idade <= 50,'21-50','51-100'))) %>% 
  mutate(Idade = factor(Idade, levels = c('0-20', '21-50','51-100' ),ordered = TRUE))


base = base %>% mutate(Ext_bicicletarios = ifelse(base$OpcaoBicicleta1 == 'EXIST�NCIA DE BICICLET�RIOS'|
                                                    base$OpcaoBicicleta2 == 'EXIST�NCIA DE BICICLET�RIOS'|
                                                    base$OpcaoBicicleta3 == 'EXIST�NCIA DE BICICLET�RIOS',1,0))%>%
  mutate(Ext_bicicletarios = ifelse( is.na(Ext_bicicletarios),0,Ext_bicicletarios))

base = base %>% mutate(Ext_ciclovias = ifelse(base$OpcaoBicicleta1 == 'EXIST�NCIA DE CICLOVIAS/CICLOFAIXAS AT� O LOCAL'|
                                                    base$OpcaoBicicleta2 == 'EXIST�NCIA DE CICLOVIAS/CICLOFAIXAS AT� O LOCAL'|
                                                    base$OpcaoBicicleta3 == 'EXIST�NCIA DE CICLOVIAS/CICLOFAIXAS AT� O LOCAL',1,0))%>%
  mutate(Ext_ciclovias = ifelse( is.na(Ext_ciclovias),0,Ext_ciclovias))

base = base %>% mutate(Ext_integracao = ifelse(base$OpcaoBicicleta1 == 'INTREGA��O COM O TRANSPORTE P�BLICO'|
                                                base$OpcaoBicicleta2 == 'INTREGA��O COM O TRANSPORTE P�BLICO'|
                                                base$OpcaoBicicleta3 == 'INTREGA��O COM O TRANSPORTE P�BLICO',1,0))%>%
  mutate(Ext_integracao = ifelse( is.na(Ext_integracao),0,Ext_integracao))

base = base %>% mutate(Ext_seguranca= ifelse(base$OpcaoBicicleta1 == 'MAIOR SEGURAN�A (POLICIAMENTO)'|
                                                 base$OpcaoBicicleta2 == 'MAIOR SEGURAN�A (POLICIAMENTO)'|
                                                 base$OpcaoBicicleta3 == 'MAIOR SEGURAN�A (POLICIAMENTO)',1,0))%>%
  mutate(Ext_seguranca = ifelse( is.na(Ext_seguranca),0,Ext_seguranca))

base = base %>% mutate(Ext_arb= ifelse(base$OpcaoBicicleta1 == 'MAIS ARBORIZA��O'|
                                               base$OpcaoBicicleta2 == 'MAIS ARBORIZA��O'|
                                               base$OpcaoBicicleta3 == 'MAIS ARBORIZA��O',1,0))%>%
  mutate(Ext_arb = ifelse( is.na(Ext_arb),0,Ext_arb))

base = base %>% mutate(Ext_pav= ifelse(base$OpcaoBicicleta1 == 'MELHOR PAVIMENTO'|
                                         base$OpcaoBicicleta2 == 'MELHOR PAVIMENTO'|
                                         base$OpcaoBicicleta3 == 'MELHOR PAVIMENTO',1,0))%>%
  mutate(Ext_pav = ifelse( is.na(Ext_pav),0,Ext_pav))

base = base %>% mutate(Ext_trat= ifelse(base$OpcaoBicicleta1 == 'TRATAMENTO DO TR�NSITO (REDU��O DE CONFLITOS)'|
                                         base$OpcaoBicicleta2 == 'TRATAMENTO DO TR�NSITO (REDU��O DE CONFLITOS)'|
                                         base$OpcaoBicicleta3 == 'TRATAMENTO DO TR�NSITO (REDU��O DE CONFLITOS)',1,0))%>%
  mutate(Ext_trat = ifelse( is.na(Ext_trat),0,Ext_trat))



base = base %>%mutate(PossuiBicicleta = factor(PossuiBicicleta, levels = c('N�O', 'SIM'),ordered = TRUE))

base = base %>% mutate(UtilizaOutroMei = ifelse(UtilizaOutroMei == 'CARRO' | UtilizaOutroMei == 'MOTO',1,0)) %>% mutate(UtilizaOutroMei = factor(UtilizaOutroMei, levels = c('0', '1'),ordered = TRUE))

table(base$MotDesDe)
table(base$MotDesPara)

base = base %>% mutate(MotDesDe = factor(MotDesDe, levels = c('CASA','OUTROS', 'LAZER','COMPRAS','ESTUDO','TRABALHO','ENTREGAS'),ordered = TRUE))
base = base %>% mutate(MotDesPara = factor(MotDesPara, levels = c('CASA','OUTROS', 'LAZER','COMPRAS','ESTUDO','TRABALHO','ENTREGAS'),ordered = TRUE))
base = base %>% mutate(MotDes = ifelse(as.numeric(MotDesDe)>as.numeric(MotDesPara),as.character(MotDesDe),as.character(MotDesPara)))
base = base %>% mutate(MotDes = factor(MotDes, levels = c('CASA','OUTROS','LAZER','COMPRAS', 'ESTUDO','TRABALHO','ENTREGAS'),ordered = TRUE))
prop.table(table(base$MotDes))
base = base %>% mutate(Frequencia = factor(Frequencia, levels = c('ESPOR�DICO', 'FINAL DE SEMANA','1 DIA/SEMANA','2 DIAS/SEMANA','3 DIAS/SEMANA','4 DIAS/SEMANA',
                                                                  '5 DIAS/SEMANA', 'DIAS �TEIS','6 DIAS/SEMANA','7 DIAS/SEMANA'),ordered = TRUE))


base = base %>% mutate(Renda = factor(Renda, levels = c('SEM RENDA','AT� R$ 700,00','DE R$ 700,00 A R$ 2.000,00','DE R$ 2.000,00 A R$ 3.500,00','DE R$ 3.500,00 A R$ 5.500,00', 'ACIMA DE R$ 5.500,00'),ordered = TRUE))

base = base %>% mutate(Renda = ifelse(Renda == 'SEM RENDA','SEM RENDA','COM RENDA' )) %>% mutate(Renda = factor(Renda, levels = c('SEM RENDA','COM RENDA'),ordered = TRUE))

base = base %>% mutate(Prob_ciclovias = ifelse(base$ProbEnf1 == 'AUS�NCIA DE CICLOVIAS E CICLOFAIXAS'|
                                                    base$ProbEnf2 == 'AUS�NCIA DE CICLOVIAS E CICLOFAIXAS'|
                                                    base$ProbEnf3 == 'AUS�NCIA DE CICLOVIAS E CICLOFAIXAS',1,0))%>%
  mutate(Prob_ciclovias = ifelse( is.na(Prob_ciclovias),0,Prob_ciclovias))

base = base %>% mutate(Prob_esta = ifelse(base$ProbEnf1 == 'AUS�NCIA DE LOCAL PARA ESTACIONAR A BICICLETA'|
                                                 base$ProbEnf2 == 'AUS�NCIA DE LOCAL PARA ESTACIONAR A BICICLETA'|
                                                 base$ProbEnf3 == 'AUS�NCIA DE LOCAL PARA ESTACIONAR A BICICLETA',1,0))%>%
  mutate(Prob_esta = ifelse( is.na(Prob_esta),0,Prob_esta))

base = base %>% mutate(Prob_buracos = ifelse(base$ProbEnf1 == 'BURACOS OU PAVIMENTO RUIM'|
                                                 base$ProbEnf2 == 'BURACOS OU PAVIMENTO RUIM'|
                                                 base$ProbEnf3 == 'BURACOS OU PAVIMENTO RUIM',1,0))%>%
  mutate(Prob_buracos = ifelse( is.na(Prob_buracos),0,Prob_buracos))

base = base %>% mutate(Prob_ilumi = ifelse(base$ProbEnf1 == 'FALTA DE ILUMINA��O'|
                                            base$ProbEnf2 == 'FALTA DE ILUMINA��O'|
                                            base$ProbEnf3 == 'FALTA DE ILUMINA��O',1,0))%>%
  mutate(Prob_ilumi = ifelse( is.na(Prob_ilumi),0,Prob_ilumi))

base = base %>% mutate(Prob_transito = ifelse(base$ProbEnf1 == 'TR�NSITO INTENSO DE VE�CULOS MOTORIZADOS'|
                                               base$ProbEnf2 == 'TR�NSITO INTENSO DE VE�CULOS MOTORIZADOS'|
                                               base$ProbEnf3 == 'TR�NSITO INTENSO DE VE�CULOS MOTORIZADOS',1,0))%>%
  mutate(Prob_transito = ifelse( is.na(Prob_transito),0,Prob_transito))

base = base %>% mutate(Prob_assalto = ifelse(base$ProbEnf1 == 'PERIGO DE ASSALTO'|
                                                base$ProbEnf2 == 'PERIGO DE ASSALTO'|
                                                base$ProbEnf3 == 'PERIGO DE ASSALTO',1,0))%>%
  mutate(Prob_assalto = ifelse( is.na(Prob_assalto),0,Prob_assalto))

base = base %>% mutate(Esc_semcarro = ifelse(base$MotivoEscolha1 == 'N�O TENHO CARRO'|
                                               base$MotivoEscolha2 == 'N�O TENHO CARRO'|
                                               base$MotivoEscolha3 == 'N�O TENHO CARRO',0,1))%>%
  mutate(Esc_semcarro = ifelse( is.na(Esc_semcarro),1,Esc_semcarro))

base = base %>% mutate(Esc_semhabilit = ifelse(base$MotivoEscolha1 == 'N�O TENHO HABILITA��O PARA DIRIGIR'|
                                               base$MotivoEscolha2 == 'N�O TENHO HABILITA��O PARA DIRIGIR'|
                                               base$MotivoEscolha3 == 'N�O TENHO HABILITA��O PARA DIRIGIR',1,0))%>%
  mutate(Esc_semhabilit = ifelse( is.na(Esc_semhabilit),0,Esc_semhabilit))



base = base %>% mutate(IDH = ifelse(CasaOrig == 1, IDH,ifelse(CasaDest == 1, IDH_dest,mean(IDH))))
base = base %>% mutate(IVS = ifelse(CasaOrig == 1, IVS,ifelse(CasaDest == 1, IVS_dest,mean(IVS))))

base = base %>% mutate(Area_Com_Orig = ifelse(CasaOrig == 1, Area_Com_Orig,ifelse(CasaDest == 1, Area_Com_Dest,mean(Area_Com_Orig))))
base = base %>% mutate(Area_Com_Dest = ifelse(CasaDest == 0, Area_Com_Dest,Area_Com_Orig))

base = base %>% mutate(Acc_pop = (ACC_orig_2012+ ACC_orig_2013 + ACC_orig_2014)/POP_orig,Acc_pop_dest = (ACC_dest_2012 + ACC_dest_2013 + ACC_dest_2014)/POP_dest)

base = base %>% mutate(Acc_pop = ifelse(CasaOrig == 1, Acc_pop,ifelse(CasaDest == 1, Acc_pop_dest,mean(Acc_pop))))

base = base %>% dplyr::select(Area_Com_Orig,Area_Com_Dest,Ocup = Ocupa��o,Sexo, Idade, Acid_Quant = SofreuAcidenteQ, Roub_Quant =FoiRoubadoQtde,
                              usa_carro_moto = UtilizaOutroMei,Frequencia, Renda, Distancia_km,Distancia_cat, MotDes,Prob_ciclovias,
                              Prob_buracos, Prob_transito, Prob_assalto,Prob_esta,Prob_ilumi, Esc_TerCarro = Esc_semcarro, Esc_semhabilit, IDH, IVS,Acc_pop,Acc_pop_dest)

