library(readxl)
base <- read_excel("PDCI OD BICICLETAS.xlsx")

library(dplyr)
library("lavaan")
library("semPlot")
base = base %>% filter(Meio == 'BICICLETA')

base = base %>% mutate(Distancia_km = sqrt((Bairro_Destino_cent_x - Bairro_Origem_cent_x)^2 + (Bairro_Destino_cent_y - Bairro_Origem_cent_y)^2)/1000)

base = base %>% mutate(Distancia_cat =  factor(cut(Distancia_km, seq(0 , 18 , 2),right=FALSE),ordered = TRUE))

base = base %>%mutate(Ocupação = factor(Ocupação, levels = c( 'DESEMPREGADO', 'ESTUDANTE', 'AUTÔNOMO','APOSENTADO', 'EMPREGADO / FUNCIONÁRIO'),ordered = TRUE))

base = base %>%mutate(Ocupação = ifelse(Ocupação =='DESEMPREGADO','DESEMPREGADO',ifelse(Ocupação == 'ESTUDANTE','ESTUDANTE','EMPREGADO / FUNCIONÁRIO / APOSENTADO / AUTÔNOMO')))
base = base %>%mutate(Ocupação = factor(Ocupação, levels = c( 'DESEMPREGADO', 'ESTUDANTE', 'EMPREGADO / FUNCIONÁRIO / APOSENTADO / AUTÔNOMO'),ordered = TRUE))

base = base %>% mutate(Sexo = ifelse(Sexo == 'Feminino' | Sexo == 'FEMININO', 'Feminino', ifelse(Sexo == 'Masculino' | Sexo == 'MASCULINO','Masculino',Sexo))) %>% 
  mutate(Sexo = factor(Sexo, levels = c('Feminino', 'Masculino'),ordered = FALSE))

base = base %>% mutate(Idade = ifelse(Idade <= 20, '0-20', ifelse(Idade <= 50,'21-50','51-100'))) %>% 
  mutate(Idade = factor(Idade, levels = c('0-20', '21-50','51-100' ),ordered = TRUE))


base = base %>% mutate(Ext_bicicletarios = ifelse(base$OpcaoBicicleta1 == 'EXISTÊNCIA DE BICICLETÁRIOS'|
                                                    base$OpcaoBicicleta2 == 'EXISTÊNCIA DE BICICLETÁRIOS'|
                                                    base$OpcaoBicicleta3 == 'EXISTÊNCIA DE BICICLETÁRIOS',1,0))%>%
  mutate(Ext_bicicletarios = ifelse( is.na(Ext_bicicletarios),0,Ext_bicicletarios))

base = base %>% mutate(Ext_ciclovias = ifelse(base$OpcaoBicicleta1 == 'EXISTÊNCIA DE CICLOVIAS/CICLOFAIXAS ATÉ O LOCAL'|
                                                    base$OpcaoBicicleta2 == 'EXISTÊNCIA DE CICLOVIAS/CICLOFAIXAS ATÉ O LOCAL'|
                                                    base$OpcaoBicicleta3 == 'EXISTÊNCIA DE CICLOVIAS/CICLOFAIXAS ATÉ O LOCAL',1,0))%>%
  mutate(Ext_ciclovias = ifelse( is.na(Ext_ciclovias),0,Ext_ciclovias))

base = base %>% mutate(Ext_integracao = ifelse(base$OpcaoBicicleta1 == 'INTREGAÇÃO COM O TRANSPORTE PÚBLICO'|
                                                base$OpcaoBicicleta2 == 'INTREGAÇÃO COM O TRANSPORTE PÚBLICO'|
                                                base$OpcaoBicicleta3 == 'INTREGAÇÃO COM O TRANSPORTE PÚBLICO',1,0))%>%
  mutate(Ext_integracao = ifelse( is.na(Ext_integracao),0,Ext_integracao))

base = base %>% mutate(Ext_seguranca= ifelse(base$OpcaoBicicleta1 == 'MAIOR SEGURANÇA (POLICIAMENTO)'|
                                                 base$OpcaoBicicleta2 == 'MAIOR SEGURANÇA (POLICIAMENTO)'|
                                                 base$OpcaoBicicleta3 == 'MAIOR SEGURANÇA (POLICIAMENTO)',1,0))%>%
  mutate(Ext_seguranca = ifelse( is.na(Ext_seguranca),0,Ext_seguranca))

base = base %>% mutate(Ext_arb= ifelse(base$OpcaoBicicleta1 == 'MAIS ARBORIZAÇÃO'|
                                               base$OpcaoBicicleta2 == 'MAIS ARBORIZAÇÃO'|
                                               base$OpcaoBicicleta3 == 'MAIS ARBORIZAÇÃO',1,0))%>%
  mutate(Ext_arb = ifelse( is.na(Ext_arb),0,Ext_arb))

base = base %>% mutate(Ext_pav= ifelse(base$OpcaoBicicleta1 == 'MELHOR PAVIMENTO'|
                                         base$OpcaoBicicleta2 == 'MELHOR PAVIMENTO'|
                                         base$OpcaoBicicleta3 == 'MELHOR PAVIMENTO',1,0))%>%
  mutate(Ext_pav = ifelse( is.na(Ext_pav),0,Ext_pav))

base = base %>% mutate(Ext_trat= ifelse(base$OpcaoBicicleta1 == 'TRATAMENTO DO TRÂNSITO (REDUÇÃO DE CONFLITOS)'|
                                         base$OpcaoBicicleta2 == 'TRATAMENTO DO TRÂNSITO (REDUÇÃO DE CONFLITOS)'|
                                         base$OpcaoBicicleta3 == 'TRATAMENTO DO TRÂNSITO (REDUÇÃO DE CONFLITOS)',1,0))%>%
  mutate(Ext_trat = ifelse( is.na(Ext_trat),0,Ext_trat))



base = base %>%mutate(PossuiBicicleta = factor(PossuiBicicleta, levels = c('NÃO', 'SIM'),ordered = TRUE))

base = base %>% mutate(UtilizaOutroMei = ifelse(UtilizaOutroMei == 'CARRO' | UtilizaOutroMei == 'MOTO',1,0)) %>% mutate(UtilizaOutroMei = factor(UtilizaOutroMei, levels = c('0', '1'),ordered = TRUE))

table(base$MotDesDe)
table(base$MotDesPara)

base = base %>% mutate(MotDesDe = factor(MotDesDe, levels = c('CASA','OUTROS', 'LAZER','COMPRAS','ESTUDO','TRABALHO','ENTREGAS'),ordered = TRUE))
base = base %>% mutate(MotDesPara = factor(MotDesPara, levels = c('CASA','OUTROS', 'LAZER','COMPRAS','ESTUDO','TRABALHO','ENTREGAS'),ordered = TRUE))
base = base %>% mutate(MotDes = ifelse(as.numeric(MotDesDe)>as.numeric(MotDesPara),as.character(MotDesDe),as.character(MotDesPara)))
base = base %>% mutate(MotDes = factor(MotDes, levels = c('CASA','OUTROS','LAZER','COMPRAS', 'ESTUDO','TRABALHO','ENTREGAS'),ordered = TRUE))
prop.table(table(base$MotDes))
base = base %>% mutate(Frequencia = factor(Frequencia, levels = c('ESPORÁDICO', 'FINAL DE SEMANA','1 DIA/SEMANA','2 DIAS/SEMANA','3 DIAS/SEMANA','4 DIAS/SEMANA',
                                                                  '5 DIAS/SEMANA', 'DIAS ÚTEIS','6 DIAS/SEMANA','7 DIAS/SEMANA'),ordered = TRUE))


base = base %>% mutate(Renda = factor(Renda, levels = c('SEM RENDA','ATÉ R$ 700,00','DE R$ 700,00 A R$ 2.000,00','DE R$ 2.000,00 A R$ 3.500,00','DE R$ 3.500,00 A R$ 5.500,00', 'ACIMA DE R$ 5.500,00'),ordered = TRUE))

base = base %>% mutate(Renda = ifelse(Renda == 'SEM RENDA','SEM RENDA','COM RENDA' )) %>% mutate(Renda = factor(Renda, levels = c('SEM RENDA','COM RENDA'),ordered = TRUE))

base = base %>% mutate(Prob_ciclovias = ifelse(base$ProbEnf1 == 'AUSÊNCIA DE CICLOVIAS E CICLOFAIXAS'|
                                                    base$ProbEnf2 == 'AUSÊNCIA DE CICLOVIAS E CICLOFAIXAS'|
                                                    base$ProbEnf3 == 'AUSÊNCIA DE CICLOVIAS E CICLOFAIXAS',1,0))%>%
  mutate(Prob_ciclovias = ifelse( is.na(Prob_ciclovias),0,Prob_ciclovias))

base = base %>% mutate(Prob_esta = ifelse(base$ProbEnf1 == 'AUSÊNCIA DE LOCAL PARA ESTACIONAR A BICICLETA'|
                                                 base$ProbEnf2 == 'AUSÊNCIA DE LOCAL PARA ESTACIONAR A BICICLETA'|
                                                 base$ProbEnf3 == 'AUSÊNCIA DE LOCAL PARA ESTACIONAR A BICICLETA',1,0))%>%
  mutate(Prob_esta = ifelse( is.na(Prob_esta),0,Prob_esta))

base = base %>% mutate(Prob_buracos = ifelse(base$ProbEnf1 == 'BURACOS OU PAVIMENTO RUIM'|
                                                 base$ProbEnf2 == 'BURACOS OU PAVIMENTO RUIM'|
                                                 base$ProbEnf3 == 'BURACOS OU PAVIMENTO RUIM',1,0))%>%
  mutate(Prob_buracos = ifelse( is.na(Prob_buracos),0,Prob_buracos))

base = base %>% mutate(Prob_ilumi = ifelse(base$ProbEnf1 == 'FALTA DE ILUMINAÇÃO'|
                                            base$ProbEnf2 == 'FALTA DE ILUMINAÇÃO'|
                                            base$ProbEnf3 == 'FALTA DE ILUMINAÇÃO',1,0))%>%
  mutate(Prob_ilumi = ifelse( is.na(Prob_ilumi),0,Prob_ilumi))

base = base %>% mutate(Prob_transito = ifelse(base$ProbEnf1 == 'TRÂNSITO INTENSO DE VEÍCULOS MOTORIZADOS'|
                                               base$ProbEnf2 == 'TRÂNSITO INTENSO DE VEÍCULOS MOTORIZADOS'|
                                               base$ProbEnf3 == 'TRÂNSITO INTENSO DE VEÍCULOS MOTORIZADOS',1,0))%>%
  mutate(Prob_transito = ifelse( is.na(Prob_transito),0,Prob_transito))

base = base %>% mutate(Prob_assalto = ifelse(base$ProbEnf1 == 'PERIGO DE ASSALTO'|
                                                base$ProbEnf2 == 'PERIGO DE ASSALTO'|
                                                base$ProbEnf3 == 'PERIGO DE ASSALTO',1,0))%>%
  mutate(Prob_assalto = ifelse( is.na(Prob_assalto),0,Prob_assalto))

base = base %>% mutate(Esc_semcarro = ifelse(base$MotivoEscolha1 == 'NÃO TENHO CARRO'|
                                               base$MotivoEscolha2 == 'NÃO TENHO CARRO'|
                                               base$MotivoEscolha3 == 'NÃO TENHO CARRO',0,1))%>%
  mutate(Esc_semcarro = ifelse( is.na(Esc_semcarro),1,Esc_semcarro))

base = base %>% mutate(Esc_semhabilit = ifelse(base$MotivoEscolha1 == 'NÃO TENHO HABILITAÇÃO PARA DIRIGIR'|
                                               base$MotivoEscolha2 == 'NÃO TENHO HABILITAÇÃO PARA DIRIGIR'|
                                               base$MotivoEscolha3 == 'NÃO TENHO HABILITAÇÃO PARA DIRIGIR',1,0))%>%
  mutate(Esc_semhabilit = ifelse( is.na(Esc_semhabilit),0,Esc_semhabilit))



base = base %>% mutate(IDH = ifelse(CasaOrig == 1, IDH,ifelse(CasaDest == 1, IDH_dest,mean(IDH))))
base = base %>% mutate(IVS = ifelse(CasaOrig == 1, IVS,ifelse(CasaDest == 1, IVS_dest,mean(IVS))))

base = base %>% mutate(Area_Com_Orig = ifelse(CasaOrig == 1, Area_Com_Orig,ifelse(CasaDest == 1, Area_Com_Dest,mean(Area_Com_Orig))))
base = base %>% mutate(Area_Com_Dest = ifelse(CasaDest == 0, Area_Com_Dest,Area_Com_Orig))

base = base %>% mutate(Acc_pop = (ACC_orig_2012+ ACC_orig_2013 + ACC_orig_2014)/POP_orig,Acc_pop_dest = (ACC_dest_2012 + ACC_dest_2013 + ACC_dest_2014)/POP_dest)

base = base %>% mutate(Acc_pop = ifelse(CasaOrig == 1, Acc_pop,ifelse(CasaDest == 1, Acc_pop_dest,mean(Acc_pop))))

base = base %>% dplyr::select(Area_Com_Orig,Area_Com_Dest,Ocup = Ocupação,Sexo, Idade, Acid_Quant = SofreuAcidenteQ, Roub_Quant =FoiRoubadoQtde,
                              usa_carro_moto = UtilizaOutroMei,Frequencia, Renda, Distancia_km,Distancia_cat, MotDes,Prob_ciclovias,
                              Prob_buracos, Prob_transito, Prob_assalto,Prob_esta,Prob_ilumi, Esc_TerCarro = Esc_semcarro, Esc_semhabilit, IDH, IVS,Acc_pop,Acc_pop_dest)

