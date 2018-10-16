#Make some heat maps, by BIN, per family
setwd("/home/laur/Dropbox/Diptera/")
sheetdf <- read.csv("otutbl_97plus_cleaned3.csv", row.names = 1, stringsAsFactors = F)

tr <- t(sheetdf)
df <- data.frame(tr)
#change the reads from factor to numbers:
df$rank_order <- as.numeric(df$rank_order)
df[4:ncol(df)] <- sapply(df[,4:ncol(df)], as.character)
df[4:ncol(df)] <- sapply(df[,4:ncol(df)], as.numeric)

#pres-abs:
x <- df[,4:ncol(df)]
x[x>0] <- 1

dfbin <- cbind.data.frame(df[,1:3],x )

#t350b
t350b <- subset(dfbin,dfbin$trap=="T3-50B")
t350bm <- as.matrix(t350b[,4:ncol(t350b)])
t350bmt <- t(t350bm)

igg35b <- subset(dfbin, dfbin$trap=="Igg-35B")
t134b <- subset(dfbin,dfbin$trap=="T1-34B")
jos <- subset(dfbin,dfbin$trap=="Jos")
t102b <- subset(dfbin,dfbin$trap=="T1-02B")
t152b <- subset(dfbin,dfbin$trap=="T1-52B")
t164b <- subset(dfbin,dfbin$trap=="T1-64B")
t163b <- subset(dfbin,dfbin$trap=="T1-63B")
sal <- subset(dfbin,dfbin$trap=="Sal")

#Make vectors of which BINs are in families of interest (otutbl_97plus_cleaned1.ods FamiliesBINs tab):
#Chironomidae <- c("AAA8204","AAA9963","AAB3288","AAB5106","AAB5113","AAB8862","AAB9700","AAC0614","AAC4098","AAC4194","AAC4200","AAC4510","AAC6643","AAC7552","AAC9196","AAC9197","AAC9198","AAD1162","AAD1720","AAD2063","AAD4034","AAD4167","AAD5902","AAD7458","AAD8971","AAD9250","AAD9251","AAE0593","AAE0964","AAE1613","AAE3567","AAE6392","AAE7238","AAE7630","AAF2163","AAF3516","AAF4055","AAF7001","AAF7002","AAG1021","AAG6457","AAG6458","AAH2947","AAI0860","AAI0863","AAI1341","AAI1570","AAI1572","AAI1573","AAI2213","AAI4193","AAI6018","AAI6451","AAJ2129","AAJ2130","AAL0178","AAL1600","AAM5377","AAM5389","AAM5390","AAM6263","AAM6273","AAN5355","AAO1037","AAP6930","AAU2576","AAV2322","AAV2697","AAV4655","AAV5074","AAV5075","AAW0359","AAW0581","AAW1297","AAW1343","AAW1344","AAW4001","AAX3566","ABU5525","ABW5528","ACB9814","ACB9910","ACD1957","ACD4501","ACD9477","ACD9509","ACD9519","ACE0146","ACE0351","ACF2339","ACF6302","ACF6903","ACF7218","ACF7553","ACF8295","ACF9730","ACG2966","ACG3403","ACG4398","ACG4647","ACG7714","ACG8108","ACH2822","ACI8241","ACI9294","ACJ0628","ACJ3327","ACK3819","ACK5239","ACN5734","ACN9514","ACP2182","ACP4736","ACQ0651","ACQ4724","ACQ5543","ACR0263","ACR1218","ACR1773","ACR2744","ACR3672","ACT2194","ACT5784","ACT5787","ACT6969","ACT9203","ACT9205","ACT9278","ACU4667","ACU4856","ACW5117","ADC3829","ADI4999")
#Chironomidae <- c("ChiroIntGen_spAAD9250_BOLDAAD9250", "ChiroIntGen_spAAF2163_BOLDAAF2163", "ChiroIntGen_spAAI1572_BOLDAAI1572", "ChiroIntGen_spAAJ2129_BOLDAAJ2129","ChiroIntGen_spAAN5355_BOLDAAN5355","ChiroIntGen_spAAW0359_BOLDAAW0359","ChiroIntGen_spACH2822_BOLDACH2822","ChiroIntGen_spACN5734_BOLDACN5734","ChiroIntGen_spACN9514_BOLDACN9514","ChironInt11_sp._BOLDACR2744","ChironInt18_sp._BOLDACU4667","Apsectrotanypus_trifascipennis_BOLDACD1957","Brillia_bifida_BOLDADI4999","Bryophaenocladius_cf._akiensis_BOLDACR3672","Bryophaenocladius_cf._vernalis_BOLDAAP6930","Bryophaenocladius_flavoscutellatus_BOLDAAW1344","Bryophaenocladius_ictericus_BOLDAAM6273","Bryophaenocladius_sp._BOLDAAG1021","Bryophaenocladius_sp._BOLDAAW1343","Chaetocladius_melaleucus_BOLDAAD4034","Chaetocladius_perennis_BOLDACF6903","Chaetocladius_sp._BOLDAAF4055","Chaetocladius_sp._BOLDACG4647","Chaetocladius_suecicus_BOLDAAC0614","Chironomus_alpestris_BOLDAAW4001","Corynoneura_lobata_BOLDAAD1162","Corynoneura_sp._BOLDAAI0860","Corynoneura_sp._BOLDACI8241","Cricotopus_bicinctus_BOLDAAI6018","Cricotopus_rufiventris_BOLDAAM5377","Cricotopus_sp._BOLDAAU2576","Diamesa_bohemani_BOLDAAB5113","Diamesa_incallida_BOLDAAE7630","Diamesa_tonsa_BOLDAAB5106","Georthocladius_sp._BOLDACD9509","Gymnometriocnemus_brumalis_BOLDACD4501","Gymnometriocnemus_kamimegavirgus_BOLDAAE3567","Gymnometriocnemus_pallidus_BOLDAAG6457","Gymnometriocnemus_sp._BOLDAAI4193","Gymnometriocnemus_sp._BOLDACD9477","Gymnometriocnemus_sp._BOLDACE0146","Gymnometriocnemus_subnudus_BOLDAAI6451","Heterotrissocladius_marcidus_BOLDACQ0651","Krenopelopia_sp._BOLDAAC9196","Krenopelopia_sp._BOLDAAC9197","Krenopelopia_sp._BOLDAAC9198","Krenopelopia_sp._BOLDAAI2213","Krenopelopia_sp._BOLDACT5787","Krenosmittia_boreoalpina_BOLDACQ5543","Limnophyes_asquamatus_BOLDAAD1720","Limnophyes_difficilis_BOLDAAE0964","Limnophyes_edwardsi_BOLDAAB9700","Limnophyes_habilis_BOLDAAJ2130","Limnophyes_minimus_BOLDAAA8204","Limnophyes_pentaplastus_BOLDAAE6392","Limnophyes_sp._BOLDAAI1341","Limnophyes_sp._BOLDAAW1297","Limnophyes_sp._BOLDABU5525","Limnophyes_sp._BOLDABW5528","Limnophyes_sp._BOLDACG3403","Limnophyes_sp._BOLDACG4398","Limnophyes_sp._BOLDACJ0628","Limnophyes_spACJ3327_BOLDACJ3327","Macropelopia_nebulosa_BOLDAAX3566","Mesosmittia_flexuella_BOLDACU4856","Metriocnemus_albolineatus_BOLDAAC6643","Metriocnemus_cf._albolineatus_BOLDACI9294","Metriocnemus_eurynotus_BOLDAAB8862","Metriocnemus_fuscipes_BOLDAAI1573","Metriocnemus_picipes_BOLDAAI1570","Metriocnemus_sp._BOLDAAV4655","Metriocnemus_sp._BOLDACB9910","Micropsectra_appendica_BOLDAAE0593","Micropsectra_atrofasciata_BOLDAAD4167","Micropsectra_cf._longicrista_BOLDACT9203","Micropsectra_cf._roseiventris_BOLDAAF7001","Micropsectra_junci_BOLDAAA9963","Micropsectra_nana_BOLDAAE7238","Micropsectra_notescens_BOLDAAC4098","Micropsectra_pallidula_BOLDAAC7552","Micropsectra_recurvata_BOLDAAC4510","Micropsectra_roseiventris_BOLDAAF7002","Micropsectra_sp._BOLDACF2339","Micropsectra_sp._BOLDACT6969","Micropsectra_sp._BOLDADC3829","Microtendipes_pedellus_BOLDACR0263","Natarsia_punctata_BOLDAAV2697","Orthocladius_frigidus_BOLDAAE1613","Orthocladius_fuscimanus_BOLDAAV5075","Orthocladius_lignicola_BOLDACK5239","Orthocladius_oblidens_BOLDAAD8971","Orthocladius_rubicundus_BOLDAAM5389","Orthocladius_sp._BOLDAAM5390","Orthocladius_sp._BOLDAAV5074","Orthocladius_sp._BOLDACP2182","Orthocladius_sp._BOLDACR1773","Orthocladius_sp._BOLDADL0879","Parachaetocladius_abnobaeus_BOLDACF6302","Parametriocnemus_stylatus_BOLDACT9205","Paraphaenocladius_exagitans_BOLDACQ4724","Paraphaenocladius_impensus_BOLDAAC4200","Paraphaenocladius_impensus_BOLDACT5784","Paraphaenocladius_pseudirritus_BOLDAAC4194","Paraphaenocladius_sp._BOLDACF8295","Paratendipes_albimanus_BOLDAAO1037","Polypedilum_albicorne_BOLDAAL0178","Polypedilum_convictum_BOLDACT9278","Polypedilum_octopunctatum_BOLDACT2194","Prodiamesa_olivacea_BOLDAAD7458","Psectrotanypus_varius_BOLDACK3819","Pseudorthocladius_filiformis_BOLDAAD9251","Pseudorthocladius_pilosipennis_BOLDAAL1600","Pseudorthocladius_sp._BOLDACD9519","Pseudorthocladius_sp._BOLDACF9730","Pseudorthocladius_sp._BOLDACG2966","Pseudosmittia_sp._BOLDAAG6458","Pseudosmittia_sp._BOLDAAM6263","Pseudosmittia_sp._BOLDACG7714","Pseudosmittia_sp._BOLDACG8108","Rheocricotopus_atripes_BOLDAAD5902","Rheocricotopus_effusus_BOLDAAB3288","Rheocricotopus_fuscipes_BOLDAAV2322","Smittia_cf._stercoraria_BOLDACE0351","Smittia_sp._BOLDACB9814","Smittia_sp._BOLDACF7218","Smittia_sp._BOLDACP4736","Smittia_sp._BOLDACR1218","Smittia_sp._BOLDACW5117","Stempellinella_brevis_BOLDAAF3516","Tanytarsus_heusdensis_BOLDACF7553","Tavastia_sp._BOLDAAH2947","Thienemannia_fulvofasciata_BOLDAAI0863","Thienemannia_gracei_BOLDAAW0581", "Tvetenia_bavarica_BOLDAAD2063")
Chironomidae <- read.table("Chironomidae3", header=F)
Muscidae <- read.table("Muscidae3", header=F)
Cecidomyiidae <- read.table("Cecidomyiidae3", header=F)
Syrphidae <- read.table("Syrphidae3", header=F)

#heatmap(t350bmt[rownames(t350bmt) %in% Chironomidae$V1,], scale="none", Rowv=NA, Colv=NA, col=c("white","mediumslateblue") )
#And in the trap-specific ones, get rid of the BINs which do not appear (in the heatmap):
t350bmtChiro <- t350bmt[rownames(t350bmt) %in% Chironomidae$V1,]
t350bmtChiro[rowSums(t350bmtChiro) != 0,]

par(mar = c(2,1,8,7), oma=c(1,0.5,1,12))
labs <- c("May (1)", "May (2)", "June (1)", "June (2)", "July (1)", "July (2)", "August (1)", "August (2)", "Sept. (1)", "Sept. (2)")

heatmap(t350bmtChiro[rowSums(t350bmtChiro) != 0,], scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue") )
title(main="Chironomidae - T3-50B", line=7)

#T350B Muscidae:
t350bmtMusc <- t350bmt[rownames(t350bmt) %in% Muscidae$V1,]
t350bmtMusc[rowSums(t350bmtMusc) != 0,]
heatmap(t350bmtMusc[rowSums(t350bmtMusc) != 0,], scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue") )
title(main="Muscidae - T3-50B", line=7)

#T350B Cecidomyiidae:
t350bmtCecid <- t350bmt[rownames(t350bmt) %in% Cecidomyiidae$V1,]
t350bmtCecid[rowSums(t350bmtCecid) != 0,]

par(mar = c(2,1,6,7), oma=c(1,0.5,0.1,8))
heatmap(t350bmtCecid[rowSums(t350bmtCecid) != 0,], scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue") )
title(main="Cecidomyiidae - T3-50B")

png("Cecidomyiidae_t350b.png", width=700, height=1000)
par(mar = c(0.2,1,4,1), oma=c(1,0.5,0.1,8))
heatmap(t350bmtCecid[rowSums(t350bmtCecid) != 0,], scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue") )
title(main="Cecidomyiidae - T3-50B", line=1,outer=T)
dev.off()

#install.packages("gplots")
library(gplots)
par(mar = c(0,0,0,0), oma=c(1, 0.1, 0.1, 8))
heatmap.2(t350bmtCecid[rowSums(t350bmtCecid) != 0,], dendrogram="none", scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue"), key=F, trace="none", lmat=rbind(c(0,3), c(2,1), c(0,4)), lhei=c(0.25, 4, 0.25) )
heatmap.2(t350bmtCecid[rowSums(t350bmtCecid) != 0,], dendrogram="none", scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue"), key=F, trace="none", lmat=rbind(c(0,3), c(2,1), c(0,4)),  lhei=c(0.2,4,0.2),  lwid=c(0,2) )

png("Cecidomyiidae_t350b1.png", width=700, height=1500)
heatmap.2(t350bmtCecid[rowSums(t350bmtCecid) != 0,], dendrogram="none", scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue"), key=F, trace="none", lmat=rbind(c(1,2),c(3,4)), lhei=c(4,0.2), lwid=c(2,0.2) )
#title(main="Cecidomyiidae - T3-50B", line=1,outer=T)
dev.off()

#T350B Syrphidae:
t350bmtSyrph <- t350bmt[rownames(t350bmt) %in% Syrphidae$V1,]
t350bmtSyrph[rowSums(t350bmtSyrph) != 0,]

par(mar = c(0,0,0,0), oma=c(0.1, 1, 1, 5))
heatmap.2(t350bmtSyrph[rowSums(t350bmtSyrph) != 0,], dendrogram="none", scale="none", cexRow =0.8 , labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue"), key=F, trace="none", lmat=rbind(c(1,2),c(3,4)), lhei=c(4,0.5), lwid=c(2,0.5) )
#heatmap(t350bmtSyrph[rowSums(t350bmtSyrph) != 0,], scale="none", labCol=labs, Rowv=NA, Colv=NA, col=c("white", "mediumslateblue") )



#Igg-35B
igg35b <- subset(dfbin, dfbin$trap=="Igg-35B")
igg35bm <- as.matrix(igg35b[,4:ncol(igg35b)])
igg35bmt <- t(igg35bm)





#par mar 5.1 4.1 4.1 2.1
