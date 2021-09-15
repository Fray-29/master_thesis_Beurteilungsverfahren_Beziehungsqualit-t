#Datensatz
data_MEKIB_1

install.packages("readxl")
library(readxl)

install.packages("psych")
library(psych)

install.packages("car")
library(car)


#Extraktion Items 1-6 über alle Videos (DISYPS, Alter, Geschlecht)
Item1 <- data_MEKIB_1[ which(data_MEKIB_1$Index1==1), ]
Item2 <- data_MEKIB_1[ which(data_MEKIB_1$Index1==2), ]
Item3 <- data_MEKIB_1[ which(data_MEKIB_1$Index1==3), ]
Item4 <- data_MEKIB_1[ which(data_MEKIB_1$Index1==4), ]
Item5 <- data_MEKIB_1[ which(data_MEKIB_1$Index1==5), ]
Item6 <- data_MEKIB_1[ which(data_MEKIB_1$Index1==6), ]

#Überprüfung NV für abhängige Stichproben 
differenz_1 <- Item1$RaterA - Item1$RaterC
differenz_2 <- Item2$RaterA - Item2$RaterC
differenz_3 <- Item3$RaterA - Item3$RaterC
differenz_4 <- Item4$RaterA - Item4$RaterC
differenz_5 <- Item5$RaterA - Item5$RaterC
differenz_6 <- Item6$RaterA - Item6$RaterC

shapiro.test(differenz_1)
shapiro.test(differenz_2)
shapiro.test(differenz_3)
shapiro.test(differenz_4)
shapiro.test(differenz_5)
shapiro.test(differenz_6)

#Berechnung Wilcoxon-Vorzeichen-Rangtest 
wilcox.test(Item1$RaterA, Item1$RaterC, paired = TRUE, exact = FALSE, correct = TRUE, conf.int = TRUE)
wilcox.test(Item2$RaterA, Item2$RaterC, paired = TRUE, exact = FALSE, correct = TRUE, conf.int = TRUE)
wilcox.test(Item3$RaterA, Item3$RaterC, paired = TRUE, exact = FALSE, correct = TRUE, conf.int = TRUE)
wilcox.test(Item4$RaterA, Item4$RaterC, paired = TRUE, exact = FALSE, correct = TRUE, conf.int = TRUE)
wilcox.test(Item5$RaterA, Item5$RaterC, paired = TRUE, exact = FALSE, correct = TRUE, conf.int = TRUE)
wilcox.test(Item6$RaterA, Item6$RaterC, paired = TRUE, exact = FALSE, correct = TRUE, conf.int = TRUE)

#Erstellung von Datensätzen für die einzelnen Items zur Berechnung der ICC 
data_Icc_Item1 <- subset(Item1, select = c(RaterA, RaterB, RaterC))
data_Icc_Item2 <- subset(Item2, select = c(RaterA, RaterB, RaterC))
data_Icc_Item3 <- subset(Item3, select = c(RaterA, RaterB, RaterC))
data_Icc_Item4 <- subset(Item4, select = c(RaterA, RaterB, RaterC))
data_Icc_Item5 <- subset(Item5, select = c(RaterA, RaterB, RaterC))
data_Icc_Item6 <- subset(Item6, select = c(RaterA, RaterB, RaterC))

#Berechnung Intraklassen-korrelation für einzelne Items Beziehungsqualität (BQ) - Für Rater A, B und Raterin C
ICC(data_Icc_Item1)
ICC(data_Icc_Item2)
ICC(data_Icc_Item3)
ICC(data_Icc_Item4)
ICC(data_Icc_Item5)
ICC(data_Icc_Item6)                         

#Mittelwertsunterschiede und SD
data_Rater_AC_Item1 <- subset(Item1, select = c(RaterA, RaterC))
data_Rater_AC_Item2 <- subset(Item2, select = c(RaterA, RaterC))
data_Rater_AC_Item3 <- subset(Item3, select = c(RaterA, RaterC))
data_Rater_AC_Item4 <- subset(Item4, select = c(RaterA, RaterC))                    
data_Rater_AC_Item5 <- subset(Item5, select = c(RaterA, RaterC))                              
data_Rater_AC_Item6 <- subset(Item6, select = c(RaterA, RaterC))                            

describe(data_Rater_AC_Item1)                              
describe(data_Rater_AC_Item2)                           
describe(data_Rater_AC_Item3)                              
describe(data_Rater_AC_Item4)                              
describe(data_Rater_AC_Item5)                             
describe(data_Rater_AC_Item6)

#Spearmans Rangkorrelation Urteile Rater A und Raterin C
cor.test(Item1$RaterA, Item1$RaterC, method = "spearman")
cor.test(Item2$RaterA, Item2$RaterC, method = "spearman")
cor.test(Item3$RaterA, Item3$RaterC, method = "spearman")
cor.test(Item4$RaterA, Item4$RaterC, method = "spearman")
cor.test(Item5$RaterA, Item5$RaterC, method = "spearman")
cor.test(Item6$RaterA, Item6$RaterC, method = "spearman")

#Bildung von Altersvariablen 
Alter2_Item1 <- Item1[ which(Item1$Alter==2), ]
Alter3_Item1 <- Item1[ which(Item1$Alter==3), ]
Alter4_Item1 <- Item1[ which(Item1$Alter==4), ]
Alter5_Item1 <- Item1[ which(Item1$Alter==5), ]
Alter6_Item1 <- Item1[ which(Item1$Alter==6), ]

Alter2_Item2 <- Item2[ which(Item2$Alter==2), ]
Alter3_Item2 <- Item2[ which(Item2$Alter==3), ]
Alter4_Item2 <- Item2[ which(Item2$Alter==4), ]
Alter5_Item2 <- Item2[ which(Item2$Alter==5), ]
Alter6_Item2 <- Item2[ which(Item2$Alter==6), ]

Alter2_Item3 <- Item3[ which(Item3$Alter==2), ]
Alter3_Item3 <- Item3[ which(Item3$Alter==3), ]
Alter4_Item3 <- Item3[ which(Item3$Alter==4), ]
Alter5_Item3 <- Item3[ which(Item3$Alter==5), ]
Alter6_Item3 <- Item3[ which(Item3$Alter==6), ]

Alter2_Item4 <- Item4[ which(Item4$Alter==2), ]
Alter3_Item4 <- Item4[ which(Item4$Alter==3), ]
Alter4_Item4 <- Item4[ which(Item4$Alter==4), ]
Alter5_Item4 <- Item4[ which(Item4$Alter==5), ]
Alter6_Item4 <- Item4[ which(Item4$Alter==6), ]

Alter2_Item5 <- Item5[ which(Item5$Alter==2), ]
Alter3_Item5 <- Item5[ which(Item5$Alter==3), ]
Alter4_Item5 <- Item5[ which(Item5$Alter==4), ]
Alter5_Item5 <- Item5[ which(Item5$Alter==5), ]
Alter6_Item5 <- Item5[ which(Item5$Alter==6), ]

Alter2_Item6 <- Item6[ which(Item6$Alter==2), ]
Alter3_Item6 <- Item6[ which(Item6$Alter==3), ] 
Alter4_Item6 <- Item6[ which(Item6$Alter==4), ]
Alter5_Item6 <- Item6[ which(Item6$Alter==5), ]
Alter6_Item6 <- Item6[ which(Item6$Alter==6), ]

#Bildung von Gruppenvariablen 
Item1_Alter_234 <- rbind(Alter2_Item1, Alter3_Item1, Alter4_Item1)
Item2_Alter_234 <- rbind(Alter2_Item2, Alter3_Item2, Alter4_Item2)
Item3_Alter_234 <- rbind(Alter2_Item3, Alter3_Item3, Alter4_Item3)
Item4_Alter_234 <- rbind(Alter2_Item4, Alter3_Item4, Alter4_Item4)
Item5_Alter_234 <- rbind(Alter2_Item5, Alter3_Item5, Alter4_Item5)
Item6_Alter_234 <- rbind(Alter2_Item6, Alter3_Item6, Alter4_Item6)

Item1_Alter_56 <- rbind(Alter5_Item1, Alter6_Item1)
Item2_Alter_56 <- rbind(Alter5_Item2, Alter6_Item2)
Item3_Alter_56 <- rbind(Alter5_Item3, Alter6_Item3)
Item4_Alter_56 <- rbind(Alter5_Item4, Alter6_Item4)
Item5_Alter_56 <- rbind(Alter5_Item5, Alter6_Item5)
Item6_Alter_56 <- rbind(Alter5_Item6, Alter6_Item6)

#Deskriptive Mittelwert, SD etc. Alter 234 für Rater A und Raterin C

describe(Item1_Alter_234$RaterA)
describe(Item1_Alter_234$RaterC)

describe(Item2_Alter_234$RaterA)
describe(Item2_Alter_234$RaterC)

describe(Item3_Alter_234$RaterA)
describe(Item3_Alter_234$RaterC)

describe(Item4_Alter_234$RaterA)
describe(Item4_Alter_234$RaterC)

describe(Item5_Alter_234$RaterA)
describe(Item5_Alter_234$RaterC)

describe(Item6_Alter_234$RaterA)
describe(Item6_Alter_234$RaterC)

#Deskriptive Mittelwerte, SD 56 für Rater A und Raterin C

describe(Item1_Alter_56$RaterA)
describe(Item1_Alter_56$RaterC)

describe(Item2_Alter_56$RaterA)
describe(Item2_Alter_56$RaterC)

describe(Item3_Alter_56$RaterA)
describe(Item3_Alter_56$RaterC)

describe(Item4_Alter_56$RaterA)
describe(Item4_Alter_56$RaterC)

describe(Item5_Alter_56$RaterA)
describe(Item5_Alter_56$RaterC)

describe(Item6_Alter_56$RaterA)
describe(Item6_Alter_56$RaterC)


#Überprüfung Normalverteilung unabhängige Stichproben Alter 234

shapiro.test(Item1_Alter_234$RaterA)
shapiro.test(Item1_Alter_234$RaterC)

shapiro.test(Item2_Alter_234$RaterA)
shapiro.test(Item2_Alter_234$RaterC)

shapiro.test(Item3_Alter_234$RaterA)
shapiro.test(Item3_Alter_234$RaterC)

shapiro.test(Item4_Alter_234$RaterA)
shapiro.test(Item4_Alter_234$RaterC)

shapiro.test(Item5_Alter_234$RaterA)
shapiro.test(Item5_Alter_234$RaterC)

shapiro.test(Item6_Alter_234$RaterA)
shapiro.test(Item6_Alter_234$RaterC)

#Überprüfung Normalverteilung unabhängige Stichproben Alter 56

shapiro.test(Item1_Alter_56$RaterA)
shapiro.test(Item1_Alter_56$RaterC)

shapiro.test(Item2_Alter_56$RaterA)
shapiro.test(Item2_Alter_56$RaterC)

shapiro.test(Item3_Alter_56$RaterA)
shapiro.test(Item3_Alter_56$RaterC)

shapiro.test(Item4_Alter_56$RaterA)
shapiro.test(Item4_Alter_56$RaterC)

shapiro.test(Item5_Alter_56$RaterA)
shapiro.test(Item5_Alter_56$RaterC)

shapiro.test(Item6_Alter_56$RaterA)
shapiro.test(Item6_Alter_56$RaterC)


#Berechnung Mann-Whitney U Test unabhängige Stichproben Rater A 

Alter_234_56_A1 <- wilcox.test(Item1_Alter_234$RaterA, Item1_Alter_56$RaterA, exact = FALSE)
Alter_234_56_A1
Alter_234_56_A2 <- wilcox.test(Item2_Alter_234$RaterA, Item2_Alter_56$RaterA, exact = FALSE)
Alter_234_56_A2
Alter_234_56_A3 <- wilcox.test(Item3_Alter_234$RaterA, Item3_Alter_56$RaterA, exact = FALSE)
Alter_234_56_A3
Alter_234_56_A4 <- wilcox.test(Item4_Alter_234$RaterA, Item4_Alter_56$RaterA, exact = FALSE)
Alter_234_56_A4
Alter_234_56_A5 <- wilcox.test(Item5_Alter_234$RaterA, Item5_Alter_56$RaterA, exact = FALSE)
Alter_234_56_A5
Alter_234_56_A6 <- wilcox.test(Item6_Alter_234$RaterA, Item6_Alter_56$RaterA, exact = FALSE)
Alter_234_56_A6

#Berechnung Mann-Whitney U Test statt t-Test unabhängige Stichproben Rater C

Alter_234_56_C1 <- wilcox.test(Item1_Alter_234$RaterC, Item1_Alter_56$RaterC, exact = FALSE)
Alter_234_56_C1
Alter_234_56_C2 <- wilcox.test(Item2_Alter_234$RaterC, Item2_Alter_56$RaterC, exact = FALSE)
Alter_234_56_C2
Alter_234_56_C3 <- wilcox.test(Item3_Alter_234$RaterC, Item3_Alter_56$RaterC, exact = FALSE)
Alter_234_56_C3
Alter_234_56_C4 <- wilcox.test(Item4_Alter_234$RaterC, Item4_Alter_56$RaterC, exact = FALSE)
Alter_234_56_C4
Alter_234_56_C5 <- wilcox.test(Item5_Alter_234$RaterC, Item5_Alter_56$RaterC, exact = FALSE)
Alter_234_56_C5
Alter_234_56_C6 <- wilcox.test(Item6_Alter_234$RaterC, Item6_Alter_56$RaterC, exact = FALSE)
Alter_234_56_C6

#Spearmans Rangkorrelation Rater A und C für kk vs. vk
cor.test(Item1_Alter_234$RaterA, Item1_Alter_56$RaterA, method = "spearman")
cor.test(Item1_Alter_234$RaterC, Item1_Alter_56$RaterC, method = "spearman")
cor.test(Item2_Alter_234$RaterA, Item2_Alter_56$RaterA, method = "spearman")
cor.test(Item2_Alter_234$RaterC, Item2_Alter_56$RaterC, method = "spearman")
cor.test(Item3_Alter_234$RaterA, Item3_Alter_56$RaterA, method = "spearman")
cor.test(Item3_Alter_234$RaterC, Item3_Alter_56$RaterC, method = "spearman")
cor.test(Item4_Alter_234$RaterA, Item4_Alter_56$RaterA, method = "spearman")
cor.test(Item4_Alter_234$RaterC, Item4_Alter_56$RaterC, method = "spearman")
cor.test(Item5_Alter_234$RaterA, Item5_Alter_56$RaterA, method = "spearman")
cor.test(Item5_Alter_234$RaterC, Item5_Alter_56$RaterC, method = "spearman")
cor.test(Item6_Alter_234$RaterA, Item6_Alter_56$RaterA, method = "spearman")
cor.test(Item6_Alter_234$RaterC, Item6_Alter_56$RaterC, method = "spearman")

#bildung Subset alter 56 alle Items 
Alter_234 <- rbind(Item1_Alter_56, Item2_Alter_56,Item3_Alter_56,Item4_Alter_234, Item5_Alter_234, Item6_Alter_234)
#Berechnung NV für 56
































