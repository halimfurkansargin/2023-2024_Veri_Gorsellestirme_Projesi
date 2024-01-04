#FEN BAŞARI

install.packages("dplyr")
library(dplyr)

estu_2023 <- filter(thestats2023data, X1 >= 448 & X1 <= 492)

estu_2023 <- subset(estu_2023, !(X1 >= 461 & X1 <= 480))

estu_diger <- filter(scores_x, id == "1124")


df1 <- data.frame (id = estu_2023$X9,
                   yıl = "2023",
                   min = as.numeric(estu_2023$X16),
                   max = as.numeric(estu_2023$X18))

df2 <- data.frame (id = estu_diger$department,
                   yıl = estu_diger$year,
                   min = as.numeric(estu_diger$X15),
                   max = as.numeric(estu_diger$X17))


df1$max <- as.numeric(gsub("\\.", "", as.character(df1$max)))
df1$min <- as.numeric(gsub("\\.", "", as.character(df1$min)))




birlesmis_df <- rbind(df1, df2)

secilmek_istenen_bolumler_ff <- c("Biyoloji", "Fizik","Kimya","Matematik","İstatistik")

birlesmis_df_ff <- birlesmis_df[birlesmis_df$id %in% secilmek_istenen_bolumler_ff, ]


install.packages("ggplot2")
library(ggplot2)



ggplot(birlesmis_df_ff, aes(y = id, colour = factor(yıl))) +
  geom_linerange(aes(xmin = min, xmax = max), linetype = 1, position = position_dodge(width = 0.5), size = 1.3) +
  scale_color_manual(values =c("#f59f7a","#eb815d","#d96543","#c04c28","#a8361e","#921e10"))+
  labs(title = "ESTÜ Fen Fakültesi Bölümleri 6 Yıllık Başarı Sıralamaları",
       x = "Başarı Sırası",
       y = "Bölüm",
       color = "Yıllar") +  # Legend başlığını değiştirin
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme_minimal()+
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

#FEN BAŞARI
------------------------------------------------------------------------------------
#MÜH BAŞARI  

birlesmis_df$id <- gsub("\\( İngilizce\\)", "", birlesmis_df$id)
birlesmis_df$id <- trimws(birlesmis_df$id)


secilmek_istenen_bolumler_mf <- c("Bilgisayar Mühendisliği",
                                  "Çevre Mühendisliği",
                                  "Elektrik-Elektronik Mühendisliği","Endüstri Mühendisliği","İnşaat Mühendisliği",
                                  "Kimya Mühendisliği","Makine Mühendisliği","Malzeme Bilimi ve Mühendisliği"
                                  )

birlesmis_df_mf <- birlesmis_df[birlesmis_df$id %in% secilmek_istenen_bolumler_mf, ]

ggplot(birlesmis_df_mf, aes(y = id, colour = factor(yıl))) +
  geom_linerange(aes(xmin = min, xmax = max), linetype = 1, position = position_dodge(width = 0.5), size = 1.3) +
  scale_color_manual(values =c("#f59f7a","#eb815d","#d96543","#c04c28","#a8361e","#921e10"))+
  labs(title = "ESTÜ Mühendislik Fakültesi Bölümleri 6 Yıllık Başarı Sıralamaları",
       x = "Başarı Sırası",
       y = "Bölüm",
       color = "Yıllar") +  # Legend başlığını değiştirin
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme_minimal()+
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

#MÜH BAŞARI

--------------------------------------------------------------------------------------
#FEN TERCİH
  
dfyeni1 <- data.frame(id = estu_2023$X9,
                     yıl = "2023",
                     min = as.numeric(estu_2023$X16),
                     max = as.numeric(estu_2023$X18),
                     ortalama_tercih= as.numeric(estu_2023$X141))

estu_yenidiger <- filter(scores_x, id == "1124"& (year=="2022"| year=="2021"))

dfyeni2 <- data.frame (id = estu_yenidiger$department,
                   yıl = estu_yenidiger$year,
                   min = as.numeric(estu_yenidiger$X15),
                   max = as.numeric(estu_yenidiger$X17),
                   ortalama_tercih= as.numeric(estu_yenidiger$X140))


yenibirlestir_df<- rbind(dfyeni1, dfyeni2)

secilmek_istenen_bolumler_ff <- c("Biyoloji", "Fizik","Kimya","Matematik","İstatistik")

yenibirlestir_df <- yenibirlestir_df[yenibirlestir_df$id %in% secilmek_istenen_bolumler_ff, ]



ggplot(yenibirlestir_df, aes(x = ortalama_tercih, y = id, fill = yıl)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.7) +
  labs(title = "ESTÜ Fen Fakültesi Bölümleri 3 Yıllık Ortalama Tercih Sıraları ",
       x = "Ortalama Tercih Sırası",
       y = "Bölüm",
       fill = "Yıllar") +  # Legend başlığını değiştirin
  theme_minimal()+
  scale_fill_manual(values = c("2021" = "#b52b17", "2022" = "#921e10", "2023" = "#7f160d")) +
  theme_minimal()


#FEN TERCİH
--------------------------------------------------------
#MÜH TERCİH

  yenibirlestir_df$id <- gsub("\\(İngilizce\\)", "", yenibirlestir_df$id)
yenibirlestir_df$id <- trimws(yenibirlestir_df$id)



secilmek_istenen_bolumler_mf <- c("Bilgisayar Mühendisliği",
                                  "Çevre Mühendisliği",
                                  "Elektrik-Elektronik Mühendisliği","Endüstri Mühendisliği","İnşaat Mühendisliği",
                                  "Kimya Mühendisliği","Makine Mühendisliği","Malzeme Bilimi ve Mühendisliği")


yenibirlestir_df <- yenibirlestir_df[yenibirlestir_df$id %in% secilmek_istenen_bolumler_mf, ]




ggplot(yenibirlestir_df, aes(x = ortalama_tercih, y = id, fill = yıl)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.7) +
  labs(title = "ESTÜ Mühendislik Fakültesi Bölümleri 3 Yıllık Ortalama Tercih Sıraları ",
       x = "Ortalama Tercih Sırası",
       y = "Bölüm",
       fill = "Yıllar") +  # Legend başlığını değiştirin
  theme_minimal()+
  scale_fill_manual(values = c("2021" = "#b52b17", "2022" = "#921e10", "2023" = "#7f160d")) +
  theme_minimal()



#MÜH TERCİH

#-----------------------------RADAR--------------------------------------------
# Library
library(fmsb)

radar2023 <- data.frame (id = radar2023_1$X5,
                         yıl = "2023",
                         tercihsayisi = as.numeric(radar2023_1$X140))


radardiger <- data.frame (id = radardiger_1$university,
                          yıl = radardiger_1$year,
                          tercihsayisi = as.numeric(radardiger_1$X139))



radar2023_1<- filter(thestats2023data, X9=="İstatistik")



radardiger_1 <- filter(scores_x, department=="İstatistik")
radardiger_1 <- filter(radardiger_1, year=="2021"|year=="2022")


radarmerge_df <- rbind(radar2023, radardiger)

radarmerge_df<- filter(radarmerge_df, id %in% c("SÜLEYMAN DEMİREL ÜNİVERSİTESİ","ESKİŞEHİR OSMANGAZİ ÜNİVERSİTESİ","ESKİŞEHİR TEKNİK ÜNİVERSİTESİ",
                                                "GAZİ ÜNİVERSİTESİ","HACETTEPE ÜNİVERSİTESİ","MARMARA ÜNİVERSİTESİ",
                                                "ÇUKUROVA ÜNİVERSİTESİ","ONDOKUZ MAYIS ÜNİVERSİTESİ","YILDIZ TEKNİK ÜNİVERSİTESİ","DOKUZ EYLÜL ÜNİVERSİTESİ"))



radarmerge_df <- radarmerge_df[order(radarmerge_df$yıl), ]


radaryeni <-  matrix(as.numeric(radarmerge_df$tercihsayisi) , ncol=10,byrow = TRUE)




colnames(radaryeni) <- c("ÇUKUROVA" , "DOKUZ EYLÜL" , "ESOGÜ" , "ESTÜ" , "GAZİ","HACETTEPE","MARMARA","ONDOKUZ MAYIS","SDÜ","YILDIZ TEKNİK" )

rownames(radaryeni) <- c("2021","2022","2023")



# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
radaryeni <- rbind(rep(3000,10) , rep(0,10) , radaryeni)

# Set graphic colors
library(RColorBrewer)

# Bordo tonları içeren "RdPu" paletini kullanma
coul <- brewer.pal(5, "RdPu")

# Sınır renkleri
colors_border <- coul

library(scales)

# İç renkleri, alpha değerini değiştirmek istiyorsanız 0.3'ü başka bir değerle değiştirebilirsiniz.
colors_in <- alpha(coul, 0.1)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( as.data.frame(radaryeni)  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.2, y=1.8, legend = rownames(radaryeni), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1.2, pt.cex=3)

-----------------------------------------------------------------------------------
  # Library
  library(fmsb)


radar2023_end<- filter(thestats2023data, X9=="Endüstri Mühendisliği")



radardiger_end <- filter(scores_x, department=="Endüstri Mühendisliği")
radardiger_end <- filter(radardiger_1, year=="2021"|year=="2022")



radar2023 <- data.frame (id = radar2023_end$X5,
                         yıl = "2023",
                         tercihsayisi = as.numeric(radar2023_end$X140)
)

radardiger <- data.frame (id = radardiger_end$university,
                          yıl = radardiger_end$year,
                          tercihsayisi = as.numeric(radardiger_end$X139))





radarmerge_df <- rbind(radar2023, radardiger)

radarmerge_df<- filter(radarmerge_df, id %in% c("SÜLEYMAN DEMİREL ÜNİVERSİTESİ","ESKİŞEHİR OSMANGAZİ ÜNİVERSİTESİ","ESKİŞEHİR TEKNİK ÜNİVERSİTESİ",
                                                "SAKARYA ÜNİVERSİTESİ","ERCİYES ÜNİVERSİTESİ","BALIKESİR ÜNİVERSİTESİ",
                                                "ÇUKUROVA ÜNİVERSİTESİ","ONDOKUZ MAYIS ÜNİVERSİTESİ","KOCAELİ ÜNİVERSİTESİ","DOKUZ EYLÜL ÜNİVERSİTESİ"))







radarmerge_df <- radarmerge_df[order(radarmerge_df$yıl), ]


radaryeni <-  matrix(as.numeric(radarmerge_df$tercihsayisi) , ncol=10,byrow = TRUE)




colnames(radaryeni) <- c("BALIKESİR","ÇUKUROVA" , "DOKUZ EYLÜL" ,"ERCİYES", "ESOGÜ" , "ESTÜ" , "KOCAELİ","ONDOKUZ MAYIS","SAKARYA","SDÜ" )

rownames(radaryeni) <- c("2021","2022","2023")



# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
radaryeni <- rbind(rep(3000,10) , rep(0,10) , radaryeni)

# Set graphic colors
library(RColorBrewer)

# Bordo tonları içeren "RdPu" paletini kullanma
coul <- brewer.pal(5, "RdPu")

# Sınır renkleri
colors_border <- coul

library(scales)

# İç renkleri, alpha değerini değiştirmek istiyorsanız 0.3'ü başka bir değerle değiştirebilirsiniz.
colors_in <- alpha(coul, 0.1)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( as.data.frame(radaryeni)  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.2, y=1.8, legend = rownames(radaryeni), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1.2, pt.cex=3)






