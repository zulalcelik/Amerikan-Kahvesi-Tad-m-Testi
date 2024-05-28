# AMERİKAN KAHVESİ TADIM TESTİ


# VERİ KAYNAĞI
Aşağıdaki linkten yönergeler izlenebilir
https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-05-14/readme.md

Veri kaynağından veri seti alındıktan sonra yapıldıktan sonra baz veri seti hazırlanmıştır.

```{r}
coffee <- coffee_survey_rn

##NA değerleri kaldırın

coffee_main <- 
  coffee %>% 
  drop_na(age,
          favorite,
          roast_level,
          prefer_overall,
          gender) %>%
  select("age","favorite","roast_level",
            "prefer_overall","gender")
```

# GRAFİKLER
# Grafik 1 - Cinsiyete Göre Favori Kahveler

```{r}
#Cinsiyete Göre Favori Kahveler Veri Setini oluşturun
favorite_c_perc <-
  coffee_main %>%
  select("favorite","gender") %>%
  filter(gender %in% c("Male","Female") & 
           (favorite != "Blended drink (e.g. Frappuccino)") & 
           (favorite != "Other")
         ) %>%
  group_by(gender,favorite) %>%
  count(favorite) %>%
  group_by(gender) %>%
  mutate(perc= (n/sum(n))) %>%
  arrange(desc(gender))

library(stringi)
favorite_c_perc$gender <- stri_replace_all_regex(favorite_c_perc$gender,
                                            pattern=c('Male', 'Female'),
                                            replacement=c('Erkek', 'Kadın'),
                                            vectorize=FALSE)
favorite_c_perc$favorite <- stri_replace_all_regex(favorite_c_perc$favorite,
                                              pattern=c('Regular drip coffee', 'Iced coffee','Cold brew'),
                                              replacement=c('Filtre Kahve', 'Soğuk Kahve', 'Cold Brew'),
                                              vectorize=FALSE)

##Grafik 1 için custom lollipop chart oluşturun

favorite_c_g <-
  ggplot(favorite_c_perc, 
         aes(x=reorder(favorite,+perc), 
             y=perc, 
             colour = gender)) +
  geom_segment(aes(x=favorite, xend=favorite, y=0, yend=perc), 
               stat="identity", 
               color="#AAB8BB") +
  geom_point(
             size=10, #(4) grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa eski değer atanabilir.
             alpha=0.8) +
  scale_color_manual(values = c("#829B88", "#A36361"),
                     guide  = guide_legend()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme(
    rect = element_rect(fill = "transparent"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "#6c3c0c"),
    axis.text.y = element_text(color = "#6c3c0c"),
    
    text = element_text(family = "mono", size = 30), #(18) grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa eski değer atanabilir.
    
    plot.title = element_text(size = 50, ##(30) grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa eski değer atanabilir.
                              color = "#6c3c0c",
                              hjust = -1.5, 
                              vjust = 1),
    
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(colour = "#6c3c0c", size = 25), #(18) grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa eski değer atanabilir.
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = NA_character_),
    
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
  ) +
  labs(
    title = "Cinsiyete Göre Favori Kahveler"
       )

favorite_c_g

##Grafiği kaydedin

png('favorite_c_g.png',width=1200,height=800,units="px",bg = "transparent")
print(favorite_c_g)
dev.off()
```
![image](https://github.com/zulalcelik/Amerikan-Kahvesi-Tadim-Testi/assets/83671332/0bb71790-04aa-4993-a374-30f0c2430f3b)

Grafikte katılımcıların gündelik hayatlarında tercih ettikleri favori kahvelerinin cinsiyete göre yüzdelikleri görüntülenmektedir.
Buna göre Kadın katılımcıların %30'undan fazlasının favori içeceğini Latte olurken, Erkek katılımcıların %30'undan fazlasının favori içeceği Purover olduğu gözlemlenmektedir.


# Grafik 2 - Cinsiyete Göre Kavrulmuş Kahve Tercihi
```{r}
##Cinsiyete Göre Kavrulmuş Kahve Tercihi Veri Setini oluşturun


roast_c <-
  coffee_main %>%
  select("roast_level","gender") %>%
  filter(
    gender %in% c("Male","Female")
    &
    roast_level %in% c("Dark","Medium","Light")
    ) 

library(stringi)

roast_c$gender <- stri_replace_all_regex(roast_c$gender,
                                                 pattern=c('Male', 'Female'),
                                                 replacement=c('Erkek', 'Kadın'),
                                                 vectorize=FALSE)
roast_c$roast_level <- stri_replace_all_regex(roast_c$roast_level,
                                                   pattern=c('Dark', 'Medium','Light'),
                                                   replacement=c('Koyu', 'Orta', 'Açık'),
                                                   vectorize=FALSE)
roast_c$roast_level <- factor(roast_c$roast_level, levels = c("Açık","Orta","Koyu"))

roast_c <-
  roast_c %>%
  select("roast_level","gender") %>%
  arrange(roast_level)

##Grafik 2 için mosaic plot oluşturun

library(ggplot2)
library(ggmosaic)

roast_c_g <-
  ggplot(data = roast_c) +
  geom_mosaic(aes(x = product(gender), fill = roast_level)) +
    scale_fill_manual(values = c("Açık"="#EDCC8B", "Orta"="#CAA46E","Koyu"="#A96338")) +
    theme(
      rect = element_rect(fill = "transparent"),
      
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      
      axis.title = element_blank(),
      axis.text.x = element_text(color = "#6c3c0c"),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(color = "#6c3c0c"),
      axis.ticks.y = element_blank(),
      text = element_text(family = "mono", size = 30), #grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
      
      plot.title = element_text(size = 50, color = "#6c3c0c", hjust = 0.5, vjust = 1),#grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
      plot.background = element_rect(fill = "transparent",
                                     colour = NA_character_),
      
      panel.background = element_rect(fill = "transparent",
                                      colour = NA_character_),
      
      legend.title = element_blank(),
      legend.text = element_text(colour = "#6c3c0c", size = 25),#grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
      legend.background = element_rect(fill = "transparent")
    ) +
    labs(
      title = "Cinsiyete Göre Kavrulmuş Kahve Tercihi"
    )
  
roast_c_g

##Grafiği kaydedin

png('roast_c_g.png',width=1200,height=800,units="px",bg = "transparent")
print(roast_c_g)
dev.off()

```

![image](https://github.com/zulalcelik/Amerikan-Kahvesi-Tadim-Testi/assets/83671332/c69b587f-a66a-4e31-9c7e-ba22542c9f21)


Grafikte Erkek katılımcıların büyük oranı açık kavrulmuş kahveyi tercih ederken; 
Kadın katılımcıların açık kavrulmuş kahveyi en az tercih ettikleri koyu ve orta kavrulmuş kahveyi tercih ettikleri gözlemlenmektedir.

# Grafik 3 - Yaşa Göre Kavrulmuş Kahve Tercihi
```{r}
##Yaşa Göre Kavrulmuş Kahve Tercihi Veri Setini oluşturun

roast_age_c <-
  coffee_main %>%
  select("roast_level","age") %>%
  filter(
    roast_level %in% c("Dark","Medium","Light")
  ) 

library(stringi)

roast_age_c$roast_level <- stri_replace_all_regex(roast_age_c$roast_level,
                                              pattern=c('Dark', 'Medium','Light'),
                                              replacement=c('Koyu', 'Orta', 'Açık'),
                                              vectorize=FALSE)
roast_age_c$roast_level <- factor(roast_age_c$roast_level, levels = c("Açık","Orta","Koyu"))

roast_age_c$age <- stri_replace_all_regex(roast_age_c$age,
                                                  pattern=c('<18 years old', '18-24 years old','25-34 years old',
                                                            '35-44 years old', '45-54 years old','55-64 years old',
                                                            '>65 years old'),
                                                  replacement=c('<18', '18-24 yaş', '25-34 yaş',
                                                                '35-44 yaş', '45-54 yaş','55-64 yaş',
                                                                '>65'),
                                                  vectorize=FALSE)
roast_age_c$age <- factor(roast_age_c$age, levels = c("<18", 
                                                      "18-24 yaş",
                                                      "25-34 yaş",
                                                      "35-44 yaş",
                                                      "45-54 yaş",
                                                      "55-64 yaş",
                                                      ">65"))
roast_age_c <-
  roast_age_c %>%
  select("roast_level","age") %>%
  arrange(age,roast_level)

##Grafik 3 için mosaic plot oluşturun

library(ggplot2)
library(ggmosaic)

roast_age_c_g <-
  ggplot(data = roast_age_c) +
  geom_mosaic(aes(x = product(age), fill=roast_level)) +
  scale_fill_manual(values = c("Açık"="#EDCC8B", "Orta"="#CAA46E","Koyu"="#A96338")) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    
    axis.title = element_blank(),
    axis.text.x = element_text(color = "#6c3c0c", angle = 90),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "#6c3c0c"),
    axis.ticks.y = element_blank(),
    
    text = element_text(family = "mono", size = 25),#grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
    
    plot.title = element_text(size = 50, color = "#6c3c0c", hjust = 0.5, vjust = 1),#grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    
    legend.title = element_blank(),
    legend.text = element_text(colour = "#6c3c0c", size = 20),#grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
    legend.background = element_rect(fill = "transparent")
  ) +
  labs(
    title = "Yaşa Göre Kavrulmuş Kahve Tercihi"
  )

roast_age_c_g

##Grafiği kaydedin

png('roast_age_c_g.png',width=1200,height=800,units="px",bg = "transparent")
print(roast_age_c_g)
dev.off()
```

![image](https://github.com/zulalcelik/Amerikan-Kahvesi-Tadim-Testi/assets/83671332/3a2cd0d9-295d-4b6f-80c3-c66f75cba3a1)

Grafikte katılımcıların büyük bir çoğunluğunun 25-34 yaş aralığında olduğu görülmektedir. 
Bununla birlikte katılımcılar içerisinde Açık Kavrulmuş Kahve daha çok tercih edilmektedir.

# Grafik 4 - Katılımcı Profillerinin Kahve Tercihleri
```{r}
##Katılımcı Profillerinin Kahve Tercihleri Veri Setini oluşturun

all_cat_c <-
  coffee_main %>%
  select("gender","age","roast_level","prefer_overall") %>%
  filter(
    roast_level %in% c("Dark","Medium","Light")
    &
    gender %in% c("Male","Female")
  )


all_cat_c$roast_level <- stri_replace_all_regex(all_cat_c$roast_level,
                                                  pattern=c('Dark', 'Medium','Light'),
                                                  replacement=c('Koyu', 'Orta', 'Açık'),
                                                  vectorize=FALSE)
all_cat_c$roast_level <- factor(all_cat_c$roast_level, levels = c("Açık","Orta","Koyu"))

all_cat_c$age <- stri_replace_all_regex(all_cat_c$age,
                                          pattern=c('<18 years old', '18-24 years old','25-34 years old',
                                                    '35-44 years old', '45-54 years old','55-64 years old',
                                                    '>65 years old'),
                                          replacement=c('<18', '18-24', '25-34',
                                                        '35-44', '45-54','55-64',
                                                        '>65'),
                                          vectorize=FALSE)
all_cat_c$age <- factor(all_cat_c$age, levels = c("<18", 
                                                      "18-24",
                                                      "25-34",
                                                      "35-44",
                                                      "45-54",
                                                      "55-64",
                                                      ">65"))

all_cat_c$gender <- stri_replace_all_regex(all_cat_c$gender,
                                         pattern=c('Male', 'Female'),
                                         replacement=c('Erkek', 'Kadın'),
                                         vectorize=FALSE)

all_cat_c$prefer_overall <- stri_replace_all_regex(all_cat_c$prefer_overall,
                                           pattern=c('Coffee A', 'Coffee B', 'Coffee C', 'Coffee D'),
                                           replacement=c('Kahve A', 'Kahve B', 'Kahve C', 'Kahve D'),
                                           vectorize=FALSE)

all_cat_c <-
  all_cat_c %>%
  select("gender","age","roast_level","prefer_overall")  %>%
  arrange(age,roast_level)


##Grafik 4 için sankey diagram oluşturun

##install.packages("devtools")
##devtools::install_github("davidsjoberg/ggsankey")

library(ggsankey)
library(ggplot2)
library(dplyr)


all_cat_c$age<- as.integer(all_cat_c$age)
all_cat_c$age <- cut(all_cat_c$age, 
                  breaks = c("<18",
                             "18-24",
                             "25-34",
                             "35-44",
                             "45-54",
                             "55-64",
                             ">65"), 
                  labels = c("<18",
                             "18-24",
                             "25-34",
                             "35-44",
                             "45-54",
                             "55-64",
                             ">65"),
                  right = FALSE)


df <- all_cat_c %>%
  make_long(gender,age,roast_level, prefer_overall)


dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()


df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)


sankey_d <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = ifelse(node == 1, "<18", 
                              ifelse(node == 2, "18-24", 
                              ifelse(node == 3, "25-34",
                              ifelse(node == 4, "35-44",
                              ifelse(node == 5, "45-54",
                              ifelse(node == 6, "55-64",
                              ifelse(node == 7, ">65",
                              paste0(node))))))))
                      )
             ) +
  geom_sankey(
    flow.alpha = 0.8, 
    show.legend = TRUE, 
    width = 0.15,
    position = "identity"
    ) +
  geom_sankey_label(
    family = "mono", 
    size = 12, #grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
    color = "white", 
    hjust = 0.5, 
    angle = 90
    ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "mono", 
                              size = 60, #grafik kaydedilirken boyutu değiştirilmiştir. R'da görüntülenmek isteniyorsa yeni değer atanabilir.
                              color = "#6c3c0c", 
                              hjust = 0.5, 
                              vjust = 1),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),


    legend.background = element_rect(fill = "transparent"),
    axis.text.x = element_blank(),
    ) + 
  scale_fill_manual(values = c('Koyu'="#A96338", 
                               'Orta'="#CAA46E", 
                               'Açık'="#EDCC8B",
                               'Erkek' = "#829B88", 
                               'Kadın' = "#A36361",
                               '1' = "#AAB8BB",
                               '2' = "#CBD5C0",
                               '3' = "#BEE3AB",
                               '4' = "#79B791",
                               '5' = "#6C8976",
                               '6' = "#609595",
                               '7' = "#596869",
                               'Kahve A' = "#E8A995",
                               'Kahve B' = "#BBCBD2",
                               'Kahve C' = "#63767A",
                               'Kahve D' = "#C28B7C")
                    ) +

  labs(
    title = "Katılımcı Profillerinin Kahve Tercihleri"
  )

sankey_d


##Grafiği kaydedin
png('sankey_d.png',width=3000,height=1500,units="px",bg = "transparent")
print(sankey_d)
dev.off()
```


![image](https://github.com/zulalcelik/Amerikan-Kahvesi-Tadim-Testi/assets/83671332/1bb1300b-9ad8-4374-9575-a170467a01b2)

Akış diyagramında tadım testi yapılan 4 farklı kahveyi deneyimleyen katılımcıların cinsiyet, yaş, kavrulmuş kahve tercihlerine göre hangi kahveye yöneldikleri gözlemlenmektedir. Katılımcıların bir çoğu 25-44 yaş aralığındaki kişilerden oluştuğu ve Orta - Açık kavrulmuş kahveyi tercih ettikleri görülmektedir. Katılımcılar arasında en çok Kahve D'nin tercih edildiği ve bu katılımcıların kavrulmuş kahve tercihinin Açık olduğu söylenebilir. Koyu kavrulmuş kahve tercihi olan kişiler ise neredeyse eşit oranda Kahve B ve Kahve C ye yöneldiği söylenebilir.

