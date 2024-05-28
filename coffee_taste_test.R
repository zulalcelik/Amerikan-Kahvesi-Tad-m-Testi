######################Aşağıdaki linkten yönergeler izlenebilir######################
##https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-05-14/readme.md##

# Option 1: tidytuesdayR package 
install.packages("tidytuesdayR")
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2024-05-14')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 20)

coffee_survey <- tuesdata$coffee_survey


# Option 2: Read directly from GitHub

coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

##Cleaning Script  

##install.packages("tidyverse")
##install.packages("janitor")
##install.packages("here")
##install.packages("fs")


library(tidyverse)
library(janitor)
library(here)
library(fs)

working_dir <- here::here("tidytuesday_data", "2024", "2024-05-14")

url <- "https://bit.ly/gacttCSV"

coffee_survey_raw <- readr::read_csv(url)

# Grab the raw questions for the dictionary.
coffee_survey_raw |> 
  colnames() |> 
  cat(sep = "\n")

coffee_survey_rn <- coffee_survey_raw |> 
  janitor::clean_names() |> 
  # Get rid of one-hot encoding; users can do that if they'd like. Also,
  # "flavorings" columns are empty.
  dplyr::select(
    submission_id,
    age = what_is_your_age,
    cups = how_many_cups_of_coffee_do_you_typically_drink_per_day,
    where_drink = where_do_you_typically_drink_coffee,
    brew = how_do_you_brew_coffee_at_home,
    brew_other = how_else_do_you_brew_coffee_at_home,
    purchase = on_the_go_where_do_you_typically_purchase_coffee,
    purchase_other = where_else_do_you_purchase_coffee,
    favorite = what_is_your_favorite_coffee_drink,
    favorite_specify = please_specify_what_your_favorite_coffee_drink_is,
    additions = do_you_usually_add_anything_to_your_coffee,
    additions_other = what_else_do_you_add_to_your_coffee,
    dairy = what_kind_of_dairy_do_you_add,
    sweetener = what_kind_of_sugar_or_sweetener_do_you_add,
    style = before_todays_tasting_which_of_the_following_best_described_what_kind_of_coffee_you_like,
    strength = how_strong_do_you_like_your_coffee,
    roast_level = what_roast_level_of_coffee_do_you_prefer,
    caffeine = how_much_caffeine_do_you_like_in_your_coffee,
    expertise = lastly_how_would_you_rate_your_own_coffee_expertise,
    starts_with("coffee"),
    prefer_abc = between_coffee_a_coffee_b_and_coffee_c_which_did_you_prefer,
    prefer_ad = between_coffee_a_and_coffee_d_which_did_you_prefer,
    prefer_overall = lastly_what_was_your_favorite_overall_coffee,
    wfh = do_you_work_from_home_or_in_person,
    total_spend = in_total_much_money_do_you_typically_spend_on_coffee_in_a_month,
    why_drink = why_do_you_drink_coffee,
    why_drink_other = other_reason_for_drinking_coffee,
    taste = do_you_like_the_taste_of_coffee,
    know_source = do_you_know_where_your_coffee_comes_from,
    most_paid = what_is_the_most_youve_ever_paid_for_a_cup_of_coffee,
    most_willing = what_is_the_most_youd_ever_be_willing_to_pay_for_a_cup_of_coffee,
    value_cafe = do_you_feel_like_you_re_getting_good_value_for_your_money_when_you_buy_coffee_at_a_cafe,
    spent_equipment = approximately_how_much_have_you_spent_on_coffee_equipment_in_the_past_5_years,
    value_equipment = do_you_feel_like_you_re_getting_good_value_for_your_money_with_regards_to_your_coffee_equipment,
    gender,
    gender_specify = gender_please_specify,
    education_level,
    ethnicity_race,
    ethnicity_race_specify = ethnicity_race_please_specify,
    employment_status,
    number_children = number_of_children,
    political_affiliation
  )

readr::write_csv(
  coffee_survey,
  fs::path(working_dir, "coffee_survey.csv")
)

######################Veri Setinin Hazırlanması######################

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


######################Grafik 1######################

##Cinsiyete Göre Favori Kahveler Veri Setini oluşturun

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


######################Grafik 2######################
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

######################Grafik 3######################
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


######################Grafik 4######################
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












