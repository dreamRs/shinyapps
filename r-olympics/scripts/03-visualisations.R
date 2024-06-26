# -----------------------------------------------------------------------------------
# Visualisation des données ---------------------------------------------------------
# -----------------------------------------------------------------------------------
#  ------------------------------------------------------------------------
#
# Title : Titre
#    By : Julien
#  Date : 2024-03
#
#  ------------------------------------------------------------------------

# Packages --------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Données ---------------------------------------------------------------------------

medals <- readRDS("../datas/medals_summer.rds")

# Top des pays par edition avec type de médailles (on pourra choisir le to et la discipline) ---------------------

col_medailles <- c("BRONZE"="#8E6031", "GOLD"="#D1AE31", "SILVER"="#95948E")

x <- medals %>%

        filter(

                 discipline_title %in%  c("Weightlifting")

               ) %>%

        group_by(country_name, medal_type) %>% 

        summarise(n_medailles=n()) %>%

        mutate(n_tot = sum(n_medailles)) %>%

        arrange(-n_tot) 

pre_grep <- grep(unique(x$country_name)[10], x$country_name)

x <- x %>% 

        head(n=pre_grep[length(pre_grep)])

x$medal_type <- factor(x$medal_type, c("GOLD", "SILVER", "BRONZE"))

ggplot(data=x, mapping=aes(y=reorder(country_name, n_medailles), x=n_medailles, fill=medal_type, 
                   width=.5)) + 

        geom_col() + 

        scale_fill_manual(values=col_medailles) +

        labs(x="number of medals", y="countries", 

        title="Top 10 of the countries that have won the most medals, \nby medals type", 

        fill="medal type") +

        theme_minimal()

# Distribution des médailles pour un pays avec le type de médailles pour des éditions choisies (5 dernières ici) ---------------------

x <- medals %>%

        filter(country_name == "France" & slug_game %in% unique(slug_game)[1:5]) %>%

        group_by(discipline_title, medal_type) %>%

        summarise(n_medailles = n()) %>%

        mutate(n_tot = sum(n_medailles)) %>%

        arrange(-n_tot)

ggplot(data=x, mapping=aes(y=reorder(discipline_title, n_tot), x=n_medailles, width=.5, fill=medal_type)) +

        geom_bar(stat="identity") +

        scale_fill_manual(values=col_medailles) +

        labs(x="number of medals", y="sport", 

        title="Number of medals won in each sport for the last \nOlympic Games") +

             theme_minimal()

# Evolution de la popularité des disciplines   ---------------------
#
#x <- data.frame(matrix(data=NA, nrow=0, ncol=4))
#
#for (year in unique(medals$game_year)){
#
#       x2 <- medals %>%
#
#                select(game_year, discipline_title) %>%
#
#                filter(game_year == year) %>%
#
#                group_by(discipline_title) %>%
#
#                arrange(-n()) %>%
#
#                head(n=3)
#
#       x <- rbind(x, x2) 
#
#}

#print(x)

#x2 <- medals %>%
#
#                select(game_year, discipline_title) %>%
#
#                filter(game_year == 2012) %>%
#
#                group_by(discipline_title) %>%
#
#                summarise(n_medals = n()) %>%
#
#                arrange(-n()) 
#
#print(x2)

cur_med <- medals %>% 

        select(country_name, discipline_title) %>%

        filter(discipline_title == "Swimming") %>%

        group_by(country_name) %>%

        summarise(n_medals=n()) %>%

        arrange(-n_medals) %>%

        head(10)

ggplot(data=cur_med, mapping=aes(y=reorder(country_name, n_medals),
                                 x=n_medals)) +

        geom_col() +

        theme_minimal() +

        labs()




