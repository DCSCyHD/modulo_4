library(tidyverse)
library(tidymodels)

radios <- read_csv('./clase4/data/radios_hogar.csv')

x<-radios%>% filter(provincia=="Buenos Aires") %>%
        select(depto) %>% unique()


radios <- radios %>%
        mutate(cod_depto = str_sub(radio, 3,5))

sm <- radios %>%
        filter(cod_depto == 371)

recipe_km <- sm %>% 
        select(banio_uso_exclusivo:regimen_tenencia_propietario) %>%
        recipe(~.) %>%
        step_normalize(all_numeric()) %>%
        step_pca(all_numeric(), num_comp=6)


dist <- recipe_km %>%
        prep() %>%
        bake(sm) %>%
        dist(.x, method="manhattan")



hc_clust <- hclust(dist, method="ward.D2")

plot(hc_clust)

sm <- sm %>% mutate(hc = as.factor(cutree(hc_clust, k=4)))

sm %>%
        select(hc, banio_uso_exclusivo:regimen_tenencia_propietario, hogares_sin_nbi) %>%
        pivot_longer(cols = banio_uso_exclusivo:hogares_sin_nbi) %>%
        ggplot(aes(y=value, fill=hc)) + 
        geom_boxplot() +
        theme_minimal() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        facet_wrap(~name)


### K-MEDIAS