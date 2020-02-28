library(tidyverse)
library(xml2)
library(XML)
library(ggwordcloud)

set.seed(12324)
setwd("g:/My Drive/R/Street Names Analysis/")
### Street names in Brussels ###

# Brussels ----------------------------------------------------------------
pth_bxl <-"./data/BrusselsStreetname20200216/BrusselsStreetname20200216L72.xml"
pth_mun_bxl <- "./data/BrusselsMunicipality20200216/BrusselsMunicipality20200216.xml"


street_address_bxl <- read_xml(pth_bxl)
municipality_bxl <- read_xml(pth_mun_bxl)

language_bxl <- xml_text(xml_find_all(street_address_bxl, ".//com:language"))
commune_bxl <-  xml_text(xml_find_all(street_address_bxl, ".//com:Municipality//com:objectIdentifier"))
adress_bxl <-  xml_text(xml_find_all(street_address_bxl, ".//com:spelling"))

int_street_df <- tibble(
  lang = language_bxl,
  address = adress_bxl
)

int_street_df_fr <- int_street_df %>% 
  filter(lang == "fr") %>% 
  mutate(code_mun = commune_bxl)


int_street_df_nl <- int_street_df %>% 
  filter(lang == "nl") %>% 
  mutate(code_mun = commune_bxl)

street_df_bxl <- int_street_df_fr %>% 
  union_all(int_street_df_nl)%>% 
  mutate(region = "bxl")


language_mun_bxl <- xml_text(xml_find_all(municipality_bxl, ".//com:language"))
commune_mun_bxl <-  xml_text(xml_find_all(municipality_bxl, ".//com:objectIdentifier"))
adress_mun_bxl <-  xml_text(xml_find_all(municipality_bxl, ".//com:spelling"))

int_mun_df <-  tibble(
  municipality = adress_mun_bxl,
  lang = language_mun_bxl
)

int_mun_df_fr<- int_mun_df %>% 
  filter(lang == "fr") %>% 
  mutate(code_mun = commune_mun_bxl)


int_mun_df_nl<- int_mun_df %>% 
  filter(lang == "nl") %>% 
  mutate(code_mun = commune_mun_bxl)

mun_df_bxl <- int_mun_df_fr %>% 
  union_all(int_mun_df_nl) %>% 
  mutate(region = "bxl")


##joined tables##

street_mun_df_bxl <- street_df_bxl %>% 
  left_join(mun_df_bxl, by = c("code_mun","lang")) %>% 
  select(lang, address, code_mun, region = region.x, municipality)

##Wordcloud##
street_mun_df_bxl %>% 
  filter(lang == "fr") %>% 
  separate(address, into = c("one","two","three","four","five","six","seven")) %>% 
  gather(key = order, value  = name, -lang, -region, -code_mun,- municipality) %>% 
  count(name) %>% 
  arrange(desc(n)) %>%
  top_n(200) %>% 
  filter(!is.na(name),
         !(name %in% c("de","la","des","d","et","du","l","Van","Rue","Avenue","Place","Clos","Boulevard","rue","Chaussée",
                       "Le","De","La","Square","Drève","Chemin","à","II","A","V", "Pré","Impasse","aux","Quai","Allée","Venelle","Val","ten","J","l","van","Job","Aa","Mail","t"))) %>% 
  ggplot(aes(label = name,size=n))+
  geom_text_wordcloud(shape = "pentagon")+
  scale_size_area(max_size = 20)+
  theme_minimal()
  
### Street Names in Flanders ###

# Vlanderen ---------------------------------------------------------------


pth_vl <-"./data/FlandersStreetname20200216L72/FlandersStreetname20200216L72.xml"
pth_mun_vl <- "./data/FlandersMunicipality20200216L72/FlandersMunicipality20200216L72.xml"

street_address_vl <- read_xml(pth_vl)
municipality_vl <- read_xml(pth_mun_vl)

language_vl <- xml_text(xml_find_all(street_address_vl, ".//com:language"))
commune_vl <-  xml_text(xml_find_all(street_address_vl, ".//com:Municipality//com:objectIdentifier"))
adress_vl <-  xml_text(xml_find_all(street_address_vl, ".//com:spelling"))

int_street_df_vl <- tibble(
  lang = language_vl,
  address = adress_vl
)


street_df_vl <- int_street_df_vl %>% 
  filter(lang == "nl") %>% 
  mutate(code_mun = commune_vl)


language_mun_vl <- xml_text(xml_find_all(municipality_vl, ".//com:language"))
commune_mun_vl <-  xml_text(xml_find_all(municipality_vl, ".//com:objectIdentifier"))
adress_mun_vl <-  xml_text(xml_find_all(municipality_vl, ".//com:spelling"))

int_mun_df_vl <- tibble(
  lang = language_mun_vl,
  municipality = adress_mun_vl
)

mun_df_vl <- int_mun_df_vl %>% 
  filter(lang =="nl") %>% 
  mutate(code_mun = commune_mun_vl)

### joined tables ###

street_mun_df_vl <- street_df_vl %>% 
  left_join(mun_df_vl) %>% 
  mutate(region = "vl")

street_mun_df_vl %>% 
  separate(address, into = c("one","two","three","four","five","six","seven")) %>% 
  gather(key = order, value  = name, -lang, -region, -code_mun,- municipality) %>% 
  count(name) %>% 
  arrange(desc(n)) %>%
  top_n(200) %>% 
  filter(!is.na(name),
         !(name %in% c("de","la","des","d","et","du","l","Van","Rue","Avenue","Place","Clos","Boulevard","rue","Chaussée",
                       "Le","De","La","Square","Drève","Chemin","à","II","A","V", "Pré","Impasse","aux","Quai","Allée","Venelle","Val","ten","J","l","van","Job","Aa","Mail","t"))) %>% 
ggplot(aes(label = name,size=n))+
  geom_text_wordcloud(shape = "pentagon")+
  scale_size_area(max_size = 20)+
  theme_minimal()

### Street Names in Wallonia ###

# Wallonia ---------------------------------------------------------------


pth_wl <-"./data/WalloniaStreetname20200214L72/WalloniaStreetname20200214.xml"
pth_mun_wl <- "./data/WalloniaMunicipality20200214/WalloniaMunicipality20200214.xml"

street_address_wl <- read_xml(pth_wl)
municipality_wl <- read_xml(pth_mun_wl)

language_wl <- xml_text(xml_find_all(street_address_wl, ".//com:language"))
commune_wl <-  xml_text(xml_find_all(street_address_wl, ".//com:Municipality//com:objectIdentifier"))
adress_wl <-  xml_text(xml_find_all(street_address_wl, ".//com:spelling"))

int_street_df_wl <- tibble(
  lang = language_wl,
  address = adress_wl
)

street_df_wl <- int_street_df_wl %>% 
  filter(lang == "fr") %>% 
  mutate(code_mun = commune_wl[xml_text(xml_find_all(street_address_wl, ".//com:language")) =="fr"])


language_mun_wl <- xml_text(xml_find_all(municipality_wl, ".//com:language"))
commune_mun_wl <-  xml_text(xml_find_all(municipality_wl, ".//com:objectIdentifier"))
adress_mun_wl <-  xml_text(xml_find_all(municipality_wl, ".//com:spelling"))

int_mun_df_wl <- tibble(
  lang = language_mun_wl,
  municipality = adress_mun_wl
)

mun_df_wl <- int_mun_df_wl %>% 
  filter(lang =="fr") %>% 
  mutate(code_mun = commune_mun_wl[xml_text(xml_find_all(municipality_wl, ".//com:language")) == "fr"])

### joined tables ###

street_mun_df_wl <- street_df_wl %>% 
  left_join(mun_df_wl) %>% 
  mutate(region = "wl")

street_mun_df_wl %>% 
  separate(address, into = c("one","two","three","four","five","six","seven","eight","nine","10")) %>% 
  gather(key = order, value  = name, -lang, -region, -code_mun,- municipality) %>% 
  count(name) %>% 
  arrange(desc(n)) %>%
  top_n(200) %>% 
  filter(!is.na(name),
         !(name %in% c("de","la","des","d","et","du","l","Van","Rue","Avenue","Place","Clos","Boulevard","rue","Chaussée",
                       "Le","De","La","Square","Drève","Chemin","à","II","A","V", "Pré","Impasse","aux","Quai","Allée","Venelle","Val","ten","J","l","van","Job","Aa","Mail","t"))) %>%
ggplot(aes(label = name,size=n))+
  geom_text_wordcloud(shape = "pentagon")+
  scale_size_area(max_size = 20)+
  theme_minimal()



### Union All and export ###

final_address_belgium <- street_mun_df_bxl %>% 
  # filter(lang =="fr") %>% 
  union_all(street_mun_df_vl) %>% 
  union_all(street_mun_df_wl)

write_csv(final_address_belgium, "./final_address.csv")
