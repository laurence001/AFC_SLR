library(tidyverse)
library(highcharter)
library(formattable)

corpus <- as.data.frame(main_corpus)

corpus$Type <- as.factor(corpus$Type)
corpus$Year <- as.factor(corpus$Year)
corpus$Journalism <- as.factor(corpus$Journalism)

#distribution of the corpus by year and type of paper

by_year <- corpus %>%
  select(Type,Year) %>%
  count(Type,group=Year) %>%
  mutate(pc=(n/267)*100)

by_year$pc <- round(by_year$pc,2)

tt_year <- by_year %>%
  mutate(sum = sum(n))
tt_year

ttyear <- aggregate(tt_year["n"],by=tt_year["group"],sum)
ttyear

by_year %>% 
  hchart(
    'column', hcaes(x = group, y = n, group = Type)
  )  %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}"
      ),
      stacking = "normal",
      showInLegend = TRUE
    ) ) %>%
  hc_yAxis(min = 0, max=100,title = list(text = "N = 267")) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_add_theme(themer) %>%
  hc_add_series(
  ttyear,
  type = "spline",
  hcaes(x = group, y = n),
  lineWidth = 1,
  showInLegend = FALSE,
  color = "#c86558"
  )


#distribution of the corpus by type of paper
type <- prop.table(table(corpus$Type))
type <- as.data.frame(type)
colnames(type) <- c("Type","N")
type$N <- round(type$N,4)
type$N <- type$N*100

highchart() %>%
  hc_add_series(
    data = type,
    type = "pie",
    hcaes(x = Type, y = N)
  ) %>%
  hc_tooltip(pointFormat = "<b>{Type}: </b>{point.y}%") %>%
  hc_plotOptions(
    pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}%"
      ),
      showInLegend = TRUE
    ) ) %>%
  hc_add_theme(themer) 

journalism <- prop.table(table(corpus$Journalism))
journalism <- as.data.frame(journalism)
colnames(journalism) <- c("Journalism","N")
levels(journalism$Journalism) <- c("No","Yes")
journalism$N <- round(journalism$N,4)
journalism$N <- journalism$N*100
journalism

highchart() %>%
  hc_add_series(
    data = journalism,
    type = "pie",
    hcaes(x = Journalism, y = N)
  ) %>%
  hc_tooltip(pointFormat = "<b>{Journalism}: </b>{point.y}%") %>%
  hc_plotOptions(
    pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}%"
      ),
      showInLegend = TRUE
    ) ) %>%
  hc_add_theme(themer) 

corpus$Field <- as.factor(corpus$Field)
field <- prop.table(table(corpus$Field))
field <- as.data.frame(field)
colnames(field) <- c("Field","N")
field$N <- round(field$N,4)
field$N <- field$N*100
field

#distribution of the corpus by research area
highchart() %>%
  hc_add_series(
    data = field,
    type = "pie",
    hcaes(x = Field, y = N)
  ) %>%
  hc_tooltip(pointFormat = "<b>{Field}: </b>{point.y}%") %>%
  hc_plotOptions(
    pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}%"
      ),
      showInLegend = TRUE
    ) ) %>%
  #hc_title(text = "Distribution of the corpus by type of publication") %>%
  hc_add_theme(themer)

#papers related to the uses
use <- prop.table(table(corpus$`Use related`))
use <- as.data.frame(use)
colnames(use) <- c("use","N")
levels(use$use) <- c("No","Yes")
use$N <- round(use$N,4)
use$N <- use$N*100
use

highchart() %>%
  hc_add_series(
    data = use,
    type = "pie",
    hcaes(x = use, y = N)
  ) %>%
  hc_tooltip(pointFormat = "<b>{use}: </b>{point.y}%") %>%
  hc_plotOptions(
    pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}%"
      ),
      showInLegend = TRUE
    ) ) %>%
  hc_add_theme(themer) 

#distribution by year
scholar <- corpus %>% 
  select(Code,Year) %>%
  filter(str_detect(Code, "^ML")) %>%
  count(Year) %>%
  mutate (Source = "Google Scholar")

semschol <- corpus %>% 
  select(Code,Year) %>%
  filter(str_detect(Code, "^SS")) %>%
  count(Year) %>%
  mutate (Source = "Semantic Scholar")

scopus <- corpus %>% 
  select(Code,Year) %>%
  filter(str_detect(Code, "^SC")) %>%
  count(Year)  %>%
  mutate (Source = "Scopus")

count_corpus <- rbind(scholar,semschol,scopus)

colnames(count_corpus) <- c("Year","Total","Source")

count_corpus %>% 
  hchart(
    'bar', hcaes(x = Year, y = Total, group = Source)
  )  %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}"
      ),
      stacking = "normal",
      showInLegend = TRUE
    ) ) %>%
  hc_add_theme(themer) 

#Meta Subcorpus2

subcorpus1 <- as.data.frame(subcorpus1)
subcorpus2 <- as.data.frame(subcorpus2)

table_sb2 <- subcorpus2 %>%
  select(Title, Year,Field,Type...9,Function,Task,APA)
colnames(table_sb2) <- c("Title","Year","Field","Type","Function","Task","APA")

formattable(table_sb2)

meta_sub2 <- subcorpus2 %>%
  select(Type...9,Function,Task)

colnames(meta_sub2) <- c("type","funct","task")

meta_type <- as.data.frame(meta_sub2$type)
prop.table(table(meta_type))

meta_function <- as.data.frame(meta_sub2$task)
meta <- prop.table(table(meta_function))
meta_sub2 <- as.data.frame(meta)

hc <- meta_sub2 %>% 
  hchart('column', hcaes(x = 'meta_function', y = 'Freq')) %>%
  hc_colors(c("#0073C2FF", "#EFC000FF"))

hc

#Filters (applied on the full text)

corpus <- as.data.frame(main_corpus)

corpus <- corpus %>%
  rename(Text = Abstract)

wikipedia <- function(df) {
  temp <- as.data.frame(df)
  wp <- df %>%
    filter(grepl('claim detection', Text)) %>%
    select(Code,Title)
  formattable(wp)
  count(wp)
}

wikipedia(corpus)
wikipedia(subcorpus1)
wikipedia(subcorpus2)

df <- corpus$Text
df <- as.data.frame(df)
colnames(df) <- "Text"

NLP <- df %>%
  filter(grepl('NLP', Text))

Nl <- df %>%
  filter(grepl('natural language', Text))

nlp <- rbind(NLP,Nl)
count(nlp)
nlp <- unique(nlp)
nlp <- count(nlp)

ds <- df %>%
  filter(grepl('dataset', Text))
ds <- count(ds)
pc <- ds/267*100


ml <- df %>%
  filter(grepl('machine learning', Text))

ml <- count(ml)

dl <- df %>%
  filter(grepl('deep learning', Text))
dl <- count(dl)

DL <- df %>%
  filter(grepl('neural network', Text))
DL <- count(DL)

deep <- rbind(dl,DL)
deep <- unique(deep)
deep <- count(deep)

kg <- df %>%
  filter(grepl('knowledge graph', Text))
kg <- count(kg)

bch <- df %>%
  filter(grepl('blockchain', Text))
bch <- count(bch)

techno <- c(nlp,ml,deep,kg,bch)

techno <- as.data.frame(unlist(techno))

labels <- c("Natural Language Processing","Machine Learning","Deep Learning","Knowledge Graphs","Blockchain")

tech <- cbind(labels,techno)
colnames(tech) <- c("Technology","N")

techno <- tech %>%
  mutate(Percentage = N/267*100)

techno$Percentage <- round(techno$Percentage,2)

techno <- techno %>%
  arrange(desc(Percentage))
techno

techno %>% 
  hchart(
    'column', hcaes(x = Technology, y = Percentage)
  )  %>%
  hc_yAxis(min = 0, max=100,title = list(text = "Representation in the abstract corpus")) %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y}%"
      ),
      stacking = "normal",
      showInLegend = FALSE
    ) ) %>%
  hc_xAxis(title = list(text = "Topic")) %>%
  hc_add_theme(themer)
