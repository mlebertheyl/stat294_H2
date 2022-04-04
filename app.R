
library(readxl)
library(tidyverse)
library(shiny)
library(plotly)
library(bibliometrix)
library(DT)
library(shinydashboard)
library(quanteda)
library(topicmodels)
library(tools)
library(quanteda.textplots)

############
### data ###
############

data <- read_excel("data/data.xlsx")

main_information <- data.frame(Description=c("Timespan", "Documents", "Ecosystems", "Threats"),
                               Counts=c("1990-2021","760", "6","6"))

df1<-data %>% 
  filter(!is.na(Ecosystem))%>%
  group_by(Ecosystem) %>%
  count(PY) %>% 
  mutate(cumulative = cumsum(n))

df2<-data %>% 
  filter(!is.na(Threat))%>%
  group_by(Threat) %>%
  count(PY) %>% 
  mutate(cumulative = cumsum(n))

conteo<-data%>%
  count(PY)%>%
  mutate(cumulative=cumsum(n)) %>%
  mutate(percentage=100*(n / sum(n)))%>%
  mutate(percentage_cum=100*(cumulative / sum(n))) %>% 
  mutate(across(where(is.numeric), round, 2))


## Data Keywords
data_1999<-data %>%
  filter(!is.na(AB))%>%
  filter(PY< 2000)

data_2009<-data%>%
  filter(!is.na(AB))%>%
  filter(PY>1999 & PY< 2010)

data_2021<-data %>% 
  filter(!is.na(AB))%>%
  filter(PY> 2009)

corpus_abstract_1999 <- corpus(data_1999$AB, docnames = data_1999$doc_id, docvars = data.frame(year = data_1999$PY))
corpus_abstract_2009 <- corpus(data_2009$AB, docnames = data_2009$doc_id, docvars = data.frame(year = data_2009$PY))
corpus_abstract_2021 <- corpus(data_2021$AB, docnames = data_2021$doc_id, docvars = data.frame(year = data_2021$PY))

list_stopwords <- readLines("list_stopwords.csv", encoding = "UTF-8")
other_stopwords<- c("elsevier","b.v","rights"," reserved","-1","-2","yr")
original_word<-c("areas","soils","rivers","activities","concentration","level","cu","sources","risks","effects","sample","pb","higher","value","zn","forest","mma","dma")
new_word<-c("area","soil","river","activity","concentrations","levels","copper","source","risk","effect","samples","lead","high","values","zinc","forests","monomethylarsonic acid","dimethylarsinic acid")

toks_1999 <- corpus_abstract_1999 %>%
  tokens(remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>% 
  tokens_select(c(-1,list_stopwords,other_stopwords), selection = "remove", padding = FALSE) %>% 
  tokens_replace(pattern=original_word, 
                 replacement=new_word)

toks_2009 <- corpus_abstract_2009 %>%
  tokens(remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>% 
  tokens_select(c(-1,list_stopwords,other_stopwords), selection = "remove", padding = FALSE) %>% 
  tokens_replace(pattern=original_word, 
                 replacement=new_word)

toks_2021 <- corpus_abstract_2021 %>%
  tokens(remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>% 
  tokens_select(c(-1,list_stopwords,other_stopwords), selection = "remove", padding = FALSE) %>% 
  tokens_replace(pattern=original_word, 
                 replacement=new_word)


wfreq_1999 <- toks_1999 %>%
  unlist()%>% 
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(20) %>%
  rename("unigram 1990-1999" = 1,frequency = 2)

wfreq_2009 <- toks_2009 %>%
  unlist()%>% 
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(20) %>%
  rename("unigram 2000-2009" = 1,frequency = 2)

wfreq_2021 <- toks_2021 %>%
  unlist()%>% 
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(20) %>%
  rename("unigram 2010-2021" = 1,frequency = 2)



###########
## app.R ##
###########

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Literature Review"),
  
  dashboardSidebar(sidebarMenu(
    
    menuItem("Database", tabName = "database", icon = icon("database"), badgeLabel = "readme", badgeColor = "green"),
    menuItem("Main Information", tabName = "information", icon = icon("database")),
    menuItem("Ecosystems", tabName = "ecosystem", icon = icon("dashboard")),
    menuItem("Threats", tabName = "threat", icon = icon("dashboard")),
    menuItem("Keywords", tabName = "keywords", icon = icon("dashboard")))),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "database",
              h2("Anthropogenic threats on Chilean Ecosystems"),
              fluidRow(
                infoBox(width = 12,"Literature Review", "Anthropogenic threats to Chilean ecosystems: a systematic literature review", icon = icon("list"), color = "red", fill = F),
                infoBox(width = 12,"Web Of Science", "1990-2021", icon = icon("list"), color = "red", fill = TRUE),
                infoBox(width = 12,"boolean search", "TS= ((human OR anthropogenic OR mining OR industrial* OR agricult* OR domestic)  AND  (pollut* OR contaminat OR threat* OR disturbanc* )  AND (chile*))", 
                        icon = icon("list"), color = "orange", fill = TRUE),
                infoBox(width = 12,"Research Articles", "760", icon = icon("list"), color = "yellow", fill = TRUE),
                infoBox(width = 12,"Manual classification", "Ecosystems and Threats", icon = icon("list"), color = "teal", fill = TRUE),
                infoBox(width = 12,"Temporal Trends", "Number of publications", icon = icon("list"), color = "aqua", fill = TRUE),
                infoBox(width = 12,"Text Mining", "Most frequent words", icon = icon("list"), color = "light-blue", fill = TRUE))),
      
      tabItem(tabName = "information",
              h2("Main information from publications included in the database."),
              fluidRow(
                box(width = 4, DTOutput(outputId = "table"),status = "warning", solidHeader = TRUE,
                    collapsible = TRUE)),
              h2("Annual number of publications from 1990 to 2021."),
              fluidRow(  
                box(plotlyOutput(outputId = "conteo"),status = "info", solidHeader = TRUE,
                    collapsible = TRUE))),
      
      tabItem(tabName = "ecosystem",
              h2("Cumulative number of publications per Ecosystem"),
              fluidRow(
                box(width = 4, radioButtons("Ecosystem", "Ecosystem:",
                                            c("Terrestrial", "Urban", "Freshwater","Marine","Glaciers", "Multiple")),status = "info", solidHeader = TRUE,
                    collapsible = TRUE),
                box(plotlyOutput(outputId = "plot1"),status = "info", solidHeader = TRUE,
                    collapsible = TRUE))),
      
      tabItem(tabName = "threat",
              h2("Cumulative number of publications per Antrhopogenic Threat"),
              fluidRow(
                box(width = 4, radioButtons("Threat", "Threat",
                                            c("Pollution", "Habitat change", "Overexploitation","Invasive alien species","Climate change", "Multiple")),
                    status = "info", solidHeader = TRUE,
                    collapsible = TRUE),
                box(plotlyOutput(outputId = "plot2"),status = "info", solidHeader = TRUE,
                    collapsible = TRUE))),
      
      tabItem(tabName = "keywords",
              h2("Top 20 most relevant words based on publishes abstracts"),
              fluidRow(
                box(width=8,title = "1990-2000", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    plotlyOutput(outputId = "plot3")),
                box(width=8,title = "2001-2009", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    plotlyOutput(outputId = "plot4")),
                box(width=8,title = "2010-2021", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    plotlyOutput(outputId = "plot5"))
              )
      )
    )))



server <- function(input, output) {
  
  output$conteo<-renderPlotly({
    ggplotly({
      plot_conteo<-ggplot(conteo, aes(x = PY, y = n))+
        geom_line(color="#00BFFF", size=0.5,alpha =0.5)+
        geom_point(aes(x = PY, y = n), color="#00BFFF",size =2)+
        scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015, 2021),limits=c(1989, 2021))+
        scale_y_continuous(breaks=c(0,30,60,90,120),limits=c(0,120))+
        labs(x="", y="Number of publications")+ xlab("Year")+
        theme(axis.text =element_text(size=12,color="#191919"),
              axis.title=element_text(size=12,color="#191919"),
              panel.background = element_blank(),
              panel.border = element_rect(color="#191919", size=1, fill=NA),
              legend.position = "none")
      plot_conteo
      
    })
  })
  
  filtered_data1 <- reactive({
    subset(df1,
           Ecosystem %in% input$Ecosystem)})
  
  output$plot1 <- renderPlotly({
    ggplotly({
      p1 <- ggplot(filtered_data1(), aes(x = PY, y = cumulative))+
        geom_line(color="#00BFFF", size=0.5,alpha =0.5)+
        geom_point(aes(x = PY, y = cumulative), color="#00BFFF",size =2)+
        scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015, 2021),limits=c(1989, 2021))+
        scale_y_continuous(breaks=c(0,30,60,90,120,150,180,210,240),limits=c(0,240))+
        labs(title=NULL,x="", y="Cumulative number of publications")+ xlab("Year")+
        ggtitle(input$Ecosystem) +
        theme(axis.text =element_text(size=12,color="#191919"),
              axis.title=element_text(size=12,color="#191919"),
              panel.background = element_blank(),
              panel.border = element_rect(color="#191919", size=1, fill=NA),
              legend.position = "none")
      
      p1
      
    })
  })
  
  filtered_data2 <- reactive({
    subset(df2,
           Threat %in% input$Threat)})
  
  output$plot2 <- renderPlotly({
    ggplotly({
      p2 <- ggplot(filtered_data2(), aes(x = PY, y = cumulative))+
        geom_line(color="#00BFFF", size=0.5,alpha =0.5)+
        geom_point(aes(x = PY, y = cumulative), color="#00BFFF",size =2)+
        scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015, 2021),limits=c(1989, 2021))+
        scale_y_continuous(breaks=c(0,40,80,120,160,200,240,280,320,360,400),limits=c(0,400))+
        labs(title=NULL,x="", y="Cumulative number of publications")+ xlab("Year")+
        ggtitle(input$Threat) +
        theme(axis.text =element_text(size=12,color="#191919"),
              axis.title=element_text(size=12,color="#191919"),
              panel.background = element_blank(),
              panel.border = element_rect(color="#191919", size=1, fill=NA),
              legend.position = "none")
      
      p2    
    })
  })
  output$table <- renderDT(main_information,
                           options = list(dom = 't'))
  
  output$plot3 <- renderPlotly({
    ggplotly({
      hist_1999<-wfreq_1999 %>%
        rename(unigram="unigram 1990-1999") %>% 
        ggplot(aes(x = reorder(unigram, -frequency, mean), y = frequency)) +
        geom_bar(stat = "identity") +
        labs(x=NULL, y="Counts")+
        theme(axis.text =element_text(size=10,color="#191919"),
              axis.text.x = element_text(angle = 45,hjust = 1),
              axis.title=element_text(size=12,color="#191919"),
              plot.title = element_text(color="#191919", hjust=0),
              panel.background = element_blank()) 
    })
  })
  
  output$plot4 <- renderPlotly({
    ggplotly({
      hist_2009<-wfreq_2009 %>%
        rename(unigram="unigram 2000-2009") %>% 
        ggplot(aes(x = reorder(unigram, -frequency, mean), y = frequency)) +
        geom_bar(stat = "identity") +
        labs(x=NULL, y="Counts") +
        theme(axis.text =element_text(size=10,color="#191919"),
              axis.text.x = element_text(angle = 45,hjust = 1),
              axis.title=element_text(size=12,color="#191919"),
              plot.title = element_text(color="#191919", hjust=0),
              panel.background = element_blank())
    })
  })
  
  output$plot5 <- renderPlotly({
    ggplotly({
      hist_2021<-wfreq_2021 %>%
        rename(unigram="unigram 2010-2021") %>% 
        ggplot(aes(x = reorder(unigram, -frequency, mean), y = frequency)) +
        geom_bar(stat = "identity") +
        labs(x=NULL, y="Counts")+
        theme(axis.text =element_text(size=10,color="#191919"),
              axis.text.x = element_text(angle = 45,hjust = 1),
              axis.title=element_text(size=12,color="#191919"),
              plot.title = element_text(color="#191919", hjust=0),
              panel.background = element_blank()) 
    })
  })
  
}

shinyApp(ui = ui, server = server)

