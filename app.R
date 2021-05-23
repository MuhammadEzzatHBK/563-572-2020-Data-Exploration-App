library(shiny)
library(tidyverse)
library(ggthemes)

contamination_data <- read_csv('data/contaminations_data.csv')

source_summary <- contamination_data %>%
                    filter(source != 'unknown') %>%
                        group_by(source) %>%
                            summarize(count = n())

source_pie <- ggplot(source_summary,aes(x="",y=count,fill = source))+
                geom_bar(stat="identity", width=1) +
                    coord_polar("y", start=0)+
                        theme_void()+theme(legend.key.size = unit(1, "cm"))+
                                scale_fill_brewer(palette = "Set1")

host_summary <- contamination_data %>%
                    group_by(host) %>%
                        summarize(count = n())

host_bars <- ggplot(host_summary,aes(reorder(host,count),count))+geom_col()+
                coord_flip()+theme_classic()

virus_host_summary <- contamination_data %>%
                        group_by(virus,host) %>%
                            summarize(count = n())%>%
                                mutate(virus_host_duo = paste(virus,host,sep =' -> '))%>%
                                    arrange(-count)%>%
                                        head(12)

virus_host_bars <- ggplot(virus_host_summary,aes(reorder(virus_host_duo ,-count),count))+geom_col()+
                        theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust=1))

ui <- fluidPage(

    titlePanel("(563-572-2020) Data Exploration App"),

    sidebarLayout(
        sidebarPanel(width = 3

        ),

        mainPanel(width = 9,
            tabsetPanel(
                tabPanel('Contamination Incidents',
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                              plotOutput("host_bars"), plotOutput("source_pie"))),
                         fluidRow(plotOutput('virus_host_bars'))),
                tabPanel('Viruses'),
                tabPanel('BioProducts Testing'),
                tabPanel('Testing Methods')
            )
        )
    )
)

server <- function(input, output) {
    output$source_pie <- renderPlot(source_pie)
    output$host_bars <- renderPlot(host_bars)
    output$virus_host_bars <- renderPlot(virus_host_bars)

}

shinyApp(ui = ui, server = server)
