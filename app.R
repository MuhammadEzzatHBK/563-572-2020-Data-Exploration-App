library(shiny)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)

contamination_data <- read_csv('data/contaminations_data.csv')

source_summary <- contamination_data %>%
                    filter(source != 'unknown') %>%
                        group_by(source) %>%
                            summarize(count = n())

source_pie <- ggplot(source_summary,aes(x=2,y=count,fill = source))+
                geom_bar(stat="identity", width=1) +
                    coord_polar("y", start=0)+
                            theme_void()+theme(legend.key.size = unit(1.5, "cm"))+
                                scale_fill_brewer(palette = "Set1")+
                                    xlim(0.5, 2.5)+
                                        ggtitle('Proportions of contamination sources')

host_summary <- contamination_data %>%
                    group_by(host) %>%
                        summarize(count = n())

host_bars <- ggplot(host_summary,aes(reorder(host,count),count))+geom_col()+
                coord_flip()+theme_classic()+
                    labs(x='Host Cells',y ='Times Contaminated',title = 'Most visited host cells.',
                         subtitle = 'The contamination occurs when a virus enters a host cell in the enivrounment')

virus_host_summary <- contamination_data %>%
                        group_by(virus,host) %>%
                            summarize(count = n())%>%
                                mutate(virus_host_duo = paste(virus,host,sep =' -> '))%>%
                                    arrange(-count)%>%
                                        head(12)

virus_host_bars <- ggplot(virus_host_summary,aes(reorder(virus_host_duo ,-count),count))+geom_col()+
                        theme_classic()+theme(axis.text.x = element_text(angle = 30, hjust=1,face = 'bold'))+
                            labs(x = 'Virus -> Host cell duo',y ='Times Occurred',
                                 title = 'Top 12 Virus/Host cell combinations')

virus_data <- read_csv('data/viruses_data.csv')

product_testing_data <- read_csv('data/product_testing_data.csv')

product_testing_summary <- mutate(product_testing_data,group = paste(protocol,quality,sep = ' '))%>%
                                group_by(product,group)%>%
                                    summarize(count = n())

stacked_testing_bars <- ggplot(product_testing_summary,aes(x = reorder(product,count),y=count,fill=reorder(group,count)))+
                            geom_bar(position="stack", stat="identity")+theme_classic()+
                                coord_flip()+scale_fill_manual(values = c("#984EA3", "#377EB8", "#4DAF4A","#E41A1C"))+
                                    scale_y_continuous(breaks= scales::pretty_breaks())+
                                        theme(legend.position = 'bottom',
                                              legend.title = element_blank(),
                                              legend.key.size = unit(1.5, "cm"))+
                                                    labs(title = 'Virus testing of contaminated processes',
                                                         subtitle = 'A replica of Figure 1 (563-572-2020)',
                                                         x = 'Product', y = 'Case Count')+
                                                            theme(plot.title = element_text(size = 20, face = "bold"))

testing_metods_data <- read_csv('data/testing_methods.csv')

pivoted_testing_methods <- pivot_longer(select(testing_metods_data,-X1),!method,names_to = 'Purpose',values_to = 'value')

dodge_bars <- ggplot(data = pivoted_testing_methods, aes(x = reorder(method,-value), y = value, fill = Purpose)) +
                geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)+theme_classic()+
                    scale_fill_brewer(palette = 'Set1')+theme(legend.position = 'right',legend.key.size = unit(1.5, "cm"))+
                        theme(axis.text.x = element_text(angle = 30, hjust=1,size = 10,face = 'bold'))+
                            labs(x = 'Testing Methods', y ='Case Count',title = 'Testing Methods Used',
                                 subtitle = 'Multiple tests may have been used in each event')+
                                    theme(plot.title = element_text(size = 20, face = "bold"))+
                                        theme(legend.text = element_text(size = 12.5))

about_this_article <- read_lines('app_text/about_this_article.txt')
about_the_data <- read_lines('app_text/about_the_data.txt')

ui <- fluidPage(

    titlePanel("(563-572-2020) Data Exploration App"),

    sidebarLayout(
        sidebarPanel(width = 3,
            HTML("<b> Viral contamination in biologic manufacture and implications for emerging therapies - (563-572-2020) Paul W. Barone1 et al </b>"),
            HTML("<BR><BR>"),
            HTML("<u>About this article :</u><BR>"),
            HTML(about_this_article),
            HTML('<BR><BR>'),
            HTML("<u>About the data :</u><BR>"),
            HTML(about_the_data),
            HTML('<BR><BR>'),
            uiOutput("github")
            
            
        ),

        mainPanel(width = 9,
            tabsetPanel(
                tabPanel('Contamination Incidents',
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                              plotOutput("host_bars"), plotOutput("source_pie"))),
                         fluidRow(plotOutput('virus_host_bars'))),
                tabPanel('Viruses',
                         column(width = 6,
                                fluidRow(selectInput('virus_input','Select a Virus',virus_data$name,'BLV'),
                                         tableOutput('virus_data')),
                                fluidRow(plotOutput('virus_pie'))),
                         column(width = 6,
                                HTML('<BR>'),
                                HTML('<b>Found in</b>'),
                                HTML('<BR>'),
                                DT::dataTableOutput('contamination_for_virus'),
                            )),
                tabPanel('BioProducts Testing',
                         HTML('<BR>'),
                         plotOutput('stacked_testing_bars',width = '95%')),
                tabPanel('Testing Methods',
                         HTML('<BR>'),
                         plotOutput('dodge_bars',width = '95%',height = '600px'))
            )
        )
    )
)

server <- function(input, output) {
    output$source_pie <- renderPlot(source_pie)
    output$host_bars <- renderPlot(host_bars)
    output$virus_host_bars <- renderPlot(virus_host_bars)
    
    virusDT <- reactive({filter(virus_data,name == input$virus_input)%>%select(-X1)%>%
                                    `colnames<-`(c("Virus Name", "Pathogeny", "Short Description"))})
    output$virus_data <- renderTable(virusDT())
    
    virus_summary <- reactive({mutate(contamination_data,ParticipatingVirus = ifelse(contamination_data$virus==input$virus_input,
                                    input$virus_input,'Others'))%>%group_by(ParticipatingVirus)%>%
                                        summarize(count = n())%>%arrange(count)})
    virus_pie <- reactive({ggplot(virus_summary(),aes(x = 2,y=count,fill = reorder(ParticipatingVirus,count)))+
                            geom_bar(stat="identity", width=1)+coord_polar("y", start=0)+
                                theme_void()+scale_fill_brewer(palette = "Set1")+
                                    ggtitle('Participation in Contaminations')+
                                        theme(plot.title = element_text(size = 20, face = "bold"))+
                                            labs(fill='Participating Virus') })
    output$virus_pie <- renderPlot(virus_pie())
    
    contamination_for_virus <- reactive({filter(contamination_data,virus == input$virus_input)%>%
                                    select(-X1)%>%`colnames<-`(c("Virus Name", "Host Cell", "Source of contamination"))})
    output$contamination_for_virus <- DT::renderDataTable(contamination_for_virus())
    
    output$stacked_testing_bars <- renderPlot(stacked_testing_bars)
    output$dodge_bars <- renderPlot(dodge_bars)
    
    code <- a("(563-572-2020) Data Exploration App", href="https://github.com/MuhammadEzzatHBK/563-572-2020-Data-Exploration-App")
    output$github <- renderUI({tagList("Source Code:", code)})

}

shinyApp(ui = ui, server = server)
