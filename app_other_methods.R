##This shinyapp uses electoral data from legislative elections in Ecuador (2017)
##to demonstrate the difference between the D'Hondt and Webster methods of allocation.
##

library(shiny)
library(ggplot2)
library(formattable)
library(htmlwidgets)
library(expss)
library(gridExtra)
source("apportionment.R")

ui <- fluidPage(
    
    titlePanel("Legislative Seats Allocation- Ecuador 2017"),
    tabsetPanel(tabsetPanel(type="tab",
                            tabPanel("District Results", fluid=T, sidebarLayout( 
                                sidebarPanel(
                                    selectizeInput('nivel1', 'District', sort(unique(newdata_prov$distrito_nombre)),
                                                   options = list(
                                                       placeholder = 'Please select an option below',
                                                       onInitialize = I('function() { this.setValue(""); }')
                                                   )),
                                    helpText("Ecuador's legislative assembly consists of 137 members;
                                        Starting in 2013, 122 of the seats would be selected using the
                                        D'Hondt (not Webster) method of allocation. Select a district
                                        above to see the results of the 2017 election and how the D'Hondt
                                        method differs from the Webster method. ")
                                    ),
                                mainPanel(
                                     formattableOutput("bar"),
                                     div(
                                         style="position:relative",
                                         plotOutput("gg",
                                                    hover=hoverOpts("plot_hover", delay=100, delayType = "debounce")),
                                         uiOutput("hover_info")
                                     ),
                                     width=7)
                            )), 
                            tabPanel("National Level Results", fluid=T,
                                     sidebarLayout(sidebarPanel(helpText("Ecuador's legislative 
                                                    assembly consists of 137 members; 122 of which are
                                                    elected from 34 sub-national districts using the
                                                    D'Hondt Method. This plot shows the cumulative
                                                    (national) effect of using the D'Hondt method in 2017.
                                                    The governing party (Alianza Pais) had a net gain
                                                    of 8 seats and another net gain of 5 seats from
                                                    its alliances for an impressive total net gain of 13 seats
                                                    (~10% of the total seats in the legislature).
                                                    Meanwhile, the biggest loser of the D'Hondt method
                                                    is the Partido Social Cristiano who had a net loss
                                                    of 6 seats. Most other parties were unaffected on net
                                                    but the governing party gained enough to have, 
                                                    by itself, not just a plurality but an absolute
                                                    majority of seats.")),
                                                   mainPanel(div(
                                                       style="position:relative",
                                                       plotOutput("nat", height = "600px",
                                                                  hover=hoverOpts("plot_hover2",
                                                                                  delay=100, 
                                                                                  delayType = "debounce")),
                                                       uiOutput("hover_info2")
                                                   )))
                                     )
                            )),
    
    tags$style(type="text/css", 
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }" #this code suppresses error messages
    )  
)


server <- function(input, output) {
    #data changes based on user input
    reactive_data = reactive({
        selected_district = input$nivel1
        return(newdata_prov[(newdata_prov$distrito_nombre==selected_district),])
        
    })
    #table of results based on user input
    output$bar <- renderFormattable({
        partyvotes<- reactive_data() %>%
            group_by(op_codigo, add=T) %>% #group by political party
            summarise(v1=sum(votos_candidato), distrito=first(distrito),
                      op_nombre=first(op_nombre), seats=max(candidato_orden_lista))
        partyvotes$op_nombre<-as.character(partyvotes$op_nombre)
        #calculate dhondt seats
        partyvotes$s<-0
        partyvotes$s<-ifelse(partyvotes$v1==max(partyvotes$v1), partyvotes$s + 1, partyvotes$s)
        partyvotes$v<-partyvotes$v1
        
        seats <- max(partyvotes$seats) 
        for(i in 2:seats){
            partyvotes$v<-partyvotes$v1 / (partyvotes$s + 1)
            partyvotes$s<-ifelse(partyvotes$v==max(partyvotes$v), partyvotes$s + 1, partyvotes$s)
        }
        
        partyvotes$percent_total<-round((partyvotes$v1/sum(partyvotes$v1))*100,1)
        partyvotes$percentseats_total<-round((partyvotes$s/sum(partyvotes$s))*100,1)
        partyvotes<-partyvotes[,c(4,2,6,8,9)]
        partyvotes<-arrange(partyvotes, desc(v1))
        colnames(partyvotes)<-c("Party","Votes","Seats.Dhondt", "Percent.Votes", "Percent.Seats.Dhondt")
        
        #design table
        f1<-formatter("span", style = ~ ifelse((Seats.Dhondt >0), 
                                               style(color = "green", font.weight = "bold"), NA))
        formattable(partyvotes, align=c("l", "c", "r","r"),
                    list(
                        Votes=F,
                        area(col = c(Percent.Votes, Percent.Seats.Dhondt)) ~ proportion_bar("lightgreen"),
                        Party = f1
                        
                    ))
    })  
    #make scatterplot    
    output$gg <- renderPlot({
        #prep df based on user input
        partyvotes2<- reactive_data() %>%
            group_by(op_codigo, add=T) %>% #group by political party
            summarise(v1=sum(votos_candidato), distrito=first(distrito),
                      op_nombre=first(op_nombre), seats=max(candidato_orden_lista))
        partyvotes2$op_nombre<-as.character(partyvotes2$op_nombre)
        
        #dhondt method
        partyvotes2$s_dhondt<-0
        partyvotes2$s_dhondt<-ifelse(partyvotes2$v1==max(partyvotes2$v1), partyvotes2$s_dhondt + 1,
                                     partyvotes2$s_dhondt)
        partyvotes2$v_dhondt<-partyvotes2$v1
        
        seats <- max(partyvotes2$seats) 
        for(i in 2:seats){
            partyvotes2$v_dhondt<-partyvotes2$v1 / (partyvotes2$s_dhondt + 1)
            partyvotes2$s_dhondt<-ifelse(partyvotes2$v_dhondt==max(partyvotes2$v_dhondt),
                                         partyvotes2$s_dhondt + 1, partyvotes2$s_dhondt)
        }
        
        partyvotes2$percent_total<-round((partyvotes2$v1/sum(partyvotes2$v1))*100,1)
        partyvotes2$percentseats_total_dhondt<-round((partyvotes2$s_dhondt/sum(partyvotes2$s_dhondt))*100,1)
       
        #webster method
        partyvotes2$s_webster<-0
        partyvotes2$s_webster<-ifelse(partyvotes2$v1==max(partyvotes2$v1), partyvotes2$s_webster + 1,
                                      partyvotes2$s_webster)
        partyvotes2$v_webster<-partyvotes2$v1
        
        seats <- max(partyvotes2$seats) 
        for(i in 2:seats){
            partyvotes2$v_webster<-partyvotes2$v1 / (partyvotes2$s_webster*2 + 1)
            partyvotes2$s_webster<-ifelse(partyvotes2$v_webster==max(partyvotes2$v_webster),
                                          partyvotes2$s_webster + 1, partyvotes2$s_webster)
        }
        
        partyvotes2$percent_total<-round((partyvotes2$v1/sum(partyvotes2$v1))*100,1)
        partyvotes2$percentseats_total_webster<-round((partyvotes2$s_webster/sum(partyvotes2$s_webster))*100,1)
        
        
        #difference between dhondt and webster
        partyvotes2$diff<-partyvotes2$s_dhondt-partyvotes2$s_webster
        
        #plot
        theme_set(theme_bw(base_size = 20))
        ggplot(partyvotes2, aes(x=percent_total)) + 
            geom_point(aes(y=diff),size=6, col="lightgreen") +
            ylim(c(-max(partyvotes2$diff)-1, max(partyvotes2$diff)+1)) + 
            labs(subtitle="Positive values signify more seats with D'Hondt", 
                 y="Seats difference", 
                 x="% of Total Votes", 
                 title="Seats difference between D'Hondt and Webster by vote proportion", 
                 caption = "Source: CNE")
    })
        #hover tool tip for district plot 
        output$hover_info <- renderUI({
            partyvotes3<- reactive_data() %>%
                group_by(op_codigo, add=T) %>% #group by political party
                summarise(v1=sum(votos_candidato), distrito=first(distrito),
                          op_nombre=first(op_nombre), seats=max(candidato_orden_lista))
            partyvotes3$op_nombre<-as.character(partyvotes3$op_nombre)
            #calculate dhondt
            partyvotes3$s_dhondt<-0
            partyvotes3$s_dhondt<-ifelse(partyvotes3$v1==max(partyvotes3$v1), 
                                         partyvotes3$s_dhondt + 1, partyvotes3$s_dhondt)
            partyvotes3$v_dhondt<-partyvotes3$v1
            
            seats <- max(partyvotes3$seats) 
            for(i in 2:seats){
                partyvotes3$v_dhondt<-partyvotes3$v1 / (partyvotes3$s_dhondt + 1)
                partyvotes3$s_dhondt<-ifelse(partyvotes3$v_dhondt==max(partyvotes3$v_dhondt),
                                             partyvotes3$s_dhondt + 1, partyvotes3$s_dhondt)
            }
            
            partyvotes3$percent_total<-round((partyvotes3$v1/sum(partyvotes3$v1))*100,1)
            partyvotes3$percentseats_total_dhondt<-round((partyvotes3$s_dhondt/sum(partyvotes3$s_dhondt))*100,1)
            #calculate webster
            partyvotes3$s_webster<-0
            partyvotes3$s_webster<-ifelse(partyvotes3$v1==max(partyvotes3$v1),
                                          partyvotes3$s_webster + 1, partyvotes3$s_webster)
            partyvotes3$v_webster<-partyvotes3$v1
            
            seats <- max(partyvotes3$seats) 
            for(i in 2:seats){
                partyvotes3$v_webster<-partyvotes3$v1 / (partyvotes3$s_webster*2 + 1)
                partyvotes3$s_webster<-ifelse(partyvotes3$v_webster==max(partyvotes3$v_webster),
                                              partyvotes3$s_webster + 1, partyvotes3$s_webster)
            }
            
            partyvotes3$percent_total<-round((partyvotes3$v1/sum(partyvotes3$v1))*100,1)
            partyvotes3$percentseats_total_webster<-round((partyvotes3$s_webster/sum(partyvotes3$s_webster))*100,1)
            #difference between dhondt and webster
            partyvotes3$diff<-partyvotes3$s_dhondt-partyvotes3$s_webster
            
            #hover tool tip
            hover <- input$plot_hover
            point <- nearPoints(partyvotes3, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
                style = style,
                p(HTML(paste0("<b> Party: </b>", point$op_nombre, "<br/>",
                              "<b> Percent of votes: </b>", point$percent_total, "<br/>")))
            )
        })
        #static scatterplot for national level data
        output$nat<-renderPlot({
            ggplot(newdata_prov3, aes(x=percent_total)) + 
                geom_point(aes(y=diff),size=6, col="lightgreen") +
                ylim(c(-max(newdata_prov3$diff)-1, max(newdata_prov3$diff)+1)) +
                xlim(0,50)+
                scale_y_continuous(breaks = round(seq(min(newdata_prov3$diff), max(newdata_prov3$diff), by = 2),1))+
                labs(subtitle="Positive values signify more seats with D'Hondt", 
                     y="Seats difference", 
                     x="% of Total Votes", 
                     title="Seats difference between D'Hondt and Webster by vote proportion", 
                     caption = "Source: CNE")
        })
        #hover tooltip for national level scatterplot
        output$hover_info2 <- renderUI({
            
            #hover tool tip
            hover2 <- input$plot_hover2
            point2 <- nearPoints(newdata_prov3, hover2, threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point2) == 0) return(NULL)
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct2 <- (hover2$x - hover2$domain$left) / (hover2$domain$right - hover2$domain$left)
            top_pct2 <- (hover2$domain$top - hover2$y) / (hover2$domain$top - hover2$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px2 <- hover2$range$left + left_pct2 * (hover2$range$right - hover2$range$left)
            top_px2 <- hover2$range$top + top_pct2 * (hover2$range$bottom - hover2$range$top)
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure tooltip will be on top
            style2 <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                            "left:", left_px2 + 2, "px; top:", top_px2 + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
                style = style2,
                p(HTML(paste0("<b> Party: </b>", point2$op_nombre, "<br/>",
                              "<b> Percent of votes: </b>", point2$percent_total, "<br/>",
                              "<b> Total seats (D'Hondt): </b>", point2$dhondt, "<br/>",
                              "<b> Total seats (Webster): </b>", point2$webster, "<br/>")))
            )
        })
        
    }

# Run the application 
shinyApp(ui = ui, server = server)

