###########################################################
# Name of script: Dashboard - Costbook Overview.R
# Written by: Nicole Jarvie - Costbook team 
#             within Resources 
#
# Type of script: Setup environment
# Written/run on: R Studio Server 
# Version of R: 3.6.1
#
# Description: Dashboard Cost Overview 
###########################################################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "CostBook"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Total Costs Overview",tabName ="totals",icon = icon("gbp", lib = "glyphicon")),
      menuItem("Hospital Costs",tabName = "hospital",icon = icon("bed", lib = "glyphicon")),
      menuItem("Community Costs",tabName = "community",icon = icon("home", lib = "glyphicon")),
      menuItem("Family Costs",tabName = "family",icon = icon("user", lib = "glyphicon")),
      menuItem("Resource Costs",tabName = "resource",icon = icon("transfer", lib = "glyphicon")),
      menuItem("Mental Health Costs",tabName = "mentalhealth",icon = icon("tasks", lib = "glyphicon"),menuSubItem("Overview",
                                                                                                                  tabName = "mentalhealth",icon = icon("signal",lib = "glyphicon")), menuSubItem("CAMHs",
                                                                                                                                                                                               tabName = "CAMHs",icon = icon("user",lib = "glyphicon")))
    )
  ),
  dashboardBody( 
    
  setBackgroundColor(color = "#FFFFFF",shinydashboard = TRUE),
    
  tabItems(
      
  tabItem(tabName = "totals",
          
    fluidPage(
    tags$h2("Scottish Health Service Costs - Dashboard"),
    setBackgroundColor("ghostwhite"),
    
    helpText("The information is primarily derived from Scottish Financial Returns (SFRs) compiled by NHS Boards. The dashboard provides the ability to carry out board and trend analysis."),
    helpText("The below data is for NHS Scotland from the latest finanical quarter."),
    helpText("To select another board/time period, use the boxes below:"), 
  
  
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(5,
           selectInput("board",
                       "Select Board:",
                       c(unique(exsumc$Name)),selected = "NHS Scotland")
      ),
      column(5,
           selectInput("finyear",
                       "Select Financial Year:",
                       c(unique(subset(exsumc$Year, exsumc$Year >= min(exsumc$Year) + 4))),selected = 2018)

      ),
    
    valueBoxOutput("TotalCost"),
    valueBoxOutput("CostChange"),
    valueBoxOutput("HospitalCost"),
    valueBoxOutput("CommunityCost"),
    valueBoxOutput("FamilyCost"),
    valueBoxOutput("ResourceCost"),
    ),
  
    helpText("The below section provides an overview of high level costs, costs can be looked at by board and year."),
  
    
    tabPanel(
    "2 columns",
    fluidRow(
      box(title = "Percentage Breakdown of Total Costs - Single Year Analysis",imageOutput("pieplot")),
      box(title = "Percentage Breakdown of Total Costs - Five Year Analysis",imageOutput("barplot")),
      column(width =12, 
             dataTableOutput("costsummary"))
   
    )
  )
  )
    
  ),
  
  tabItem(tabName = "mentalhealth",
    
      tags$h2("Mental Health Services - Cost Overview"),
      helpText("Mental health expenditure as recorded in the Costs Book (Report R340)."),
      helpText("Data covers expenditure identified by NHS Boards as relating to NHS Mental Health services delivered in the hospital or the community."),
      helpText("To select another board/time period, use the boxes below:"),
      
      fluidRow(
      column(5,
             selectInput("mhboard",
                         "Select Board:",
                         c(unique(CompleteYear$Board)),selected = "Scotland")),
      column(5,
             selectInput("mhyear",
                         "Select Financial Year:",
                         c(unique(subset(CompleteYear$Year, CompleteYear$Year >= min(CompleteYear$Year) + 4)),selected = 2018))
      )
      ),

           
      valueBoxOutput("MHCost"),
      valueBoxOutput("MHCostChange"),
      valueBoxOutput("MHPercent"),
      fluidRow(
        column(width =12,
        box(title = "Mental Health - Board Analysis - Total Spend ",imageOutput("MHGraph")),
        box(title = "Mental Health - Board Analysis - Percentage of Total Spend",imageOutput("MHPercentGraph"))),
        column(width =12, 
               dataTableOutput("MHTable"))

      )
  ),
  
  tabItem(tabName ="CAMHs",
  tags$h2("Child and Adolescent Mental Health Services - Cost Overview"),
  helpText("The figures shown cover expenditure that Boards have identified as relating to Mental Health services delivered in hospitals or in the community and that are aimed specifically at children and adolescents."),
  helpText("Note that, within the Cost Book, some Boards many not distinguish all expenditure relating specifically to this age group. Note also that the figures shown are in cash terms and so are not adjusted for inflation."),
  
  fluidRow(
    column(5,
           selectInput("camhboard",
                       "Select Board:",
                       c(unique(CompleteYear$Board)),selected = "Scotland")),
    column(5,
           selectInput("camhyear",
                       "Select Financial Year:",
                       c(unique(subset(CompleteYear$Year, CompleteYear$Year >= min(CompleteYear$Year) + 4)),selected = '2018'))
    )
  ),
  
  valueBoxOutput("CAMHCost"),
  valueBoxOutput("CAMHCostChange"),
  valueBoxOutput("CAMHPercent"),
  
  fluidRow(
    column(width =12,
           box(title = "CAMHs - Board Analysis - Total Spend ",imageOutput("CAMHGraph")),
           box(title = "CAMHs - Board Analysis - Percentage of Mental Health Total Spend",imageOutput("CAMHPercentGraph"))),
    column(width =12, 
           dataTableOutput("CAMHTable"))
  )
  ),
  tabItem(tabName = "hospital"),
  tabItem(tabName = "community"),
  tabItem(tabName = "family"),
  tabItem(tabName = "resource")
  
 )
)
)

server <- function(input, output) {
  
  # Filter data based on selections
  
  output$costsummary <- renderDataTable({
    data <- exsumc
    minyear <- as.numeric(input$finyear) - 4
    maxyear <- as.numeric(input$finyear)
    data <- data[data$Name %in% input$board & (data$Year >= minyear & data$Year <= maxyear),]
    data <- data[,c(3,6,4)]
    data  <- dcast(data, Defn ~ Year)
    data <- data[c(5,3,2,1,4),]
    data$PercentageChange <- paste(round(((data[,ncol(data)] - data[,2])/ data[,2]) *100,2),"%","")
    data$PercentageChange <- improvement_formatter(data$PercentageChange)
    data[,2] <- paste("£",format(data[,2],  big.mark=","))
    data[,3] <- paste("£",format(data[,3],  big.mark=","))
    data[,4] <- paste("£",format(data[,4],  big.mark=","))
    data[,5] <- paste("£",format(data[,5],  big.mark=","))
    data[,6] <- paste("£",format(data[,6],  big.mark=","))
    colnames(data)[1] <- "Cost Type"
    data <- as.datatable(formattable(data),rownames = FALSE) 
  })
  
  output$pieplot <- renderPlot({
    data <- exsumc[exsumc$Year %in% input$finyear & exsumc$Name %in% input$board & exsumc$repline %in% c("020","030","040","045"),]
    pie <- ggplot(data, aes(x=2, y=Percentage, fill=Defn))+
      geom_bar(width = 1, stat = "identity",color = "white") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") + blank_theme + 
      ggtitle(paste(input$board,"-",input$finyear ))+ theme(axis.text.x=element_blank()) + geom_text(aes(x=1.8,label=Percentage) ,size = 4, family = "Helvetica Neue", position = position_stack(vjust = 0.5)) +
      xlim(0.5, 2.5) + theme_void() + ##labs(main="Pie Chart") + 
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) + theme(plot.title = element_text(family = "Helvetica Neue",size = 14,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(size=8,family = "Helvetica Neue")) 
    
    pie
  })
  
  output$barplot <- renderPlot({
    data <- exsumc[exsumc$Name %in% input$board & exsumc$repline != "010",]
    minyear <- as.numeric(input$finyear) - 4
    maxyear <- as.numeric(input$finyear)
    data <- data[(data$Year >= minyear & data$Year <= maxyear),]
    data <- data[,c(3,6,5)]
    plot <- ggplot(data, aes(x=Year, y=Percentage, fill = Defn)) + theme_bw() + 
      ggtitle(paste(input$board,"-", (max(data$Year)-4),"to", max(data$Year))) + 
      geom_bar(stat="identity", width = 0.6) +  scale_fill_brewer(palette="Blues") + 
      geom_text(aes(label=Percentage) ,family = "Helvetica Neue", size = 4, position = position_stack(vjust = 0.5)) + 
      theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
      theme(plot.title = element_text(family = "Helvetica Neue",size = 14,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(family = "Helvetica Neue", size=8))
    
    plot
  })
  
  
  output$TotalCost <- renderValueBox({
    totalcost <- exsumc[(exsumc$Year %in% input$finyear & exsumc$Name %in% input$board & exsumc$repline %in% c("010")),]
    totalcost <- paste("£",format(round(totalcost$Cost / 1e9, 1), trim = TRUE), "Billion")
    
    valueBox(
      totalcost, "Total Expenditure", icon = icon("gbp", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$HospitalCost <- renderValueBox({
    hospitalcost <- exsumc[(exsumc$Year %in% input$finyear & exsumc$Name %in% input$board & exsumc$repline %in% c("020")),]
    hospitalcost <- paste("£",format(round(hospitalcost$Cost / 1e9, 1), trim = TRUE), "Billion")
    
    valueBox(
      hospitalcost, "Hospital Spend", icon = icon("bed", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$CommunityCost <- renderValueBox({
    
    communitycost <- exsumc[(exsumc$Year %in% input$finyear & exsumc$Name %in% input$board & exsumc$repline %in% c("030")),]
    communitycost  <- paste("£",format(round(communitycost$Cost / 1e9, 1), trim = TRUE), "Billion")
    
    valueBox(
      communitycost, "Community Spend", icon = icon("home", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$FamilyCost <- renderValueBox({
    
    familycost <- exsumc[(exsumc$Year %in% input$finyear & exsumc$Name %in% input$board & exsumc$repline %in% c("040")),]
    familycost  <- paste("£",format(round(familycost$Cost / 1e9, 1), trim = TRUE), "Billion")
    
    valueBox(
      familycost, "Family Health Sector Spend", icon = icon("user", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$ResourceCost <- renderValueBox({
    
    resourcecost <- exsumc[(exsumc$Year %in% input$finyear & exsumc$Name %in% input$board & exsumc$repline %in% c("045")),]
    resourcecost  <- paste("£",format(round(resourcecost$Cost / 1e6, 1), trim = TRUE), "Million")
    
    valueBox(
      resourcecost, "Resource transfer (to local authority)", icon = icon("transfer", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$CostChange <- renderValueBox({
    
    totalcostcomp <- exsumc[((exsumc$Year == input$finyear | exsumc$Year == (as.numeric(input$finyear)-1))  & exsumc$repline %in% c("010") & exsumc$Name %in% input$board),]
    totalcostcomp$diff <-totalcostcomp$Cost - lag(totalcostcomp$Cost)
    
    totalcostdiff <- totalcostcomp[!(is.na(totalcostcomp$diff)),]
    change <- ifelse((totalcostdiff$diff > 0),"Increase","Decrease")
    totalcostdiff <- paste("£",format(round(totalcostdiff$diff / 1e6, 1), trim = TRUE), "Million")
    
    
    valueBox(
      totalcostdiff, paste(change,"since last year"), icon = icon("arrow-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$CAMHTableTotal <- renderDataTable({
    data <- dcast(CAMHs , Board ~ Year)
    data <- as.datatable(formattable(data),rownames = FALSE) 
  })
  
  
  output$MHTable <- renderDataTable({
    MentalHealth <- CompleteYear[,c(1,10,12)]
    colnames(MentalHealth) <- c("Board","Cost","Year")
    
    minyear <- as.numeric(input$mhyear) - 4
    maxyear <- as.numeric(input$mhyear)
    MentalHealth <- MentalHealth[(MentalHealth$Year >= minyear & MentalHealth$Year <= maxyear),]
    MentalHealth <- MentalHealth[,c(1,3,2)]
    MentalHealth$Cost <- ifelse(is.na(MentalHealth$Cost)==TRUE,"NA",paste("£",format(MentalHealth$Cost,  big.mark=",")))
    MentalHealth <- dcast(MentalHealth , Board ~ Year)
    MentalHealth <- MentalHealth[c(16,1:15,17),]
    MentalHealth <- as.datatable(formattable(MentalHealth),rownames = FALSE) 
   })
  
  output$MHCost <- renderValueBox({
    
    MentalHealthLatest <- CompleteYear[,c(1,10,12)]
    colnames(MentalHealthLatest) <- c("Board","Cost","Year")
    
    MentalHealthLatest <- MentalHealthLatest[(MentalHealthLatest$Year %in% input$mhyear & MentalHealthLatest$Board %in% input$mhboard),]
    
    MentalHealthLatest <- MentalHealthLatest$Cost
    MentalHealthLatest<- paste("£",format(round(MentalHealthLatest / 1e6, 1), trim = TRUE), "Million")
    
    valueBox(
      MentalHealthLatest, "Expenditure on Mental Health", icon = icon("gbp", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$MHPercent <- renderValueBox({
    
    MentalHealthPercent <- CompleteYear[,c(1,8,10,12)]
    colnames(MentalHealthPercent) <- c("Board","TotalCost","Cost","Year")
    
    MentalHealthPercent <- MentalHealthPercent[(MentalHealthPercent$Year %in% input$mhyear & MentalHealthPercent$Board %in% input$mhboard),]
    
    MentalHealthPercent <- paste(round((MentalHealthPercent$Cost/MentalHealthPercent$TotalCost)*100,2),"%")
 
    valueBox(
      MentalHealthPercent, "Percent of total NHS Board Expenditure ", icon = icon("signal", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$MHCostChange <- renderValueBox({
    
    
    mhtotalcost <- CompleteYear[,c(1,10,12)]
    colnames(mhtotalcost) <- c("Board","Cost","Year")
    
    mhtotalcost <- mhtotalcost[((mhtotalcost$Year == input$mhyear | mhtotalcost$Year == (as.numeric(input$mhyear)-1))  & mhtotalcost$Board %in% input$mhboard),]
    mhtotalcost$diff <- mhtotalcost$Cost - lag(mhtotalcost$Cost)
  
    
    mhtotalcost <- mhtotalcost[!(is.na(mhtotalcost$diff)),]
    change <- ifelse((mhtotalcost$diff > 0),"Increase","Decrease")
    mhtotalcost <- paste("£",format(round(mhtotalcost$diff / 1e6, 1), trim = TRUE), "Million")
    
    
    valueBox(
      mhtotalcost, paste(change,"since last year"), icon = icon("arrow-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  
  
  output$MHGraph <- renderPlot ({
    
    MHdata <- CompleteYear[,c(1,10,12)]
    colnames(MHdata) <- c("Board","Cost","Year")
    minyear <- as.numeric(input$mhyear) - 4
    maxyear <- as.numeric(input$mhyear)
    
    MHdata <- MHdata[((MHdata$Year >= minyear & MHdata$Year <= maxyear) & MHdata$Board %in% input$mhboard) ,]
    
    
    plot <- ggplot(MHdata, aes(x=Year, y=Cost, fill = Year)) + theme_bw() + 
      ggtitle(paste(input$mhboard,"-", min(MHdata$Year),"to", max(MHdata$Year))) + 
      geom_bar(stat="identity", width = 0.6,show.legend = FALSE) + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +  ##scale_fill_brewer(palette="Blues") + 
      theme(plot.title = element_text(family = "Helvetica Neue",size = 14,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(family = "Helvetica Neue", size=8))
    
    plot

  })
  
  
  output$CAMHGraph <- renderPlot ({
    
    CAMHdata <- CompleteYear[,c(1,7,12)]
    colnames(CAMHdata) <- c("Board","Cost","Year")
    minyear <- as.numeric(input$camhyear) - 4
    maxyear <- as.numeric(input$camhyear)
    
    CAMHdata <- CAMHdata[((CAMHdata$Year >= minyear & CAMHdata$Year <= maxyear) & CAMHdata$Board %in% input$camhboard) ,]
    
    plot <- ggplot(CAMHdata, aes(x=Year, y=Cost, fill = Year)) + theme_bw() + 
      ggtitle(paste(input$camhboard,"-", min(CAMHdata$Year),"to", max(CAMHdata$Year))) + 
      geom_bar(stat="identity", width = 0.6,show.legend = FALSE) + scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +  ##scale_fill_brewer(palette="Blues") + 
      theme(plot.title = element_text(family = "Helvetica Neue",size = 14,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(family = "Helvetica Neue", size=8))
    
    plot
    
  })
  
  output$MHPercentGraph <- renderPlot ({
    
    MHPercent <- CompleteYear[,c(1,8,10,12)]
    colnames(MHPercent) <- c("Board","TotalCost","Cost","Year")
    minyear <- as.numeric(input$mhyear) - 4
    maxyear <- as.numeric(input$mhyear)
    
    MHPercent <- MHPercent[(MHPercent$Year >= minyear & MHPercent$Year <= maxyear & MHPercent$Board %in% input$mhboard),]
    
    MHPercent$Percentage <- round((MHPercent$Cost/MHPercent$TotalCost)*100,2)
    MHPercent <- MHPercent[,c(1,4,5)]
    
    plot <- ggplot(MHPercent, aes(x=Year, y=Percentage, fill = Year)) + theme_bw() + 
      ggtitle(paste(input$mhboard,"-", min(MHPercent$Year),"to", max(MHPercent$Year))) + 
      geom_bar(stat="identity", width = 0.6,show.legend = FALSE) + ylim(0,100) +
      theme(plot.title = element_text(family = "Helvetica Neue",size = 14,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(family = "Helvetica Neue", size=8))
    
    plot
    
    

    
  })
  
  output$CAMHTable <- renderDataTable ({
    
    CAMH <- CompleteYear[,c(1,7,12)]
    colnames(CAMH) <- c("Board","Cost","Year")
    
    minyear <- as.numeric(input$camhyear) - 4
    maxyear <- as.numeric(input$camhyear)
    CAMH <- CAMH[(CAMH$Year >= minyear & CAMH$Year <= maxyear),]
    CAMH <- CAMH[,c(1,3,2)]
    CAMH$Cost <- ifelse(is.na(CAMH$Cost)==TRUE,"NA",paste("£",format(CAMH$Cost,  big.mark=",")))
    CAMH <- dcast(CAMH , Board ~ Year)
    CAMH <- CAMH[c(16,1:15,17),]
    CAMH <- as.datatable(formattable(CAMH),rownames = FALSE) 
    
    
  })
  
  output$CAMHPercentGraph <- renderPlot ({
    
    CAMHPercent <- CompleteYear[,c(1,7,10,12)]
    colnames(CAMHPercent) <- c("Board","Cost","TotalMHCost","Year")
    minyear <- as.numeric(input$camhyear) - 4
    maxyear <- as.numeric(input$camhyear)
    
    CAMHPercent <- CAMHPercent[(CAMHPercent$Year >= minyear & CAMHPercent$Year <= maxyear & CAMHPercent$Board %in% input$camhboard),]
    
    CAMHPercent$Percentage <- round((CAMHPercent$Cost/CAMHPercent$TotalMHCost)*100,2)
    CAMHPercent <- CAMHPercent[,c(1,4,5)]
    
    plot <- ggplot(CAMHPercent, aes(x=Year, y=Percentage, fill = Year)) + theme_bw() + 
      ggtitle(paste(input$mhboard,"-", min(CAMHPercent$Year),"to", max(CAMHPercent$Year))) + 
      geom_bar(stat="identity", width = 0.6,show.legend = FALSE) + ylim(0,100) +
      theme(plot.title = element_text(family = "Helvetica Neue",size = 14,hjust = 0.5),strip.text = element_text(size = 15), legend.text=element_text(family = "Helvetica Neue", size=8))
    
    plot
    
    
    
    
  })
  
  output$CAMHCost <- renderValueBox({
    
    CAMHLatest <- CompleteYear[,c(1,7,12)]
    colnames(CAMHLatest) <- c("Board","Cost","Year")
    
    CAMHLatest <- CAMHLatest[(CAMHLatest$Year %in% input$camhyear & CAMHLatest$Board %in% input$camhboard),]
    
    CAMHLatest <- CAMHLatest$Cost
    CAMHLatest<- paste("£",format(round(CAMHLatest / 1e3, 1), trim = TRUE), "(000s)")
    
    valueBox(
      CAMHLatest, "Expenditure on CAMHs", icon = icon("gbp", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$CAMHPercent <- renderValueBox({
    
    CAMHPercent <- CompleteYear[,c(1,7,10,12)]
    colnames(CAMHPercent) <- c("Board","Cost","TotalMHCost","Year")
    
    CAMHPercent <- CAMHPercent[(CAMHPercent$Year %in% input$camhyear & CAMHPercent$Board %in% input$camhboard),]
    
    CAMHPercent <- paste(round((CAMHPercent$Cost/CAMHPercent$TotalMHCost)*100,2),"%")
    
    valueBox(
      CAMHPercent, "Percent of total Mental Health Expenditure ", icon = icon("signal", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$CAMHCostChange <- renderValueBox({
    
    
    camhtotalcost <- CompleteYear[,c(1,7,12)]
    colnames(camhtotalcost) <- c("Board","Cost","Year")
    
    camhtotalcost <- camhtotalcost[((camhtotalcost$Year == input$camhyear | camhtotalcost$Year == (as.numeric(input$camhyear)-1))  & camhtotalcost$Board %in% input$camhboard),]
    camhtotalcost$diff <- camhtotalcost$Cost - lag(camhtotalcost$Cost)
    
    
    camhtotalcost <- camhtotalcost[!(is.na(camhtotalcost$diff)),]
    change <- ifelse((camhtotalcost$diff > 0),"Increase","Decrease")
    camhtotalcost <- paste("£",format(round(camhtotalcost$diff / 1e3, 1), trim = TRUE), "(000s)")
    
    
    valueBox(
      camhtotalcost, paste(change,"since last year"), icon = icon("arrow-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })

}

# Run the app ----

shinyApp(ui, server)
