required_packages <- c("shiny", "tidyverse", "DT","scales", "plotly")  
# sass not used, we precompiled the scss into css
# shinythemes not used used custom css
# Valid shinythemes are: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
# Try switching to "cerulean", "darkly", or "cosmo" 

# Check and install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# Function for Rate Aggregate with Summation for Numerator and Population Check
############## ensure no pupulation #################
derived_rate <- function(number, population) {
  if (is.null(population) || population == 0) {
    return(0)
  }
  ( (sum(number) / sum(population) ) * 100000 )
}

# Load data
data <- read.csv("C:/Users/HP/Downloads/Group_X_Application/project/data/processed/Kenya_11years_derived_population.csv", stringsAsFactors = FALSE)

# images in key highlights tab
img_base64_widespread <- base64enc::dataURI(file = "C:/Users/HP/Downloads/Group_X_Application/__MACOSX/project/images/._widespread_undertreated_underresources_WHO.png")
img_base64_main_highlights <- base64enc::dataURI(file = "C:/Users/HP/Downloads/Group_X_Application/__MACOSX/project/images/._highlights_main.png")

# head(data,3)

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")  # Load globally
  ),
  
  #theme = shinytheme("cerulean"),  # cerulean, flaty, journal , united, 
  
  titlePanel("Mental Health Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Measure_name", "Select Measure Name:", 
                  choices = c(unique(data$Measure_name))
                  #, selected = "All"
      ),
      selectInput("Sex_name", "Select Sex:", 
                  choices = c("All", unique(data$Sex_name)), 
                  selected = "All"),
      selectInput("Cause_name", "Select Cause:", 
                  choices = c("All", unique(data$Cause_name)), 
                  selected = "All")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Key Highlights",
                 # Adding an image with a caption
                 h3("World Mental Health, The Key Highlights"),
                 includeHTML("highlights.html"),
                 div(
                   
                   # img(src = img_base64_main_highlights, alt = "Mental Health Stats", style = "width:100%; height:auto;"),
                   # p("Source: Source: https://www.hopechest.org/global-mental-health-statistics/"), 
                   # tags$a(href = "https://www.hopechest.org/global-mental-health-statistics/", 
                   #        target = "_blank", "Link to the full WHO report"),

                   h3("Global Mental Health Statistics Overview"),
                   img(src = img_base64_widespread, alt = "Mental Health Stats", style = "width:100%; height:auto;"),
                   p("Source: IHME, 2019 (98); WHO, 2021 (5)"), 
                   tags$a(href = "https://iris.who.int/bitstream/handle/10665/356119/9789240049338-eng.pdf", 
                          target = "_blank", "Link to the full WHO report")
                 )
        ),
        
        tabPanel("World Statistics", 
                 tabsetPanel(
                   tabPanel("Global", DTOutput("global_table")),
                   tabPanel("Continents", 
                            DTOutput("continent_table")
                            #, plotOutput("histPlot")
                            ),
                   tabPanel("Countries", DTOutput("country_table"))
                 )
        ),
        
        tabPanel("Kenya Statistics", 
                 tabsetPanel(
                   tabPanel("Country"
                            , h3("Country Statistics")
                            , plotOutput("Kenya_country_trend")),
                   tabPanel("Gender", h3("Analysis by gender")
                             ),
                   tabPanel("Age Groups", h3("Analysis by Age Groups")
                            , plotOutput("Kenya_age_trend"))
                 )
        ),
        
        tabPanel("About", 
                 includeHTML("about.html") )
      )
    )
  )
)
  
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      #   filter(Sex_name %in% input$Sex_name, Cause_name %in% input$Cause_name)
      filter(
        Measure_name == input$Measure_name,  # Ensure exact match
        (input$Sex_name == "All" | Sex_name %in% input$Sex_name),
        (input$Cause_name == "All" | Cause_name %in% input$Cause_name)
      )
  })

  output$global_table <- renderDT({
    filtered_data() %>% 
      group_by(Year,Cause_name) %>% 
      summarise(Number = sum(Number) #, Rate = ( (sum(number) / sum(population) ) * 100000 )#derived_rate( ("Number"), ("Population") ) 
                , .groups = "drop") %>%
      arrange(desc(Number)) %>%
      mutate(Number = comma(round(Number, 0))
             #, Rate = comma(round(Rate, 0))
             )  # Format numbers
  })
  
  output$continent_table <- renderDT({
    filtered_data() %>% 
      group_by(Continent, Year) %>% 
      summarise(Number = sum(Number), .groups = "drop") %>%
      arrange(desc(Number)) %>%
      mutate(Number = comma(round(Number, 0)))  # Format numbers
  })
  
  output$country_table <- renderDT({
    filtered_data() %>% 
      group_by(Location_name, Year) %>% 
      summarise(Number = sum(Number), .groups = "drop") %>%
      arrange(desc(Number)) %>%
      mutate(Number = comma(round(Number, 0)))  # Format numbers
  })
  
  # Lets try add at least one graph from the mtcars data
  
  # Kenya Statistics
  
  output$histPlot <- renderPlot({
    ggplot(mtcars, aes(x = mpg)) +
      geom_histogram(bins = input$bins, fill = "lightblue", color = "black") +
      labs(title = "Histogram of MPG", x = "Miles Per Gallon (MPG)", y = "Count")
  })
 
  output$Kenya_country_trend <- renderPlot({
    
    # Load the data
    df <- read_csv("C:/Users/HP/Downloads/Group_X_Application/__MACOSX/project/data/processed/._Kenya_11years_derived_population_both_sexes_wrate.csv")
    
    # Ensure data is loaded
    validate(need(nrow(df) > 0, "Data not loaded or empty"))
    
    # Format Rate to 6 decimal places
    df$Rate <- round(df$Rate, 6)
    
    # Plot
    ggplot(df, aes(x = Year, y = Rate, color = Cause_name)) +
      geom_line() +
      facet_wrap(~ Measure_name, scales = "free_y", ncol = 1) +  
      scale_y_continuous(labels = scales::number_format(accuracy = 0.000001)) +  
      labs(title = "Trend of Rates by Cause and Measure",
           x = "Year", y = "Rate - Number Per 100,000 Persons") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 16),
        plot.margin = margin(10, 40, 10, 40)
      )
  }, height = 600, width = 800)  # Adjust height and width here
   
  output$Kenya_age_trend <- renderPlot({
    
    # Load the data
    df2 <- read_csv("C:/Users/HP/Downloads/Group_X_Application/__MACOSX/project/data/processed/._Kenya_11years_all_agegroups_derived_population.csv")
    
    df2 <- df2 |> 
      filter(Measure_name == "YLDs (Years Lived with Disability)") |> 
      group_by(across(-c(Age_id, Number, Rate, Population, Cause_name)))  |> 
      summarise(across(c(Number), sum, na.rm = TRUE))
    head(df2)
    # Ensure data is loaded
    validate(need(nrow(df2) > 0, "Data not loaded or empty"))
    
    # Format Rate to 6 decimal places
    df2$Number <- round(df2$Number, 6)
    
    # Plot
    ggplot(df2, aes(x = Year, y = Number, color = Age_name)) +
      geom_line() +
      facet_wrap(~ Sex_name, scales = "free_y", ncol = 1) +  
      scale_y_continuous(labels = scales::number_format(accuracy = 0.000001)) +  
      labs(title = "Trend of Rates by Cause and Measure",
           x = "Year", y = "Rate - Number Per 100,000 Persons") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 16),
        plot.margin = margin(10, 40, 10, 40)
      )
  }, height = 600, width = 800)  # Adjust height and width here
  
}

shinyApp(ui, server)