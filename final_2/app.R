library(shiny)
library(palmerpenguins)
library(DT)
library(dplyr)
library(ggplot2)

axises = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    "펭귄 데이터 분석 : 미래자동차공학과 2018016244 추현욱"
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("species", label = '펭귄 종류를 선택하세요',
                         choices = unique(penguins$species),
                         selected = unique(penguins$species)[1]),
      selectInput("x_axis", "x축을 선택하세요.",
                  choices = axises,
                  selected = axises[1]),
      selectInput("y_axis", "y축을 선택하세요.",
                  choices = axises,
                  selected = axises[4]),
      sliderInput("pt_size", "점 크기를 선택하세요", min = 1, max = 10, value = 5),
    ),
    
    mainPanel(
      dataTableOutput('penguins_table'),
      plotOutput('penguins_plot'),
    ),
    
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sel_penguins = reactive({
    penguins %>%
      filter(species %in% input$species)
  })

  sel_x_axis = reactive({
    input$x_axis
  })

  sel_y_axis = reactive({
    input$y_axis
  })

  sel_pt_size = reactive({
    input$pt_size
  })
  
  output$penguins_table = renderDataTable({
    sel_penguins() %>%
      datatable()
  })
  
  output$penguins_plot = renderPlot(
    sel_penguins() %>%
      ggplot(aes_string(sel_x_axis(), sel_y_axis(), color = "species", shape = "sex")) +
      geom_point(size = sel_pt_size())
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
