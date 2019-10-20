# nn_model is global variable
# contour plot part
# 4 layers 5 nodes repetition 4
plot(nn.model[[1]][[4]],radius = 0.1,x.entry = 0.12,x.out = 0.85,
     fontsize = 8,col.intercept = "red",col.entry = "green",col.out = "yellow",
     arrow.length = 0.08,intercept.factor = 0.2,
     dimension = 6,information.pos = -2.0,information = TRUE) # model info


library(shiny)
library(shinythemes)
shinyApp(
  
  ui = fluidPage(
    theme = shinytheme("spacelab"),
    # Feature selection
    fixedRow(
      # Input: a
      column(6,sliderInput(inputId = "featureInput1", label = "a",min = 0, max = 0.9,
                           step = 0.045, value = 0)),
      # Input: b
      column(6,sliderInput(inputId = "featureInput2", label = "b",min = 0, max = 0.9,
                           step = 0.045, value = 0))),
    # First row
    fixedRow(
      column(6, plotlyOutput("Plot1",height = "600px")),
      column(6, plotlyOutput("Plot2",height = "600px"))),
    
    # Feature selection
    fixedRow(
      # Input: tBCB
      column(6,sliderInput(inputId = "featureInput4", label = "tBCB",min = 0.1, max = 0.9,
                           step = 0.04, value = 0.5)),
      # Input: r_MDA
      column(6,sliderInput(inputId = "featureInput5", label = "r_MDA",min = 0.61111, max = 0.77778,
                           step = 0.0083335, value = 0.77778))
    ),
    # Second row
    fixedRow(
      column(6, plotlyOutput("Plot3",height = "600px")),
      column(6, plotlyOutput("Plot4",height = "600px"))
    )
  ),
  
  server = function(input, output) {
    grid.abfixed <- reactive({
      filter(grid.pred[[1]][[4]],round(a,3) == input$featureInput1 &
               round(b,3) == input$featureInput2 )
    })
    grid.trfixed <- reactive({
      filter(grid.pred[[1]][[4]],round(tBCB,3) == input$featureInput4 &
               round(r_MDA,6) == input$featureInput5 )
    })
    
    output$Plot1 <- renderPlotly({plot_ly(data=grid.abfixed(),type="contour",
                                          x=~tBCB,y=~r_MDA,z=~minref,colors=c("red","green"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="minref") %>%
        colorbar(title="minref")
      
    })
    output$Plot2 <- renderPlotly({plot_ly(data=grid.abfixed(),type="contour",
                                          x=~tBCB,y=~r_MDA,z=~maxtrans,
                                          colors=c("green","red"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="maxtrans") %>%
        colorbar(title="maxtrans")
      
    })
    output$Plot3 <- renderPlotly({plot_ly(data=grid.trfixed(),type="contour",
                                          x=~a,y=~b,z=~minref,colors=c("red","green"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="minref") %>%
        colorbar(title="minref")
      
    })
    output$Plot4 <- renderPlotly({plot_ly(data=grid.trfixed(),type="contour",
                                          x=~a,y=~b,z=~maxtrans,colors=c("green","red"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="maxtrans") %>%
        colorbar(title="maxtrans")
      
    })
    
    
  },
  options = list(height = 500)
)