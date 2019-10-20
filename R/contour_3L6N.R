# Add Frequency as input
# Choose 6 * 3 structure as NN model
data3.minref<-select(data3,a:freq1)%>%
  mutate(norm.freq1 = (freq1-mean(freq1))/sqrt(var(freq1)))
data3.maxtra<-select(data3,a:r_MDA,maxtrans:freq2) %>%
  mutate(norm.freq2 = (freq2-mean(freq2))/sqrt(var(freq2)))


# nn_1<-neuralnet(minref~a+b+tBCB+r_MDA+norm.freq1,data=data3.minref,
#               hidden=rep(6,3),stepmax=1e+05,
#                algorithm="rprop+",err.fct="sse",
#                act.fct="logistic",linear.output=FALSE)
# saveRDS(nn_1,"nn_1.rds")
nn_1<-readRDS("nn_1.rds")
# nn_2<-neuralnet(maxtrans~a+b+tBCB+r_MDA+norm.freq2,data=data3.maxtra,
#               hidden=rep(6,3),stepmax=1e+05,
#                algorithm="rprop+",err.fct="sse",
#                act.fct="logistic",linear.output=FALSE)
# saveRDS(nn_2,"nn_2.rds")
nn_2<-readRDS("nn_2.rds")
# grid prediction
grid.freq <- expand.grid(a=round(seq(0.0,0.9,0.045),3),b=round(seq(0,0.9,0.045),3),
                         tBCB=round(seq(0.1,0.9,0.04),3),r_MDA=seq(0.61111,0.77778,0.0083335),
                         freq = seq(47.0,47.8,0.2))%>%
  mutate(norm.freq = (freq-mean(freq))/sqrt(var(freq)),
         pred.minref=neuralnet::compute(nn_1,cbind(a,b,tBCB,r_MDA,norm.freq))$net.result[,1],
         pred.maxtra=neuralnet::compute(nn_2,cbind(a,b,tBCB,r_MDA,norm.freq))$net.result[,1])



# contour plot ShinyApp
library(shiny)
library(shinythemes)
shinyApp(
  
  ui = fluidPage(
    theme = shinytheme("spacelab"),
    # Feature selection
    fixedRow(
      # Input: a
      column(4,sliderInput(inputId = "featureInput1", label = "a",min = 0, max = 0.9,
                           step = 0.045, value = 0)),
      # Input: b
      column(4,sliderInput(inputId = "featureInput2", label = "b",min = 0, max = 0.9,
                           step = 0.045, value = 0)),
      # Input: frequency
      column(4,sliderInput(inputId = "featureInput3", label = "frequency",min = 47,
                           max = 47.8, step = 0.2, value = 47))),
    # First row
    fixedRow(
      column(6, plotlyOutput("Plot1",height = "600px")),
      column(6, plotlyOutput("Plot2",height = "600px"))),
    
    # Feature selection
    fixedRow(
      # Input: tBCB
      column(4,sliderInput(inputId = "featureInput4", label = "tBCB",min = 0.1, max = 0.9,
                           step = 0.04, value = 0.5)),
      # Input: r_MDA
      column(4,sliderInput(inputId = "featureInput5", label = "r_MDA",min = 0.61111, max = 0.77778,
                           step = 0.0083335, value = 0.77778)),
      # Input: frequency
      column(4,sliderInput(inputId = "featureInput6", label = "frequency",min = 47,
                           max = 47.8, step = 0.2, value = 47))
    ),
    # Second row
    fixedRow(
      column(6, plotlyOutput("Plot3",height = "600px")),
      column(6, plotlyOutput("Plot4",height = "600px"))
    )
  ),
  
  server = function(input, output) {
    grid.abfixed <- reactive({
      filter(grid.freq,round(a,3) == input$featureInput1 &
               round(b,3) == input$featureInput2 &
               round(freq,2) == input$featureInput3)
    })
    grid.trfixed <- reactive({
      filter(grid.freq,round(tBCB,3) == input$featureInput4 &
               round(r_MDA,6) == input$featureInput5 &
               round(freq,2) == input$featureInput6)
    })
    
    output$Plot1 <- renderPlotly({plot_ly(data=grid.abfixed(),type="contour",
                                          x=~tBCB,y=~r_MDA,z=~pred.minref,colors=c("red","green"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="minref") %>%
        colorbar(title="minref")
      
    })
    output$Plot2 <- renderPlotly({plot_ly(data=grid.abfixed(),type="contour",
                                          x=~tBCB,y=~r_MDA,z=~pred.maxtra,
                                          colors=c("green","red"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="maxtrans") %>%
        colorbar(title="maxtrans")
      
    })
    output$Plot3 <- renderPlotly({plot_ly(data=grid.trfixed(),type="contour",
                                          x=~a,y=~b,z=~pred.minref,colors=c("red","green"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="minref") %>%
        colorbar(title="minref")
      
    })
    output$Plot4 <- renderPlotly({plot_ly(data=grid.trfixed(),type="contour",
                                          x=~a,y=~b,z=~pred.maxtra,colors=c("green","red"),
                                          contours=list(coloring = "heatmap",showlabels=TRUE),
                                          hoverinfo="all",name="maxtrans") %>%
        colorbar(title="maxtrans")
      
    })
    
    
  },
  options = list(height = 500)
)