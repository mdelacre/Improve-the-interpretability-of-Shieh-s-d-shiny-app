#install.packages("shiny")
library(devtools)
library(shiny)
 
ui <-fluidPage(
  withMathJax(),
  titlePanel("How to transform Shieh's \\(\\delta\\) value in order to facilitate interpretability?"),br(),
  h4(
    "The main criticism of Shieh's \\(\\delta\\) is its dependency to the sample size ratio (nratio).",br(),br(),"In order to facilitate interpretability, we could answer the following question: 'What value of Shieh's \\(\\delta\\) would we obtain if design were balanced (i.e. \\(n_{1}\\)=\\(n_{2}\\))?'"
  ),br(),
  selectInput("plot", "plot:", 
              c("nratio" = "nratio",
                "log(nratio)" = "lognratio")),
  sidebarLayout(
    sidebarPanel(
      p("Set the following population parameters, so as to determine the values of the Shieh's delta as a function of the nratio"),
      sliderInput("N", "Total number of observations (N):", min = 10, max = 1000, value = 100,step=2),
      sliderInput("mudiff", "Raw mean difference (\\(\\mu_{1}\\)-\\(\\mu_{2}\\)):", min = -50, max = 50, value = 5),
      sliderInput("sd1", "Standard deviation of the first group (\\(\\sigma_{1}\\)):", min = 1, max = 100, value = 10,step=1),
      sliderInput("sd2", "Standard deviation of the second group (\\(\\sigma_{2}\\)):", min = 1, max = 100, value = 10,step=1),
    ),
    mainPanel(
      plotOutput("plot"), # Place for the graph
    ),
  ),
  br(),
  br(),
  textOutput("eq")   # Place for the equation 
  
)

server <- function(input,output){

  output$plot <- renderPlot(
    {
      
      sto <- data.frame(n1=rep(0,N-1),
                        n2=rep(0,N-1),
                        nratio=rep(0,N-1),
                        shieh=rep(0,N-1),
                        cohen=rep(0,N-1))
      
      for (i in seq_len(input$N-1)){
        n1 <- i
        n2 <- input$N-n1
        q1<-n1/input$N
        q2<-n2/input$N
        shieh_d<-input$mudiff/sqrt(input$sd1^2/q1+input$sd2^2/q2)
        pooled_sd<-sqrt(((n1-1)*input$sd1^2+(n2-1)*input$sd2^2)/(n1+n2-2))
        cohen_d<-input$mudiff/pooled_sd
        sto[i,] <- cbind(n1,n2,n1/n2,shieh_d,cohen_d)  
      }

    if(input$plot=="nratio"){
      plot(sto$nratio,sto$shieh,pch=19,ylim=c(min(sto$shieh,sto$cohen),max(sto$shieh,sto$cohen)),cex=.2,xlab="nratio",ylab=expression(paste("effect size ",delta)),col="lightblue")
      points(sto$nratio,sto$cohen,pch=19,cex=.2,col="green")
      abline(v=1,lty=2,col="lightgrey")
      points(1,sto$shieh[sto$nratio==1],pch=19,cex=.5,col="black")
      par(xpd=TRUE)
      text(1,sto$shieh[sto$nratio==1],
           labels=  as.character(round(sto$shieh[sto$nratio==1],4)),
           col="black",lwd = 1,pos = 4,cex = 1)
      points(1,sto$cohen[sto$nratio==1],pch=19,col="black",cex=.5)
      text(1,sto$cohen[sto$nratio==1],
           labels=  as.character(round(sto$cohen[sto$nratio==1],4)),
           col="black",lwd = 1,pos = 4,cex = 1)
      legend("topright", 
             legend=c(expression(paste("Cohen's ",delta)),expression(paste("Shieh's ",delta))),
             fill=c("green","lightblue"),
             horiz=F)
      
    } else {
      par(xpd=FALSE)
      plot(log(sto$nratio),sto$shieh,ylim=c(min(sto$shieh,sto$cohen),max(sto$shieh,sto$cohen)),pch=19,cex=.3,xlab="log(nratio)",ylab=expression(paste("effect size ",delta)),col="lightblue")
      points(log(sto$nratio),sto$cohen,pch=19,cex=.3,col="green")
      abline(v=0,lty=2,col="lightgrey")  
      par(xpd=TRUE)
      points(0,sto$shieh[log(sto$nratio)==0],pch=19,cex=.5,col="black")
      text(0,sto$shieh[log(sto$nratio)==0],
           labels=  as.character(round(sto$shieh[sto$nratio==1],4)),
           col="black",lwd = 1,pos = 4,cex = 1)
      points(0,sto$cohen[log(sto$nratio)==0],pch=19,col="black",cex=.5)
      text(0,sto$cohen[log(sto$nratio)==0],
           labels=  as.character(round(sto$cohen[sto$nratio==1],4)),
           col="black",lwd = 1,pos = 4,cex = 1)
      legend("topright", 
             legend=c(expression(paste("Cohen's ",delta)),expression(paste("Shieh's ",delta))),
             fill=c("green","lightblue"),
             horiz=F)
    }
      
      output$eq <- renderText({
        
        withMathJax()
        paste0(round(sto$shieh[sto$nratio==1],4),
               " = \\(\\delta_{Shieh}\\) \\(\\times\\) \\(\\frac{(nratio+1) \\times\\ \\sigma_{n_1 \\neq\\ n_2}\\ } {2 \\times\\ ",
               round(sqrt((input$sd1^2+input$sd2^2)/2),4),
               " \\times\\ \\sqrt{nratio}}\\), with \\(\\sigma_{n_1 \\neq\\ n_2}\\) =","\\(\\sqrt{(1- \\frac{n_1}{",input$N,"}\\ ) \\times\\ ",round(input$sd1^2,4)," + (1- \\frac{n_2}{",input$N,"}\\ ) \\times\\ ",round(input$sd2^2,4),"}\\)")
        
      })
    }
    
    
  )

  
}
shinyApp(ui=ui,server=server)
