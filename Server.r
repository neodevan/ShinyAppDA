# Contributed by Shyam Krishnan(10533809) and Amal Joy (10533602)
library(shiny)
x<- na.omit(as.data.frame(airquality))
x
library(shiny)
# Other useful packages
library(datasets)
library(rpart)
library(party)
library(fpc)

# Define colors
palette(c("#E73032", "#377EB8", "#4DAF4A", "#984EA3",
       "#FF7F00", "#FFDD33", "#A65628", "#F781BF", "#999999"))

# Define server logic  Contributed by Amal Joy (10533602)
shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "all x data" = x,
           "May" = subset(x, x$Month == "5"),
           "June" = subset(x, x$Month == "6"),
           "July" = subset(x, x$Month == "7"),
           "August" = subset(x, x$Month == "8"),
           "September" = subset(x, x$Month == "9"))
  })
  
  colX <- reactive({
    switch(input$Xvar,
           "Ozone" = x$Ozone,
           "Solar.R" = x$Solar.R,
           "Wind" = x$Wind,
           "Temp" = x$Temp)
  })
  
  colY <- reactive({
    switch(input$Yvar,
           "Ozone" = x$Ozone,
           "Solar.R" = x$Solar.R,
           "Wind" = x$Wind,
           "Temp" = x$Temp)
  })
  
  colK <- reactive({
    switch(input$Kvar,
           "Ozone" = x$Ozone,
           "Solar.R" = x$Solar.R,
           "Wind" = x$Wind,
           "Temp" = x$Temp)
  })
  
  
  myColors <- reactive({
    switch(input$dataset,
           "all x data" = c(palette()[1],palette()[2],palette()[3],palette()[4],palette()[5]),
           "May" = palette()[1],
           "June" = palette()[2],
           "July" = palette()[3],
           "August" = palette()[4],
           "September" = palette()[5])
  })
#HIstogram
  output$myhist <- renderPlot(
    {
      colm = as.numeric(input$var)
      hist(x[,colm], col =input$colour, xlim = c(0, max(x[,colm])), main = "Histogram of airquality", breaks = seq(0, max(x[,colm]),l=input$bin+1), xlab = names(x[colm]))
    
  })
#Hypothesis testing Contributed by Vipin Mavila Pathayapurayil (10537195)
  
  output$hypt<- renderPlot({
    df_x <- datasetInput()
    plot(df_x[,c(input$Yvar)], df_x[,c(input$Kvar)],ylab=names(df_x[input$Kvar]),xlab=names(df_x[input$Yvar]))
  })
  output$hyp <- renderPrint({
    df_x <- datasetInput()
    hyp <- cor.test (df_x[,c(input$Yvar)], df_x[,c(input$Kvar)])
    hyp
  })
#Linear regression Contributed by Vipin Mavila Pathayapurayil (10537195)
  output$linreg<- renderPlot({
    df_x <- datasetInput()
    p<- na.omit(as.data.frame(airquality))
  tmpwind<-glm(df_x[,c(input$Kvar)]~df_x[,c(input$Yvar)],p,family="poisson")
  tmpwind1<-lm(df_x[,c(input$Kvar)]~df_x[,c(input$Yvar)],p)
  
  #you can compare the two output which gives similar estimates of the slope and intercept Contributed by Amal Joy (10533602)
  
  #model validation plot, residuals vs fitted values, residuals vs predictors and histogramm of #residuals
  r<-residuals(tmpwind)
  f<-fitted.values(tmpwind)
  par(mfrow=c(2,2))
  plot(r~f)
  plot(r~df_x[,c(input$Yvar)])
  hist(r)
  
  #add a plot with the estimated intercept and slope (which are both significantly different from #0)
  plot(df_x[,c(input$Kvar)]~df_x[,c(input$Yvar)],p,xlab=names(df_x[input$Kvar]),ylab=names(df_x[input$Yvar]))
  abline(a=coef(tmpwind1)["(Intercept)"],b=coef(tmpwind1)[2],col="red")
  })
  
# Probability Model Normal Distribution Contributed by Amal Joy (10533602)
  output$prob_dist<- renderPlot({
  df_x <- datasetInput()  
  mean=mean(df_x[,c(input$Yvar)]); sd=sd(df_x[,c(input$Yvar)])
  lb=80; ub=120
  
  x1 <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x1,mean,sd)
  plot(x1, hx, type="n", xlab=names(df_x[input$Yvar]), ylab="Density",
       main="Normal Distribution", axes=FALSE)
  
  i <- x1 >= lb & x1 <= ub
  lines(x1, hx)
  polygon(c(lb,x1[i],ub), c(0,hx[i],0), col="red")
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb,"< IQ <",ub,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(40, 160, 20), pos=0)
  
  })
  
# Generate a summary of the dataset (or subset by x.Month) Contributed by Amal Joy (10533602)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first n observations Contributed by Amal Joy (10533602)
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  output$NbRows <- renderText({ 
    paste("You have selected to show ", input$obs," lines.")
  })
  
# Show boxplot Contributed by Amal Joy (10533602)
  output$boxPlot <- renderPlot({
    df_x <- datasetInput()
    
    if (input$dataset == "all x data") {
      boxplot(df_x[,c(input$Yvar)] ~ df_x[,5], xlab = "Month", ylab = input$Yvar, main = "x", 
              border = "black", col = myColors())
    }
    else {
      boxplot(df_x[,c(input$Yvar)], xlab = "Month", ylab = input$Yvar, main = toupper(input$dataset),
              border = "black", col = myColors())
    }
  })
  
 # Create a .csv file with dataframe inside  Contributed by Shyam Krishnan(10533809)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-x-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(x, con)
    }
  )
  })