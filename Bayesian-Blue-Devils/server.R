library(shiny)
library(data.table)
library(ggplot2)
library(blme)

data1 = as.data.frame(fread('/Users/Sanjay/Desktop/Bayesian-Blue-Devils/IN_PLAY1.csv', drop = 1))
data2 = as.data.frame(fread('/Users/Sanjay/Desktop/Bayesian-Blue-Devils/IN_PLAY2.csv', drop = 1))
IN_PLAY <- rbind(data1, data2)
rm(data1)
rm(data2)

IN_PLAY[,"bat_side"] <- factor(IN_PLAY[,"bat_side"])
IN_PLAY[,"throws"] <- factor(IN_PLAY[,"throws"])

res <- blmer(BIP_VALUE ~ bat_side + throws + plate_dist + break_x + break_z + 
               pos_dist + init_vel_x + init_vel_y + init_vel_z + init_accel_x + init_accel_y + init_accel_z +
               (bat_side + throws|pitcher_id), data = IN_PLAY, 
             cov.prior = wishart, fixef.prior = normal)

shinyServer(function(input, output, session) {
  

  
  predict_data <- reactive({c(input$bat_side, input$throws, input$plate_dist, input$break_x, input$break_z, input$post_dist, 
               input$init_vel_x, input$init_vel_y, input$init_vel_z, input$init_accel_x, input$init_accel_y, input$init_accel_z,
               input$bat_side, input$throws)})
  

  damage <- predict(res, predict_data())
  
  
  output$predictDamage = renderText(str(damage))
  
  
  #need to filter the data based on the user input and use this
  #pitch = reactive({filter(IN_PLAY, pitcher_id == input$pitcher)})
  #output$displayTable = renderTable(pitch)
  
  #storing the ggplot output of the heat map by event outcome 
  #output$displayHeat <- renderPlot(plot(plate_z ~ plate_x, xlab="", ylab="", data = pitch(), xlim = c(-4.5, 4.5), ylim = c(-2.5,2.5), col = BIP_VALUE))
})