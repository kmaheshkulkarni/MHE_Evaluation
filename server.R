function(input, output, session){
  observeEvent(input$Getinsights,{
    if(is.null(input$Getinsights)||input$Getinsights==0)
    {
      returnValue()
    }
    else
    {
      db<- read.csv("Data/Final.csv")
      db<- db[-1]
      main<- read.csv("Data/MHET.csv")
      rownames = c("Manual_Hand_Trolley", "Powered_Cart", "Stock_Chaser", "Tugger_Cart",
                   "Forklift", "Pallet_Jack_Hand", "Pallet_Jack_Electric", "Reach_Trucks",
                   "AGV", "Pallet_Stacker")
      colnames = c("Dist_Btwn_Src_Destn", "Trips_Per_Shift", "Aisle_Width", "Turning_Radius",
                   "Vertical_Reach", "Max_Wt", "Floor_Condition")
      dsm<- matrix(data = NA, nrow = 10, ncol = 7, dimnames = (list(rownames, colnames)))
      # dbmat<- matrix(data = 3:10, nrow = 10, ncol = 7, dimnames = (list(rownames, colnames)))
      dsm<- cbind(dsm, Total = rowSums(dsm))
      dsm<- as.data.frame(dsm)
      # write.csv(db, "Data/dsm.csv")
      # dsm<- read.csv("Data/dsm.csv")
      Powered_Cart<- dsm[2,8]
      Manual_Hand_Trolley<- dsm[1,8]
      Forklift<- dsm[5,8]
      # db<- db[-1]
      # write.csv(db, "Final.csv")
      dsm$Floor_Condition<- ifelse(input$FloorCond == "Concrete", 3, ifelse(input$FloorCond == "Smooth", 5,
                                  ifelse(input$FloorCond == "Rough_Smooth", 2,ifelse(input$FloorCond == "Clean", 4,
                                  ifelse(input$FloorCond == "Rough", 1, 0))))) 
      dsm$Dist_Btwn_Src_Destn<- ifelse(input$Dist <= 150, 5, ifelse(input$Dist >= 151 & input$Dist <= 300, 4,
                                ifelse(input$Dist >= 301 & input$Dist <= 500, 3, ifelse(input$Dist >= 501 & 
                                input$Dist <= 1000, ifelse(input$Dist >= 1001 & input$Dist <= 2000, 2,
                                ifel)))))
    }
  })
}