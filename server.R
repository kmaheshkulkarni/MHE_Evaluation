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
      
      rownames = c("Manual_Hand_Trolley", "Powered_Cart", "Stock_Chaser", "Tugger_Cart",
                   "Forklift", "Pallet_Jack_Hand", "Pallet_Jack_Electric", "Reach_Trucks",
                   "AGV", "Pallet_Stacker")
      colnames = c("Dist_Btwn_Src_Destn", "Trips_Per_Shift", "Aisle_Width", "Turning_Radius",
                   "Vertical_Reach", "Max_Wt", "Floor_Condition")
      dsm<- matrix(data = NA, nrow = 10, ncol = 7, dimnames = (list(rownames, colnames)))
      # dbmat<- matrix(data = 3:10, nrow = 10, ncol = 7, dimnames = (list(rownames, colnames)))
      dsm<- as.data.frame(dsm)
      print("DSM b4 calculations")
      print(dsm)
      # write.csv(db, "Data/dsm.csv")
      # dsm<- read.csv("Data/dsm.csv")
      
      # db<- db[-1]
      # write.csv(db, "Final.csv")
      dsm$Floor_Condition<- ifelse(input$FloorCond == "Concrete", 3, ifelse(input$FloorCond == "Smooth", 5,
                                  ifelse(input$FloorCond == "Rough_Smooth", 2,ifelse(input$FloorCond == "Clean", 4,
                                  ifelse(input$FloorCond == "Rough", 1, ""))))) 
      
      dsm$Dist_Btwn_Src_Destn<- ifelse(input$Dist <= 150, 5, ifelse(input$Dist >= 151 & input$Dist <= 300, 4,
                                ifelse(input$Dist >= 301 & input$Dist <= 500, 3, ifelse(input$Dist >= 501 & 
                                input$Dist <= 1000, ifelse(input$Dist >= 1001 & input$Dist <= 2000, 2,
                                ifelse(input$Dist >=2001 & input$Dist <= 5000, 1, 0))))))
      
      dsm$Trips_Per_Shift<- ifelse(input$Trips <=100, 5, ifelse(input$Trips >= 101 & input$Trips <= 200, 4,
                            ifelse(input$Trips >=201 & input$Trips <= 500, 3, ifelse(input$Trips >= 501 &
                            input$Trips <= 2000, 2, 
                            ifelse(input$Trips >=2001 & input$Trips <=5000, 1, 0)))))
      
      dsm$Aisle_Width<- ifelse(input$AisleWidth <= 5, 5, ifelse(input$AisleWidth >=6 & input$AisleWidth<= 10, 4,
                        ifelse(input$AisleWidth >=11 & input$AisleWidth <=15, 3,
                        ifelse(input$AisleWidth >= 16 & input$AisleWidth <= 20, 2,
                        ifelse(input$AisleWidth >=21 & input$AisleWidth <= 30, 1, 0)))))
      
      dsm$Turning_Radius<- ifelse(input$TurnRads <= 5, 5, ifelse(input$TurnRads >=6 & input$TurnRads<= 10, 4,
                           ifelse(input$TurnRads >=11 & input$TurnRads <=15, 3,
                           ifelse(input$TurnRads >= 16 & input$TurnRads <= 20, 2,
                           ifelse(input$TurnRads >=21 & input$TurnRads <= 30, 1, 0)))))
      
      dsm$Vertical_Reach<- ifelse(input$VertReach <= 22, 5, ifelse(input$VertReach >=23 & input$VertReach <=28, 4,
                           ifelse(input$VertReach >= 29 & input$VertReach <= 36, 3,
                           ifelse(input$VertReach >=37 & input$VertReach <= 50, 2,
                           ifelse(input$VertReach >= 50 & input$VertReach <= 100, 1, 0)))))
      
      dsm$Max_Wt<- ifelse(input$MaxWt <= 500, 5, ifelse(input$MaxWt >= 501 & input$MaxWt >= 800, 4,
                   ifelse(input$MaxWt >= 801 & input$MaxWt <= 1000, 3, 
                   ifelse(input$MaxWt >=1001 & input$MaxWt <= 5000, 2,
                   ifelse(input$MaxWt >= 5001 & input$MaxWt <= 12000, 1, 0)))))
      
      dsm<- cbind(dsm, Total = rowSums(dsm)/7)
      
      print("DSM")
      print(dsm)
      
      Manual_Hand_Trolley<- dsm[1,8]
      print("Manual_Hand_Trolley")
      print(Manual_Hand_Trolley)
      
      Powered_Cart<- dsm[2,8]
      print("Powered_Cart")
      print(Powered_Cart)
      
      Stock_Chaser<- dsm[3,8]
      print("Stock_Chaser")
      print(Stock_Chaser)
      
      Tugger_Cart<- dsm[4,8]
      print("Stock_Chaser")
      print(Stock_Chaser)
      
      ForkliftV<- dsm[5,8]
      print("Tugger_Cart")
      print(Tugger_Cart)
      
      Pallet_Jack_Hand<- dsm[6,8]
      print("Pallet_Jack_Hand")
      print(Pallet_Jack_Hand)
      
      Pallet_Jack_Electric<- dsm[7,8]
      print("Pallet_Jack_Electric")
      print(Pallet_Jack_Electric)
      
      ReachTrucks<- dsm[8,8]
      print("ReachTrucks")
      print(ReachTrucks)
      
      AGV<- dsm[9,8]
      print("AGV")
      print(AGV)
      
      Pallet_Stacker<- dsm[10,8]
      print("Pallet_Stacker")
      print(Pallet_Stacker)
      
      output$SChaser<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Stock_Chaser),2), subtitle = "Storck Chaser", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$P_Cart<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Powered_Cart),2), subtitle = "Powered Cart", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$MHT<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Manual_Hand_Trolley),2), subtitle = "Manual Hand Trolley", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Tugger<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Tugger_Cart),2), subtitle = "Tugger Cart", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Forklift<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(ForkliftV),2), subtitle = "Forklift", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Pallet_JH<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Pallet_Jack_Hand),2), subtitle = "Pallet Jack Hand", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Pallet_JE<-renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Pallet_Jack_Electric),2), subtitle = "Pallet Jack Electric", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Reach_Trucks<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(ReachTrucks),2), subtitle = "Reach Trucks", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
    }
  })
}