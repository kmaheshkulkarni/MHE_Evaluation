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
      
      ######################################### Manual_Hand_Trolley #########################################
      
      Manual_Hand_Trolley<- dsm[1,]
      Manual_Hand_Trolley$Dist_Btwn_Src_Destn<- ifelse(input$Dist <= 100, 5, 0)
      Manual_Hand_Trolley$Trips_Per_Shift<- ifelse(input$Trips <= 200, 5, 0)
      Manual_Hand_Trolley$Aisle_Width<- ifelse(input$AisleWidth == 4, 5, 0)
      Manual_Hand_Trolley$Turning_Radius<- ifelse(input$TurnRads<= 0, 5, 0)
      Manual_Hand_Trolley$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      Manual_Hand_Trolley$Floor_Condition<- ifelse(input$FloorCond == "Concrete", 5, 0)
      Manual_Hand_Trolley$Max_Wt<- ifelse(input$MaxWt <= 500, 5, 0)
      Manual_Hand_Trolley<- cbind(Manual_Hand_Trolley, Total = rowSums(Manual_Hand_Trolley)/7)
      print("Manual_Hand_Trolley- Total")
      print(Manual_Hand_Trolley$Total)
      print(rowSums(Manual_Hand_Trolley))
      ######################################### Powered_Cart #########################################
      
      Powered_Cart<- dsm[2,]
      Powered_Cart$Dist_Btwn_Src_Destn<- ifelse(input$Dist <= 100, 5, 0)
      Powered_Cart$Trips_Per_Shift<- ifelse(input$Trips <= 200, 5, 0)
      Powered_Cart$Aisle_Width<- ifelse(input$AisleWidth == 4, 5, 0)
      Powered_Cart$Turning_Radius<- ifelse(input$TurnRads<= 0, 5, 0)
      Powered_Cart$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      Powered_Cart$Floor_Condition<- ifelse(input$FloorCond == "Concrete", 5, 0)
      Powered_Cart$Max_Wt<- ifelse(input$MaxWt >= 501 & input$MaxWt <= 1000, 5, 0)
      Powered_Cart<- cbind(Powered_Cart, Total = rowSums(Powered_Cart)/7)
      print("Powered_Cart- Total")
      print(Powered_Cart$Total)
      print(rowSums(Powered_Cart))
      
      
      ######################################### Stock_Chaser #########################################
      Stock_Chaser<- dsm[3,]
      Stock_Chaser$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 821 & input$Dist <= 1000, 5, 0)
      Stock_Chaser$Trips_Per_Shift<- ifelse(input$Trips >= 200 & input$Trips <= 400, 5, 0)
      Stock_Chaser$Aisle_Width<- ifelse(input$AisleWidth <= 3, 5, 0)
      Stock_Chaser$Turning_Radius<- ifelse(input$TurnRads <= 0, 5, 0)
      Stock_Chaser$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      Stock_Chaser$Floor_Condition<- ifelse(input$FloorCond == "Rough", 5, 0)
      Stock_Chaser$Max_Wt<- ifelse(input$MaxWt >= 6000 & input$MaxWt <= 10000, 5, 0)
      Stock_Chaser<- cbind(Stock_Chaser, Total = rowSums(Stock_Chaser)/7)
      print("Stock_Chaser- Total")
      print(Stock_Chaser$Total)
      print(rowSums(Stock_Chaser))
      
      
      ######################################### Tugger_Cart #########################################
      
      Tugger_Cart<- dsm[4,]
      Tugger_Cart$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 821 & input$Dist <= 1000, 5, 0)
      Tugger_Cart$Trips_Per_Shift<- ifelse(input$Trips >= 200 & input$Trips <= 400, 5, 0)
      Tugger_Cart$Aisle_Width<- ifelse(input$AisleWidth == 11, 5, 0)
      Tugger_Cart$Turning_Radius<- ifelse(input$TurnRads >= 4 & input$TurnRads <=8, 5, 0)
      Tugger_Cart$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      Tugger_Cart$Floor_Condition<- ifelse(input$FloorCond == "Smooth", 5, 0)
      Tugger_Cart$Max_Wt<- ifelse(input$MaxWt >= 10001 & input$MaxWt <= 13000 , 5, 0)
      Tugger_Cart<- cbind(Tugger_Cart, Total = rowSums(Tugger_Cart)/7)
      print("Tugger_Cart- Total")
      print(Tugger_Cart$Total)
      print(rowSums(Tugger_Cart))
      
      
      ######################################### Forklift #########################################
      
      Forklift<- dsm[5,]
      Forklift$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 331 & input$Dist <= 820, 5, 0)
      Forklift$Trips_Per_Shift<- ifelse(input$Trips >= 26 & input$Trips <= 74, 5, 0)
      Forklift$Aisle_Width<- ifelse(input$AisleWidth == 10, 5, 0)
      Forklift$Turning_Radius<- ifelse(input$TurnRads >= 5.5 & input$TurnRads <= 6, 5, 0)
      Forklift$Vertical_Reach<- ifelse(input$VertReach >= 23 & input$VertReach <= 28, 5, 0)
      Forklift$Floor_Condition<- ifelse(input$FloorCond == "Rough_Smooth", 5, 0)
      Forklift$Max_Wt<- ifelse(input$MaxWt >= 3000 & input$MaxWt <= 70000, 5, 0)
      Forklift<- cbind(Forklift, Total = rowSums(Forklift)/7)
      print("Forklift- Total")
      print(Forklift$Total)
      print(rowSums(Forklift))
      print(Forklift)
      
      ######################################### Pallet_Jack_Hand #########################################
      
      Pallet_Jack_Hand<- dsm[6,]
      Pallet_Jack_Hand$Dist_Btwn_Src_Destn<- ifelse(input$Dist <= 100, 5, 0)
      Pallet_Jack_Hand$Trips_Per_Shift<- ifelse(input$Trips <= 200, 5, 0)
      Pallet_Jack_Hand$Aisle_Width<- ifelse(input$AisleWidth == 4, 5, 0)
      Pallet_Jack_Hand$Turning_Radius<- ifelse(input$TurnRads<= 0, 5, 0)
      Pallet_Jack_Hand$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      Pallet_Jack_Hand$Floor_Condition<- ifelse(input$FloorCond == "Clean", 5, 0)
      Pallet_Jack_Hand$Max_Wt<- ifelse(input$MaxWt >= 1001 & input$MaxWt <= 1500, 5, 0)
      Pallet_Jack_Hand<- cbind(Pallet_Jack_Hand, Total = rowSums(Pallet_Jack_Hand)/7)
      print("Pallet_Jack_Hand- Total")
      print(Pallet_Jack_Hand$Total)
      print(rowSums(Pallet_Jack_Hand))
      
      
      ######################################### Pallet_Jack_Electric #########################################
      
      Pallet_Jack_Electric<- dsm[7,]
      Pallet_Jack_Electric$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 101 & input$Dist <= 250, 5, 0)
      Pallet_Jack_Electric$Trips_Per_Shift<- ifelse(input$Trips >= 201 & input$Trips <= 400, 5, 0)
      Pallet_Jack_Electric$Aisle_Width<- ifelse(input$AisleWidth == 4, 5, 0)
      Pallet_Jack_Electric$Turning_Radius<- ifelse(input$TurnRads<= 0, 5, 0)
      Pallet_Jack_Electric$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      Pallet_Jack_Electric$Floor_Condition<- ifelse(input$FloorCond == "Clean", 5, 0)
      Pallet_Jack_Electric$Max_Wt<- ifelse(input$MaxWt >= 1500 & input$MaxWt <=5000, 5, 0)
      Pallet_Jack_Electric<- cbind(Pallet_Jack_Electric, Total = rowSums(Pallet_Jack_Electric)/7)
      print("Pallet_Jack_Electric- Total")
      print(Pallet_Jack_Electric$Total)
      print(rowSums(Pallet_Jack_Electric))
      
      ######################################### Reach_Trucks #########################################
      
      Reach_Trucks<- dsm[8,]
      Reach_Trucks$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 251 & input$Dist <= 328, 5, 0)
      Reach_Trucks$Trips_Per_Shift<- ifelse(input$Trips <= 25, 5, 0)
      Reach_Trucks$Aisle_Width<- ifelse(input$AisleWidth >= 7.8 & input$AisleWidth <= 10, 5, 0)
      Reach_Trucks$Turning_Radius<- ifelse(input$TurnRads ==5, 5, 0)
      Reach_Trucks$Vertical_Reach<- ifelse(input$VertReach >= 29 & input$VertReach <= 36, 5, 0)
      Reach_Trucks$Floor_Condition<- ifelse(input$FloorCond == "Concrete", 5, 0)
      Reach_Trucks$Max_Wt<- ifelse(input$MaxWt >= 1001 & input$MaxWt <=4500, 5, 0)
      Reach_Trucks<- cbind(Reach_Trucks, Total = rowSums(Reach_Trucks)/7)
      print("Reach_Trucks- Total")
      print(Reach_Trucks$Total)
      print(rowSums(Reach_Trucks))
      ######################################### AGV #########################################
      
      AGV<- dsm[9,]
      AGV$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 1001 & input$Dist <= 50000, 5, 0)
      AGV$Trips_Per_Shift<- ifelse(input$Trips >= 401 & input$Trips <= 6250, 5, 0)
      AGV$Aisle_Width<- ifelse(input$AisleWidth >4 & input$AisleWidth <= 6, 5, 0)
      AGV$Turning_Radius<- ifelse(input$TurnRads >= 0 & input$TurnRads <= 3.5, 5, 0)
      AGV$Vertical_Reach<- ifelse(input$VertReach == 0, 5, 0)
      AGV$Floor_Condition<- ifelse(input$FloorCond == "Concrete", 5, 0)
      AGV$Max_Wt<- ifelse(input$MaxWt >= 50 & input$MaxWt <=1000, 5, 0)
      AGV<- cbind(AGV, Total = rowSums(AGV)/7)
      print("AGV- Total")
      print(AGV$Total)
      print(rowSums(AGV))
      
      
      ######################################### Pallet_Stacker #########################################
      
      Pallet_Stacker<- dsm[10,]
      Pallet_Stacker$Dist_Btwn_Src_Destn<- ifelse(input$Dist >= 251 & input$Dist <= 330, 5, 0)
      Pallet_Stacker$Trips_Per_Shift<- ifelse(input$Trips <= 200, 5, 0)
      Pallet_Stacker$Aisle_Width<- ifelse(input$AisleWidth >= 6 & input$AisleWidth <= 7.7, 5, 0)
      Pallet_Stacker$Turning_Radius<- ifelse(input$TurnRads > 4 & input$TurnRads <= 4.5, 5, 0)
      Pallet_Stacker$Vertical_Reach<- ifelse(input$VertReach >=0 & input$VertReach <= 22, 5, 0)
      Pallet_Stacker$Floor_Condition<- ifelse(input$FloorCond == "Rough_Smooth", 5, 0)
      Pallet_Stacker$Max_Wt<- ifelse(input$MaxWt >= 1500 & input$MaxWt <= 4500, 5, 0)
      Pallet_Stacker<- cbind(Pallet_Stacker, Total = rowSums(Pallet_Stacker)/7)
      print("Pallet_Stacker- Total")
      print(Pallet_Stacker$Total)
      print(rowSums(Pallet_Stacker))
      

      
      
      output$SChaser<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Stock_Chaser$Total),2), subtitle = "Storck Chaser", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$P_Cart<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Powered_Cart$Total),2), subtitle = "Powered Cart", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$MHT<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Manual_Hand_Trolley$Total),2), subtitle = "Manual Hand Trolley", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Tugger<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Tugger_Cart$Total),2), subtitle = "Tugger Cart", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$ForkliftV<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Forklift$Total),2), subtitle = "Forklift", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Pallet_JH<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Pallet_Jack_Hand$Total),2), subtitle = "Pallet Jack Hand", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Pallet_JE<-renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Pallet_Jack_Electric$Total),2), subtitle = "Pallet Jack Electric", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$Reach_Trucks<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Reach_Trucks$Total),2), subtitle = "Reach Trucks", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$AGVQ<-renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(AGV$Total),2), subtitle = "AVG", status = "success",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
      output$PalStack<- renderbs4ValueBox(
        bs4ValueBox(value = round(as.numeric(Pallet_Stacker$Total),2), subtitle = "Pallet Stacker", status = "warning",
                    footer = "Rating On a Scale of 5", icon = "database", width = 12)
      )
    }
  })
}