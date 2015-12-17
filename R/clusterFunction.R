clusterFunction <-
function(clusterNo, all_fP, all_long_fP){
  
  if (clusterNo==1){
    trashold_noSignal_x=0.01
    trashold_noSignal_y=0.1
    trashold_lysis_a<-0
    trashold_lysis_b<-0.75
    trashold_AIC=-10
    temp<-dplyr::summarize(all_fP , cluster =
                             if (is.na(linear_slope_Estimate_Corr) | is.na(dif_AIC_value) | is.na(mod10_A2_Estimate)){"NA"}
                           else if (linear_slope_Estimate_Corr<trashold_noSignal_x & (max_nRat-min_nRat)<trashold_noSignal_y){"no signal"} 
                           else if (mod05_AIC_value>trashold_AIC & mod10_AIC_value>trashold_AIC) {"ambiguous"}
                           else if (dif_AIC_value>trashold_lysis_a & mod10_A2_Estimate<trashold_lysis_b & mod10_M1_Estimate-0.5*4/mod10_B1_Estimate<0){"ambiguous"}
                           else if (dif_AIC_value>trashold_lysis_a & mod10_A2_Estimate<trashold_lysis_b){"infection & lysis"}
                           else if (mod05_M1_Estimate-0.5*1.5*mod05_Ka_Estimate/mod05_B1_Estimate<0){"ambiguous"}
                           else if (mod05_M1_Estimate+0.5*1.5*mod05_Ka_Estimate/mod05_B1_Estimate>24){"ambiguous"}
                           else {"infection"})
  }
  
  return(temp)
}
