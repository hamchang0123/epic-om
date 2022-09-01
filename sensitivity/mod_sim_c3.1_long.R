source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters.R')
pacman::p_load(tidyverse, rio, deSolve, reshape2)

#Two weeks longer
mod_sim_c3.1_long <- function(time, state, parameters) {
  with(as.list(c(state, parameters)),{
    N_y <- S_y + E_y + Ip_y + Ic_y + Is_y + R_y
    #total number of children in the population
    N_a <- S_a + E_a + Ip_a + Ic_a + Is_a + R_a
    #total number of adults in the population
    
    #--Defining the force of infection
    if (time == 19) {
      c_yy = c_yy * intv_c1.1 * intv_c4.4_y 
      c_ya = c_ya * intv_c4.4_a
      c_ay = c_ay * intv_c4.4_y
      c_aa = c_aa_c2.2_max * intv_c4.4_a}
    
    else if (time < 22){
      c_yy =  c_yy * intv_c1.1 * intv_c4.4_y 
      c_ya = c_ya * intv_c4.4_a
      c_ay = c_ay * intv_c4.4_y
      c_aa = c_aa_c2.2 * intv_c4.4_a}
    #On day 0-43, intv_c3.2 was also in place.
    #However, due to the similarity of the nature and expected effect of two interventions,
    #only the effect of intv_c4.4 was included in the model to avoid over-estimating the effect of control interventions
    #In fact, OxCGRT team assumed intv_c3.2 was in place when intv_c4.4. 
    
    else if (time > 21 & time <44) {
      c_yy =  c_yy * intv_c4.4_y 
      c_ya = c_ya * intv_c4.4_a
      c_ay = c_ay * intv_c4.4_y
      c_aa = c_aa_c2.2 * intv_c4.4_a
    }
    
    else if (time > 43 & time < 92){
      c_yy =  c_yy * intv_c3.1_y 
      c_ya = c_ya * intv_c3.1_a
      c_ay = c_ay * intv_c3.1_y
      c_aa = c_aa_c2.2 * intv_c3.1_a}
    
    else if (time > 91 & time < 121){
      c_yy =  c_yy * intv_c3.1_y 
      c_ya = c_ya * intv_c3.1_a
      c_ay = c_ay * intv_c3.1_y
      c_aa = c_aa * intv_c3.1_a}
    
    else {
      c_yy = c_yy 
      c_ya = c_ya 
      c_ay = c_ay 
      c_aa = c_aa}
      #These contact parameters were estimated based on the POLYMOD study before the pandemic
      #Hence, they may not accurately capture the contact patterns during the pandemic.
      #Also, in theory, c_ya and c_ay should be equal due to the reciprocal nature of contacts
      #However, they were different, which may be due to.. 
    
    
    #---Force of infection acting on susceptible children
    foi_y <- u_y * c_ya * k * (Ip_a + Ic_a + f * Is_a)/N_a + u_y * c_yy * k * (Ip_y + Ic_y + f * Is_y)/N_y
    
    #---Force of infection acting on susceptible adults
    foi_a <- u_a * c_aa * k * (Ip_a + Ic_a + f * Is_a)/N_a + u_a * k * c_ay * (Ip_y + Ic_y + f * Is_y)/N_y
    
    #--The differential equations
    
    #---Rate of change in children
    dE_y <- foi_y * S_y - infous_rate * E_y 
    dIp_y <- infous_rate * p * E_y - cl_infous_rate * Ip_y
    dIc_y <- cl_infous_rate * Ip_y - rec_cl_rate * Ic_y
    dIs_y <- infous_rate * (1-p)* E_y- rec_sub_rate * Is_y
    dR_y <- rec_cl_rate * Ic_y + rec_sub_rate * Is_y
    dS_y <- -foi_y  * S_y 
    
    
    #---Rate of change in adults
    dE_a <- foi_a * S_a - infous_rate * E_a 
    dIp_a <- infous_rate * p * E_a - cl_infous_rate * Ip_a
    dIc_a <- cl_infous_rate * Ip_a - rec_cl_rate * Ic_a
    dIs_a <- infous_rate * (1-p) * E_a - rec_sub_rate * Is_a
    dR_a <- rec_cl_rate * Ic_a + rec_sub_rate * Is_a
    dS_a <- -foi_a  * S_a 
    
    #--Output
    return(list(c(dIc_y, dE_y, dIp_y, dIs_y, dR_y, dS_y, dIc_a, dE_a, dIp_a, dIs_a, dR_a, dS_a)))
  })
}

#--MODEL OUTPUT
output_sim_c3.1_long <- as.data.frame(ode(y = initial, 
                                times = times, 
                                func = mod_sim_c3.1_long,
                                parms = parameters))


#--Add 'reported' daily incidence estimated by the model
output2_sim_c3.1_long <- output_sim_c3.1_long %>%
  mutate(
    D_y = Ic_y * parameters['frac_rep'],
    D_a = Ic_a * parameters['frac_rep'],
    D_t = D_y + D_a,
    Dy_per_m = D_y / parameters['popn_y'] * 1E6,
    Da_per_m = D_a / parameters['popn_a'] * 1E6,
    Dt_per_m = D_t / parameters['popn'] * 1E6
  )


