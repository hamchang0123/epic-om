source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/r_baseline.R')

########No c3.1
#---No c3.1 day 44-91 betas
parameters['b_aa_d44_91_no_c3.1'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2']) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d44_91_no_c3.1'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d44_91_no_c3.1'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d44_91_no_c3.1'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']) * parameters['k'] / parameters['popn_y']

#---No c3.1 44-91 reproduction numbers
parameters['r_aa_d44_91_no_c3.1'] = parameters['b_aa_d44_91_no_c3.1'] * Sa_44 * parameters['sub_infous_period']
parameters['r_ay_d44_91_no_c3.1'] = parameters['b_ay_d44_91_no_c3.1'] * Sa_44 * parameters['sub_infous_period']
parameters['r_yy_d44_91_no_c3.1'] = parameters['b_yy_d44_91_no_c3.1'] * Sy_44 * parameters['sub_infous_period']
parameters['r_ya_d44_91_no_c3.1'] = parameters['b_ya_d44_91_no_c3.1'] * Sy_44 * parameters['sub_infous_period']



names <- names(parameters)

df_param <- data.frame(
  variable = names,
  value = parameters)

r_param <- as.data.frame(t(df_param)) %>%
  select(contains('r_'))%>%
  select(contains('_d'))%>%
  select(contains('_c')) 

r_param <- as.data.frame(t(r_param)) 

export(r_param, 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/r_no_c3.1.csv')
