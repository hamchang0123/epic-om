setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om/main')
source('./parameters.R')
source('./r_baseline.R')

#####################No c4.4
#---No c4.4 day 19 betas
parameters['b_aa_d19_no_c4.4'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2_max'] ) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d19_no_c4.4'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d19_no_c4.4'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['intv_c1.1'] *parameters['c_yy']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d19_no_c4.4'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']) * parameters['k'] / parameters['popn_y']

#---No c4.4 day 19 reproduction numbers
parameters['r_aa_d19_no_c4.4'] = parameters['b_aa_d19_no_c4.4'] * Sa_19 * parameters['sub_infous_period']
parameters['r_ay_d19_no_c4.4'] = parameters['b_ay_d19_no_c4.4'] * Sa_19 * parameters['sub_infous_period']
parameters['r_yy_d19_no_c4.4'] = parameters['b_yy_d19_no_c4.4'] * Sy_19 * parameters['sub_infous_period']
parameters['r_ya_d19_no_c4.4'] = parameters['b_ya_d19_no_c4.4'] * Sy_19 * parameters['sub_infous_period']

#---No c4.4 day 0-21 betas
parameters['b_aa_d0_21_no_c4.4'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2']) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d0_21_no_c4.4'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d0_21_no_c4.4'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['intv_c1.1'] *parameters['c_yy']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d0_21_no_c4.4'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']) * parameters['k'] / parameters['popn_y']

#---No c4.4 day 0-21 reproduction numbers
parameters['r_aa_d0_21_no_c4.4'] = parameters['b_aa_d0_21_no_c4.4'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_ay_d0_21_no_c4.4'] = parameters['b_ay_d0_21_no_c4.4'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_yy_d0_21_no_c4.4'] = parameters['b_yy_d0_21_no_c4.4'] * initial['S_y'] * parameters['sub_infous_period']
parameters['r_ya_d0_21_no_c4.4'] = parameters['b_ya_d0_21_no_c4.4'] * initial['S_y'] * parameters['sub_infous_period']

#---No c4.4 day 22-43 betas
parameters['b_aa_d22_43_no_c4.4'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2']) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d22_43_no_c4.4'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d22_43_no_c4.4'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d22_43_no_c4.4'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']) * parameters['k'] / parameters['popn_y']

#---No c4.4 day 22-43 reproduction numbers
parameters['r_aa_d22_43_no_c4.4'] = parameters['b_aa_d22_43_no_c4.4'] * Sa_22 * parameters['sub_infous_period']
parameters['r_ay_d22_43_no_c4.4'] = parameters['b_ay_d22_43_no_c4.4'] * Sa_22 * parameters['sub_infous_period']
parameters['r_yy_d22_43_no_c4.4'] = parameters['b_yy_d22_43_no_c4.4'] * Sy_22 * parameters['sub_infous_period']
parameters['r_ya_d22_43_no_c4.4'] = parameters['b_ya_d22_43_no_c4.4'] * Sy_22 * parameters['sub_infous_period']


names <- names(parameters)

df_param <- data.frame(
  variable = names,
  value = parameters)

r_param <- as.data.frame(t(df_param)) %>%
  select(contains('r_'))%>%
  select(contains('_d'))%>%
  select(contains('_c')) 

r_param <- as.data.frame(t(r_param)) 

export(r_param, 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/r_no_c4.4.csv')
