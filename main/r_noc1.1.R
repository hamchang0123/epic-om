source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/parameters.R')
source('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/r_baseline.R')

#################No c1.1

#---No c1.1 Day 19 betas
parameters['b_aa_d19_no_c1.1'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2_max'] * parameters['intv_c4.4_a']) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d19_no_c1.1'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay'] * parameters['intv_c4.4_y']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d19_no_c1.1'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy']* parameters['intv_c4.4_y']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d19_no_c1.1'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya'] * parameters['intv_c4.4_a']) * parameters['k'] / parameters['popn_y']

#---No c1.1 Day 19 reproduction numbers
parameters['r_aa_d19_no_c1.1'] = parameters['b_aa_d19_no_c1.1'] * Sa_19 * parameters['sub_infous_period']
parameters['r_ay_d19_no_c1.1'] = parameters['b_ay_d19_no_c1.1'] * Sa_19 * parameters['sub_infous_period']
parameters['r_yy_d19_no_c1.1'] = parameters['b_yy_d19_no_c1.1'] * Sy_19 * parameters['sub_infous_period']
parameters['r_ya_d19_no_c1.1'] = parameters['b_ya_d19_no_c1.1'] * Sy_19 * parameters['sub_infous_period']

#---No c1.1 Day 0-21 betas
parameters['b_aa_d0_21_no_c1.1'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2'] * parameters['intv_c4.4_a']) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d0_21_no_c1.1'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay'] * parameters['intv_c4.4_y']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d0_21_no_c1.1'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy']* parameters['intv_c4.4_y']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d0_21_no_c1.1'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya'] * parameters['intv_c4.4_a']) * parameters['k'] / parameters['popn_y']

#---No c1.1 Day 0-21 reproduction numbers
parameters['r_aa_d0_21_no_c1.1'] = parameters['b_aa_d0_21_no_c1.1'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_ay_d0_21_no_c1.1'] = parameters['b_ay_d0_21_no_c1.1'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_yy_d0_21_no_c1.1'] = parameters['b_yy_d0_21_no_c1.1'] * initial['S_y'] * parameters['sub_infous_period']
parameters['r_ya_d0_21_no_c1.1'] = parameters['b_ya_d0_21_no_c1.1'] * initial['S_y'] * parameters['sub_infous_period']

names <- names(parameters)

df_param <- data.frame(
  variable = names,
  value = parameters)

r_param <- as.data.frame(t(df_param)) %>%
  select(contains('r_'))%>%
  select(contains('_d'))%>%
  select(contains('_c'))

r_param <- as.data.frame(t(r_param)) 
#r_yy_d19_no_c1.1
#1.062e+00
#r_yy_d19
#8.874e-01
export(r_param, 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/r_no_c1.1.csv')
