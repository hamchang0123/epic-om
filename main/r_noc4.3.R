setwd('C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/script/fin/epic-om/main')
source('./parameters.R')
source('./r_baseline.R')

pacman::p_load(tidyverse, rio, magrittr, ggpubr)

sub_weight_Ia = 0.5 * initial['Is_a'] / (initial['Ic_a'] + initial['Ip_a'] + initial['Is_a'])
pre_cl_weight_Ia = 1 * (initial['Ip_a'] + initial['Ic_a']) /(initial['Ic_a'] + initial['Ip_a'] + initial['Is_a'])
sub_weight_Iy = 0.5 * initial['Is_y'] / (initial['Ic_y'] + initial['Ip_y'] + initial['Is_y'])
pre_cl_weight_Iy = 1 * (initial['Ip_y'] + initial['Ic_y']) /(initial['Ic_y'] + initial['Ip_y'] + initial['Is_a'])

#---Betas
parameters['b_aa'] = (sub_weight_Ia + pre_cl_weight_Ia) * parameters['u_a'] * parameters['c_aa'] * parameters['k'] / parameters['popn_a']
parameters['b_ay'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * parameters['c_ay'] * parameters['k'] / parameters['popn_a']
parameters['b_yy'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * parameters['c_yy'] * parameters['k'] / parameters['popn_y']
parameters['b_ya'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * parameters['c_ya'] * parameters['k'] / parameters['popn_y']

#---Reproduction numbers
parameters['r_aa'] = parameters['b_aa'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_ay'] = parameters['b_ay'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_yy'] = parameters['b_yy'] * initial['S_y'] * parameters['sub_infous_period']
parameters['r_ya'] = parameters['b_ya'] * initial['S_y'] * parameters['sub_infous_period']

#---Day 19 betas
parameters['b_aa_d19'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2_max']  ) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d19'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']  ) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d19'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['intv_c1.1'] * parameters['c_yy'] ) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d19'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']  ) * parameters['k'] / parameters['popn_y']

#---Day 19 reproduction numbers
parameters['r_aa_d19'] = parameters['b_aa_d19'] * Sa_19 * parameters['sub_infous_period']
parameters['r_ay_d19'] = parameters['b_ay_d19'] * Sa_19 * parameters['sub_infous_period']
parameters['r_yy_d19'] = parameters['b_yy_d19'] * Sy_19 * parameters['sub_infous_period']
parameters['r_ya_d19'] = parameters['b_ya_d19'] * Sy_19 * parameters['sub_infous_period']

#---Day 0-21 betas
parameters['b_aa_d0_21'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2']  ) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d0_21'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']  ) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d0_21'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['intv_c1.1'] * parameters['c_yy'] ) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d0_21'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']  ) * parameters['k'] / parameters['popn_y']

#---Day 0-21 reproduction numbers
parameters['r_aa_d0_21'] = parameters['b_aa_d0_21'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_ay_d0_21'] = parameters['b_ay_d0_21'] * initial['S_a'] * parameters['sub_infous_period']
parameters['r_yy_d0_21'] = parameters['b_yy_d0_21'] * initial['S_y'] * parameters['sub_infous_period']
parameters['r_ya_d0_21'] = parameters['b_ya_d0_21'] * initial['S_y'] * parameters['sub_infous_period']

#---Day 22-43 betas
parameters['b_aa_d22_43'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2']  ) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d22_43'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']  ) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d22_43'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy'] ) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d22_43'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']  ) * parameters['k'] / parameters['popn_y']

#---Day 22-43 reproduction numbers
parameters['r_aa_d22_43'] = parameters['b_aa_d22_43'] * Sa_22 * parameters['sub_infous_period']
parameters['r_ay_d22_43'] = parameters['b_ay_d22_43'] * Sa_22 * parameters['sub_infous_period']
parameters['r_yy_d22_43'] = parameters['b_yy_d22_43'] * Sy_22 * parameters['sub_infous_period']
parameters['r_ya_d22_43'] = parameters['b_ya_d22_43'] * Sy_22 * parameters['sub_infous_period']


#---Day 44-91 betas
parameters['b_aa_d44_91'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa_c2.2']   * parameters['intv_c3.1_a']) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d44_91'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']   * parameters['intv_c3.1_y']) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d44_91'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy']  *parameters['intv_c3.1_y']) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d44_91'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']   * parameters['intv_c3.1_a']) * parameters['k'] / parameters['popn_y']

#---Day 44-91 reproduction numbers
parameters['r_aa_d44_91'] = parameters['b_aa_d44_91'] * Sa_44 * parameters['sub_infous_period']
parameters['r_ay_d44_91'] = parameters['b_ay_d44_91'] * Sa_44 * parameters['sub_infous_period']
parameters['r_yy_d44_91'] = parameters['b_yy_d44_91'] * Sy_44 * parameters['sub_infous_period']
parameters['r_ya_d44_91'] = parameters['b_ya_d44_91'] * Sy_44 * parameters['sub_infous_period']

#---Day 92-165 betas
parameters['b_aa_d_92_165'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_a'] * (parameters['c_aa']  ) * parameters['k'] / parameters['popn_a']
parameters['b_ay_d_92_165'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_a'] * (parameters['c_ay']  ) * parameters['k'] / parameters['popn_a']
parameters['b_yy_d_92_165'] = (sub_weight_Iy + pre_cl_weight_Iy) *parameters['u_y'] * (parameters['c_yy'] ) * parameters['k'] / parameters['popn_y']
parameters['b_ya_d_92_165'] = (sub_weight_Ia + pre_cl_weight_Ia) *parameters['u_y'] * (parameters['c_ya']  ) * parameters['k'] / parameters['popn_y']

#---Day 92-165 reproduction numbers
parameters['r_aa_d_92_165'] = parameters['b_aa_d_92_165'] * Sa_92 * parameters['sub_infous_period']
parameters['r_ay_d_92_165'] = parameters['b_ay_d_92_165'] * Sa_92 * parameters['sub_infous_period']
parameters['r_yy_d_92_165'] = parameters['b_yy_d_92_165'] * Sy_92 * parameters['sub_infous_period']
parameters['r_ya_d_92_165'] = parameters['b_ya_d_92_165'] * Sy_92 * parameters['sub_infous_period']

names <- names(parameters)

df_param <- data.frame(
  variable = names,
  value = parameters)

r_param <- as.data.frame(t(df_param)) %>%
  select(contains('r_'))%>%
  select(contains('_d')) 

r_param <- as.data.frame(t(r_param)) 

export(r_param, 'C:/Users/hanna/OneDrive - London School of Hygiene and Tropical Medicine/3_summer/output/r_noc4.3_long.csv')
  


