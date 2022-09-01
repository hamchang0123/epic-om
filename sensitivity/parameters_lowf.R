##-1. Inputs: parameters_lowf
options(digits = 4)
parameters_lowf <- c()

#--Infection-related parameters_lowf: duration
parameters_lowf['preinfous_period'] = 2
parameters_lowf['precl_infous_period'] = 6 * 0.5
parameters_lowf['cl_infous_period'] = 6 * 0.5
parameters_lowf['sub_infous_period'] = 6


#--Infection-related parameters_lowf: rate
parameters_lowf['infous_rate']= 1/ parameters_lowf['preinfous_period']
parameters_lowf['cl_infous_rate']= 1/ parameters_lowf['precl_infous_period']
parameters_lowf['rec_cl_rate']= 1/ parameters_lowf['cl_infous_period']
parameters_lowf['rec_sub_rate']= 1/ parameters_lowf['sub_infous_period']

#--Infection-related parameters_lowf: adjustment factors
parameters_lowf['f'] = 0.35
#relative infectiousness of subclinical cases
parameters_lowf['u_a'] = 1
#susceptibility to sars-cov-2 infection in adults
parameters_lowf['u_y'] = 0.56
#susceptibility to sars-cov-2 infection in children
parameters_lowf['p'] = 0.6
#probability of clinical symptoms on infection 
#synonymous with frac_sympt in the pandemic example (proportion of infected individuals who experience clinical symptoms)
parameters_lowf['k'] = 0.083
#transmission risk
#k = 1.5 / (param1['sub_infous_period'] * (param2['popn_y']+param2['popn_y']))
parameters_lowf['frac_rep'] = 0.11
#Proportion of infected individuals who are reported as cases

#--Contact-related parameters_lowf
parameters_lowf['c_aa'] = 9.67
#Mean number of contacts that an adult has with another adult per day
parameters_lowf['c_ay'] = 4.63
#Mean number of contacts that an adult1 has with a child per day
parameters_lowf['c_yy'] = 9.63
#Mean number of contacts that a child has with another child per day
parameters_lowf['c_ya'] = 2.43
#Mean number of contacts that a child has with an adult per day

#--Vaccination-related parameters_lowf
parameters_lowf['vacc_cov_y'] = 0.3362316
parameters_lowf['vacc_cov_a'] = 0.9627143
parameters_lowf['vacc_eff'] = 0.9738687 * 0.655 + 0.02613126 * 0.489

#--Demographic parameters_lowf
parameters_lowf['popn_y'] = 42223753
parameters_lowf['popn_a'] = 215894499
parameters_lowf['popn'] = parameters_lowf['popn_y']+parameters_lowf['popn_a']

#--Observed data
parameters_lowf['rep_y0'] = 1166
#observed incidence cases in children on day 0 
parameters_lowf['rep_a0'] = 3024
#observed incidence cases in adults on day 0 

#--Intervention-related parameters_lowf
parameters_lowf['intv_c1.1'] = 0.7492/0.8966

parameters_lowf['c_aa_c2.2'] = 9.466173286
parameters_lowf['c_aa_c2.2_max'] = 8.318489286

parameters_lowf['intv_c4.4_a'] = 0.8041996
parameters_lowf['intv_c4.4_y'] = 0.851311
parameters_lowf['intv_c4.3_a'] = 1-((1-parameters_lowf['intv_c4.4_a'])*(0.6557336/6.367187))
parameters_lowf['intv_c4.3_y'] = 1-((1-parameters_lowf['intv_c4.4_y'])*(0.6557336/6.367187))

#parameters_lowf['intv_c3.2_a'] = parameters_lowf['intv_c4.4_a’]
#parameters_lowf['intv_c3.2_y'] = parameters_lowf['intv_c4.4_y’]

parameters_lowf['intv_c3.1_a'] = 0.8893738
parameters_lowf['intv_c3.1_y'] = 0.9030866
parameters_lowf['intv_c3.1_a_up'] = 0.8465918
parameters_lowf['intv_c3.1_y_up'] = 0.8748755
parameters_lowf['intv_c3.1_a_down1'] = 0.9166493
parameters_lowf['intv_c3.1_y_down1'] = 0.9243279
parameters_lowf['intv_c3.1_a_down2'] = 0.9874793
parameters_lowf['intv_c3.1_y_down2'] = 0.984069


names(parameters_lowf)
parameters_lowf

##-2. Initial values

#--Initial values
initial <- c()

#--Children
initial['Ic_y'] = parameters_lowf['rep_y0'] * parameters_lowf['cl_infous_period']/parameters_lowf['frac_rep']
#divided by fraction reported to obtain reporting-adjusted clinical infection cases 

initial['E_y'] = initial['Ic_y']*parameters_lowf['preinfous_period']/(parameters_lowf['preinfous_period']+parameters_lowf['cl_infous_period'])
#assumed that the number of pre-infectious (infected) individuals would be proportional to the number of clinically infectious individuals and the ratio of preinfectious period to clinically infectious period

initial['Ip_y'] =initial['Ic_y']*parameters_lowf['precl_infous_period']/(parameters_lowf['precl_infous_period']+parameters_lowf['cl_infous_period'])
#assumed that the number of pre-infectious (infected) individuals would be proportional to the number of clinically infectious individuals and the ratio of preclinically infectious period to clinically infectious period

initial['Is_y'] = initial['Ic_y']*parameters_lowf['sub_infous_period']/(parameters_lowf['cl_infous_period']+parameters_lowf['sub_infous_period'])
#assumed that the number of pre-infectious (infected) individuals would be proportional to the number of clinically infectious individuals and the ratio of subinfectious period to clinically infectious period

initial['R_y'] = parameters_lowf['popn_y']*parameters_lowf['vacc_cov_y']*parameters_lowf['vacc_eff']

initial['S_y'] = parameters_lowf['popn_y'] - initial['E_y'] - initial['Ip_y'] - initial['Ic_y'] - initial['Is_y'] - initial['R_y']

#--Adults
initial['Ic_a']= parameters_lowf['rep_a0'] * parameters_lowf['cl_infous_period']/parameters_lowf['frac_rep']

initial['E_a'] = initial['Ic_a']*parameters_lowf['preinfous_period']/(parameters_lowf['preinfous_period']+parameters_lowf['cl_infous_period'])

initial['Ip_a'] = initial['Ic_a']*parameters_lowf['precl_infous_period']/(parameters_lowf['precl_infous_period']+parameters_lowf['cl_infous_period'])

initial['Is_a'] =initial['Ic_a']*parameters_lowf['sub_infous_period']/(parameters_lowf['cl_infous_period']+parameters_lowf['sub_infous_period'])

initial['R_a'] = parameters_lowf['popn_a'] * parameters_lowf['vacc_cov_a'] * parameters_lowf['vacc_eff']

initial['S_a'] = parameters_lowf['popn_a'] - initial['E_a'] - initial['Ip_a'] - initial['Ic_a'] - initial['Is_a'] -initial['R_a']

names(initial)
initial


##-3. Time steps

#--Run simulation between Jan. 16 and June 30 (day 0 to 165)
as.Date('2022-06-30') - as.Date('2022-01-16')
#165 days between Jan. 16 and June 30

times <- seq(from = 0, to = 165)