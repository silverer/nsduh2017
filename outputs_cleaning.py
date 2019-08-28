# -*- coding: utf-8 -*-
"""
Created on Tue Aug 27 10:51:36 2019

@author: ers2244

Cleans all multivariate regression results from R code
"""

import pandas as pd


def get_cov_type(x):
   cov_types = ['(Intercept)', 'sex', 'employment', 'edF', 
                'race', 'age',
                'hasInsurance', 'marStat', 'metro', 'incomeF', 'health']
   
   for i in range(0, len(cov_types)):
       if cov_types[i] in x:
           return cov_types[i]

def get_cov_levels(x):
    cov_types = ['(Intercept)', 'sex', 'employment', 'edF', 'race', 'age',
                'hasInsurance', 'marStat', 'metro', 'incomeF', 'health']
    
    for i in range(0, len(cov_types)):
        
        if cov_types[i] in x:
            if 'metro' in x:
                return x
            if '(Intercept)' in x:
                return x
            x = x.replace(cov_types[i], '')
            return x
        
def round_val(x):
    return str(round(x, 2))

old = pd.read_csv('all_outputs_multivariate_v2.csv')

old.drop(columns = ['Unnamed: 0'], inplace = True)

old_names = old['outcome'].drop_duplicates(keep='first').to_list()

new_names = ['Lifetime POU', 'Lifetime POMU', 'Past year POU',
             'Past Year POMU', 'Dependence among past-year POU',
             'Source-one doctor', 'Source-multiple doctors',
             'Source-drug dealer', 'Source-bought from friend/relative',
             'Source-took from friend/relative', 'Source-got from friend/relative',
             'Main Reason-Pain', 'Main Reason-Emotions', 'Main Reason-Get High',
             'Main Reason-Relax', 'Main Reason-Experiment', 'Main Reason-Sleep']


name_dict = dict(zip(old_names, new_names))

old['new_names'] = old['outcome'].map(name_dict)
#print(old['new_names'])
#print(old['outcome'])

old['cov_type'] = old['covariate'].apply(get_cov_type)
#print(old['cov_type'])
old['cov_level'] = old['covariate'].apply(get_cov_levels)
#print(old['cov_level'])


#ref level for insurance is NO INSURANCE
#ref level for mar stat is UNMARRIED
data = pd.read_csv('opioids_cleaned_data_v2.csv')
var_types = ['sexF','employment', 'education', 'raceRecode', 'ageCourseF',
             'incomeF', 'healthStatus', 'hasInsurance', 'metro', 'marStat']
refs = []
var_vals = {}
for v in var_types:
    var_vals[v] = data[v].drop_duplicates(keep = 'first').to_list()
    if v == 'sexF':
        refs.append('Women')
    elif v == 'hasInsurance':
        var_vals[v] = ['uninsured', 'insured']
        refs.append('uninsured')
    elif v == 'marStat':
        var_vals[v] = ['unmarried', 'married']
        refs.append('unmarried')
    elif v == 'metro':
        var_vals[v] = ['non-metro', 'metro']
        refs.append('non-metro')
    elif v == 'raceRecode':
        var_vals[v]= data[v].drop_duplicates(keep = 'first').to_list()
        refs.append('asian')
    elif v == 'ageCourseF':
        var_vals[v]= data[v].drop_duplicates(keep = 'first').to_list()
        refs.append('18-21')
    elif v == 'incomeF':
        var_vals[v]= data[v].drop_duplicates(keep = 'first').to_list()
        refs.append('<20k')
    else:
        var_vals[v] = data[v].drop_duplicates(keep = 'first').to_list()
        refs.append(var_vals[v][0])
    
#print(var_vals)
#print(refs)

var_vals['(Intercept)'] = ['(Intercept)']
cov_types = ['sex','employment', 'edF', 'race', 'age','incomeF', 'health',
             'hasInsurance', 'metro', 'marStat', '(Intercept)']
results_vars_dict = dict(zip(cov_types, var_vals))
ref_dict = dict(zip(cov_types, refs))




#need to add reference levels to every outcome and cov type
k = 0
for k in range(0, len(new_names)):
    i = 0
    for i in range(0, len(cov_types)):
        if cov_types[i] == '(Intercept)':
            skip = True
        else:
            old.loc[len(old), 'cov_type'] = cov_types[i]
            old.loc[len(old)-1, 'cov_level'] = ref_dict[cov_types[i]]
            old.loc[len(old)-1, 'OR'] = 1
            old.loc[len(old)-1, 'new_names'] = new_names[k]
    
old['formatted_or'] = old['OR'].apply(round_val)
old['formatted_low_bound'] = old['CI_lower_95'].apply(round_val)
old['formatted_up_bound'] = old['CI_upper_95'].apply(round_val)

old['new_ci'] = old['formatted_or'] + ' (' + old['formatted_low_bound']+ ' - '+ old['formatted_up_bound'] + ')'
print(old['new_ci'])



new = pd.DataFrame(columns = new_names)
cov_types = ['sex','employment', 'edF', 'race', 'age','incomeF', 'health',
             'hasInsurance', 'metro', 'marStat', '(Intercept)']
results_vars_dict = dict(zip(cov_types, var_vals.values()))
levels = list(results_vars_dict.values())
new_cov_list = []
for sublist in levels:
    for item in sublist:
        new_cov_list.append(item)

new_cov_list.append('(Intercept)')
new['Covariate'] = new_cov_list
new = new.dropna(subset = ['Covariate'])
new['temp_covariate'] = new['Covariate'].copy()
new = new.set_index('Covariate')


i = 0
for i in range(0, len(new_names)):
    
    temp = old[old['new_names'] == new_names[i]]
    temp = temp.set_index('cov_level')
    for c in new.index:
        new.loc[c, new_names[i]] = temp.loc[c, 'new_ci']
        
        if 'nan' in temp.loc[c, 'new_ci']:
            new.loc[c, new_names[i]] = '1.0 (ref)'
    
 
new.to_csv('formatted_all_outputs.csv', index = False)












