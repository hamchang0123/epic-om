# EPIC-OM
## Effect of Public health Interventions on Covid-19 transmission in south korea after the dominantion of the OMicron variant (B.1.1.529)

Code for an MSc Epidemiology summer project

### :rocket:Quick start guide

This repository has four folders mainly: 
1. **Data**, storing the datasets used for the project
2. **Main**, storing the scripts for model simulation and fitting
3. **Parameter**, storing the scripts for parameter estimation
4. **Sensitivity**, storing the scripts used in parameter sensitivity analysis for 
  - relative infectiousness of subclinical infections
  - measure that recommends the cancellation of public events

### :bulb:Coding

The measures in the scripts were coded as follows:
1. **c1.1**, corresponding to the measure that recommended the school closure
2. **c2.2**, corresponding to the measure that required the cancellation of workplace closure
3. **c4.4**, corresponding to the measure that required the restriction of gatherings to 10 people or fewer
4. **c4.3**, corresponding to the measure that required the restriction of gatherings to 100 people or fewer
5. **c3.2**, corresponding to the measure that required the cancellation of public events
6. **c3.1**, corresponding to the measure that recommended the cancellation of public events 

### :mag:Main plot
![main plot](https://github.com/hamchang0123/epic-om/blob/main/plot.png?raw=true)
- (A) Without c1.1, (B) Without c2.2, (C) Without c4.4, (D) Without c3.1 