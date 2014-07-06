
# coding: utf-8

## Exploratory Data Analysis

# In[113]:

#import relevant packages
import csv as csv
import numpy as np


# In[115]:

#open file
csv_file_object = csv.reader(open(                '../Documents/Kaggle/Titanic/train.csv','rb'))
#defines header - include variable names
header = csv_file_object.next()
print header


# In[116]:

#create empty variable
data = []
#load data, appending row by row
for row in csv_file_object:
    data.append(row)
#converts list to an array
data = np.array(data)


# In[117]:

#print sample data
print data[1]
print data[1][3]


# In[118]:

#playing with the size(), sum() function
#size of data takes count of all rows, over column 1
num_passenger = np.size(data[0::,1].astype(np.float))
num_survived = np.sum(data[0::,1].astype(np.float))
prop_survived = num_survived / num_passenger
print prop_survived


# In[119]:

#select columns conditionally
female_rows = data[0::,4] == "female"
women_onboard = data[female_rows, 0::]
print women_onboard[1]
male_rows = data[0::,4] != "female"
men_onboard = data[male_rows, 0::]
print men_onboard[1]


# In[120]:

#find proportion of men/women survived
men_survived = men_onboard[0::,1].astype(np.float)
women_survived = women_onboard[0::,1].astype(np.float)
prop_women_surv = np.sum(women_survived)/np.size(women_survived)
prop_men_surv = np.sum(men_survived)/np.size(men_survived)


# In[121]:

#print string with proportion
print 'Proportion of women who survived is %s' %prop_women_surv
print 'Proportion of men who survived is %s' %prop_men_surv


# In[122]:

#read in test file
test_file = open('../Documents/Kaggle/Titanic/test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()
print header


# In[123]:

#create open file to write to
prediction_file = open("../Documents/Kaggle/Titanic/genderbasedmodel.csv", "wb")
prediction_file_object = csv.writer(prediction_file)


# In[124]:

#assign survival by gender
#each row in the file, write the following variables
prediction_file_object.writerow(["PassengerId", "Survived"])
#if female, survive = 1; else survive = 0
for row in test_file_object:
    if row[3] == 'female':
        prediction_file_object.writerow([row[0],'1'])
    else:
        prediction_file_object.writerow([row[0],'0'])
test_file.close()
prediction_file.close()


# In[125]:

#bin ticket price into 4 brackets - [(0,9),(10,19),(20,29),(30,high)]
#include class, gender, and ticket price
fare_ceiling = 40
#set all values above fare ceiling to $39
data[data[0::, 9].astype(np.float) > fare_ceiling, 9] = fare_ceiling - 1.0

fare_bracket_size = 10
number_of_price_brackets = fare_ceiling / fare_bracket_size
number_of_classes = len(np.unique(data[0::, 2]))
#create 3D table of zeros
survival_table = np.zeros((2, number_of_classes, number_of_price_brackets))


# In[126]:

#note: efficient looping in accordance with survival_table
for i in xrange(number_of_classes):
    for j in xrange(number_of_price_brackets):
        print 'i=%s, j=%s' %(i,j)
        #subset dataset by following criteria
        women_only_stats = data[                            (data[0::, 4] == "female")                            &(data[0::,2].astype(np.float) == i+1)                            &(data[0::,9].astype(np.float) >= j*fare_bracket_size)                            &(data[0::,9].astype(np.float) < (j+1)*fare_bracket_size),                            1
                ]
        men_only_stats = data[                            (data[0::, 4] != "female")                            &(data[0::,2].astype(np.float) == i+1)                            &(data[0::,9].astype(np.float) >= j*fare_bracket_size)                            &(data[0::,9].astype(np.float) < (j+1)*fare_bracket_size),                            1
                ]
        #respectively fill survival table with average survival by subset
        survival_table[0, i, j] = np.mean(women_only_stats.astype(np.float))
        survival_table[1, i, j] = np.mean(men_only_stats.astype(np.float))


# In[127]:

#for entries that don't match criteria, replace nan entries with 0
survival_table[survival_table != survival_table] = 0
print survival_table


# In[128]:

#values: greater than 0.5 = 1; less than 0.5 = 0
survival_table[survival_table >= 0.5] = 1
survival_table[survival_table < 0.5] = 0
print survival_table


# In[133]:

test_file = open('../Documents/Kaggle/Titanic/test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()
predictions_file = open("../Documents/Kaggle/Titanic/genderclassmodel.csv", "wb")
p = csv.writer(predictions_file)
p.writerow(["PassengerId", "Survived"])

for row in test_file_object:
    for j in xrange(number_of_price_brackets):
    #this creates fare bins based on file information
        try:
            row[8] = float(row[8])
        except: #if no fare info, use passenger class to  bin
            bin_fare = 3 - float(row[1])
            break
        if row[8] > fare_ceiling:
            bin_fare = number_of_price_brackets - 1
            break
        if row[8] >= j * fare_bracket_size             and row[8] < (j+1) * fare_bracket_size:
            bin_fare = j
            break
    #selects survival prediction based on how criteria is matched
        if row[3] == 'female':
            p.writerow([row[0], "%d"                %int(survival_table[0, float(row[1])-1, bin_fare])])
        else:
            p.writerow([row[0], "%d"                %int(survival_table[1, float(row[1])-1, bin_fare])])
test_file.close()
prediction_file.close()


# In[ ]:

#Lingering questions:
#(1) How to select columns by variable name?
#    Very difficult to remember which index refers to which column

