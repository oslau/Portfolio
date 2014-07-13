
# coding: utf-8

# In[64]:

#import relevant packages and datasets
import csv as csv
import numpy as np
import pandas as pd

# Lets apply a random forest!
#import package
from sklearn.ensemble import RandomForestClassifier
#needs imputs as a np.array

#This data was formatted in 'Kaggle - Titanic (2)'
#Variables: PassengerId, Survived, Pclass, SibSp, Parch, Fare, Gender
#           Embarked2, AgeFill, AgeIsNull, FamilySize, Age*Class
train_object = csv.reader(open('../Documents/Kaggle/Titanic/train_data.csv', 'rb'))
train_header = train_object.next()
train_data = []
for row in train_object:
    train_data.append(row)
train_data = np.array(train_data)


# In[65]:

#There's an extra passenger column, delete
train_data = np.delete(train_data,[0,1], 1)


# In[66]:

train_header = np.delete(train_header, [0,1])


# In[67]:

train_header


# In[68]:

#Let's import the test data and format the data accordingly
test_df = pd.read_csv('../Documents/Kaggle/Titanic/test.csv', header=0)
test_df.head(3)


# In[69]:

#define Gender variable
test_df['Gender'] = 4
test_df['Gender'] = test_df['Sex'].map(lambda x: x[0].upper())
test_df['Gender'] = test_df['Sex'].map( {'female': 0, 'male': 1} ).astype(int)


# In[70]:

#define Embarked2 variable
test_df['Embarked2'] = test_df['Embarked']
test_df['Embarked2'].fillna('NA', inplace=True)
test_df['Embarked2'] = test_df['Embarked2'].map( {'NA': 0, 'S': 1, 'C': 2, 'Q': 3} ).astype(int)


# In[71]:

#Define AgeFill
median_ages = np.zeros((2,3))
for i in range(0,2):
       for j in range(0,3):
        median_ages[i, j] = test_df[(test_df['Gender'] == i)                               & (test_df['Pclass'] == j + 1)]['Age'].dropna().median()
test_df['AgeFill'] = test_df['Age']
for i in range(0,2):
    for j in range(0,3):
        test_df.loc[(test_df.Age.isnull())                     & (test_df.Gender == i)                     & (test_df.Pclass == j+1),'AgeFill']             = median_ages[i, j]


# In[72]:

#Define AgeIsNull
test_df['AgeIsNull'] = pd.isnull(test_df.Age).astype(int)


# In[73]:

#Define FamilySize
test_df['FamilySize'] = test_df['Parch'] + test_df['SibSp']


# In[74]:

#Define Age*Class
test_df['Age*Class'] = test_df.AgeFill * test_df.Pclass


# In[75]:

#Drop unused variables
PassengerId = test_df['PassengerId']
test_df = test_df.drop(['PassengerId','Age','Name', 'Sex', 'Ticket',                        'Cabin', 'Embarked'], axis =1)


# In[76]:

#Fill null values in Fare
test_df['Fare'].fillna(0, inplace=True)


# In[77]:

#Put values into an array
test_data = test_df.values
test_data


# In[78]:

test_df.dtypes


# In[79]:

# Lets apply a random forest!
#import package
from sklearn.ensemble import RandomForestClassifier

#n_estimators is number of trees in the forest
forest = RandomForestClassifier(n_estimators=100)#creates the object
forest = forest.fit(train_data[0::,1::], train_data[0::,0])


# In[80]:

#For QA purposes
#train_data[0]


# In[81]:

#For QA purposes
test_data[0]


# In[95]:

#This predicts survival
output = forest.predict(test_data)


# In[96]:

output = np.vstack([PassengerId, output]).T


# In[97]:

output


# In[98]:

with open("../Documents/Kaggle/Titanic/prediction_v1.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(output)


# In[ ]:



