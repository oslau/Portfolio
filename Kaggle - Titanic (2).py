
# coding: utf-8

# In[53]:

import csv as csv
import numpy as np
csv_file_object = csv.reader(open(                '../Documents/Kaggle/Titanic/train.csv', 'rb'))
header = csv_file_object.next()
data = []
for row in csv_file_object:
    data.append(row)
data = np.array(data)

print data


# In[8]:

data[0:15,5]


# In[9]:

type(data[0::,5])


# In[10]:

ages_onboard = data[0::,5].astype(np.float)


# In[11]:

#above, runs into error when trying to convert empty cell to float


# In[54]:

import pandas as pd
import numpy as np

#creates dataframe
df = pd.read_csv('../Documents/Kaggle/Titanic/train.csv', header=0)
df[0:5]


# In[32]:

df.head(3)


# In[33]:

type(df)
df.dtypes


# In[7]:

df.describe()


# In[34]:

df['Age'][0:10]


# In[9]:

df.Age[0:10]


# In[28]:

df.Cabin[0:10]


# In[30]:

df.Age.mean()
#or, df['Age'].mean()


# In[35]:

#prints list of 3 variables: df[['Sex','Pclass','Age']]


# In[35]:

df[df['Age']>60].isnull()[['Sex', 'Pclass', 'Age', 'Survived']]


# In[55]:

for i in range(1,4):
    print i, len(df[(df['Sex'] == 'male') & (df['Pclass'] == i)])


# In[12]:

import pylab as P
df['Age'].hist()
P.show()


# In[13]:

df['Age'].dropna().hist(bins=16, range=(0,80), alpha = .5)
P.show()


# In[56]:

df['Gender'] = 4


# In[47]:

df.head(2)


# In[57]:

#defines gender as the first letter, upper-case, of the variable Sex
df['Gender'] = df['Sex'].map(lambda x: x[0].upper())


# In[58]:

#maps m/f to binary 1/0
df['Gender'] = df['Sex'].map( {'female': 0, 'male': 1} ).astype(int)


# In[59]:

df.head(3)


# In[79]:

#do something similar for Embarked values
#df['Embarked'].unique()
df['Embarked2'] = df['Embarked']
#fill NaN values with 'NA'
df['Embarked2'].fillna('NA', inplace=True)
df['Embarked2'] = df['Embarked2'].map( {'NA': 0, 'S': 1, 'C': 2, 'Q': 3} ).astype(int)


# In[81]:

#calculate median ages 
median_ages = np.zeros((2,3))
for i in range(0,2):
    #note: range(start, stop) creates a range of numbers inclusive of start, exclusive of stop
    for j in range(0,3):
        #print "i = %s, j = %s" %(i, j)
        median_ages[i, j] = df[(df['Gender'] == i)                               & (df['Pclass'] == j + 1)]['Age'].dropna().median()


# In[82]:

median_ages


# In[83]:

df['AgeFill'] = df['Age']
df[ df['Age'].isnull() ][['PassengerId','Gender','Pclass','Age','AgeFill']].head(10)


# In[84]:

#fill NaN entries with median age for gender and pclass
#df['Pclass'] = df['Pclass'].astype(int) convert class just in case
for i in range(0,2):
    for j in range(0,3):
        df.loc[(df.Age.isnull()) & (df.Gender == i) & (df.Pclass == j+1),'AgeFill']             = median_ages[i, j]

df[ df['Age'].isnull() ][['PassengerId','Gender','Pclass','Age','AgeFill']].head(10)


# In[85]:

df['AgeIsNull'] = pd.isnull(df.Age).astype(int)


# In[86]:

df['FamilySize'] = df['Parch'] + df['SibSp']


# In[87]:

#df["Pclass"] = df["Pclass"].astype(int)
df['Age*Class'] = df.AgeFill * df.Pclass


# In[80]:




# In[ ]:



