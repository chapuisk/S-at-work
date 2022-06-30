import pandas as pd
import numpy as np

from sklearn import linear_model as lm
from statsmodels.miscmodels.ordinal_model import OrderedModel as om

import matplotlib.pyplot as plt


df = pd.read_csv("../../includes/ewcs2015_short_fr.csv")

X = df[['Q1', 'Q2b', 'Q24', 'Q104_euro', 'autonomy', 'interaction', 'intensity',
        'meaningful']]
y = df['Q88']

##
# linear regression model

model = lm.LinearRegression()
model.fit(X, y)

print(model.coef_)
print(model.intercept_)
print(model.score(X, y))

##
# ordered logistic regression model

prob_model = om(df['Q88'], df[['Q1', 'Q2b', 'Q24', 'Q104_euro', 'autonomy', 'interaction', 'intensity', 'meaningful']],
                distr='probit')
res_prob = prob_model.fit(method='bfgs')
print(res_prob.summary())

##
# Viz

df_w = df.loc[df['Q2a'].isin(['F'])]
df_m = df.loc[df['Q2a'].isin(['M'])]
df_a24 = df.loc[df['Q2b'] <= 24]
df_a54 = df.loc[(df['Q2b'] > 24) & (df['Q2b'] <= 54)]
df_ao = df.loc[df['Q2b'] > 54]



#fig, ax = plt.subplots()
plt.scatter(df_w['Q2b'], df_w['autonomy'], s=df_w['Q104_euro']/25, c=df_w['Q88'], cmap=plt.cm.get_cmap('RdYlGn_r'),
            alpha=0.5)
plt.plot(res_prob.model.predict())
#plt.set_title('Women/age satisfaction')
plt.colorbar()
plt.show()
