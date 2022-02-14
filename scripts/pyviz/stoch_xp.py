import pandas as pd
import matplotlib.pylab as plt
import random
import math
import seaborn as sns

# Data
data = pd.read_csv("../../batch_output/stochastic_xp.csv")

print(data["a index"].drop_duplicates())

dPlot_s = dict()
dPlot_a = dict()
dPlot_g = dict()
for i in range(0, 200):
    sub_data_s = data.loc[0:i, ["s index"]]
    sub_data_a = data.loc[0:i, ["a index"]]
    sub_data_g = data.loc[0:i, ["g index"]]
    dPlot_s[i] = sub_data_s.sem()[0]
    dPlot_a[i] = sub_data_a.sem()[0]
    dPlot_g[i] = sub_data_g.sem()[0]

color = ['chocolate', 'darkgreen', 'orange']
#color = ["#"+''.join([random.choice('0123456789ABCDEF') for j in range(6)])
#            for i in range(3)]

i = 0
l = ["s index", "a index", "g index"]
for p in [dPlot_s, dPlot_a, dPlot_g]:
    lists = sorted(p.items())
    x, y = zip(*lists)
    plt.plot(x, y, color[i], label=l[i])
    i += 1

plt.legend()
plt.xlabel("Number of repetitions")
plt.ylabel("Standard error")
plt.show()
