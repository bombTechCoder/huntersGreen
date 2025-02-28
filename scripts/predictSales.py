# %%
import pandas as pd # type: ignore
import numpy as np # type: ignore
import matplotlib.pyplot as plt # type: ignore
import seaborn as sns # type: ignore

import statsmodels.formula.api as smf # type: ignore
from statsmodels.stats.outliers_influence import variance_inflation_factor as vif # type: ignore
from stargazer.stargazer import Stargazer # type: ignore
from scipy.stats import shapiro, bartlett
from statsmodels.stats.stattools import durbin_watson

from IPython.display import display, HTML # type: ignore
pd.set_option('display.max_columns', None)


# %%
# Bring in the data
df = pd.read_csv('../data/HuntersGreenHomeSales_prepped.csv')

# Have a peak
df.head()


# %%
# show me null data
df.isnull().sum()

# %% [markdown]
# Let's drop the spa and splsale columns before we drop null rows so we don't lose a ton of data.  splsale has been recaptured by breakout columns.

# %%
df = df.drop(['spa', 'splsale'], axis=1)
df = df.dropna()

# %%
df.describe()

# %%
# histograms
plt.rcParams.update({'font.size': 8})
fig, ((Beds, bathsfull, bathstotal, ds_moy, garages, pricesold),
      (listprice, lat, long, lotsqft, sqft, adom),
      (pool_None, roof_Tile, splsal_shortsale, BankOwnedREO, yrblt, lppersqft)) = plt.subplots(nrows=3, ncols=6)

Beds.hist(df['Beds'], 5)
Beds.set_title('Beds', fontsize=8)
bathsfull.hist(df['bathsfull'], 5)
bathsfull.set_title('bathsfull', fontsize=8)
bathstotal.hist(df['bathstotal'], 7)
bathstotal.set_title('bathstotal', fontsize=8)
ds_moy.hist(df['ds_moy'], 12)
ds_moy.set_title('ds_moy', fontsize=8)
garages.hist(df['garages'],5)
garages.set_title('garages', fontsize=8)
pricesold.hist(df['pricesold'], 'auto')
pricesold.set_title('pricesold', fontsize=8)
listprice.hist(df['listprice'], 'auto')
listprice.set_title('listprice', fontsize=8)
lat.hist(df['lat'], 'auto')
lat.set_title('lat', fontsize=8)
long.hist(df['long'], 'auto')
long.set_title('long', fontsize=8)
adom.hist(df['adom'], 'auto')
adom.set_title('adom', fontsize=8)
lotsqft.hist(df['lotsqft'], 'auto')
lotsqft.set_title('lotsqft', fontsize=8)
sqft.hist(df['sqft'], 'auto')
sqft.set_title('sqft', fontsize=8)
pool_None.hist(df['pool_None'], 2)
pool_None.set_title('pool_None', fontsize=8)
roof_Tile.hist(df['roof_Tile'], 2)
roof_Tile.set_title('roof_Tile', fontsize=8)
splsal_shortsale.hist(df['splsal_shortsale'], 2)
splsal_shortsale.set_title('splsal_shortsale', fontsize=8)
BankOwnedREO.hist(df['BankOwnedREO'], 2)
BankOwnedREO.set_title('BankOwnedREO', fontsize=8)
yrblt.hist(df['yrblt'], 'auto')
yrblt.set_title('yrblt', fontsize=8)
lppersqft.hist(df['lppersqft'], 'auto')
lppersqft.set_title('lppersqft', fontsize=8)

pricesold.set_facecolor('#FFFD55')
pricesold.spines['top'].set_color('red')
pricesold.spines['right'].set_color('red')
pricesold.spines['bottom'].set_color('red')
pricesold.spines['left'].set_color('red')
adom.set_facecolor('#FFFD55')
adom.spines['top'].set_color('red')
adom.spines['right'].set_color('red')
adom.spines['bottom'].set_color('red')
adom.spines['left'].set_color('red')
sqft.spines['top'].set_color('red')
sqft.spines['right'].set_color('red')
sqft.spines['bottom'].set_color('red')
sqft.spines['left'].set_color('red')
lotsqft.spines['top'].set_color('red')
lotsqft.spines['right'].set_color('red')
lotsqft.spines['bottom'].set_color('red')
lotsqft.spines['left'].set_color('red')
listprice.spines['top'].set_color('red')
listprice.spines['right'].set_color('red')
listprice.spines['bottom'].set_color('red')
listprice.spines['left'].set_color('red')
fig.tight_layout()
fig.set_figheight(8)
fig.set_figwidth(15)
plt.subplots_adjust(wspace=0.2, hspace=0.2)
plt.show()

# %%
df_corr = df[['Beds', 'bathsfull', 'bathstotal', 'ds_moy', 'garages',
      'listprice', 'lat', 'long', 'lotsqft', 'sqft', 'pool_None', 'roof_Tile',
      'splsal_shortsale', 'BankOwnedREO', 'yrblt', 'lppersqft']]

plt.figure(figsize=(15,15))
sns.heatmap(df_corr.corr(), annot=True, cmap='vlag')

plt.title('Correlation Matrix')
plt.show()

# %%
# let's transform
df['log_sqft'] = np.log1p(df['sqft'])
df['house_age'] = df['pd_year'] - df['yrblt']
df['log_pricesold'] = np.log1p(df['pricesold'])
df['log_listprice'] = np.log1p(df['listprice'])
df['log_lotsqft'] = np.log1p(df['lotsqft'])
df['log_adom'] = np.log1p(df['adom'])
df.describe()
# df.head()

# %%
# updated histograms
plt.rcParams.update({'font.size': 8})
fig, ((Beds, bathsfull, bathstotal, ds_moy, garages, log_pricesold),
      (log_listprice, lat, long, log_lotsqft, log_sqft, log_adom),
      (pool_None, roof_Tile, splsal_shortsale, BankOwnedREO, house_age, lppersqft)) = plt.subplots(nrows=3, ncols=6)

Beds.hist(df['Beds'], 5)
Beds.set_title('Beds', fontsize=8)
bathsfull.hist(df['bathsfull'], 5)
bathsfull.set_title('bathsfull', fontsize=8)
bathstotal.hist(df['bathstotal'], 7)
bathstotal.set_title('bathstotal', fontsize=8)
ds_moy.hist(df['ds_moy'], 12)
ds_moy.set_title('ds_moy', fontsize=8)
garages.hist(df['garages'],5)
garages.set_title('garages', fontsize=8)
log_pricesold.hist(df['log_pricesold'], 'auto')
log_pricesold.set_title('log_pricesold', fontsize=8)
log_listprice.hist(df['log_listprice'], 'auto')
log_listprice.set_title('log_listprice', fontsize=8)
lat.hist(df['lat'], 'auto')
lat.set_title('lat', fontsize=8)
long.hist(df['long'], 'auto')
long.set_title('long', fontsize=8)
log_adom.hist(df['log_adom'], 'auto')
log_adom.set_title('log_adom', fontsize=8)
log_lotsqft.hist(df['log_lotsqft'], 'auto')
log_lotsqft.set_title('log_lotsqft', fontsize=8)
log_sqft.hist(df['log_sqft'], 'auto')
log_sqft.set_title('log_sqft', fontsize=8)
pool_None.hist(df['pool_None'], 2)
pool_None.set_title('pool_None', fontsize=8)
roof_Tile.hist(df['roof_Tile'], 2)
roof_Tile.set_title('roof_Tile', fontsize=8)
splsal_shortsale.hist(df['splsal_shortsale'], 2)
splsal_shortsale.set_title('splsal_shortsale', fontsize=8)
BankOwnedREO.hist(df['BankOwnedREO'], 2)
BankOwnedREO.set_title('BankOwnedREO', fontsize=8)
house_age.hist(df['house_age'], 'auto')
house_age.set_title('house_age', fontsize=8)
lppersqft.hist(df['lppersqft'], 'auto')
lppersqft.set_title('lppersqft', fontsize=8)

log_pricesold.set_facecolor('#FFFD55')
log_pricesold.spines['top'].set_color('red')
log_pricesold.spines['right'].set_color('red')
log_pricesold.spines['bottom'].set_color('red')
log_pricesold.spines['left'].set_color('red')
log_adom.set_facecolor('#FFFD55')
log_adom.spines['top'].set_color('red')
log_adom.spines['right'].set_color('red')
log_adom.spines['bottom'].set_color('red')
log_adom.spines['left'].set_color('red')
log_sqft.spines['top'].set_color('red')
log_sqft.spines['right'].set_color('red')
log_sqft.spines['bottom'].set_color('red')
log_sqft.spines['left'].set_color('red')
log_lotsqft.spines['top'].set_color('red')
log_lotsqft.spines['right'].set_color('red')
log_lotsqft.spines['bottom'].set_color('red')
log_lotsqft.spines['left'].set_color('red')
log_listprice.spines['top'].set_color('red')
log_listprice.spines['right'].set_color('red')
log_listprice.spines['bottom'].set_color('red')
log_listprice.spines['left'].set_color('red')
fig.tight_layout()
fig.set_figheight(8)
fig.set_figwidth(15)
plt.subplots_adjust(wspace=0.2, hspace=0.2)
plt.show()

# %%
df_corr_t = df[['Beds', 'bathsfull', 'bathstotal', 'ds_moy', 'garages',
      'log_listprice', 'lat', 'long', 'log_lotsqft', 'log_sqft', 'pool_None', 'roof_Tile',
      'splsal_shortsale', 'BankOwnedREO', 'house_age', 'lppersqft']]
plt.figure(figsize=(15,15))
sns.heatmap(df_corr_t.corr(), annot=True, cmap='vlag')
plt.title('Correlation Matrix')
plt.show()

# %%
model1 = smf.ols(formula='pricesold ~ Beds + bathstotal + sqft + C(BankOwnedREO)', data=df).fit()

display(model1.summary())

# %%
model2 = smf.ols(formula='log_pricesold ~ bathstotal + log_sqft + C(BankOwnedREO)', data=df).fit()

display(model2.summary())

# %%
model3 = smf.ols(formula='pricesold ~ long + C(pool_None) + C(roof_Tile) + house_age', data=df).fit()

display(model3.summary())

# %%
stargazer = Stargazer([model1, model2, model3])
html = stargazer.render_html()

display(HTML(html))

# %%
amodel1 = smf.ols(formula='adom ~ lppersqft + house_age + listprice', data=df).fit()

display(amodel1.summary())

# %%
amodel2 = smf.ols(formula='log_adom ~ lppersqft + house_age + log_listprice', data=df).fit()

display(amodel2.summary())

# %%
amodel3 = smf.ols(formula='log_adom ~ house_age + Beds', data=df).fit()

display(amodel3.summary())


# %%
astargazer = Stargazer([amodel1, amodel2, amodel3])
ahtml = astargazer.render_html()

display(HTML(ahtml))

# %%
# scatter plots linearity
plt.rcParams.update({'font.size': 8})
fig, ((price_long, adom_bed),
      (price_age, adom_age)) = plt.subplots(nrows=2, ncols=2)

price_long.scatter(df['long'], df['pricesold'])
adom_bed.scatter(df['Beds'], df['log_adom'])
price_age.scatter(df['house_age'], df['pricesold'])
adom_age.scatter(df['house_age'], df['log_adom'])

fig.tight_layout()
fig.set_figheight(8)
fig.set_figwidth(15)
plt.subplots_adjust(wspace=0.2, hspace=0.2)
plt.show()

# %%
print(shapiro(df['long']), shapiro(df['house_age']), shapiro(df['pricesold']))
print(shapiro(df['Beds']), shapiro(df['house_age']), shapiro(df['log_adom']))

# %%
print(bartlett(df['long'], df['house_age']))
print(bartlett(df['Beds'], df['house_age']))

# %%
df_ps = df[['long','house_age']]
vd = pd.DataFrame()
vd['feature'] = df_ps.columns
vd['VIF'] = [vif(df_ps.values, i) for i in range(len(df_ps.columns))]
print(vd)

# %%
df_ad = df[['Beds','house_age']]
vd = pd.DataFrame()
vd['feature'] = df_ad.columns
vd['VIF'] = [vif(df_ad.values, i) for i in range(len(df_ad.columns))]
print(vd)

# %%
print(durbin_watson(model3.resid))
print(durbin_watson(amodel3.resid))

