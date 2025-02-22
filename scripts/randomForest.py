import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score

# 1. Load Data
df = pd.read_csv('data.csv')

# 2. Data Cleaning (very simplistic example)
df.dropna(inplace=True)

# 3. Feature Engineering (example)
df['log_square_feet'] = np.log1p(df['square_feet'])

# 4. Convert categorical to dummy variables (example)
df = pd.get_dummies(df, columns=['neighborhood'], drop_first=True)

# 5. Select X and y
feature_cols = ['log_square_feet', 'bedrooms', 'bathrooms', 'has_garage', 'year_built'] \
               + [col for col in df.columns if 'neighborhood_' in col]
X = df[feature_cols]
y = df['price_sold']

# 6. Train/Test Split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# 7. Train a Random Forest
rf = RandomForestRegressor(n_estimators=100, random_state=42)
rf.fit(X_train, y_train)

# 8. Predict and Evaluate
y_pred = rf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print("Random Forest MSE:", mse)
print("Random Forest R^2:", r2)
