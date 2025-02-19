import pandas as pd
import os

print(os.path.dirname(os.path.realpath(__file__)))

raw_data = pd.read_excel('..\data\HuntersGreenHomeSales-2.xlsx')

print(raw_data)