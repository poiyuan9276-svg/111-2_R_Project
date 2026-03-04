import pandas as pd
import glob

# 指定要合併的CSV檔案路徑
path = 'C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2020data/*.csv'

# 讀取所有CSV檔案，並將它們存成一個DataFrame清單
all_files = glob.glob(path)
dfs = [pd.read_csv(f,header=[0,1]) for f in all_files]

# 將所有DataFrame合併
merged_df = pd.concat(dfs, ignore_index=True)

# 將合併後的DataFrame存成新的CSV檔案
merged_df.to_csv('2020weather.csv', index=False)
