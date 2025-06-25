import json
import pandas as pd

df = pd.read_csv("~/All Projects/basketball-data/draft/2025-prospect-profile.csv")

logos = pd.DataFrame({
  "Source": ['Hoop Intellect | 6/3', 'No Ceilings | 6/11', 'CBS | 6/22','Yahoo | 6/16', 'The Ringer | 6/10', 'Athletic | 6/23','NBA Big Board | 6/9', 'ESPN | 6/23', 'Bleacher Report | 6/23','Fox Sports | 6/20'],
  "Logo": [
    "https://yt3.googleusercontent.com/k1lLk4IvxhWJIDIqACblVGoQVvNKuYHPeqGXB43-YCvS-ibxWsxeu3HKbhNnGU4hi-f5JQvLzQ=s900-c-k-c0x00ffffff-no-rj",
    "https://substackcdn.com/image/fetch/w_288,h_288,c_fill,f_auto,q_auto:good,fl_progressive:steep,g_auto/https%3A%2F%2Fsubstack-post-media.s3.amazonaws.com%2Fpublic%2Fimages%2F5cbe790f-e221-4250-9e16-742bd08d81a4_800x800.png",
    "https://seeklogo.com/images/C/cbs-sports-logo-5DF461F39C-seeklogo.com.png",
    "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRivBRUTfux45V5xatzdpDnvakIRZthPVcmkg&s",
    "https://storage.googleapis.com/nbadraft-theringer-com-cms/legacy/img/2020/logo-square.png",
    "https://play-lh.googleusercontent.com/eFPXeouX939-rfn0S00kTbShOffYoipvpsFPIRUsmHCTtxu91VvnTQWRM5_4VZ4VSe9Q",
    "https://pbs.twimg.com/profile_images/1512285937278939137/Xni4HCCj_400x400.jpg",
    "https://m.media-amazon.com/images/I/316HySxdvoL.png",
    "https://yt3.googleusercontent.com/DqADENRwYEp0MJg9Li1IUebRpFmXryH59PkXG9Ko2EySPrRsQgcAe9uHlr6hBjtLt_9XuY0L6g=s900-c-k-c0x00ffffff-no-rj",
    "https://brandlogos.net/wp-content/uploads/2022/08/fox_sports-logo_brandlogos.net_w7nct-512x512.png"
  ]
})

mocks = (
  pd.read_csv("https://raw.githubusercontent.com/adambushman/basketball-data/refs/heads/master/draft/2025_Industry_Boards.csv")
  .melt(id_vars = "Rank", var_name = "Source", value_name = "Prospect")
  .dropna()
  .merge(logos, on="Source")
)

result = (
    mocks
    .groupby('Prospect')
    .agg(mocks=('Rank', lambda r: [
        {'source': source, 'rank': rank, 'logo': logo}
        for rank, source, logo in zip(r, mocks.loc[r.index, 'Source'], mocks.loc[r.index, 'Logo'])
    ]))
    .reset_index()
)

full_data = (
  df
  .merge(result, left_on="name", right_on="Prospect")
  .drop("Prospect", axis=1)
)

full_data.to_json("prospects.json", orient="records", indent=2)

