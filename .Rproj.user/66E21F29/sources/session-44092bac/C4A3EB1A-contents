
import pandas as pd
import asyncio
from curl_cffi.requests import AsyncSession

df = pd.read_csv('highCorr.csv')


dt = df

coinA = []
coinB = []
for _, row in dt.iterrows():
  coinA.append(row.coinA)
  coinB.append(row.coinB)


async def fetchCandles(symbols):
    async def fetch(session, symbol, semaphore):
        async with semaphore:
            response = await session.get(f'https://api.bitget.com/api/v2/spot/market/candles?symbol={symbol}&granularity=15min&limit=100')
        
        data = response.json()['data']
        df = pd.DataFrame(data)
        df['Pair'] = symbol
      
        
        return df
        

    semaphore = asyncio.Semaphore(10)
    
    async with AsyncSession() as session:
        tasks = [fetch(session, symbol, semaphore) for symbol in symbols]
        coins = await asyncio.gather(*tasks)
        
        
    return coins


candlesA = asyncio.run(fetchCandles(coinA))
candlesB = asyncio.run(fetchCandles(coinB))


dfA = pd.concat(candlesA,  ignore_index=True)
dfA.columns = ['Time', 'Opening price', 'Highest price', 'Lowest price', 'Closing price', 'Trading volume', 'Trading volume in USDT', 'Trading volume in quote currency', 'Pair']
#dfA.Time = pd.to_datetime(dfA.Time, unit= 'ms')


dfB = pd.concat(candlesB,  ignore_index=True)
dfB.columns = ['Time', 'Opening price', 'Highest price', 'Lowest price', 'Closing price', 'Trading volume', 'Trading volume in USDT', 'Trading volume in quote currency', 'Pair']
#dfB.Time = pd.to_datetime(dfB.Time, unit= 'ms')


dfA.to_csv('dfA.csv', index = False)
dfB.to_csv('dfB.csv', index = False)
