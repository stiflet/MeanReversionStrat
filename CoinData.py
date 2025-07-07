from curl_cffi import requests
import pandas as pd
from curl_cffi.requests import AsyncSession
import asyncio


output_path = 'CoinData.csv'
coinResponse = requests.get('https://api.bitget.com/api/v2/spot/public/symbols').json()
coinData = coinResponse['data']



symbols = [coin.get('symbol') for coin in coinData]



async def fetchCandles(symbols):
    async def fetch(session, symbol, semaphore):
        async with semaphore:
            response = await session.get(f'https://api.bitget.com/api/v2/spot/market/candles?symbol={symbol}&granularity=1Dutc&limit=1000')
        
        data = response.json()['data']
        df = pd.DataFrame(data)
        df['Pair'] = symbol
        
        #print(f'Fetching {symbol} - Status: {response.status_code}')
        
        return df
        

    semaphore = asyncio.Semaphore(1000)
    
    async with AsyncSession() as session:
        tasks = [fetch(session, symbol, semaphore) for symbol in symbols]
        coins = await asyncio.gather(*tasks)
        
        
    return coins


coins = asyncio.run(fetchCandles(symbols))

df = pd.concat(coins,  ignore_index=True)
df.columns = ['Time', 'Opening price', 'Highest price', 'Lowest price', 'Closing price', 'Trading volume', 'Trading volume in USDT', 'Trading volume in quote currency', 'Pair']
df.Time = pd.to_datetime(pd.to_numeric(df.Time), unit= 'ms')



df.to_csv(output_path, index=False)
