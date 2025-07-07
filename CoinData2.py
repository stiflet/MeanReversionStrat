from curl_cffi import requests
import pandas as pd
from curl_cffi.requests import AsyncSession
import asyncio
import os



class Bitget:
    def __init__(self):
        Data = self.Data()
        
        
    class Data:
        def __init__(self):
            pass
        
        def getSymbols(self, save: bool = False, nrOfSymbols: int = 50):        
            response = requests.get('https://api.bitget.com/api/v2/spot/public/symbols').json()
            data = response['data']
            
            pd.DataFrame(data).to_csv('Output/symbols_raw.csv', index=False)
            symbols = [coin.get('symbol') for coin in data]
            
            if save:
                os.makedirs('Output', exist_ok=True)
                pd.DataFrame(symbols, columns=['Symbol']).to_csv('Output/symbols.csv', index=False)
                print("Symbols saved to symbols.csv")
            
            return symbols


        def getCandles(self, symbols, candle_limit: int = 1000,
                    semaphore_limit: int = 1000, save: bool = False, granularity: str = '15min'):
            async def Candles(symbols):
                async def fetch(session, symbol, semaphore):
                    async with semaphore:
                        url = f'https://api.bitget.com/api/v2/spot/market/candles?symbol={symbol}&granularity={granularity}&limit={candle_limit}'
                        response = await session.get(url)
                    
                    data = response.json()['data']
                    df = pd.DataFrame(data)
                    df['Pair'] = symbol
                    
                    return df
                    

                semaphore = asyncio.Semaphore(semaphore_limit)
                
                async with AsyncSession() as session:
                    tasks = [fetch(session, symbol, semaphore) for symbol in symbols]
                    candles = await asyncio.gather(*tasks)
                    
                    
                return candles


            candles = asyncio.run(Candles(symbols))
            df = pd.concat(candles, ignore_index=True)
            df.columns = ['Time', 'Opening price', 'Highest price', 'Lowest price', 'Closing price', 'Trading volume', 'Trading volume in USDT', 'Trading volume in quote currency', 'Pair']
            df = df[['Time', 'Pair', 'Opening price', 'Highest price', 'Lowest price', 'Closing price', 'Trading volume', 'Trading volume in USDT', 'Trading volume in quote currency']]
            df.Time = pd.to_datetime(pd.to_numeric(df.Time), unit= 'ms')
            
            #df = df[df['Pair'] != 'BTCUSDT']
            #df = df[df['Pair'] != 'ETHUSDT']
            #df = df[df['Pair'] != 'SOONUSDT']
            
            
            if save:
                output_path = 'Output/candles.csv'
                os.makedirs('Output', exist_ok=True)
                df.to_csv(output_path, index=False)
                print(f"Candles saved to {output_path}")
            
            return df
        
        
        def Pivot_df(self, df, minVol: int = 10**6, save: bool = False):
            totalVol = df.groupby('Pair')['Trading volume in USDT'].sum()
            valid_pairs = totalVol[pd.to_numeric(totalVol) > pd.to_numeric(minVol)].index


            df = df[df['Pair'].isin(valid_pairs)]
            dfFinal = df.pivot(index = 'Time', columns='Pair', values='Closing price')
            
            if save:
                output_path = 'Output/pivoted_candles.csv'
                os.makedirs('Output', exist_ok=True)
                dfFinal.to_csv(output_path, index=True)
                print(f"Pivoted DataFrame saved to {output_path}")
            
            return dfFinal







if __name__ == "__main__":

    getdata = Bitget().Data()

    symbols = getdata.getSymbols(save=True)  # load symbols from Bitget
    candles = getdata.getCandles(symbols, candle_limit=1000, granularity = '1h', save=True)
    #df = pd.read_csv('Output/candles.csv', parse_dates=['Time'])
    #dfA = getdata.Pivot_df(df, minVol=10**6, save=True)
    
    
