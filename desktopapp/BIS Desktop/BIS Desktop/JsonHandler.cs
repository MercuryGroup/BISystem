using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using System.IO;
using Newtonsoft.Json.Linq;
using System.Net;
using System.Net.Http;


namespace BIS_Desktop
{
    class JsonHandler
    {

        // make a controller method that builds query
        // get single stock
        // get latest stocks
        // get market
        // get news


        // API call: *Type*/*Period*/*Sub type*/*Symbol*.
        //*Symbol* is optional.
        //Type: stocks, markets, news.
        //Period: all (only for news type), day, week, month.
        //Sub type (only for markets type): stocks.
        //Symbol: lse, nyse, omx (only for markets type and stocks sub type), otherwise a stock symbol.


        private HttpClient client;
        private String URL = "http://mercury.dyndns.org:8080/JAXRS-BISystem/api/"; 
 
        public JsonHandler()
        {
           client = new HttpClient();
        }

     
        public List<News> getNews(String Market)
        {
          
            return new List<News>(); 

        }
        public List<Market> getSingleMarket(String marketSymbol, String Period){

            List<Market> marketData = new List<Market>();

            URL += "markets/" + Period + "/index/" + marketSymbol;

            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    string json = sr.ReadToEnd();

                    JArray JA = JArray.Parse(json);

                    foreach (JObject market in JA)
                    {

                        Market m = parseMarket(market);

                        marketData.Add(m);

                    }
                }

            }catch (AggregateException e){
                Console.WriteLine(e.Data);
            }
           

            return marketData;
        }
        

     
        public List<Stock> getSingleStock(String Symbol, String Period)
        {

            /* Period = month week or day
            * symbol = the symbol of the stock 
            * example: jasonHandler.getSignleStock("III", "month"); 
            */
            List<Stock> stocks = new List<Stock>();

            URL += "stocks/" + Period + "/" + Symbol;

            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    string json = sr.ReadToEnd();

                    JArray JA = JArray.Parse(json);

                    foreach (JObject stock in JA)
                    {

                        Stock st = parseStock(stock);

                        stocks.Add(st);

                    }
                }

            }catch (AggregateException e){
                Console.WriteLine(e.Data); 
            }

                return stocks;
        }
           

        public List<Stock> getAllStocks(){
            

            List<Stock> stocks = new List<Stock>();

            //String url = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=\"1383565321852\"&endkey=\"1383565328964\"";
            String url = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22";
            try
            {
                using (var s = client.GetStreamAsync(url).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    //JObject jo = (JObject)JToken.ReadFrom(new JsonTextReader(sr));
                    //var children = jo["rows"];

                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

                    foreach (JObject stock in JA)
                    {

                        Stock st = parseStock(stock);

                        stocks.Add(st);

                    }

                    //foreach (JObject stock in children.ToList())
                   // {
                      //  Stock st = parseStock(stock);

                      //  stocks.Add(st); ;
                   // }


                }

            }catch (AggregateException e){
                Console.WriteLine(e.Data); 
            }
            return stocks;
        }

        private Market parseMarket(JObject jo)
        {
            Market market = new Market();

            JObject temp = jo.Value<JObject>("value");

            market.Latest = temp.Value<string>("latest");
            market.Change = temp.Value<string>("change");
            market.Percent = temp.Value<string>("percent");
            market.Highest = temp.Value<string>("highest");
            market.Lowest = temp.Value<string>("lowest");
            market.ClosingValue = temp.Value<string>("closingVal");
            market.OpeningValue = temp.Value<string>("openVal");
            market.Updated = temp.Value<string>("updated");
            market.MarketName = temp.Value<string>("market");

            return market; 
        }

        private Stock parseStock(JObject jo)
        {

            Stock stock = new Stock();

            JObject temp = jo.Value<JObject>("value");

            stock.Name = temp.Value<string>("name");
            stock.Symbol = temp.Value<string>("symbol");
            stock.Latest = temp.Value<string>("latest");
            stock.Change = temp.Value<string>("change");
            stock.Percent = temp.Value<string>("percent");
            stock.Volume = temp.Value<string>("volume");
            stock.OpenVal = temp.Value<string>("openVal");
            stock.Updated = temp.Value<string>("updated");
            stock.Market = temp.Value<string>("market");
            stock.Type = temp.Value<string>("type");

            if (stock.Volume == null)
            {
                stock.Volume = "N/A";
            }

            return stock;

        }
    }
}
