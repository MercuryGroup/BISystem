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

        private Stock parseStock(JObject jo){

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


        public List<News> getNews(String Market)
        {
          
            return new List<News>(); 

        }

     
        public List<Stock> getSingleStock(String Symbol, String Period)
        {
            List<Stock> stocks = new List<Stock>();

            URL += "stocks/" + Period + "/" + Symbol;

            using(var s = client.GetStreamAsync(URL).Result)
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

            return stocks;
        }

        public List<Stock> getAllStocks(String Market){
            

            List<Stock> stocks = new List<Stock>();

            String url = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=\"1383565321852\"&endkey=\"1383565328964\"";

           
            using(var s = client.GetStreamAsync(url).Result)
            using (StreamReader sr = new StreamReader(s))
            {
                JObject jo = (JObject)JToken.ReadFrom(new JsonTextReader(sr));

                var children = jo["rows"];

                foreach (JObject stock in children.ToList())
                {
                    Stock st = parseStock(stock);

                    stocks.Add(st); ;  
                } 
               
                  
            }

            return stocks;
        }
    }
}
