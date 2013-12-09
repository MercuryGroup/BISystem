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
        private Controller c; 

        private String URL = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/";

        private DateTime now;
        private DateTime pastDate;
        public JsonHandler()
        {
           client = new HttpClient();
           c = new Controller(); 
        }

        public List<Stock> getDummyStocks()
        {
            List<Stock> stocks = new List<Stock>();

            string dir = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.FullName;
            Console.WriteLine(dir);
            String filePath = Path.Combine(dir, "\\nyse.txt");

             try
            {

                using (StreamReader sr = new StreamReader(filePath))
                {

                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

                    foreach (JObject stock in JA)
                    {

                        Stock st = parseStock(stock);

                        stocks.Add(st);

                    }

                }

            }
            catch (AggregateException e)
            {
                Console.WriteLine(e.Data);
            }

            return stocks;
        
        }
  
        /// <summary>
        /// 
        /// </summary>
        /// <returns>news list with all the news from a 24 hour span</returns>
        public List<News> getAllNews()
        {
            List<News> news = new List<News>();

            // get the news within the lastest 24 hours
            now = DateTime.Now;

            pastDate = now.AddDays(-1);

            URL += "news_list?startkey=\"" + c.getTimeStamp(pastDate) + "\"&endkey=\"" + c.getTimeStamp(now) + "\"";
            Console.WriteLine(URL);
            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

                    foreach (JObject n in JA)
                    {

                        News temp = parseNews(n);

                        news.Add(temp);

                    }
                }
                  
            }catch (AggregateException e){
                Console.WriteLine(e.Data);
            }

            return news;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="Symbol"></param>
        /// <param name="Market"></param>
        /// <returns> A list containing all the news from a single stock</returns>
        public List<News> getSingleNews(String Symbol, String Market)
        {

            List<News> news = new List<News>();
            URL += "news?key=[\"" + Symbol + "\",\"" + Market + "\"]";

            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {

                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

                    foreach (JObject n in JA)
                    {

                        News temp = parseNews(n);

                        news.Add(temp);

                    }
                }

            }
            catch (AggregateException e)
            {
                Console.WriteLine(e.Data);
            }

            return news;

        }

        public List<Market> getSingleMarket(String Market, String Period){

            List<Market> marketData = new List<Market>();
            
            now = DateTime.Now;

            if (Period == "day")
            {
                pastDate = now.AddDays(-1);
            }
            else if (Period == "week")
            {
                pastDate = now.AddDays(-7);
            }
            else if (Period == "month")
            {
                pastDate = now.AddDays(-30);
                Console.WriteLine("DATE: " + pastDate); 
            } 

            URL += Market.ToLower() + "_market?startkey=\"" + c.getTimeStamp(pastDate) + "\"&endkey=\"" + c.getTimeStamp(now) + "\"";
            Console.WriteLine(URL); 
            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {

                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

                    foreach (JObject n in JA)
                    {

                        Market temp = parseMarket(n);

                        marketData.Add(temp);

                    }
                }

            }
            catch (AggregateException e)
            {
                Console.WriteLine(e.Data);
            }

            return marketData;
        }
     
        public List<Stock> getSingleStock(String Market, String Symbol, String Period)
        {

            /* Period = month week or day
            * symbol = the symbol of the stock 
            * example: jasonHandler.getSignleStock("III", "month"); 
            */
            List<Stock> stocks = new List<Stock>();

            now = DateTime.Now;

            if (Period == "day")
            {
                pastDate = now.AddDays(-1);
            }
            else if (Period == "week")
            {
                pastDate = now.AddDays(-7);
            }
            else if (Period == "month")
            {
                pastDate = now.AddDays(-30);
            }
            
            string temp = Market.ToLower();
            URL += "nyse" + "_stock?startkey=[\""+Symbol+ "\",\"" + c.getTimeStamp(pastDate) + "\"]&endkey=[\"" +Symbol + "," + c.getTimeStamp(now) + "\"]";
                //&endkey=[\""+Symbol+"\",\""+ c.getTimeStamp(now) + "\"]";
            Console.WriteLine("LALALLALALA: " + URL); 

            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {

                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

                    foreach (JObject stock in JA)
                    {

                        Stock st = parseStock(stock);

                        stocks.Add(st);

                    }

                }

            }
            catch (AggregateException e)
            {
                Console.WriteLine(e.Data);
            }

            return stocks;
        }
           

        public List<Stock> getAllStocks(){
            

            List<Stock> stocks = new List<Stock>();

            String url = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22";
            try
            {
                using (var s = client.GetStreamAsync(url).Result)
                using (StreamReader sr = new StreamReader(s))
                {

                    string json = sr.ReadToEnd();

                    JObject jo = JObject.Parse(json);

                    JArray JA = (JArray)jo["rows"];

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

        private News parseNews(JObject jo)
        {
            News news = new News();
      

            JObject temp = jo.Value<JObject>("value");

            news.symbol = temp.Value<string>("symbol");
            news.id = temp.Value<string>("id");
            news.key = temp.Value<string>("key");
            news.title = temp.Value<string>("title");
            news.link = temp.Value<string>("link");
            news.description = temp.Value<string>("description");
            news.pubDate = temp.Value<string>("pubDate");
            news.type = temp.Value<string>("type");
            news.market = temp.Value<string>("market");

            return news; 
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
            market.OpenVal = temp.Value<string>("openVal");
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
