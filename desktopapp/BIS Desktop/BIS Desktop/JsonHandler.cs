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
        private HttpClient client; // http client for requesting the database
        private Controller c; // Controller class
        private String URL = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/"; // url
        private DateTime now; // DateTime for holding the current date
        private DateTime pastDate; // DateTime for holding the past date
        /// <summary>
        /// Class for retreiving json arrays for stock information and news from the database
        /// </summary>
        public JsonHandler()
        {
            // initilize classes
           client = new HttpClient();
           c = new Controller(); 
        }
        /// <summary>
        /// getAllNews, gets all the news within the lastest 24 hours
        /// </summary>
        /// <returns>news list with all the news from a 24 hour span</returns>
        public List<News> getAllNews()
        {
            List<News> news = new List<News>();
            // get current date
            now = DateTime.Now;
            // get past date
            pastDate = now.AddDays(-1);
            // build url
            URL += "news_list?startkey=\"" + c.getTimeStamp(pastDate) + "\"&endkey=\"" + c.getTimeStamp(now) + "\"";
            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    // parse the json 
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
                Console.WriteLine("exception while retriving data: "+ e.Data);
            }
            return news;
        }

        /// <summary>
        /// getSingleNews, gets all the news within from a specific symbol
        /// </summary>
        /// <param name="Symbol"></param>
        /// <param name="Market"></param>
        /// <returns> A list containing all the news from a single stock</returns>
        public List<News> getSingleNews(String Symbol, String Market)
        {
            List<News> news = new List<News>();
            // build the url
            URL += "news?key=[\"" + Symbol + "\",\"" + Market + "\"]";
            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    // parse the jason
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
                Console.WriteLine("exception while retriving data: " + e.Data);
            }
            return news;
        }
        /// <summary>
        /// getSingleMarket, gets market data from a single market and within a period
        /// </summary>
        /// <param name="Market"></param>
        /// <param name="Period"></param>
        /// <returns>a list of market object</returns>
        public List<Market> getSingleMarket(String Market, String Period){

            List<Market> marketData = new List<Market>();
            // get current date
            now = DateTime.Now;
            // get past date depending on the period
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
            // build the url
            URL += Market.ToLower() + "_market?startkey=\"" + c.getTimeStamp(pastDate) + "\"&endkey=\"" + c.getTimeStamp(now) + "\"";
            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    // parse the json
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
        /// <summary>
        /// getSingleStock, get data for a single stock within a period
        /// </summary>
        /// <param name="Market"></param>
        /// <param name="Symbol"></param>
        /// <param name="Period"></param>
        /// <returns>a list of stock objects</returns>
        public List<Stock> getSingleStock(String Market, String Symbol, String Period)
        {
            List<Stock> stocks = new List<Stock>();
            // get current date
            now = DateTime.Now;
            // get past date depending on period
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
            // build url
            string temp = Market.ToLower();
            URL += temp + "_stock?startkey=[\""+Symbol+ "\",\"" + c.getTimeStamp(pastDate) + "\"]&endkey=[\"" +Symbol + "," + c.getTimeStamp(now) + "\"]";
            try
            {
                using (var s = client.GetStreamAsync(URL).Result)
                using (StreamReader sr = new StreamReader(s))
                {
                    // parse the json
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
        /// getAllStocks, get all the latest stocks of the database
        /// </summary>
        /// <returns> a list of stock objects</returns>
        public List<Stock> getAllStocks(){
            
            List<Stock> stocks = new List<Stock>();
            // in this case we do not have to build the url, since there are no symbols or periods involved etc. So we just use the below direct link
            string url = "http://mercury.dyndns.org:5984/mercury_latest/_design/bi/_view/stock";
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
        /// <summary>
        /// Method for parsing json obejct and creating a news object
        /// </summary>
        /// <param name="jo"></param>
        /// <returns>News object</returns>
        private News parseNews(JObject jo)
        {
            News news = new News();
            JObject temp = jo.Value<JObject>("value");
            news.symbol = temp.Value<string>("symbol");
            news.id = temp.Value<string>("id");
            news.title = temp.Value<string>("title");
            news.link = temp.Value<string>("link");
            news.description = temp.Value<string>("description");
            news.pubDate = temp.Value<string>("pubDate");
            news.type = temp.Value<string>("type");
            news.market = temp.Value<string>("market");
            return news; 
        }
        /// <summary>
        ///  Method for parsing json obejct and creating a market object
        /// </summary>
        /// <param name="jo"></param>
        /// <returns>Market object</returns>
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
        /// <summary>
        ///  Method for parsing json obejct and creating a stock object
        /// </summary>
        /// <param name="jo"></param>
        /// <returns>Stock object</returns>
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
            if (stock.Volume == null)
            {
                // if the volume is null 
                stock.Volume = "N/A";
            }

            return stock;

        }
    }
}
