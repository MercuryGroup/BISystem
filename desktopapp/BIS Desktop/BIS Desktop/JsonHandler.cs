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

        public List<News> getNews(String Market)
        {
           
            
         //byte[] buff = new byte[16 * 1024];
           // HttpClient client = new HttpClient();
            //using(MemoryStream  ms = new MemoryStream()){

              //  int read;
                //while (( read = ms.Read(buff, 0, buff.Length)> 0)
                //{

                //}


     
           
      

            return new List<News>(); 

        }

        public List<Stock> getStocks(String Market){
            

            List<Stock> stocks = new List<Stock>();

            String url = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=\"1383565321852\"&endkey=\"1383565328964\"";

            HttpClient client = new HttpClient();
            using(var s = client.GetStreamAsync(url).Result)
            using (StreamReader sr = new StreamReader(s))
            {
                JObject jo = (JObject)JToken.ReadFrom(new JsonTextReader(sr));

                var children = jo["rows"];

                foreach (JObject stock in children.ToList())
                {
                    Stock st = new Stock();
                    st.Key = stock.Value<string>("key");
                    JObject temp = stock.Value<JObject>("value");

                    st.Name = temp.Value<string>("name");
                    st.Symbol = temp.Value<string>("symbol");
                    st.Latest = temp.Value<string>("latest");
                    st.Change = temp.Value<string>("change");
                    st.Percent = temp.Value<string>("percent");
                    st.OpenVal = temp.Value<string>("openVal");
                    st.Updated = temp.Value<string>("updated");
                    st.Market = temp.Value<string>("market");
                    st.Type = temp.Value<string>("type");

                    stocks.Add(st);  
                } 
               
                  
            }

            return stocks;
        }
    }
}
