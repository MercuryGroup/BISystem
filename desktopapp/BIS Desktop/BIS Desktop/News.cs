using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BIS_Desktop
{
   public class News
    {
        public String symbol { get; set; } // String for holding the symbol of the news

        public String id { get; set; } // String for holding the id of the news

        public String key { get; set; } // String for holding the key of the news

        public String title { get; set; } // String for holding the title of the news

        public String link { get; set; } // String for holding the link of the news

        public String description { get; set; } // String for holding the description of the news

        public String pubDate { get; set; } // String for holding the published date of the news

        public String type { get; set; } // String for holding the type of the news

        public String market { get; set; }// String for holding the market of the news
    }
}
