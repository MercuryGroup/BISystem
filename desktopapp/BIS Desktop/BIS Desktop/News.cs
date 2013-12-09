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
        public String symbol { get; set; }

        public String id { get; set; }

        public String key { get; set; }

        public String title { get; set; }

        public String link { get; set; }

        public String description { get; set; } 

        public String pubDate { get; set; }

        public String type { get; set; }

        public String market { get; set; }
    }
}
