using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BIS_Desktop
{
    class News
    {

        public String Id { get; set; }

        public String Key { get; set; }

        public String title { get; set; }

        public String link { get; set; }

        public String description { get; set; } 

        public String guid { get; set; }

        public String pubDate { get; set; }

        public String type { get; set; }
    }
}
