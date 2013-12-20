using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BIS_Desktop
{
   public class Stock
    {     
        public String Id { get; set; } // String for holding the identification number of the stock

        public String Symbol { get; set; } // String for holding the stock symbol of the stock

        public String Name { get; set; } // String for holding the name of the stock

        public String Latest { get; set; } // String for holding the latest value of the stock

        public String Change { get; set; } // String for holding the change value of the stock

        public String Percent { get; set; } // String for holding the change in percent of the stock

        public String Volume { get; set; } // String for holding the volume of the stock

        public String OpenVal { get; set; } // String for holding the opening value of the stock

        public String Updated { get; set; } // String for holding the timestamp of when the stock was updated

        public String Market { get; set; }  // String for holding the stocks market name 
 
    }
    
}
