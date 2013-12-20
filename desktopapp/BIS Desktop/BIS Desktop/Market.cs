using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BIS_Desktop
{
    class Market
    {

        public String Latest { get; set; } // String for holding the latest value of the market

        public String Change { get; set; } // String for holding the latest change value of the market

        public String Percent { get; set; } // String for holding the percent of the market

        public String Highest { get; set; } // String for holding the highest value of the market

        public String Lowest { get; set; } // String for holding the lowest value of the market

        public String ClosingValue { get; set; } // String for holding the closing value of the market

        public String OpenVal { get; set; } // String for holding the open value of the market

        public String Updated { get; set; } // String for holding the updated value of the market 

        public String MarketName { get; set; } // String for holding the market name
       
    }
}
