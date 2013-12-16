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

        public String Percent{ get; set; }

        public String Highest{ get; set; }

        public String Lowest { get; set; }

        public String ClosingValue{ get; set; }

        public String OpenVal{ get; set; }

        public String Updated{ get; set; }

        public String MarketName{ get; set; }
       
    }
}
