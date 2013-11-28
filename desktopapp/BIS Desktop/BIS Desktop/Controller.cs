using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace BIS_Desktop
{
    class Controller
    {
        public Color mercuryBlue,
           mercuryRed,
           mercuryGrey,
           highlightWhite,
           mercuryBeige,
           loading;
        public Controller()
        {
            //Set color for different buttons
            mercuryGrey = System.Drawing.ColorTranslator.FromHtml("#374140");
            mercuryBlue = System.Drawing.ColorTranslator.FromHtml("#354A69");
            mercuryRed = System.Drawing.ColorTranslator.FromHtml("#DC352");
            highlightWhite = System.Drawing.ColorTranslator.FromHtml("#FAFAFA");
            mercuryBeige = System.Drawing.ColorTranslator.FromHtml("#D9CB9E");
            loading = System.Drawing.ColorTranslator.FromHtml("#F2F2F2");
        }
        public double getMinMaxValue(List<Stock> stocks_, String minMax)
        {
            List<double> values_ = new List<double>();
            foreach (Stock s in stocks_)
            {
                //Console.WriteLine("VALUE: " + s.Latest);
                values_.Add(double.Parse(s.Latest, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture));
            }
            if (minMax == "max")
            {
                return values_.Max();
            }
            else if (minMax == "min")
            {
                return values_.Min();
            }
            return 0;
        }
    }
}
