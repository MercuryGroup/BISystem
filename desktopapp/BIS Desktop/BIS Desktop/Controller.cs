using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Linq;
using System.Text.RegularExpressions;
namespace BIS_Desktop
{
    class Controller
    {
        public Font mercuryFont;
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
            mercuryRed = System.Drawing.ColorTranslator.FromHtml("#DC3522");
            highlightWhite = System.Drawing.ColorTranslator.FromHtml("#FAFAFA");
            mercuryBeige = System.Drawing.ColorTranslator.FromHtml("#D9CB9E");
            loading = System.Drawing.ColorTranslator.FromHtml("#F2F2F2");
            //Set font
            mercuryFont = new Font("Segoe UI", 10, FontStyle.Regular);
        }
        public double getStockMinMaxValue(List<Stock> stocks_, String minMax)
        {
            List<double> values_ = new List<double>();
            foreach (Stock s in stocks_)
            {
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
            return 0.0; 
        }
        public List<Stock> sortStockList(List<Stock> list_, String type_, Boolean descending_)
        {
            
            Console.WriteLine("Size of list entered: " + list_.Count);
            Object sTemp = list_[0];
            if (sTemp.GetType().GetProperty(type_) != null)
            {
                List<Stock> newList_ = quickSort(list_, type_, descending_);
                return newList_;
            } 
            return null;
        }

        private List<Stock> quickSort(List<Stock> list, String type_, Boolean descending){
            Console.Write("");
            //Return list if size is 1 or less
            if (list.Count() == 1)
            {
                return list;
            }
            //Create new list
            List<Stock> newList = new List<Stock>();
            //Create pivot Object
            Stock pivot = list[0];
            //Remove pivot Object from list
            list.RemoveAt(0);
            //Get value of pivot Object
            String tempPivotVal_ = pivot.GetType().GetProperty(type_).GetValue(pivot, null).ToString();
            //Create list for each side of pivot
            List<Stock> l = new List<Stock>();
            List<Stock> g = new List<Stock>();
            
            
            try
            {
                //Get value of Object (in double)
                Double PivotVal_ = double.Parse(tempPivotVal_, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                foreach (Stock s in list)
                {
                    String tempVal_ = s.GetType().GetProperty(type_).GetValue(s, null).ToString();
                    Double ObjectVal_ = double.Parse(tempVal_, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                    //Compare value with pivot value
                    if (ObjectVal_ >= PivotVal_)
                    {
                        if (!descending){
                            g.Add(s);
                        }
                        else{
                            l.Add(s);
                        }
                    }
                    else
                    {
                        if (!descending)
                        {
                            l.Add(s);
                        }
                        else
                        {
                            g.Add(s);
                        }
                    }
                    
                }
            }
            catch (FormatException e)
            {
                if (!descending)
                {
                    newList = list.OrderBy(s => s.GetType().GetProperty(type_).GetValue(s, null)).ToList();
                }
                else
                {
                    newList = list.OrderByDescending(s => s.GetType().GetProperty(type_).GetValue(s, null)).ToList();
                }
            }
            if (l.Count() > 0)
            {
                newList.AddRange(quickSort(l, type_, descending));
            }
            newList.Add(pivot);
            if (g.Count() > 0)
            {
                newList.AddRange(quickSort(g, type_, descending));
            }
            
            return newList;
        }
        public List<Stock> getFilteredList(List<Stock> list, DateTime date, int days)
        {
            try
            {
                
                for (int i = 0; i < list.Count(); i++)
                {
                    DateTime stockDate_ = getDate(list.ElementAt(i).Updated);
                    Console.WriteLine("Current date: " + stockDate_);
                    int difference = (date-stockDate_).Days;
                    Console.WriteLine("Difference: " + difference);
                    if (difference < 0){
                        difference *=-1;
                    }
                    if (difference > days)
                    {
                        {
                            return list.GetRange(0, i);
                        }
                    }
                    
                }
                return list;
            }
            catch (Exception e)
            {
                Console.WriteLine("adasdasasd " + e.Message);
            }
            return null;
        }
        public DateTime getDate(String microSec)
        {
            long milliSec = (long.Parse(microSec));
            DateTime startTime = new DateTime(1970, 1, 1);

            TimeSpan time = TimeSpan.FromMilliseconds(milliSec);
            DateTime date = startTime.Add(time);
            return date;
        }


    }
}
