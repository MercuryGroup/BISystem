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
            mercuryRed = System.Drawing.ColorTranslator.FromHtml("#DC352");
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
                //Console.WriteLine("L " + s.Latest);
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
            Stock s = new Stock();
            if (s.GetType().GetProperty(type_) != null)
            {
                return quickSort(list_, type_, descending_);
            }
    
            
            
        return null;
        }

        private List<Stock> quickSort(List<Stock> list_, String type_, Boolean descending){
            //Return list if size is 1 or less
            if (list_.Count() <= 1)
            {
                return list_;
            }
            //Create new list
            List<Stock> newList = new List<Stock>();
            //Create pivot stock
            Stock pivot = list_[0];
            //Remove pivot stock from list
            list_.RemoveAt(0);
            //Get value of pivot stock
            String tempPivotVal_ = pivot.GetType().GetProperty(type_).GetValue(pivot, null).ToString();
            //Create list for each side of pivot
            List<Stock> l = new List<Stock>();
            List<Stock> g = new List<Stock>();
            
            
            try
            {
                //Get value of stock (in double)
               
                Double PivotVal_ = double.Parse(tempPivotVal_, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                foreach (Stock s in list_)
                {
                    String tempVal_ = s.GetType().GetProperty(type_).GetValue(s, null).ToString();
                    Double stockVal_ = double.Parse(tempVal_, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                    //Compare value with pivot value
                    if (stockVal_ >= PivotVal_)
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
                if (descending)
                {
                    newList = list_.OrderBy(s => s.GetType().GetProperty(type_).GetValue(s, null)).ToList();
                }
                else
                {
                    newList = list_.OrderByDescending(s => s.GetType().GetProperty(type_).GetValue(s, null)).ToList();
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
        public List<Stock> getFilteredList(List<Stock> list_, int days)
        {
            try
            {
                DateTime date = DateTime.Today.AddDays(days * -1);
                var timestamp = date.ToUniversalTime().Subtract(
                    new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalMilliseconds;
                list_ = sortStockList(list_, "Updated", true);
                for (int i = 0; i < list_.Count(); i++)
                {
                    Console.WriteLine(long.Parse(list_.ElementAt(i).Updated) + " " + timestamp + " " +i);
                     if (long.Parse(list_.ElementAt(i).Updated) <= timestamp)
                     {
                        return list_.GetRange(0, i);
                     }
                }
                return list_;
            }
            catch (Exception e)
            {
                Console.WriteLine("adasdasasd " + e.Message);
            }
            return null;
        }
        
    }
}
