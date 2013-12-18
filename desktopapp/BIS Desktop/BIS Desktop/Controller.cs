using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Text.RegularExpressions;
using System.IO;
using System.Windows.Forms;
using System.Collections;

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

        private string filePath; 
        
        public Controller()
        {           

            //Set color for different buttons
            mercuryGrey = System.Drawing.ColorTranslator.FromHtml("#374140");
            mercuryBlue = System.Drawing.ColorTranslator.FromHtml("#354A69");
            mercuryRed = System.Drawing.ColorTranslator.FromHtml("#DC3522");
            highlightWhite = System.Drawing.ColorTranslator.FromHtml("#F6F6F8");
            mercuryBeige = System.Drawing.ColorTranslator.FromHtml("#D9CB9E");
            loading = System.Drawing.ColorTranslator.FromHtml("#F2F2F2");
            //Set font
            mercuryFont = new Font("Segoe UI", 10, FontStyle.Regular);

            //string dir = Directory.GetParent(Environment.CurrentDirectory).Parent.ToString();
            //string dir = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.FullName;
            string dir = AppDomain.CurrentDomain.BaseDirectory;
            //Console.WriteLine(dir); 
            filePath = dir + "\\portfolio.txt"; 

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
        public List<News> sortNewsList(List<News> list_, String type_, Boolean descending_)
        {

            Console.WriteLine("Size of list entered: " + list_.Count);
            Object sTemp = list_[0];
            if (sTemp.GetType().GetProperty(type_) != null)
            {
                List<News> newList_ = newsQuickSort(list_, type_, descending_);
                return newList_;
            }
            return null;
        }

        private List<News> newsQuickSort(List<News> list, String type_, Boolean descending)
        {
            Console.Write("");
            //Return list if size is 1 or less
            if (list.Count() == 1)
            {
                return list;
            }
            //Create new list
            List<News> newList = new List<News>();
            //Create pivot Object
            News pivot = list[0];
            //Remove pivot Object from list
            list.RemoveAt(0);
            //Get value of pivot Object
            String tempPivotVal_ = pivot.GetType().GetProperty(type_).GetValue(pivot, null).ToString();
            //Create list for each side of pivot
            List<News> l = new List<News>();
            List<News> g = new List<News>();


            try
            {
                //Get value of Object (in double)
                Double PivotVal_ = double.Parse(tempPivotVal_, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                foreach (News n in list)
                {
                    String tempVal_ = n.GetType().GetProperty(type_).GetValue(n, null).ToString();
                    Double ObjectVal_ = double.Parse(tempVal_, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                    //Compare value with pivot value
                    if (ObjectVal_ >= PivotVal_)
                    {
                        if (!descending)
                        {
                            g.Add(n);
                        }
                        else
                        {
                            l.Add(n);
                        }
                    }
                    else
                    {
                        if (!descending)
                        {
                            l.Add(n);
                        }
                        else
                        {
                            g.Add(n);
                        }
                    }

                }
            }
            catch (FormatException e)
            {
                if (!descending)
                {
                    newList = list.OrderBy(n => n.GetType().GetProperty(type_).GetValue(n, null)).ToList();
                }
                else
                {
                    newList = list.OrderByDescending(n => n.GetType().GetProperty(type_).GetValue(n, null)).ToList();
                }
            }
            if (l.Count() > 0)
            {
                newList.AddRange(newsQuickSort(l, type_, descending));
            }
            newList.Add(pivot);
            if (g.Count() > 0)
            {
                newList.AddRange(newsQuickSort(g, type_, descending));
            }
            return newList;
        }

        public List<Stock> getFilteredList2(List<Stock> list, DateTime date, int days)
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
                    if (difference <= days)
                    {
                        {
                            return list.GetRange((list.Count-difference), list.Count-1);
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
        public List<Stock> getFilteredList(List<Stock> list, DateTime date, int days)
        {
            try
            {

                for (int i = 0; i < list.Count(); i++)
                {
                    DateTime stockDate_ = getDate(list.ElementAt(i).Updated);
                    Console.WriteLine("Current date: " + stockDate_);
                    int difference = (date - stockDate_).Days;
                    Console.WriteLine("Difference: " + difference);
                    if (difference < 0)
                    {
                        difference *= -1;
                    }
                    if (difference <= days)
                    {
                        {
                            return list.GetRange(i, list.Count - i);
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
        /// <summary>
        /// Method for getting the timestamp from a date
        /// </summary>
        /// <param name="date"></param>
        /// <returns>long time stamp</returns>
        public long getTimeStamp(DateTime date)
        {
            // calculate as date
            DateTime startTime = new DateTime(1970, 1, 1);
            TimeSpan time = date - startTime;
            // convert to milliseconds
            long timeStamp = (long)time.TotalMilliseconds; 
            return timeStamp; 
        }
        /// <summary>
        /// Method for searching the list for a search input
        /// </summary>
        /// <param name="list"></param>
        /// <param name="target"></param>
        /// <returns></returns>
        public List<Stock> search(List<Stock> list, String target)
        {
            List<Stock> matchList = new List<Stock>(); 
            foreach (Stock s in list)
            {
                if (s.Name.ToLower().Contains(target.ToLower()) || s.Symbol.ToLower().Contains(target.ToLower()))
                {
                    matchList.Add(s);
                }
            }
            return matchList; 
        }
        /// <summary>
        /// Method for adding stock symbols to the portfolio
        /// </summary>
        /// <param name="symbol"></param>
        public void addToPortfolio(String symbol)
        {
            // check if the file allready exists
            if (File.Exists(filePath))
            {
                Boolean isDuplicate = false;
                // check the portfolio for duplicates
                List<String> temp = readFromPortfolio(); 
                foreach (String stockInPortfolio in temp)
                {
                    if(symbol == stockInPortfolio){ isDuplicate = true; }
                }
                // if there´s no duplicates
                if (!isDuplicate)
                    {
                        try
                        {
                            // add the symbol to the portfolio
                            using (TextWriter writer = new StreamWriter(filePath, true))
                            {
                                writer.WriteLine(symbol);
                                writer.Close();
                            }
                        }
                        catch (IOException e)
                        {
                            Console.WriteLine("Error while writing to portfolio: " + e.Data);
                        }
                    }
            }
            // if the file doesn´t exists
            else
            {
                try
                {
                    // just create and write
                    using (TextWriter writer = new StreamWriter(filePath, true))
                    {
                        writer.WriteLine(symbol);
                        writer.Close();
                    }
                }
                catch (IOException e)
                {
                    Console.WriteLine("Error while writing to portfolio: " + e.Data);
                }
            }        
        }
        /// <summary>
        /// Method for reading the portfolio, NOTE DO NOT USE TO LOAD ResultList, 
        /// this method is only used for checking for duplicates in the addToPortfolio method. 
        /// </summary>
        /// <returns>String list of symbols</returns>
        public List<String> readFromPortfolio()
        {
            List<String> symbols = new List<String>();
            try
            {
                // try reading the lines
                using (TextReader reader = new StreamReader(filePath))
                {
                    string line;
                    // while the lines are not empty add them
                    while ((line = reader.ReadLine()) != null)
                    {
                        symbols.Add(line);
                    }
                    reader.Close();
                }
            }
            catch (IOException e)
            {
                Console.WriteLine("error while reading for portfolio: " + e.Data); 
            }    
            return symbols;
        }     
        /// <summary>
        /// Method used for loading the stocks from the portfolio, reads the symbols and get the stock object from these
        /// </summary>
        /// <returns>List of stocks</returns>
        public List<Stock> loadFromPortfolio()
        {
            List<Stock> stocks = new List<Stock>();         
            // get the symbols from the portfolio
            List<string> symbols = readFromPortfolio();
            // get all the stocks
            JsonHandler jh = new JsonHandler();
            List<Stock> allStocks = jh.getAllStocks();
            // sort the stock list
            List<Stock> sortedAllStocks = quickSort(allStocks, "Symbol", false);
            // foreach symbol 
            foreach (string s in symbols)
            {
                // search the stock list for the symbol
                int searchResult = SearchAlgorithm.binarySearch(s, sortedAllStocks);
                if (searchResult != -1)
                {
                    // if a match is found add it to the list
                    stocks.Add(sortedAllStocks[searchResult]); 
                }
            }                                                                                         
            return stocks;          
        }   
        /// <summary>
        /// Reset button
        /// </summary>
        public void resetButton(object sender)
        {
            MercuryButton b = sender as MercuryButton;
            //Set clicked boolean to true
            b.clicked = false;
            //Change back color 
            if (b.Enabled == false)
            {
                b.BackColor = Color.LightGray;
            }
            else
            {
                b.BackColor = System.Drawing.ColorTranslator.FromHtml("#374140");
            }
            
        }
        /// <summary>
        /// filterStocks, returns a filtered list of Stock, which market matches the functions argument Market
        /// </summary>
        /// <param name="allStocks"></param>
        /// <param name="Market"></param>
        /// <returns>list of stocks</returns>
        public List<Stock> filterStocks(List<Stock> allStocks, String Market)
        {
            List<Stock> stocks = new List<Stock>();
            foreach (Stock s in allStocks)
            {
                if (s.Market.ToLower() == Market) // nyse couse we get it from db, later change to Market
                {
                    stocks.Add(s);
                }
            }
            return stocks;
        }

        /// <summary>
        /// filterNews, returns a filtered list of News, which market matches the functions argument Market
        /// </summary>
        /// <param name="allStocks"></param>
        /// <param name="Market"></param>
        /// <returns></returns>
        public List<News> filterNews(List<News> allStocks, String Market)
        {
            List<News> news = new List<News>();
            foreach (News temp in allStocks)
            {
                if (temp.market.ToLower() == Market)
                {
                    news.Add(temp);
                }
            }
            return news;
        }        
    }
    public class MercuryButton : Button
    {
        //Boolean to check if button is clicked
        public Boolean clicked;
        public String buttonType;
        //Constructor
        public MercuryButton(String text_, String buttonType_)
        {
            buttonType = buttonType_;
            Controller c = new Controller();
            //Reset values and color
            c.resetButton(this);
            //Fore color
            ForeColor = Color.White;
            //Set text
            Text = text_;
            TextAlign = ContentAlignment.MiddleCenter;
            //Appeance settings (flat)
            FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            FlatAppearance.BorderColor = Color.White;
            FlatAppearance.BorderSize = 0;
            FlatAppearance.MouseDownBackColor = System.Drawing.ColorTranslator.FromHtml("#DC3522");
            FlatAppearance.MouseOverBackColor = Color.Black;
            //Add event handler
            Click += new EventHandler(buttonClicked);
        }
        public void buttonClicked(object sender, EventArgs e)
        {
            //If button hasn't yet been clicked
            if (!clicked)
            {
                //Change back color
                BackColor = System.Drawing.ColorTranslator.FromHtml("#354A69");
                //Set button to clicked
                clicked = true;
            }
        }
        
    }

    public static class SearchAlgorithm
    {
        /// <summary>
        /// Binary search method, searches a sorted list for a target symbol
        /// </summary>
        /// <param name="target"></param>
        /// <param name="collection"></param>
        /// <returns>a int (-1) if no match was found else if a match was found it returns a int coresponding to the index of the match</returns>
        public static int binarySearch(String target, List<Stock> collection)
        { 
            int low = 0, high = collection.Count - 1;
            // check so that the target is not empty string
            if (target == "") { return -1; }
            // else if the 0th element of the list match return 0
            else if (collection[0].Symbol == target) { return 0; }
            // else if the max of the list match return max
            else if (collection[high].Symbol == target) { return high; }

            else
            {
                // while there is a list left to search
                while (low <= high)
                {              
                    int midPoint = (high + low) / 2;
                    int compSym = compare(collection[midPoint].Symbol, target);
                    
                    if (compSym == 0)
                    {
                        // we´ve found a match
                        return midPoint;
                    }
                    else if (compSym > 0)
                    {
                        // target is greater than the collection[midPoint]
                        high = midPoint - 1;
                    }
                    else
                    {
                        // target is less than the collection[midPoint]
                        low = midPoint + 1;
                    }
                }
                return -1;

            }
        }
        /// <summary>
        /// Method for comparing two objects 
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns>return a integer, 0 if match else positive integer corresponding to the difference of the objects</returns>
        public static int compare(this object a, object b) { return Comparer.DefaultInvariant.Compare(a, b); }
    }
}
