using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Linq;
using System.Text.RegularExpressions;
using System.IO;
using System.Windows.Forms;

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
            highlightWhite = System.Drawing.ColorTranslator.FromHtml("#FAFAF6");
            mercuryBeige = System.Drawing.ColorTranslator.FromHtml("#D9CB9E");
            loading = System.Drawing.ColorTranslator.FromHtml("#F2F2F2");
            //Set font
            mercuryFont = new Font("Segoe UI", 10, FontStyle.Regular);

            //string dir = Directory.GetParent(Environment.CurrentDirectory).Parent.ToString();
            string dir = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.FullName;
            Console.WriteLine(dir); 
            filePath = Path.Combine(dir, "\\portfolio.txt"); 

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

        public long getTimeStamp(DateTime date)
        {
            DateTime startTime = new DateTime(1970, 1, 1);
            TimeSpan time = date - startTime;

            long timeStamp = (long)time.TotalMilliseconds; 
            return timeStamp; 
        }

        public void addToPortfolio(Stock s)
        {  

            if (File.Exists(filePath))
            {
                
                Boolean isDuplicate = false;
                List<Stock> temp = readFromPortfolio(); // temp list for checking for duplicates in portfolio.txt
            
         
                foreach (Stock stockInPortfolio in temp)
                {
                    if (s.Symbol == stockInPortfolio.Symbol)
                    {
                        isDuplicate = true;
                    }
                }

                if (!isDuplicate)
                {
                    try
                    {
                        using (TextWriter writer = new StreamWriter(filePath, true))
                        {
                            writer.WriteLine(s.Id + "\\#" + s.Symbol + "\\#" + s.Name + "\\#" + s.Latest + "\\#" + s.Change + "\\#" + s.Percent + "\\#" + s.Volume + s.OpenVal + "\\#" + s.Updated + "\\#" + s.Market + "\\#" + s.Type);
                            writer.Close();
                        }
                    }
                    catch(IOException e)
                    {
                        Console.WriteLine("Error while writing to portfolio: " + e.Data);
                    }
                   
                }
            }
            else
            {
                try 
                {
                    using (TextWriter writer = new StreamWriter(filePath, true))
                    {
                        writer.WriteLine(s.Id + "\\#" + s.Symbol + "\\#" + s.Name + "\\#" + s.Latest + "\\#" + s.Change + "\\#" + s.Percent + "\\#" + s.Volume  + s.OpenVal + "\\#" + s.Updated + "\\#" + s.Market + "\\#" + s.Type);
                        writer.Close();
                    }
                }
                catch(IOException e)
                {
                    Console.WriteLine("Error while writing to portfolio: " + e.Data);
                }
                

            }
            

        }

      
      
        public List<Stock> readFromPortfolio()
        {
            List<Stock> stocks = new List<Stock>();;

            try
            {

                if (File.Exists(filePath))
                {
                   
                    using (TextReader reader = new StreamReader(filePath))
                    {
                        string line;

                        while ((line = reader.ReadLine()) != null)
                        {

                            string[] splitArray = line.Split(new[] { "\\#" }, StringSplitOptions.None);

                            Stock s = new Stock();

                            s.Id = splitArray[0];
                            s.Symbol = splitArray[1];
                            s.Name = splitArray[2];
                            s.Latest = splitArray[3];
                            s.Change = splitArray[4];
                            s.Percent = splitArray[5];
                            s.OpenVal = splitArray[6];
                            s.Updated = splitArray[7];
                            s.Market = splitArray[8];
                            s.Type = splitArray[9];

                            stocks.Add(s);

                        }

                        reader.Close();

                    }
                }
                else
                {
                    stocks = null;
                }
            }
            catch (IOException e)
            {
                Console.WriteLine("Error while adding to portfolio: " + e.Data);
            }
           
            return stocks; 
        }
        /// <summary>
        /// Reset button
        /// </summary>
        public void resetButton(object sender)
        {
            mercuryButton b = sender as mercuryButton;
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
        
    }
    public class mercuryButton : Button
    {
        //Boolean to check if button is clicked
        public Boolean clicked;
        public String buttonType;
        //Constructor
        public mercuryButton(String text_, String buttonType_)
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
                Console.WriteLine("My name is mercury button, i am clicked!");
            }
        }
        
    }
    
}
