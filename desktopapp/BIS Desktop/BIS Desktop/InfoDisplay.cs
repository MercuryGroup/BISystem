using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;
using System.Drawing;
using System.Linq;

namespace BIS_Desktop
{
    class InfoDisplay: FlowLayoutPanel
    {
        //Symbol Name Latest Change Percent Open Value
        private Chart chart;
        private ChartArea chartArea;
        private Series series;
        private RadioButton rbMonth, rbWeek, rbDay;
        private Panel buttonPanel;
        private List<Stock> temp, stockSpan;
        private String typeOfChart = "candlestick";
        private String Symbol;
        private Panel chartPanel;
        private Controller c;
        
        
        ToolTip tooltip;
        JsonHandler js;
        /*
         * Show day high and low for candlestick??
         * TODO:
         * [ ] Add panel to display latest values
         * 
        */
        public InfoDisplay(String s)
        {
            stockSpan = new List<Stock>();
            js = new JsonHandler();
            chartPanel = new Panel();

            c = new Controller();
            Symbol = s;
            temp = js.getSingleStock(Symbol, "month");
            
            
            initilizeChart(typeOfChart, "month");
            this.BackColor = Color.White;

            initilizeRadioButtons();
            

            this.Controls.Add(chart);
            this.Controls.Add(buttonPanel);
            

        }
        private void initilizeChart(String typeOfChart, String timeSpan)
        {
            /**
             * TODO:
             * [X] Set y maximum value based on content
             * [ ] Set interval based on day/week/month
             * [X] Get largest and smallest value
             * [ ] Get list of stocks based on timestamp
             * CURRENT
             * [/] Add mouse hover listener to chart areas
             */

            

            chart = new Chart();
            chartArea = new ChartArea();
            //Insert if-case for amount of days to show
            switch (timeSpan)
            {
                case "day":
                    //TEMP VALUE
                    stockSpan = c.getFilteredList(c.sortStockList(temp, "Updated", false), DateTime.Today, -1);
                    chartArea.AxisX.Maximum = 4;
                    break;
                case "week":
                    //TEMP VALUE
                    stockSpan = c.getFilteredList(c.sortStockList(temp, "Updated", false), DateTime.Today, -7);
                    chartArea.AxisX.Maximum = 7;
                    break;
                case "month":
                    stockSpan = c.sortStockList(temp,"Updated", false);
                    foreach (Stock ste in stockSpan)
                    {
                        //Console.WriteLine("Opening: " + ste.Updated);
                    }
                    chartArea.AxisX.Maximum = stockSpan.Count();
                    break;
            }
           
            // set max and min y values to the area (plus padding)
            double minYValue = Math.Round(c.getStockMinMaxValue(stockSpan, "min")/1.1, 1);
            double maxYValue = Math.Round(c.getStockMinMaxValue(stockSpan, "max")*1.1, 1);
            chartArea.AxisY.Minimum = minYValue;
            chartArea.AxisY.Maximum = maxYValue;
            chartArea.AxisX.Minimum = 0;
            //Set intervals
            
            double xInterval = double.Parse(stockSpan.Count().ToString()) / 5;
            double yInterval = ((maxYValue-minYValue)/5);
            if (yInterval < 0)
            {
                yInterval *= -1;
            }

            chartArea.AxisX.MajorGrid.Interval = chartArea.AxisX.Maximum+1;
            chartArea.AxisX.Interval = xInterval;
            chartArea.AxisY.MajorGrid.Interval = yInterval;
            chartArea.AxisY.Interval = yInterval;
            
            //Set colors
            chartArea.AxisX.MajorGrid.LineColor = Color.LightGray;
            chartArea.AxisY.MajorGrid.LineColor = Color.LightGray;
            //chartArea.BackColor = Color.Beige;
            
            
            //Console.WriteLine("Candle: " + test.Count);
            
            

            chart.ChartAreas.Add(chartArea);

            switch (typeOfChart)
            {
                case "candlestick":
                    //Get candlestick values
                    List<double[]> test = getCandleStickValues(stockSpan);
                    foreach (double[] candle in test)
                    {
                        Console.WriteLine("Opening value: " + candle[0] + " lowest: " + candle[1] + " highest: " + candle[2] + " closing: " + candle[3]);
                    }
                    initilizeCandeleStick();
                    break;
            }

            

            //Add buttons
            buttonPanel = new FlowLayoutPanel();
         
            buttonPanel.Controls.Add(rbMonth);
            buttonPanel.Controls.Add(rbWeek);
            buttonPanel.Controls.Add(rbDay);

            // TEMPORARY
            RichTextBox sd = new RichTextBox();
            sd.Text = "SYMBOL: " + temp.ElementAt(0).Symbol;
            Controls.Add(sd);

        }
        private List<Double[]> getCandleStickValues(List<Stock> stocks)
        {
            List<Stock> tempStocks = stocks;
            Console.WriteLine("<-" + stocks.Count);
            //List containing all candlestick values
            List<double[]> candleStickList = new List<double[]>();
            
            List<DateTime> dates = new List<DateTime>();
            DateTime currentDate = c.getDate(stocks[0].Updated);
            //Iterate through all stocks and extract all days
            for (int s = 0; s < stocks.Count; s++)
            {
                //Get temporary date at current stock
                DateTime tempDate = c.getDate(stocks[s].Updated);
                
                //Add temporary date and set to current if it's not the same as the current
                if (currentDate.Date != tempDate.Date)
                {
                    dates.Add(currentDate);
                    currentDate = c.getDate(stocks[s].Updated);
                    Console.WriteLine("ADD");
                }
                Console.WriteLine("Date: " + tempDate + " OpenVal: " + stocks[s].OpenVal + " Latest: " + stocks[s].Latest);

            }
            //TEMP
            dates.Add(c.getDate(tempStocks[tempStocks.Count-1].Updated));
            Console.WriteLine("List of dates: " + dates.Count);
            //Iterate through all dates
            for (int dateCount = 0; dateCount < dates.Count; dateCount++)
            {
                List<double> dayValues = new List<double>();
                Boolean gotOpenVal = false;
                double openVal_ = double.Parse(stocks[0].OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture); ;
                //Iterate through all stocks and compare dates
                for (int s = 0; s < tempStocks.Count; s++)
                {
                    Stock currentStock = tempStocks[s];
                    //Get date for current stock
                    DateTime currentStockDate = c.getDate(currentStock.Updated);
                    //Check if stock date is same as current date in date list
                    if (dates[dateCount].Date == currentStockDate.Date)
                    {
                        
                        //Get first opening value for stock
                        if (!gotOpenVal)
                        {
                            openVal_ = double.Parse(currentStock.OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                            gotOpenVal = true;
                        }
                        //Add value to current day
                        dayValues.Add(double.Parse(stocks[s].OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture));
                        dayValues.Add(double.Parse(stocks[s].Latest, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture));
                    }
                }
                //Reset boolean required for getting opening val
                gotOpenVal = false;
                //Create new array for candlestick information
                double[] candleStick = new double[4];
                //Add opening value
                candleStick[0] = openVal_;
                //Get lowest value
                candleStick[1] = dayValues.Min();
                //Add highest value
                candleStick[2] = dayValues.Max();
                //Add closing value
                candleStick[3] = dayValues[dayValues.Count - 1];
                //Add candlestick to list
                candleStickList.Add(candleStick);
                
            }
            return candleStickList;
        }
        
        public void initilizeRadioButtons()
        {

            rbMonth = new RadioButton();
            rbWeek = new RadioButton();
            rbDay = new RadioButton(); 

            rbMonth.Text = "Month";
            rbWeek.Text = "Week";
            rbDay.Text = "Day";

            rbMonth.Checked = true; 

            rbMonth.CheckedChanged += this.monthChecked;
            rbWeek.CheckedChanged += this.weekChecked;
            rbDay.CheckedChanged += this.dayChecked; 

           
        }

        public void initilizeCandeleStick()
        {


            series = new Series("prices");

            series.Color = c.mercuryRed;
            chart.Series.Add(series);

            chart.Series["prices"].ChartType = SeriesChartType.Candlestick;          
            chart.Series["prices"]["OpenCloseStyle"] = "Rectangle";
            chart.Series["prices"]["ShowOpenClose"] = "Both";
            chart.Series["prices"]["PointWidth"] = "0.5";

            chart.Series["prices"]["PriceUpColor"] = "Blue"; 
            chart.Series["prices"]["PriceDownColor"] = "Red";


            //Print (BASED ON TEMP DATA LENGTH)
            for (int i = 0; i < stockSpan.Count(); i++)
            {
                ToolTip tip = new ToolTip();
                //Get stock list for one day

                //adding X and high
                chart.Series["prices"].Points.AddXY(i, i*20);
                
                chart.Series["prices"].Points[i].ToolTip = "X: #VALX\nY: #VALY";
                

                // adding low
                chart.Series["prices"].Points[i].YValues[1] = i;
                //adding open
                chart.Series["prices"].Points[i].YValues[2] = ((i*2) + 50);
                // adding close
                chart.Series["prices"].Points[i].YValues[3] = ((i*2) + 300);
            }
               
           
            

        }
        public void monthChecked(object sender, System.EventArgs e)
        {
            
        }

        public void weekChecked(object sender, System.EventArgs e)
        {

        }

        public void dayChecked(object sender, System.EventArgs e)
        {

        }

        public void setSize(int W, int H)
        {
            this.Width = W;
            this.Height = H;

            chart.Width = W;
            chart.Height = H / 3;

            buttonPanel.Width = W - 200;
            buttonPanel.Height = 30;
            buttonPanel.Location = new Point((W - buttonPanel.Width) / 2);
            
        }

        
        
    }
}
