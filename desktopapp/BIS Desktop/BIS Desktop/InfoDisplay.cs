using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;
using System.Drawing;

namespace BIS_Desktop
{
    /*
     * TODO:
     * [ ] Line chart
     * [ ] Bar chart
     * [ ] Add news panel
     * [ ] Stock info panel
     */
    class InfoDisplay: FlowLayoutPanel
    {
        //Symbol Name Latest Change Percent Open Value
        private Chart chart;
        private ChartArea chartArea;
        private double xInterval;
        private double yInterval;
        private Series series;
        private RadioButton rbMonth, rbWeek, rbDay;
        private Panel chartTypePanel;
        private Panel timeSpanPanel;
        private List<Stock> stockList, stockSpan;
        private String typeOfChart = "candlestick";
        private String symbol;
        private String market;
        private String timeSpan = "month";
        private String typeOfStock;
        private Panel chartPanel;
        private Controller c;
        private List<DateTime> days;
        private int currentPointHover = -1;
        private Boolean addToPortfolio = true;
        private int marketHighest;
        private int marketLowest;
        private Button candlestickButton;
        private Button lineChartButton;
        private Button barChartButton;
        private Button dayButton;
        private Button weekButton;
        private Button monthButton;
        ToolTip tooltip;
        JsonHandler js;
        /*
         * Show day high and low for candlestick??
         * TODO:
         * [ ] Add panel to display latest values
         * 
        */
        public InfoDisplay(String type, String s, String m)
        {
            stockSpan = new List<Stock>();
            js = new JsonHandler();
            chartPanel = new Panel();

            c = new Controller();
            symbol = s;
            market = m;
            typeOfStock = type;
            if (typeOfStock == "stock")
            {
                stockList = js.getSingleStock(market, symbol, "month");
            }
            else if (typeOfStock == "market")
            {
                addToPortfolio = false;
                stockList = new List<Stock>();
                List<Market> marketList = js.getSingleMarket(market, "month");
                foreach (Market temp in marketList)
                {
                    Stock tempStock = new Stock();
                    tempStock.Latest = temp.Latest;
                    tempStock.OpenVal = temp.OpenVal;
                    tempStock.Updated = temp.Updated;
                    stockList.Add(tempStock);
                }
            }
            stockList = c.sortStockList(stockList, "Updated", false);
            
            
            this.BackColor = c.highlightWhite;

            

            // TEMPORARY
            RichTextBox sd = new RichTextBox();
            sd.Text = "SYMBOL: " + stockList.ElementAt(0).Symbol;
            Controls.Add(sd);
            

            

            chartTypePanel = new FlowLayoutPanel();
            timeSpanPanel = new FlowLayoutPanel();
            timeSpanPanel.Anchor = AnchorStyles.Bottom;

            //Create and add buttons to chart panel
            candlestickButton = new mercuryButton("Candlestick", "candlestick");
            candlestickButton.Click += new EventHandler(chooseChartType);
            lineChartButton = new mercuryButton("Line", "line");
            lineChartButton.Click += new EventHandler(chooseChartType);
            barChartButton = new mercuryButton("Bar", "bar");
            barChartButton.Click += new EventHandler(chooseChartType);
            chartTypePanel.Controls.Add(candlestickButton);
            chartTypePanel.Controls.Add(lineChartButton);
            chartTypePanel.Controls.Add(barChartButton);
            
            //Add event listener to control timespan value 
            dayButton = new mercuryButton("Day", "day");
            dayButton.Click += new EventHandler(chooseTimeSpan);
            weekButton = new mercuryButton("Week", "week");
            weekButton.Click += new EventHandler(chooseTimeSpan);
            monthButton = new mercuryButton("Month", "month");
            monthButton.Click += new EventHandler(chooseTimeSpan);
            timeSpanPanel.Controls.Add(dayButton);
            timeSpanPanel.Controls.Add(weekButton);
            timeSpanPanel.Controls.Add(monthButton);
            
            
            
            this.Controls.Add(chartPanel);
            monthButton.BackColor = c.mercuryBlue;
            dayButton.Enabled = false;
            resetchartButtons();
            resetTimeSpanButtons();
            initilizeChart("candlestick", timeSpan);
            this.Controls.Add(chartTypePanel);
            this.Controls.Add(timeSpanPanel);


            /*
             * 
             * 
             * -----------------------------------
             * HÄR KAN DU SLÄNGA IN KLASSEN, ALEX!
             * -----------------------------------
             * glöm inte att lägga till:
             * this.controls.add(din klass)
             * 
             */



        }
        private void initilizeChart(String typeOfChart_, String timeSpan_)
        {
            /**
             * TODO:
             * [X] Set y maximum value based on content
             * [X] Set interval based on day/week/month
             * [X] Get largest and smallest value
             * [X] Get list of stocks based on timestamp
             * CURRENT
             * [X] Add mouse hover listener to chart areas
             */
            switch (timeSpan_)
            {
                case "day":
                    stockSpan = c.getFilteredList(stockList, DateTime.Now, 1);
                    break;
                case "week":
                    stockSpan = c.getFilteredList(stockList, DateTime.Now, 7);
                    break;
                case "month":
                    stockSpan = stockList;
                    break;
            }

            chartPanel.Controls.Clear();
            chart = new Chart();
            chartArea = new ChartArea();
            
            //Get all days for the stocks
            days = getStockDays(stockSpan);
            //Set maximum X axis
            chartArea.AxisX.Maximum = days.Count();
            double paddingValue = (c.getStockMinMaxValue(stockSpan, "max")*1.05)-c.getStockMinMaxValue(stockSpan, "max");
            // set max and min y values to the area (plus padding)
            double minYValue = Math.Round(c.getStockMinMaxValue(stockSpan, "min")-paddingValue, 1);
            double maxYValue = Math.Round(c.getStockMinMaxValue(stockSpan, "max")+paddingValue, 1);
            chartArea.AxisY.Minimum = minYValue;
            chartArea.AxisY.Maximum = maxYValue;
            chartArea.AxisX.Minimum = 0;
            //Set intervals
            xInterval = 1;
            yInterval = ((maxYValue-minYValue)/5);
            if (yInterval < 0)
            {
                yInterval *= -1;
            }
            chartArea.AxisX.MajorGrid.Enabled = false;
            chartArea.AxisX.Interval = xInterval;
            chartArea.AxisY.MajorGrid.Interval = yInterval;
            chartArea.AxisY.Interval = yInterval;
            
            
            //Set colors
            chartArea.AxisX.MajorGrid.LineColor = Color.LightGray;
            chartArea.AxisY.MajorGrid.LineColor = Color.LightGray;
            //chartArea.BackColor = Color.Beige;
            chart.ChartAreas.Add(chartArea);
            switch (typeOfChart_)
            {
                case "candlestick":
                    //Set custom max value
                    chartArea.AxisX.Maximum = days.Count()+1;
                    //Get candlestick values
                    List<double[]> candleStickList = getCandleStickValues2(stockSpan);
                    initilizeCandleStick(candleStickList);
                    break;
            }
            //Add mouse listener
            this.chart.MouseLeave += chart_MouseExit;
            this.chart.MouseDown += chart_MouseDown;
            //Add event listener to control stock value 
            chart.GetToolTipText += getHoverLabel;
            chart.Width = chartPanel.Width;
            chart.Height = chartPanel.Height;
            chartPanel.Controls.Add(chart);
        }
        private void chooseChartType(object sender, EventArgs e)
        {
            mercuryButton mb = sender as mercuryButton;
            typeOfStock = mb.buttonType;
            if (typeOfChart == "candlestick")
            {
                dayButton.Enabled = false;
            }
            else
            {
                dayButton.Enabled = true;
            }
            resetchartButtons();
            resetTimeSpanButtons();
            initilizeChart(typeOfChart, "month");
            monthButton.BackColor = c.mercuryBlue;
            mb.BackColor = c.mercuryBlue;
            
        }
        private void chooseTimeSpan(object sender, EventArgs e)
        {
            mercuryButton mb = sender as mercuryButton;
            if (typeOfStock == "candlestick")
            {
                dayButton.Enabled = false;
            }
            resetTimeSpanButtons();
            timeSpan = mb.buttonType;
            mb.BackColor = c.mercuryBlue;
            
            initilizeChart(typeOfChart, timeSpan);
        }
        private void resetchartButtons()
        {
            c.resetButton(candlestickButton);
            c.resetButton(lineChartButton);
            c.resetButton(barChartButton);
        }
        private void resetTimeSpanButtons()
        {
            c.resetButton(dayButton);
            c.resetButton(weekButton);
            c.resetButton(monthButton);
        }
        /// <summary>
        /// Sets a custom tooltip for a chart
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void getHoverLabel(object sender, System.Windows.Forms.DataVisualization.Charting.ToolTipEventArgs e)
        {
            //Check if the current position of the mouse matches a chart element
            if (e.HitTestResult.ChartElementType == ChartElementType.DataPoint)
            {
                //Get index of element
                int i = e.HitTestResult.PointIndex;
                //Console.Write("HIT " + i + currentPointHover);
                //Update only if the mouse changes position to another element
                if (currentPointHover != i)
                {
                    //Clear all labels
                    for (int j = 0; j < chart.Series[0].Points.Count(); j++)
                    {
                        chart.Series["prices"].Points[j].Label = "";
                        chart.Series["prices"].Points[j].BorderWidth = 1;
                    }
                    DataPoint dp = e.HitTestResult.Series.Points[i];

                    //Set content of label
                    String label_ = string.Format("op: {0:F2}, cl: {1:F2}, h: {2:F2}, l: {3:F2}", dp.YValues[2], dp.YValues[3], dp.YValues[0], dp.YValues[1]);
                    
                    //Set label
                    chart.Series["prices"].Points[i].LabelBackColor = c.mercuryBeige;
                    chart.Series["prices"].Points[i].LabelAngle = 0;
                    chart.Series["prices"].SmartLabelStyle.Enabled = true;
                    chart.Series["prices"].SmartLabelStyle.AllowOutsidePlotArea = LabelOutsidePlotAreaStyle.No;
                    chart.Series["prices"].Points[i].Label = label_;
                    chart.Series["prices"].Points[i].LabelAngle = 0;
                    chart.Series["prices"].Points[i].LabelForeColor = c.highlightWhite;
                    chart.Series["prices"].Points[i].BorderWidth = 2;
                    
                    chart.Series["prices"].Points[i].LabelBackColor = System.Drawing.ColorTranslator.FromHtml(chart.Series["prices"].Points[i]["PriceDownColor"]);
                    chart.Series["prices"].Points[i].LabelBorderColor = chart.Series["prices"].Points[i].LabelBackColor;
                    chart.Series["prices"].Points[i].LabelBorderWidth = 5;
                    currentPointHover = i;
                }
            }
        }
        private void chart_MouseExit(object sender, EventArgs e)
        {
            //FIX
            Chart chart_ = sender as Chart;
            for (int i = 0; i < chart_.Series[0].Points.Count(); i++)
            {
                chart.Series["prices"].Points[i].Label = "";
                chart.Series["prices"].Points[i].BorderWidth = 1;
                currentPointHover = -1;
            }
        }
        private void chart_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == System.Windows.Forms.MouseButtons.Left)
            {
                Chart chart_ = sender as Chart;
                for (int i = 0; i < chart_.Series[0].Points.Count(); i++)
                {
                    chart.Series["prices"].Points[i].Label = "";
                    chart.Series["prices"].Points[i].BorderWidth = 1;
                    currentPointHover = -1;
                }
            }
        }
        public void initilizeCandleStick(List<double[]> list)
        {
            series = new Series("prices");
            
            series.Color = c.mercuryRed;
            chart.Series.Add(series);
            //Set content settings
            //chart.Series["prices"].CustomProperties
            chart.Series["prices"].ChartType = SeriesChartType.Candlestick;
            chart.Series["prices"].Color = c.mercuryBlue;
            //chart.Series["prices"]["OpenCloseStyle"] = "Rectangle";
            chart.Series["prices"]["ShowOpenClose"] = "Both";
            chart.Series["prices"]["PointWidth"] = "0.4";
            chart.Series["prices"].BorderWidth = 1;
            chart.Series["prices"].BorderColor = c.mercuryBlue;
            chart.Series["prices"]["PriceUpColor"] = "White";
            chart.Series["prices"]["PriceDownColor"] = "#354A69";
            

            //Print
            for (int i = 0; i < list.Count(); i++)
            {
                ToolTip tip = new ToolTip();
                //adding X and high
                chart.Series["prices"].Points.AddXY(i+1, list[i].ElementAt(3));
                //Set X axis label to date
                if (i < days.Count)
                {
                    
                    chart.Series["prices"].Points[i].AxisLabel = days[i].Day + "/" + days[i].Month;
                }
                
                //Add low
                chart.Series["prices"].Points[i].YValues[1] = list[i].ElementAt(2);
                //Add opening value
                chart.Series["prices"].Points[i].YValues[2] = (list[i].ElementAt(0));
                //Add closing value
                chart.Series["prices"].Points[i].YValues[3] = (list[i].ElementAt(1));
                Console.WriteLine("Open: " + list[i].ElementAt(0));
                if (i != 0)
                {
                    if (list[i].ElementAt(1) < list[i - 1].ElementAt(1) )
                    {
                        chart.Series["prices"].Points[i].Color = c.mercuryRed; 
                        chart.Series["prices"].Points[i].BorderColor = c.mercuryRed;
                        chart.Series["prices"].Points[i]["PriceDownColor"] = "#DC3522";
                    }
                }
            }
        }
        private List<DateTime> getStockDays(List<Stock> stocks)
        {
            List<Stock> tempStocks = stocks;
            //List containing all candlestick values
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
                    //Console.WriteLine("ADD " + currentDate);
                }
                //Console.WriteLine("Date " + s + ": " + tempDate + " OpenVal: " + stocks[s].OpenVal + " Latest: " + stocks[s].Latest);
            }
            dates.Add(c.getDate(stocks[stocks.Count - 1].Updated));
            return dates;
        }
        private List<Double[]> getCandleStickValues(List<Stock> stocks)
        {
            List<Stock> tempStocks = stocks;
            //List containing all candlestick values
            List<double[]> candleStickList = new List<double[]>();
            List<DateTime> dates = getStockDays(stocks);
            //dates.Add(c.getDate(tempStocks[tempStocks.Count-1].Updated));
            //Console.WriteLine("List of dates: " + dates.Count);
            //Iterate through all dates
            for (int dateCount = 0; dateCount < dates.Count; dateCount++)
            {
                List<double> dayValues = new List<double>();
                Boolean gotOpenVal = false;
                double openVal_ = double.Parse(stocks[0].OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
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
        private List<Double[]> getCandleStickValues2(List<Stock> stocks)
        {
            if (stocks.Count > 0){
                List<Stock> tempStocks = stocks;
            //List containing all candlestick values
            List<double[]> candleStickList = new List<double[]>();
            List<DateTime> dates = getStockDays(stocks);
            //dates.Add(c.getDate(tempStocks[tempStocks.Count-1].Updated));
            Console.WriteLine("List of dates: " + dates.Count);
            //Iterate through all dates
            for (int dateCount = 0; dateCount < dates.Count; dateCount++)
            {
                List<double> dayValues = new List<double>();
                
                double openVal_ = double.Parse(stocks[0].OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                //Iterate through all stocks and compare dates
                for (int s = 0; s < tempStocks.Count; s++)
                {
                    Stock currentStock = tempStocks[s];
                    //Get date for current stock
                    DateTime currentStockDate = c.getDate(currentStock.Updated);
                    //Check if stock date is same as current date in date list
                    if (dates[dateCount].Date == currentStockDate.Date)
                    {
                        openVal_ = double.Parse(currentStock.OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                        
                        //Add value to current day
                        dayValues.Add(double.Parse(stocks[s].Latest, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture));
                    }
                }
                //Create new array for candlestick information
                double[] candleStick = new double[4];
                //Add opening value
                candleStick[0] = openVal_;
                Console.Write(dateCount + " -> Open: " + openVal_);
                //Add closing value
                candleStick[1] = dayValues[dayValues.Count - 1];
                Console.Write("Closing: " + dayValues[dayValues.Count - 1]);
                //Add open val as value to get max and min value
                dayValues.Add(openVal_);
                //Get lowest value
                candleStick[2] = dayValues.Min();
                Console.Write("Min: " + dayValues.Min());
                //Add highest value
                candleStick[3] = dayValues.Max();
                Console.WriteLine("Max: " + dayValues.Max());
                
                //Add candlestick to list
                candleStickList.Add(candleStick);

            }
            return candleStickList;
            }
            return null;
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

            chartPanel.Width = W;
            chartPanel.Height = H / 3;
            chart.Width = chartPanel.Width;
            chart.Height = chartPanel.Height;
            chartTypePanel.Width = W;
            timeSpanPanel.Width = W;

            chartTypePanel.Width = W - 200;
            chartTypePanel.Height = 30;
            chartTypePanel.Location = new Point((W - chartTypePanel.Width) / 2);
            
        }
        

        
        
    }
}
