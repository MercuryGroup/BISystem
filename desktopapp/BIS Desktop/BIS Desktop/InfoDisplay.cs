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
    class InfoDisplay: FlowLayoutPanel
    {
        //Symbol Name Latest Change Percent Open Value
        private Chart chart;
        private ChartArea chartArea;
        private double xInterval;
        private double yInterval;
        private Series series;
        private Panel chartTypePanel;
        private Panel timeSpanPanel;
        private Panel fillerPanel;
        private Panel newsPanel; // a panel holding the news list class StockNews
        private List<Stock> stockList, stockSpan;
        private String typeOfChart = "line";
        private String symbol;
        private String market;
        private String timeSpan = "month";
        private String typeOfStock;
        private Panel chartPanel;
        private Controller c;
        private int buttonWidth, buttonHeight;
        private List<DateTime> dates;
        private int currentPointHover = -1;
        private Boolean addToPortfolio = true;
        private Button candlestickButton;
        private Button lineChartButton;
        private Button barChartButton;
        private Button dayButton;
        private Button weekButton;
        private Button monthButton;
        List<double[]> chartValues;
        JsonHandler js;
        /*
         * Show day high and low for candlestick??
         * TODO:
         * [ ] Add panel to display latest values
         * 
        */
        public InfoDisplay(String type, String s, String m)
        {
            buttonWidth = 120;
            buttonHeight = 30;
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
                    Console.WriteLine("Latest: " + tempStock.Latest + " Open: " + tempStock.OpenVal + " updated: " + tempStock.Updated);
                    stockList.Add(tempStock);
                }
            }
            stockList = c.sortStockList(stockList, "Updated", false);
            Padding = new Padding(5, 3, 5, 3);
            this.BackColor = c.highlightWhite;

            

            // TEMPORARY
            RichTextBox sd = new RichTextBox();
            sd.Text = "SYMBOL: " + stockList.ElementAt(0).Symbol;
            Controls.Add(sd);


            //Set layout of button panels
            chartTypePanel = new FlowLayoutPanel();
            timeSpanPanel = new FlowLayoutPanel();

            //Create and add buttons to chart panel
            candlestickButton = new mercuryButton("Candlestick", "candlestick");
            candlestickButton.Click += new EventHandler(chooseChartType);
            candlestickButton.Width = buttonWidth;
            candlestickButton.Height = buttonHeight;
            lineChartButton = new mercuryButton("Latest", "line");
            lineChartButton.Click += new EventHandler(chooseChartType);
            lineChartButton.Width = buttonWidth;
            lineChartButton.Height = buttonHeight;
            barChartButton = new mercuryButton("Change", "bar");
            barChartButton.Click += new EventHandler(chooseChartType);
            barChartButton.Width = buttonWidth;
            barChartButton.Height = buttonHeight;
            chartTypePanel.Controls.Add(lineChartButton);
            chartTypePanel.Controls.Add(candlestickButton);
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
            this.Controls.Add(timeSpanPanel);
            this.Controls.Add(chartTypePanel);
            resetchartButtons();
            resetTimeSpanButtons();
            initilizeChart(typeOfChart, timeSpan);
            lineChartButton.BackColor = c.mercuryBlue;
            monthButton.BackColor = c.mercuryBlue;
            


            newsPanel = new StockNews(symbol, m);
            newsPanel.BackColor = Color.Pink; 
            this.Controls.Add(newsPanel);
 


        }
        private void initilizeChart(String typeOfChart_, String timeSpan_)
        {
            switch (timeSpan_)
            {
                case "day":
                    stockSpan = c.getFilteredList(stockList, DateTime.Now, 0);
                    break;
                case "week":
                    stockSpan = c.getFilteredList(stockList, DateTime.Now, 6);
                    break;
                case "month":
                    stockSpan = stockList;
                    break;
            }
            timeSpan = timeSpan_;
            chartPanel.Controls.Clear();
            chart = new Chart();
            chartArea = new ChartArea();
            chart.BackColor = c.highlightWhite;
            //Get all days for the stocks
            
            if (timeSpan_ == "day")
            {
                dates = getAllDateTime(stockSpan);
                chartValues = getAllChartValues(stockSpan);
            }
            else
            {
                dates = getStockDays(stockSpan);
                chartValues = getChartWeekMonthValues(stockSpan);
            }
            //Set maximum X axis
            chartArea.AxisX.Maximum = dates.Count() + 1;
            
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
            chartArea.BackColor = c.highlightWhite;
            
            //Set colors of grid lines
            chartArea.AxisX.MajorGrid.LineColor = Color.LightGray;
            chartArea.AxisY.MajorGrid.LineColor = Color.LightGray;
            //chartArea.BackColor = Color.Beige;
            chart.ChartAreas.Add(chartArea);
            switch (typeOfChart_)
            {
                case "candlestick":
                    chartValues = getChartWeekMonthValues(stockSpan);
                    initilizeCandleStick(chartValues);
                    break;
                case "line":
                    initializeLineChart(chartValues);
                    break;
                case "bar":
                    List<double> barChartValues_ = getBarChartValues(chartValues);
                    
                    if (paddingValue < 0)
                    {
                        paddingValue *= -1;
                    }
                    chartArea.AxisY.Minimum = Math.Round(barChartValues_.Min() - paddingValue, 2);
                    chartArea.AxisY.Maximum = Math.Round(barChartValues_.Max() + paddingValue, 2);
                    //paddingValue = (barChartValues_.Max() * 1.1) - barChartValues_.Max();
                    
                    yInterval = ((chartArea.AxisY.Maximum - chartArea.AxisY.Minimum) / 5);
                    if (yInterval == 0)
                    {
                        yInterval = 0.2;
                    }
                    else if (yInterval < 0)
                    {
                        yInterval *= -1;
                    }
                    chartArea.AxisY.MajorGrid.Interval = yInterval;
                    chartArea.AxisY.Interval = yInterval;
                    initializeBarChart(barChartValues_);
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
            typeOfChart = mb.buttonType;
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
                    String label_ = "";
                    //Clear all labels
                    for (int j = 0; j < chart.Series[0].Points.Count(); j++)
                    {
                        chart.Series["prices"].Points[j].Label = "";
                        if (typeOfChart == "candlestick")
                        {
                            chart.Series["prices"].Points[j].BorderWidth = 1;
                        }
                        else if (typeOfChart == "line")
                        {
                            chart.Series["prices"].Points[j].BorderWidth = 2;
                            chart.Series["prices"].Points[j].MarkerSize = 5;
                        }
                    }
                    DataPoint dp = e.HitTestResult.Series.Points[i];

                    //Set content of label
                    if (typeOfChart == "candlestick")
                    {
                        label_ = string.Format("op: €{0:F2}, cl: €{1:F2}, h: €{2:F2}, l: €{3:F2}", dp.YValues[2], dp.YValues[3], dp.YValues[0], dp.YValues[1]);
                        chart.Series["prices"].Points[i].LabelBackColor = System.Drawing.ColorTranslator.FromHtml(chart.Series["prices"].Points[i]["PriceDownColor"]);
                    }
                    else if (typeOfChart == "line")
                    {
                        label_ = string.Format("Value: €{0:F2}", dp.YValues[0]);
                        chart.Series["prices"].Points[i].LabelBackColor = c.mercuryBlue;
                        chart.Series["prices"].Points[i].MarkerSize = 10;
                    }
                    else
                    {
                        label_ = string.Format("Change: €{0:F2}", dp.YValues[0]);
                        chart.Series["prices"].Points[i].LabelBackColor = chart.Series["prices"].Points[i].Color;
                    }
                    //Set label
                    chart.Series["prices"].Points[i].LabelAngle = 0;
                    chart.Series["prices"].SmartLabelStyle.Enabled = true;
                    chart.Series["prices"].SmartLabelStyle.AllowOutsidePlotArea = LabelOutsidePlotAreaStyle.No;
                    chart.Series["prices"].Points[i].Label = label_;
                    chart.Series["prices"].Points[i].LabelAngle = 0;
                    chart.Series["prices"].Points[i].LabelForeColor = c.highlightWhite;
                    chart.Series["prices"].Points[i].BorderWidth = 2;
                    
                    
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
                if (typeOfChart == "candlestick")
                {
                    chart.Series["prices"].Points[i].BorderWidth = 1;
                }
                else if (typeOfChart == "line")
                {
                    chart.Series["prices"].Points[i].BorderWidth = 2;
                    chart.Series["prices"].Points[i].MarkerSize = 5;
                }
                chart.Series["prices"].Points[i].Label = "";
                
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
                    if (typeOfChart == "candlestick")
                    {
                        chart.Series["prices"].Points[i].BorderWidth = 1;
                    }
                    else if (typeOfChart == "line")
                    {
                        chart.Series["prices"].Points[i].BorderWidth = 2;
                        chart.Series["prices"].Points[i].MarkerSize = 5;
                    }
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
            chart.Series["prices"]["ShowOpenClose"] = "Both";
            chart.Series["prices"]["PointWidth"] = "0.4";
            chart.Series["prices"].BorderWidth = 1;
            chart.Series["prices"].BorderColor = c.mercuryBlue;
            chart.Series["prices"]["PriceUpColor"] = "#FAFAF6";
            chart.Series["prices"]["PriceDownColor"] = "#354A69";
            //Print
            for (int i = 0; i < list.Count(); i++)
            {
                //adding X and high
                chart.Series["prices"].Points.AddXY(i+1, list[i].ElementAt(3));
                //Set X axis label to date
                if (i < dates.Count)
                {
                    chart.Series["prices"].Points[i].AxisLabel = dates[i].Day + "/" + dates[i].Month;
                }
                
                //Add low
                chart.Series["prices"].Points[i].YValues[1] = list[i].ElementAt(2);
                //Add opening value
                chart.Series["prices"].Points[i].YValues[2] = (list[i].ElementAt(0));
                //Add closing value
                chart.Series["prices"].Points[i].YValues[3] = (list[i].ElementAt(1));
                //Console.WriteLine("Open: " + list[i].ElementAt(0));
                if (i != 0)
                {
                    if (list[i].ElementAt(1) < list[i - 1].ElementAt(1))
                    {
                        chart.Series["prices"].Points[i].Color = c.mercuryRed; 
                        chart.Series["prices"].Points[i].BorderColor = c.mercuryRed;
                        chart.Series["prices"].Points[i]["PriceDownColor"] = "#DC3522";
                    }
                }
            }
        }
        private void initializeLineChart(List<double[]> list)
        {
            series = new Series("prices");
            chart.Series.Add(series);
            chart.Series["prices"].ChartType = SeriesChartType.Line;
            chart.Series["prices"].Color = c.mercuryBlue;
            chart.Series["prices"].BorderWidth = 2;
            chart.Series["prices"]["PointWidth"] = "4";
            chart.Series["prices"].MarkerStyle = MarkerStyle.Circle;
            for (int i = 0; i < list.Count(); i++)
            {
                chart.Series["prices"].Points.AddXY(i + 1, list[i].ElementAt(1));
                if (timeSpan == "day" && i < dates.Count && i <list.Count)
                {
                    if (i < dates.Count)
                    {
                        chart.Series["prices"].Points[i].AxisLabel = dates[i].ToShortTimeString();
                    }
                }
                else
                {
                    if (i < dates.Count)
                    {
                        chart.Series["prices"].Points[i].AxisLabel = dates[i].Day + "/" + dates[i].Month;
                    }
                }
            }

        }
        private void initializeBarChart(List<double> list)
        {
            series = new Series("prices");
            chart.Series.Add(series);
            chart.Series["prices"].Color = c.mercuryBlue;
            chart.Series["prices"].ChartType = SeriesChartType.Column;
            chart.Series["prices"].BorderWidth = 1;
            chart.Series["prices"]["PointWidth"] = "0.5";
            for (int i = 0; i < list.Count(); i++)
            {
                chart.Series["prices"].Points.AddXY(i + 1, list[i]);
                if (timeSpan == "day" && i < dates.Count && i < list.Count)
                {
                    if (i < dates.Count)
                    {
                        if (list[i] > 0)
                        {
                            chart.Series["prices"].Points[i].Color = c.mercuryBlue;
                        }
                        else
                        {
                            chart.Series["prices"].Points[i].Color = c.mercuryRed;
                        }
                        chart.Series["prices"].Points[i].AxisLabel = dates[i].ToShortTimeString();
                    }
                }
                else
                {
                    if (i < dates.Count)
                    {
                        if (list[i] > 0)
                        {
                            chart.Series["prices"].Points[i].Color = c.mercuryBlue;
                        }
                        else
                        {
                            chart.Series["prices"].Points[i].Color = c.mercuryRed;
                        }
                        chart.Series["prices"].Points[i].AxisLabel = dates[i].Day + "/" + dates[i].Month;
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
        private List<Double[]> getChartWeekMonthValues(List<Stock> stocks)
        {
            if (stocks.Count > 0){
                List<Stock> tempStocks = stocks;
            //List containing all candlestick values
            List<double[]> chartValueList = new List<double[]>();
            List<DateTime> dates = getStockDays(stocks);
            //dates.Add(c.getDate(tempStocks[tempStocks.Count-1].Updated));
            //Console.WriteLine("List of dates: " + dates.Count);
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
                double[] values = new double[4];
                //Add opening value
                values[0] = openVal_;
                //Console.Write(dateCount + " -> Open: " + openVal_);
                //Add closing value
                values[1] = dayValues[dayValues.Count - 1];
                //Console.Write("Closing: " + dayValues[dayValues.Count - 1]);
                //Add open val as value to get max and min value
                dayValues.Add(openVal_);
                //Get lowest value
                values[2] = dayValues.Min();
                //Console.Write("Min: " + dayValues.Min());
                //Add highest value
                values[3] = dayValues.Max();
                //Console.WriteLine("Max: " + dayValues.Max());
                //Add candlestick to list
                chartValueList.Add(values);

            }
            return chartValueList;
            }
            return null;
        }

        private List<Double[]> getAllChartValues(List<Stock> stocks)
        {
            if (stocks.Count > 0)
            {
                List<Stock> tempStocks = stocks;
                //List containing all candlestick values
                List<double[]> chartValueList = new List<double[]>();
                //dates.Add(c.getDate(tempStocks[tempStocks.Count-1].Updated));
                //Iterate through all dates
                for (int stockCount = 0; stockCount < stocks.Count; stockCount++)
                {
                    //Create new array for candlestick information
                    double[] values = new double[2];
                    //Add opening value
                    values[0] = double.Parse(stocks[stockCount].OpenVal, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                    //Add latest value
                    values[1] = double.Parse(stocks[stockCount].Latest, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture);
                    //Add candlestick to list
                    chartValueList.Add(values);

                }
                return chartValueList;
            }
            return null;
        }

        public void setSize(int W, int H)
        {
            this.Width = W;
            this.Height = H;

            chartPanel.Width = W -(Padding.Right*2+chartPanel.Margin.Right*2);
            
            chartPanel.Height = H / 3;
            chart.Width = chartPanel.Width;
            chart.Height = chartPanel.Height;
            chartTypePanel.Width = (buttonWidth + lineChartButton.Margin.Left + lineChartButton.Margin.Right) * 3 + chartTypePanel.Padding.All;
            timeSpanPanel.Width = (buttonWidth + lineChartButton.Margin.Left + lineChartButton.Margin.Right) * 3 + timeSpanPanel.Padding.All;
            chartTypePanel.Height = (buttonHeight + lineChartButton.Margin.Top + lineChartButton.Margin.Bottom) + timeSpanPanel.Padding.All;
            timeSpanPanel.Height = 30;
            //chartTypePanel.Location = new Point((W - chartTypePanel.Width) / 2);

            newsPanel.Width = W;

            //newsPanel.Height = H - chartPanel.Height - timeSpanPanel.Height - chartTypePanel.Height;

            newsPanel.Height = 200;  // TEMPORARY 
            newsPanel.Location = new Point((W - newsPanel.Width) / 2);
            //Console.WriteLine("WIDTH: " + chartPanel.Width + "  " + chart.Width + " "  + chartPanel.Margin.Left + " " + this.Width);
        }

        private List<DateTime> getAllDateTime(List<Stock> stockList_)
        {
            List<DateTime> listOfDates_ = new List<DateTime>();
            foreach (Stock s in stockList_)
            {
                listOfDates_.Add(c.getDate(s.Updated));
            }
            return listOfDates_; 
        }
        private List<Double> getBarChartValues(List<Double[]> list_)
        {
            List<Double> changes = new List<Double>();
            for (int count = 0; count < list_.Count; count++)
            {
                changes.Add(list_[count].ElementAt(1) - list_[count].ElementAt(0));
            }
            return changes;
        }
    }
}
