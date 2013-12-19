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
        private Chart chart; //chart to display stock data
        private ChartArea chartArea; //chart area that contains the chart
        private double xInterval; //Interval of chart values (x axis)
        private double yInterval; //Interval of chart values (y axis)
        private Series series; //Contains chart values
        //All panels used for infodisplay
        private Panel chartTypePanel; //Contains chart type buttons
        private Panel timeSpanPanel; //Contains time span buttons
        private Panel timeSpanFiller; //Filler panel used to center time span panel
        private Panel chartTypeFiller; //Filler panel used to center chart type panel
        private Panel centerTimeSpan, centerChartType; //Contains time span/chart type panel and its filler
        private Panel newsPanel; // a panel holding the news list class StockNews
        private Panel stockInfoPanel; //Contains information about the stock
        private Panel chartPanel; //Panel that contains the chart
        private List<Stock> stockList; //Contains all stock values and is used when displaying information and chart
        private String typeOfChart = "line"; //Controls the type of stock to be loaded
        private String symbol; //Stock symbol
        private String market; //Stock market
        private String timeSpan = "month"; //Controls the time span that the chart will cover
        private String typeOfStock; //Can be either stock or market
        private Controller c; //Controller class
        private IntegratedNewsList news;  //Contains news about the stock
        private int buttonWidth, buttonHeight; //Button dimensions
        private List<DateTime> dates; //Contains all dates taken from the stock
        private int currentPointHover = -1; //Used for pinpointing location of mouse when hovering over the chart
        private Boolean portfolioCompatible = true; //Indicates whether or not a stock can be added to portfolio
        private Button candlestickButton; //Candlestick button
        private Button lineChartButton;//Line chart button
        private Button barChartButton;//Bar chart button
        private Button dayButton;//Time span button (day)
        private Button weekButton;//Time span button (week)
        private Button monthButton;//Time span button (month)
        private Label stockInfoLabel;//Label that contains stock information
        private Label stockNameLabel;//Label that contains stock name and symbol
        List<double[]> chartValues;  //Contains all chart values
        JsonHandler js; //Json handler
        /// <summary>
        /// Displays information about a stock.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="s"></param>
        /// <param name="m"></param>
        public InfoDisplay(String type, String s, String m)
        {
            //Set generic width of mercury buttons
            buttonWidth = 120;
            buttonHeight = 30;
            js = new JsonHandler();
            chartPanel = new Panel();
            //Initialize controller class
            c = new Controller();
            symbol = s;
            market = m;
            //Check if type is a stock or market
            typeOfStock = type;
            if (typeOfStock == "stock")
            {
                stockList = js.getSingleStock(market, symbol, "month");
            }
            //Type is market, create dummy stocks from market values
            else if (typeOfStock == "market")
            {
                //Markets can't be added to portfolio
                portfolioCompatible = false;
                //Empty stock list
                stockList = new List<Stock>();
                List<Market> marketList = js.getSingleMarket(market, "month");
                foreach (Market temp in marketList)
                {
                    //Assign stock from current market value
                    Stock tempStock = new Stock();
                    tempStock.Symbol = temp.MarketName;
                    tempStock.Latest = temp.Latest;
                    tempStock.OpenVal = temp.OpenVal;
                    tempStock.Updated = temp.Updated;
                    tempStock.Volume = "-";
                    stockList.Add(tempStock);
                }
            }
            stockList = checkStock(stockList); 
            stockList = c.sortStockList(stockList, "Updated", false);
            Padding = new Padding(5, 3, 5, 3);
            this.BackColor = c.highlightWhite;

            //Get latest stock info
            Stock info = stockList.ElementAt(stockList.Count-1);

            //Initialize panels
            chartTypePanel = new FlowLayoutPanel();
            timeSpanPanel = new FlowLayoutPanel();
            centerChartType = new FlowLayoutPanel();
            centerTimeSpan = new FlowLayoutPanel();
            timeSpanFiller = new Panel();
            chartTypeFiller = new Panel();
            stockInfoPanel = new FlowLayoutPanel();
            //Create and add chart type buttons and listeners for charts
            //Candlestick
            candlestickButton = new MercuryButton("Candlestick", "candlestick");
            candlestickButton.Click += new EventHandler(chooseChartType);
            candlestickButton.Width = buttonWidth;
            candlestickButton.Height = buttonHeight;
            //Line chart (Latest)
            lineChartButton = new MercuryButton("Latest", "line");
            lineChartButton.Click += new EventHandler(chooseChartType);
            lineChartButton.Width = buttonWidth;
            lineChartButton.Height = buttonHeight;
            //Bar chart (Change)
            barChartButton = new MercuryButton("Change", "bar");
            barChartButton.Click += new EventHandler(chooseChartType);
            barChartButton.Width = buttonWidth;
            barChartButton.Height = buttonHeight;
            //Add chart type buttons to panel
            chartTypePanel.Controls.Add(lineChartButton);
            chartTypePanel.Controls.Add(candlestickButton);
            chartTypePanel.Controls.Add(barChartButton);
            //Add button and event listener to control timespan value 
            dayButton = new MercuryButton("Day", "day");
            dayButton.Click += new EventHandler(chooseTimeSpan);
            dayButton.Width = buttonWidth;
            dayButton.Height = buttonHeight;
            weekButton = new MercuryButton("Week", "week");
            weekButton.Click += new EventHandler(chooseTimeSpan);
            weekButton.Width = buttonWidth;
            weekButton.Height = buttonHeight;
            monthButton = new MercuryButton("Month", "month");
            monthButton.Click += new EventHandler(chooseTimeSpan);
            monthButton.Width = buttonWidth;
            monthButton.Height = buttonHeight;
            //Add timespan buttons to panel
            timeSpanPanel.Controls.Add(monthButton);
            timeSpanPanel.Controls.Add(weekButton);
            timeSpanPanel.Controls.Add(dayButton);
            //Add stock control panel to display
            this.Controls.Add(stockInfoPanel);
            //Initialize labels containing stock info
            stockInfoLabel = new Label();
            stockNameLabel = new Label();
            stockInfoLabel.Height = 30;
            stockNameLabel.Height = 30;
            stockInfoLabel.Margin = new Padding(0, 1, 0, 1);
            stockNameLabel.Margin = new Padding(0, 1, 0, 1);
            stockInfoPanel.Padding = new Padding(0, 1, 0, 1);
            //Initialize stock name
            String stockName = "";
            /*
             * If the info panel displays market info, set the name
             * of the market based on the symbol.
             */
            switch (info.Symbol)
            {
                case "LSE":
                    stockName = "London Stock Exchange";
                    break;
                case "OMX":
                    stockName = "Stockholm Stock Exchange";
                    break;
                case "NYSE":
                    stockName = "New York Stock Exchange";
                    break;
                default:
                    stockName = info.Name;
                    break;
            }
            //Initialize new tooltip
            ToolTip tt = new ToolTip();
            /*
             * Set text of first label.
             * - Latest
             * - Opening value
             * - Change (percent)
             * - Volume 
             */
            stockInfoLabel.Text = "Latest: " + info.Latest
                + "  |  Opening: " + info.OpenVal
                + "  |  Change: " + info.Percent
                + "  |  Volume: " + info.Volume;
            //Add tooltip to stock info label
            tt.SetToolTip(stockInfoLabel, stockInfoLabel.Text);
            //Set label to name and symbol of stock
            stockNameLabel.Text = stockName + "  |  " + info.Symbol;
            //Set font of info labels
            stockInfoLabel.Font = c.mercuryFont;
            stockNameLabel.Font = new Font("Segoe UI", 12, FontStyle.Regular);
            //Add labels to panel
            stockInfoPanel.Controls.Add(stockNameLabel);
            stockInfoPanel.Controls.Add(stockInfoLabel);
            //Add button for adding stocks to portfolio
            MercuryButton portfolio = new MercuryButton("Add to portfolio", info.Symbol);
            portfolio.Width = 170;
            portfolio.Height = 30;
            //Disable portfolio if current type is market
            if (!portfolioCompatible)
            {
                portfolio.Enabled = false;
                portfolio.BackColor = Color.LightGray;
            }
            //Add listener to portfolio button
            portfolio.Click += new EventHandler(addStockToPortfolio);
            //Add portfolio button to panel
            stockInfoPanel.Controls.Add(portfolio);
            //Add panels to info display
            this.Controls.Add(chartPanel);
            this.Controls.Add(centerChartType);
            //Add chart type and filler
            centerChartType.Controls.Add(chartTypeFiller);
            centerChartType.Controls.Add(chartTypePanel);
            //Add panel that centers time span buttons
            this.Controls.Add(centerTimeSpan);
            //Add time span and filler panel
            centerTimeSpan.Controls.Add(timeSpanFiller);
            centerTimeSpan.Controls.Add(timeSpanPanel);
            //Set all chart buttons to unclicked
            resetchartButtons();
            //Set all time span buttons to unclicked
            resetTimeSpanButtons();
            //Initialize new chart
            initilizeChart(typeOfChart, timeSpan);
            //Set line chart button to clicked
            lineChartButton.BackColor = c.mercuryBlue;
            //Set month button to clicked
            monthButton.BackColor = c.mercuryBlue;
            //Initliaze new integrated news list
            news = new IntegratedNewsList(symbol, m);
            //Set background color of news list
            news.BackColor = c.highlightWhite;
            //Assign news panel to news list
            newsPanel = news;
            //Add news panel to info display
            this.Controls.Add(newsPanel);
        }

        /// <summary>
        /// Initializes a new chart to display in the info display.
        /// </summary>
        /// <param name="typeOfChart_"></param>
        /// <param name="timeSpan_"></param>
        private void initilizeChart(String typeOfChart_, String timeSpan_)
        {
            //Create new list of stocks
            List<Stock> stockSpan = new List<Stock>();
            //Trim the stock list depending on the time span requested
            switch (timeSpan_)
            {
                case "day":
                    stockSpan = c.getValuesFromMidnight(stockList);
                    break;
                case "week":
                    stockSpan = c.getFilteredList(stockList, DateTime.Now, 6);
                    break;
                case "month":
                    stockSpan = stockList;
                    break;
            }
            //Set time span
            timeSpan = timeSpan_;
            //Clear current chart panel
            chartPanel.Controls.Clear();
            //Reset chart
            chart = new Chart();
            //Reset chart area
            chartArea = new ChartArea();
            //Set background color
            chart.BackColor = c.highlightWhite;
            chartArea.AxisY.Minimum = 0;
            chartArea.AxisY.Maximum = 5;
            chartArea.AxisY.Interval = 1;
            chart.ChartAreas.Add(chartArea);
            //Set size of chart based on the chart panel
            chart.Width = chartPanel.Width;
            chart.Height = chartPanel.Height;
            chartArea.BackColor = c.highlightWhite;
            //Set colors of grid lines
            chartArea.AxisX.MajorGrid.LineColor = Color.LightGray;
            chartArea.AxisY.MajorGrid.LineColor = Color.LightGray;
            //Add chart panel to info display
            chartPanel.Controls.Add(chart);
            //series = new Series("prices");
            //chart.Series.Add(series);
            if (stockSpan.Count > 0)
            {
                //If the time span is only for one day, only get dates for that day
                if (timeSpan_ == "day")
                {
                    dates = getAllDateTime(stockSpan);
                    chartValues = getAllChartValues(stockSpan);
                }
                //Get all days for the stocks
                else
                {
                    dates = getStockDays(stockSpan);
                    chartValues = getChartWeekMonthValues(stockSpan);
                }
                //Set maximum X axis
                chartArea.AxisX.Maximum = dates.Count() + 1;
                //Set padding for chart
                double paddingValue = (c.getStockMinMaxValue(stockSpan, "max") * 1.05) - c.getStockMinMaxValue(stockSpan, "max");
                //Set minimum padding value
                //if (paddingValue < 1) { paddingValue = 0.5; }
                // set max and min y values to the area (plus padding)
                double minYValue = Math.Round(c.getStockMinMaxValue(stockSpan, "min") - paddingValue, 3);
                double maxYValue = Math.Round(c.getStockMinMaxValue(stockSpan, "max") + paddingValue, 3);
                //Round off interval numbers if Y values gets to certain sizes
                if (maxYValue > 10)
                {
                    maxYValue = Math.Round(maxYValue, 2);
                }
                if (minYValue < -10)
                {
                    minYValue = Math.Round(minYValue, 2);
                }
                //Set minimum and maximum values for chart
                chartArea.AxisY.Minimum = minYValue;
                chartArea.AxisY.Maximum = maxYValue;
                chartArea.AxisX.Minimum = 0;
                //Set intervals variables
                xInterval = 1;
                yInterval = ((maxYValue - minYValue) / 5);
                if (yInterval < 0)
                {
                    yInterval *= -1;
                }
                //If interval somehow becomes 0, change interval to an accaptable value
                if (yInterval == 0)
                {
                    yInterval = double.Parse(stockSpan.ElementAt(0).Latest, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) / 5;
                }
                //Set interval of chart
                chartArea.AxisX.MajorGrid.Enabled = false;
                chartArea.AxisX.Interval = xInterval;
                chartArea.AxisY.MajorGrid.Interval = yInterval;
                chartArea.AxisY.Interval = yInterval;
                //Process the stock values depending on the chart type requested
                switch (typeOfChart_)
                {
                    case "candlestick":
                        //Initialize candle stick chart
                        initilizeCandleStick(chartValues);
                        break;
                    case "line":
                        //Initialize line chart
                        initializeLineChart(chartValues);
                        break;
                    case "bar":
                        //Get bar chart values for chart
                        List<double> barChartValues_ = getBarChartValues(chartValues);
                        //Set padding value
                        if (paddingValue < 0)
                        {
                            paddingValue *= -1;
                        }
                        //Set minimum and maximum value
                        chartArea.AxisY.Minimum = Math.Round(barChartValues_.Min() - paddingValue, 3);
                        chartArea.AxisY.Maximum = Math.Round(barChartValues_.Max() + paddingValue, 3);
                        //Round off maximum and minimum value
                        if (maxYValue > 10)
                        {
                            maxYValue = Math.Round(maxYValue, 2);
                        }
                        if (minYValue < -10)
                        {
                            minYValue = Math.Round(minYValue, 2);
                        }
                        //Set Y interval
                        yInterval = ((chartArea.AxisY.Maximum - chartArea.AxisY.Minimum) / 5);
                        if (yInterval == 0)
                        {
                            yInterval = double.Parse(stockSpan.ElementAt(0).Latest, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) / 5;
                        }
                        else if (yInterval < 0)
                        {
                            yInterval *= -1;
                        }
                        //Set grid interval
                        chartArea.AxisY.MajorGrid.Interval = yInterval;
                        chartArea.AxisY.Interval = yInterval;
                        //Initialize bar chart
                        initializeBarChart(barChartValues_);
                        break;
                }
                //Add mouse listener
                this.chart.MouseLeave += chart_MouseExit;
                this.chart.MouseDown += chart_MouseDown;
                //Add event listener to control stock value 
                chart.GetToolTipText += getHoverLabel;
                
            }
            else
            {
                
            }
        }

        /// <summary>
        /// Sets the current chart type based on the button that was clicked, then initialize the new chart.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void chooseChartType(object sender, EventArgs e)
        {
            MercuryButton mb = sender as MercuryButton;
            typeOfChart = mb.buttonType;
            if (typeOfChart == "candlestick")
            {
                dayButton.Enabled = false;
            }
            else
            {
                dayButton.Enabled = true;
            }
            //Reset time span
            timeSpan = "month";
            resetchartButtons();
            resetTimeSpanButtons();
            initilizeChart(typeOfChart, "month");
            monthButton.BackColor = c.mercuryBlue;
            mb.BackColor = c.mercuryBlue;
        }

        /// <summary>
        /// Sets the current time span based on the button that was clicked, then initializes the new chart.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void chooseTimeSpan(object sender, EventArgs e)
        {
            MercuryButton mb = sender as MercuryButton;
            if (typeOfStock == "candlestick")
            {
                dayButton.Enabled = false;
            }
            resetTimeSpanButtons();
            timeSpan = mb.buttonType;
            mb.BackColor = c.mercuryBlue;
            initilizeChart(typeOfChart, timeSpan);
        }

        /// <summary>
        /// Resets all chart type buttons.
        /// </summary>
        private void resetchartButtons()
        {
            c.resetButton(candlestickButton);
            c.resetButton(lineChartButton);
            c.resetButton(barChartButton);
        }

        /// <summary>
        /// Resets all time span buttons.
        /// </summary>
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
                        label_ = string.Format("o: €{0:F2}, c: €{1:F2}, h: €{2:F2}, l: €{3:F2}", dp.YValues[2], dp.YValues[3], dp.YValues[0], dp.YValues[1]);
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

        /// <summary>
        /// Listener that resets all chart value labels when mouse exits.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void chart_MouseExit(object sender, EventArgs e)
        {
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

        /// <summary>
        /// Listener that resets all chart value labels when left mouse button is pressed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
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

        /// <summary>
        /// Initializes a new candlestick chart.
        /// </summary>
        /// <param name="list"></param>
        public void initilizeCandleStick(List<double[]> list)
        {
            //Set new series of values
            series = new Series("prices");
            //Set series color
            series.Color = c.mercuryRed;
            //Add series to chart
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
                if (i != 0)
                {
                    //If current value is lower than previous, set color to red.
                    if (list[i].ElementAt(1) < list[i - 1].ElementAt(1))
                    {
                        chart.Series["prices"].Points[i].Color = c.mercuryRed; 
                        chart.Series["prices"].Points[i].BorderColor = c.mercuryRed;
                        chart.Series["prices"].Points[i]["PriceDownColor"] = "#DC3522";
                    }
                }
            }
        }

        /// <summary>
        /// Initializes a new line chart.
        /// </summary>
        /// <param name="list"></param>
        private void initializeLineChart(List<double[]> list)
        {
            //Set new series of values
            series = new Series("prices");
            //Add series to chart
            chart.Series.Add(series);
            //Series settings
            chart.Series["prices"].ChartType = SeriesChartType.Line;
            chart.Series["prices"].Color = c.mercuryBlue;
            chart.Series["prices"].BorderWidth = 2;
            chart.Series["prices"]["PointWidth"] = "4";
            chart.Series["prices"].MarkerStyle = MarkerStyle.Circle;
            for (int i = 0; i < list.Count(); i++)
            {
                //Add points to chart
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

        /// <summary>
        /// Initializes a new bar chart.
        /// </summary>
        /// <param name="list"></param>
        private void initializeBarChart(List<double> list)
        {
            //Set new series of values
            series = new Series("prices");
            //Add series to chart
            chart.Series.Add(series);
            //Chart series settings
            chart.Series["prices"].Color = c.mercuryBlue;
            chart.Series["prices"].ChartType = SeriesChartType.Column;
            chart.Series["prices"].BorderWidth = 1;
            chart.Series["prices"]["PointWidth"] = "0.5";
            for (int i = 0; i < list.Count(); i++)
            {
                chart.Series["prices"].Points.AddXY(i + 1, list[i]);
                if (i < dates.Count)
                {
                    //Set color of bar to red or blue based on if it's over or below 0
                    if (list[i] > 0)
                    {
                        chart.Series["prices"].Points[i].Color = c.mercuryBlue;
                    }
                    else
                    {
                        chart.Series["prices"].Points[i].Color = c.mercuryRed;
                    }
                    //Add dates as days
                    if (timeSpan != "day" && i < dates.Count && i < list.Count)
                    {
                        chart.Series["prices"].Points[i].AxisLabel = dates[i].Day + "/" + dates[i].Month;
                    }
                    //Add dates as time
                    else if (timeSpan == "day" && i < dates.Count && i < list.Count)
                    {
                        chart.Series["prices"].Points[i].AxisLabel = dates[i].ToShortTimeString();
                    }
                }
            }
        }

        /// <summary>
        /// Get all days from a list of stocks.
        /// </summary>
        /// <param name="stocks"></param>
        /// <returns></returns>
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
                }
            }
            dates.Add(c.getDate(stocks[stocks.Count - 1].Updated));
            return dates;
        }

        /// <summary>
        /// Get all stock values (opening and latest) within week or month.
        /// Each list element contains 4 values:
        /// - Opening
        /// - Closing
        /// - Lowest
        /// - Highest
        /// </summary>
        /// <param name="stocks"></param>
        /// <returns></returns>
        private List<Double[]> getChartWeekMonthValues(List<Stock> stocks)
        {
            if (stocks.Count > 0){
                List<Stock> tempStocks = stocks;
            //List containing all candlestick values
            List<double[]> chartValueList = new List<double[]>();
            List<DateTime> dates = getStockDays(stocks);
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
                //Add closing value
                values[1] = dayValues[dayValues.Count - 1];
                //Add open val as value to get max and min value
                dayValues.Add(openVal_);
                //Get lowest value
                values[2] = dayValues.Min();
                //Add highest value
                values[3] = dayValues.Max();
                //Add candlestick to list
                chartValueList.Add(values);

            }
            return chartValueList;
            }
            return null;
        }

        /// <summary>
        /// Gets all stock values. Each list element contains:
        /// - Opening
        /// - Latest
        /// </summary>
        /// <param name="stocks"></param>
        /// <returns></returns>
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

        /// <summary>
        /// Sets size of the info display
        /// </summary>
        /// <param name="W"></param>
        /// <param name="H"></param>
        public void setSize(int W, int H)
        {
            this.Width = W;
            this.Height = H;

            chartPanel.Width = W -(Padding.Right*2+chartPanel.Margin.Right*2);
            if (typeOfStock != "market")
            {
                chartPanel.Height = H / 3;

            }
            else
            {
                chartPanel.Height = H - H/3;
            }
            //Set size of panel containing stock information
            stockInfoPanel.Height = 100;
            stockInfoPanel.Width = this.Width;
            //Set size of stock info labels
            stockInfoLabel.Width = stockInfoPanel.Width;
            stockNameLabel.Width = stockInfoPanel.Width;
            chart.Width = chartPanel.Width;
            chart.Height = chartPanel.Height;
            chartTypePanel.Width = (buttonWidth + lineChartButton.Margin.Left + lineChartButton.Margin.Right) * 3 + chartTypePanel.Padding.All;
            timeSpanPanel.Width = (buttonWidth + lineChartButton.Margin.Left + lineChartButton.Margin.Right) * 3 + timeSpanPanel.Padding.All;
            chartTypePanel.Height = (buttonHeight + lineChartButton.Margin.Top + lineChartButton.Margin.Bottom) + timeSpanPanel.Padding.All;
            timeSpanPanel.Height = (buttonHeight + lineChartButton.Margin.Top + lineChartButton.Margin.Bottom) + timeSpanPanel.Padding.All;
            centerChartType.Width = W;
            centerChartType.Height = chartTypePanel.Height;
            centerTimeSpan.Width = W;
            centerTimeSpan.Height = timeSpanPanel.Height;
            timeSpanFiller.Width = (this.Width / 2) - (timeSpanPanel.Width / 2);
            chartTypeFiller.Width = (this.Width / 2) - (chartTypePanel.Width / 2);
            //Set news panel size
            newsPanel.Width = W;
            newsPanel.Height = H - chartPanel.Height - timeSpanPanel.Height - chartTypePanel.Height - this.Margin.All-130;
            newsPanel.Location = new Point((W - newsPanel.Width) / 2);
            news.setSize(W, newsPanel.Height);
        }
        /// <summary>
        /// Returns all DateTime values in a list of stocks values.
        /// </summary>
        /// <param name="stockList_"></param>
        /// <returns></returns>
        private List<DateTime> getAllDateTime(List<Stock> stockList_)
        {
            List<DateTime> listOfDates_ = new List<DateTime>();
            foreach (Stock s in stockList_)
            {
                listOfDates_.Add(c.getDate(s.Updated));
            }
            return listOfDates_; 
        }
        /// <summary>
        /// Returns all values used in bar charts.
        /// </summary>
        /// <param name="list_"></param>
        /// <returns></returns>
        private List<Double> getBarChartValues(List<Double[]> list_)
        {
            List<Double> changes = new List<Double>();
            for (int count = 0; count < list_.Count; count++)
            {
                changes.Add(list_[count].ElementAt(1) - list_[count].ElementAt(0));
            }
            return changes;
        }
        /// <summary>
        /// Adds current stock to portfolio.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void addStockToPortfolio(object sender, EventArgs e)
        {
            MercuryButton button = sender as MercuryButton;
            c.addToPortfolio(button.buttonType);
        }
        /// <summary>
        /// Checks stock list and remove invalid stocks.
        /// </summary>
        /// <param name="stockList"></param>
        /// <returns></returns>
        private List<Stock> checkStock(List<Stock> stockList)
        {
            List<Stock> checkedStocks = new List<Stock>();
            foreach (Stock s in stockList)
            {
                if (!(s.Latest == "-" || s.OpenVal == "-"))
                {
                    if (s.Latest == "-0.00")
                    {
                        s.Latest = "0.00";
                    }
                    if (s.OpenVal == "-0.00")
                    {
                        s.OpenVal = "0.00";
                    }
                    checkedStocks.Add(s);
                }
            }
            return checkedStocks; 
        }
    }
}
