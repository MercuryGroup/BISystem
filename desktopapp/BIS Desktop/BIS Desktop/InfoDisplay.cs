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
        private DateTime dateBack;
        
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
                    stockSpan = c.getFilteredList(temp, 1);
                    chartArea.AxisX.Maximum = 4;
                    break;
                case "week":
                    //TEMP VALUE
                    stockSpan = c.getFilteredList(temp, 7);
                    chartArea.AxisX.Maximum = 7;
                    break;
                case "month":
                    stockSpan = temp;
                    chartArea.AxisX.Maximum = stockSpan.Count();
                    
                    break;
            }
           
            // set max and min y values to the area (plus padding)
            double minYValue = c.getStockMinMaxValue(stockSpan, "min");
            double maxYValue = c.getStockMinMaxValue(stockSpan, "max")*1.1;
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

            chartArea.AxisX.MajorGrid.Interval = xInterval;
            chartArea.AxisX.Interval = xInterval;
            chartArea.AxisY.MajorGrid.Interval = yInterval;
            chartArea.AxisY.Interval = yInterval;
            //Set colors
            chartArea.AxisX.MajorGrid.LineColor = Color.LightGray;
            chartArea.AxisY.MajorGrid.LineColor = Color.LightGray;
            chartArea.BackColor = Color.Beige;
            
            //chartArea.AxisX.MinorGrid.Interval = 4;
            Console.WriteLine("p " + chartArea.AxisY.Interval + " " + maxYValue);
            

            chart.ChartAreas.Add(chartArea);

            switch (typeOfChart)
            {
                case "candlestick":
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
          
            series.Color = System.Drawing.Color.Blue;
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
