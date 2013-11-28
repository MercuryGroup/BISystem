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
        private Series series;
        private RadioButton rbMonth, rbWeek, rbDay;
        private Panel buttonPanel;
        private List<Stock> temp;
        private int maximumLength;
        private String typeOfChart = "candlestick";
        private String Symbol;
        private Panel chartPanel;
        private Controller c;
        
        ToolTip tooltip;
        JsonHandler js;
        /*
         * Show day high and low for candlestick??
         *
        */
        public InfoDisplay(String s)
        {
            js = new JsonHandler();
            chartPanel = new Panel();

            
            Symbol = s;
            initilizeChart(typeOfChart);
            this.BackColor = Color.White;

            initilizeRadioButtons();
            StockChart SC = new StockChart();

            this.Controls.Add(chart);
            this.Controls.Add(buttonPanel);
            //temp = js.getSingleStock(Symbol, "month");

        }
        private void initilizeChart(String typeOfChart)
        {
            /**
             * TODO:
             * [ ] Set y maximum value based on content
             * [ ] Set interval based on day/week/month
             * [ ] Get largest and smallest value
             * CURRENT
             * [/] Add mouse hover listener to chart areas
             */
            
            chart = new Chart();
            chartArea = new ChartArea();

            // set max and min values to the area
            chartArea.AxisX.Minimum = 0;

            //This value should be based on the dates
            chartArea.AxisX.Maximum = 30;
            chartArea.AxisX.Interval = 1;;

            chartArea.AxisY.Minimum = 0;

            //set y maximum value based
            chartArea.AxisY.Maximum = 1000;

            chart.ChartAreas.Add(chartArea);

            switch (typeOfChart)
            {
                case "candlestick":
                    initilizeCandeleStick();
                    break;
            }

            //Retreive data from json handler
            temp = js.getSingleStock(Symbol, "month");

            //Add buttons
            buttonPanel = new FlowLayoutPanel();
         
            buttonPanel.Controls.Add(rbMonth);
            buttonPanel.Controls.Add(rbWeek);
            buttonPanel.Controls.Add(rbDay);

            // TEMPORARY
            RichTextBox sd = new RichTextBox();

            
            maximumLength = 30;

            Console.WriteLine("MAX: " + c.getMinMaxValue(temp, "max") + " MIN: " + c.getMinMaxValue(temp, "min"));
            
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
            for (int i = 0; i < 30; i++)
            {
                ToolTip tip = new ToolTip();
                //adding X and high
                chart.Series["prices"].Points.AddXY(i, (i*20)+ 100);
                
                //chart.Series["prices"].ToolTip = "X: #VALX\nY: #VALY";
                chart.Series["prices"].Points[i].ToolTip = "";


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
