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
        
        private Chart chart;
        private ChartArea chartArea;
        private Series series;
        private RadioButton rbMonth, rbWeek, rbDay;
        private Panel buttonPanel;

        private String typeOfChart = "candlestick";
        private String Symbol;
        /*
         * Show day high and low for candlestick??
         *
        */
        public InfoDisplay(String s)
        {
            Console.WriteLine("Symbol" +s);
            Symbol = s;
            initilizeChart(typeOfChart);
            this.BackColor = Color.White;

            initilizeRadioButtons();


            this.Controls.Add(chart);
            this.Controls.Add(buttonPanel);

        }

        private void initilizeChart(String typeOfChart)
        {

            chart = new Chart();
            chartArea = new ChartArea();
            // set max and min values to the area
            chartArea.AxisX.Minimum = 0;
            chartArea.AxisX.Maximum = 1000;
            chartArea.AxisY.Minimum = 0;
            chartArea.AxisY.Maximum = 1000;

            chart.ChartAreas.Add(chartArea);

            switch (typeOfChart)
            {
                case "candlestick":
                    initilizeCandeleStick();
                    break;
            }


            buttonPanel = new FlowLayoutPanel();
         
            buttonPanel.Controls.Add(rbMonth);
            buttonPanel.Controls.Add(rbWeek);
            buttonPanel.Controls.Add(rbDay);

            // TEMPORARY
            RichTextBox sd = new RichTextBox();
            JsonHandler js = new JsonHandler();
            List<Stock> temp = js.getSingleStock(Symbol, "month");

            sd.Text = temp[0].Name+"\n" +temp[0].Volume;
            this.Controls.Add(sd); 
    
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

            for (int i = 0; i < 50; i++)
            {

                //adding X and high
                chart.Series["prices"].Points.AddXY((i * 100) + 1, (i*20)+ 100);
                // adding low
                chart.Series["prices"].Points[i].YValues[1] = ((i*2) +10);
                //adding open
                chart.Series["prices"].Points[i].YValues[2] = ((i*2) + 50);
                // adding close
                chart.Series["prices"].Points[i].YValues[3] = ((i*2) + 100);   
                
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
