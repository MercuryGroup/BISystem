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
    class InfoDisplay: Panel
    {
        
        private Chart chart;
        private ChartArea chartArea;
        private Series series; 

        private String typeOfChart = "candlestick";

        /*
         * Show day high and low for candlestick??
         *
        */
        public InfoDisplay()
        {   
            initilizeChart(typeOfChart);
            this.BackColor = Color.Pink;
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

            this.Controls.Add(chart); 
        
        }

        public void initilizeCandeleStick()
        {
            series = new Series("Stock prices");
            series.ChartType = SeriesChartType.Candlestick;
            series.Color = System.Drawing.Color.Red;
            
            series.Points.AddXY(1, 50);
		    series.Points.AddXY(2, 25);
		    series.Points.AddXY(3, 70);
            series.Points.AddXY(4, 20);
           
            chart.Series.Add(series);

        }

        public void setSize(int W, int H)
        {
            this.Width = W;
            this.Height = H;
            UpdateChart(W, H); 
        }

        public void UpdateChart(int W, int H)
        {
            chart.Width = W;
            chart.Height = 400;
        }

        
    }
}
