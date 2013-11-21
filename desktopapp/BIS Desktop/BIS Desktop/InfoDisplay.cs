using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace BIS_Desktop
{
    class InfoDisplay: Panel
    {
        private Chart chart;
        private ChartArea chartArea;
        private String typeOfChart = "candlestick";

        /*
         * Show day high and low for candlestick??
         *
        */
        public InfoDisplay()
        {
            initilizeChart(typeOfChart);
        }

        private void initilizeChart(String chart)
        {

            
            chartArea = new ChartArea();

            //chart = new Chart();

          
            switch (chart)
            {
                case "candlestick":
                    initilizeCandeleStick();
                    break;
            }
        
        }

        public void initilizeCandeleStick()
        {

            var series1 = new System.Windows.Forms.DataVisualization.Charting.Series
            {
                Name = "stock",
                Color = System.Drawing.Color.Red,
                IsVisibleInLegend = false,
                IsXValueIndexed = true,
                ChartType = SeriesChartType.Candlestick

            };
        }
        
    }
}
