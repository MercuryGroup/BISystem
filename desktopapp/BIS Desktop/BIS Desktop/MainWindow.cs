using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    public partial class MainWindow : Form
    {
        private Boolean marketClicked, stocksClicked, newsClicked, portfolioClicked;
        ResultList leftPanelValue;
        public MainWindow()
        {
            //Set all button values
            marketClicked = false;
            stocksClicked = false;
            newsClicked = false;
            portfolioClicked = false;
            
            InitializeComponent();
        }

        private void menu_Paint(object sender, PaintEventArgs e)
        {

        }
        /*
         * Function that triggers whenever a button is clicked
         */
        private void menuClick(String button, Boolean toggled)
        {
            //Color used for button that has been clicked
            Color buttonColor = Color.CornflowerBlue;
            /*
             * If the button clicked wasn't earlier,
             * set all other buttons to unclicked.
             */
            if (!toggled)
            {
                resetMenuButtons();
            }
            //Remove earlier content in left and right panel
            if(leftPanel.Contains(leftPanelValue))
            {
                leftPanel.Controls.Remove(leftPanelValue);
            }
            //Check which button that was clicked
            switch (button)
            {
                    //Market
                case "market":
                    leftPanelValue = new ResultList("market");
                    marketButton.BackColor = buttonColor;
                    break;
                    //Stocks
                case "stocks":
                    leftPanelValue = new ResultList("stocks");
                    stocksButton.BackColor = buttonColor;
                    break;
                    //News
                case "news":
                    leftPanelValue = new ResultList("news");
                    newsButton.BackColor = buttonColor;
                    break;
                    //Portfolio
                case "portfolio":
                    leftPanelValue = new ResultList("portfolio");
                    portfolioButton.BackColor = buttonColor;
                    break;
                default:
                    break;
            }
            //Add 
            leftPanel.Controls.Add(leftPanelValue);
            leftPanelValue.setSize(leftPanel.Width, leftPanel.Height);
            
        }
        private void resetMenuButtons()
        {
            marketClicked = false;
            stocksClicked = false;
            newsClicked = false;
            portfolioClicked = false;
            marketButton.BackColor = Color.LightGray;
            newsButton.BackColor = Color.LightGray;
            stocksButton.BackColor = Color.LightGray;
            portfolioButton.BackColor = Color.LightGray;
        }

        private void stocksButton_Click(object sender, EventArgs e)
        {
            
            if (!stocksClicked)
            {
                menuClick("stocks", stocksClicked);
                stocksClicked = true;
            }
        }
        private void marketButton_Click(object sender, EventArgs e)
        {
            
            if (!marketClicked)
            {
                menuClick("market", marketClicked);
                marketClicked = true;
            }
        }

        private void newsButton_Click(object sender, EventArgs e)
        {
            if (!newsClicked)
            {
                menuClick("news", newsClicked);
                newsClicked = true;
            }
        }

        private void portfolioButton_Click(object sender, EventArgs e)
        {
            if (!portfolioClicked)
            {
                menuClick("portfolio", portfolioClicked);
                portfolioClicked = true;
            }
        }
    }
}
