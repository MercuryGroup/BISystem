using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace BIS_Desktop
{

    public partial class MainWindow : Form
    {
        private bool dragging = false;
        private Point startPoint = new Point(0, 0);

        
        /// <summary>
        /// ///
        /// </summary>
        private Boolean marketClicked, 
            stocksClicked, newsClicked, 
            portfolioClicked, maximized;
        private int minimizeButtonX, 
            maximizeButtonX, closeButtonX,
            labelButtonWidth, labelButtonMargin;
        ResultList leftPanelContent;
        public MainWindow()
        {
            //Set all boolean values
            marketClicked = false;
            stocksClicked = false;
            newsClicked = false;
            portfolioClicked = false;
            maximized = false;

            labelButtonWidth = 20;
            labelButtonMargin = 6;

            InitializeComponent();
            dragPanel.MouseDown += new MouseEventHandler(dragPanel_MouseDown);
            dragPanel.MouseMove += new MouseEventHandler(dragPanel_MouseMove);
            dragPanel.MouseUp += new MouseEventHandler(dragPanel_MouseUp);
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
            if(leftPanel.Contains(leftPanelContent))
            {
                leftPanel.Controls.Remove(leftPanelContent);
            }

            
            //Check which button that was clicked
            switch (button)
            {

                    //Market
                case "market":
                    leftPanelContent = new ResultList("market");
                    marketButton.BackColor = buttonColor;
                    break;
                    //Stocks
                case "stocks":
                    leftPanelContent = new ResultList("stocks");
                    stocksButton.BackColor = buttonColor;
                    break;
                    //News
                case "news":
                    leftPanelContent = new ResultList("news");
                    newsButton.BackColor = buttonColor;
                    break;
                    //Portfolio
                case "portfolio":
                    leftPanelContent = new ResultList("portfolio");
                    portfolioButton.BackColor = buttonColor;
                    break;
                default:
                    break;
            }
            //Add content to left panel
            leftPanel.Controls.Add(leftPanelContent);
            //Set size of content based on panel
            leftPanelContent.setSize(leftPanel.Width, leftPanel.Height);
        }
        /*
         * Sets all buttons to unclicked
         */
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
        /*
         * Stock button event handler
         */
        private void stocksButton_Click(object sender, EventArgs e)
        {
            menuClick("stocks", stocksClicked);
        }
        /*
         * Market button event handler
         */
        private void marketButton_Click(object sender, EventArgs e)
        {
            menuClick("market", marketClicked);
        }
        /*
         * News button event handler
         */
        private void newsButton_Click(object sender, EventArgs e)
        {
            menuClick("news", newsClicked);
        }
        /*
         * Portfolio button event handler
         */
        private void portfolioButton_Click(object sender, EventArgs e)
        {
            menuClick("portfolio", portfolioClicked);
        }

        private void dragPanel_Paint(object sender, PaintEventArgs e)
        {
            
        }
        private void dragPanel_MouseDown(object sender, MouseEventArgs e)
        {
            dragging = true;  
            startPoint = new Point(e.X, e.Y);
        }

        private void dragPanel_MouseUp(object sender, MouseEventArgs e)
        {
            dragging = false;
        }

        private void dragPanel_MouseMove(object sender, MouseEventArgs e)
        {
            if (dragging)
            {
                Point p = PointToScreen(e.Location);
                Location = new Point(p.X - this.startPoint.X, p.Y - this.startPoint.Y);
            }
        }

        private void maximizeLabel_Click(object sender, EventArgs e)
        {
            toggleMaximize();
        }
        private void closeLabel_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }
        /// <summary>
        /// Changes all necessary settings when the window is maximized.
        /// </summary>
        private void toggleMaximize()
        {
            if (!maximized)
            {
                minimizeButtonX = minimizeLabel.Location.X;
                maximizeButtonX = maximizeLabel.Location.X;
                closeButtonX = closeLabel.Location.X;
                //Set window state to full screen
                WindowState = FormWindowState.Maximized;
                //Change color of maximize button
                maximizeLabel.ForeColor = Color.Firebrick;
                //set drag panel width
                dragPanel.Width = MainWindow.ActiveForm.Width;
                //Set maximized boolean to true
                maximized = true;
                //Change location of close button
                closeLabel.Location = new Point(MainWindow.ActiveForm.Width - (labelButtonWidth + labelButtonMargin), closeLabel.Location.Y);
                maximizeLabel.Location = new Point(closeLabel.Location.X - (labelButtonWidth + labelButtonMargin*2), maximizeLabel.Location.Y);
                minimizeLabel.Location = new Point(maximizeLabel.Location.X - (labelButtonWidth + labelButtonMargin), minimizeLabel.Location.Y);
            }
            //Window is already maximized
            else
            {
                //Change state to window
                WindowState = FormWindowState.Normal;
                //Change color of maximize button
                maximizeLabel.ForeColor = Color.Silver;
                //Set maximized boolean to false
                maximized = false;
            }
            //get close box current position
            
            
            //set close box position

        }

        
    }
    
}
