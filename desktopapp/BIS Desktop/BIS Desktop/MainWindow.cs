using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace BIS_Desktop
{

    public partial class MainWindow : Form
    {
        /**
         * TODO:
         * [ ] Add label "beneath" move window
         * [ ] Reset market buttons
         * [ ] Info display
         * [ ] Portfolio
         */

        private Point startPoint = new Point(0, 0);
        //Booleans that checks clicks
        private Boolean marketClicked, 
            stocksClicked, newsClicked, 
            portfolioClicked, maximized,
            dragging, lseClicked, nyseClicked,
            omxClicked;
        /*
         * Inner panels that will handle the parsed results 
         * for both left and right panels
         */
        ResultPanel leftPanelResults, rightPanelResults;
        //Color used for buttons that have been clicked.
        Color buttonClickedColor, buttonColor, highlightWhite,
            loading;
        public MainWindow()
        {

            //Initialize all boolean values
            dragging = false;
            marketClicked = false;
            stocksClicked = false;
            newsClicked = false;
            portfolioClicked = false;
            maximized = false;
            lseClicked = false;
            nyseClicked = false;
            omxClicked = false;
            //Initialize left and right parent panels
            leftPanel = new Panel();
            rightPanel = new Panel();
            //Initialize component
            InitializeComponent();
            //Initialize result panels
            leftPanelResults = new ResultPanel();
            rightPanelResults = new ResultPanel();
            //Add result panels to left and right parent panels
            leftPanel.Controls.Add(leftPanelResults);
            rightPanel.Controls.Add(rightPanelResults);
            //Set color for different buttons
            buttonColor = System.Drawing.ColorTranslator.FromHtml("#A2A2A2");
            buttonClickedColor = System.Drawing.ColorTranslator.FromHtml("#354A69");
            highlightWhite = System.Drawing.ColorTranslator.FromHtml("#FAFAFA");
            loading = System.Drawing.ColorTranslator.FromHtml("#F2F2F2");
            //Set background colors for left and right panel
            leftPanel.BackColor = loading;
            rightPanel.BackColor = loading;
            //Reset buttons
            resetMenuButtons();
            resetMarketButtons();
            //Refresh all content panels
            refreshContentPanels();
            //Add event handlers to all necessary panels
            dragPanel.MouseDown += new MouseEventHandler(dragPanel_MouseDown);
            dragPanel.MouseMove += new MouseEventHandler(dragPanel_MouseMove);
            dragPanel.MouseUp += new MouseEventHandler(dragPanel_MouseUp);
            this.SizeChanged += MainWindow_SizeChanged;
        }
        //Create backdrop for window
        protected override CreateParams CreateParams
        {
            get
            {
                const int CS_DROPSHADOW = 0x20000;
                CreateParams createparams = base.CreateParams;
                createparams.ClassStyle |= CS_DROPSHADOW;
                return createparams;
            }
        }
        /// <summary>
        /// Event handler that checks if the main window has changed size.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void MainWindow_SizeChanged(object sender, EventArgs e)
        {
            //Check only if the window has just been maximized
            if (WindowState == FormWindowState.Maximized)
            {
                //Change color of maximize button
                maximizeLabel.ForeColor = Color.Firebrick;
                //set drag panel width
                dragPanel.Width = MainWindow.ActiveForm.Width;
                //Set maximized boolean to true
                maximized = true;
                //Refresh content panels
                refreshContentPanels();
            }
        }

        private void menu_Paint(object sender, PaintEventArgs e)
        {

        }
        
        /// <summary>
        /// Function that triggers whenever a menu button is clicked.
        /// </summary>
        /// <param name="button"></param>
        /// <param name="toggled"></param>
        private void menuClick(String button, Boolean toggled)
        {
            
            /*
             * If the button wasn't set to clicked before,
             * set all other buttons to unclicked.
             */
            if (!toggled)
            {
                resetMenuButtons();
            }
            
            //Check which button that was clicked
            switch (button)
            {
                //Market
                case "market":

                    //Change color of button
                    loadResult(rightPanelResults, "info", button);
                    marketButton.BackColor = buttonClickedColor;
                    marketButton.ForeColor = Color.White;
                    //Change color of london button
                    //lseButton.PerformClick();
                    lseButton.BackColor = buttonClickedColor;
                    lseButton.ForeColor = Color.White;
                    break;
                //Stocks
                case "stocks":
                    //Set left panel to display a result list (stocks)
                    loadResult(leftPanelResults, "list", button);
                    //Change color of button
                    stocksButton.BackColor = buttonClickedColor;
                    stocksButton.ForeColor = Color.White;
                    //Change color of london button
                    lseButton.BackColor = buttonClickedColor;
                    lseButton.ForeColor = Color.White;
                    break;
                //News
                case "news":
                    //Set left panel to display a result list (news)
                    loadResult(leftPanelResults, "list", button);
                    //Change color of button
                    newsButton.BackColor = buttonClickedColor;
                    newsButton.ForeColor = Color.White;
                    //Change color of london button
                    lseButton.BackColor = buttonClickedColor;
                    lseButton.ForeColor = Color.White;
                    break;
                //Portfolio
                case "portfolio":
                    //Change color of button
                    portfolioButton.BackColor = buttonClickedColor;
                    portfolioButton.ForeColor = Color.White;
                    break;
                default:
                    break;
            }
            
        }
        private void marketClick(String button, Boolean toggled)
        {

            /*
             * If the button wasn't set to clicked before,
             * set all other buttons to unclicked.
             */
            if (!toggled)
            {
                resetMarketButtons();
            }

            //Check which button that was clicked
            switch (button)
            {
                //Market
                case "market":

                    
                    break;
                //Stocks
                case "stocks":
                    
                    break;
                //News
                case "news":
                    
                    break;
                //Portfolio
                case "portfolio":
                    
                    break;
                default:
                    break;
            }

        }
        /// <summary>
        /// Load result to the calling panel.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="contentType"></param>
        /// <param name="resultCategory"></param>
        private void loadResult(object sender, String resultType, String resultSource)
        {
            /**
             * TODO:
             * [X] Kolla vilken panel som skickade request
             * [X] Lägg på "loading" panel
             * [ ] Ladda ny tråd för att hämta innehåll
             */

            //Instantiate current panel
            ResultPanel panel = sender as ResultPanel;
            //Clear current panel
            panel.Controls.Clear();
            //Set current panel to loading (if the panel isn't already loading content)
            if (!panel.getLoading())
            {

                //Create loading panel
                Panel loadingPanel = new Panel();
                //Create loading label
                Label loadingLabel = new Label();
                //Set loading panel do fill
                loadingPanel.Dock = DockStyle.Fill;
                /*
                 * Set custom position for loading label.
                 * The Y position should always be based on 
                 * the right panel since that panel is the biggest.
                 */
                int x = (panel.Width / 2) - (loadingLabel.Width / 2);
                int y = (panel.Height / 2) - (loadingLabel.Height / 2);
                int yDiff = (panel.Height/2-rightPanel.Height/2);
                loadingLabel.Location = new Point(x, y+yDiff);

                //Set font for loading label
                loadingLabel.Font = new Font("Segoe UI", 15,FontStyle.Bold);
                //Set label to base its size on the content
                loadingLabel.AutoSize = true;
                //Add loading text to label
                loadingLabel.Text = "Loading...";
                //Set backcolor of loading label
                loadingPanel.BackColor = loading;
                //Set text aligntment of text inside label
                loadingLabel.TextAlign = ContentAlignment.TopCenter;
                //Add label to loading panel
                loadingPanel.Controls.Add(loadingLabel);
                //Add to current panel
                panel.Controls.Add(loadingPanel);
                //Save the loading panel as the current content
                panel.setContent(loadingPanel);
                panel.setLoading(true);
                //Start new thread for fetching information
                ThreadHandler th = new ThreadHandler();
                //
                th.fetchResult(panel, resultType, resultSource);
                
            }
            

        }

        /// <summary>
        /// Sets all menu buttons to unclicked
        /// </summary>
        private void resetMenuButtons()
        {
            //Set boolean values to false
            marketClicked = false;
            stocksClicked = false;
            newsClicked = false;
            portfolioClicked = false;
            //Change color of buttons back to gray
            marketButton.BackColor = buttonColor;
            newsButton.BackColor = buttonColor;
            stocksButton.BackColor = buttonColor;
            portfolioButton.BackColor = buttonColor;
            //Set color for foreground
            marketButton.ForeColor = Color.Black;
            newsButton.ForeColor = Color.Black;
            stocksButton.ForeColor = Color.Black;
            portfolioButton.ForeColor = Color.Black;
        }
        private void resetMarketButtons()
        {

            lseClicked = false;
            nyseClicked = false;
            omxClicked = false;
            //Change color of buttons back to gray
            lseButton.BackColor = buttonColor;
            nyseButton.BackColor = buttonColor;
            omxButton.BackColor = buttonColor;
            //Set color for foreground
            lseButton.ForeColor = Color.Black;
            nyseButton.ForeColor = Color.Black;
            omxButton.ForeColor = Color.Black;
        }
        /// <summary>
        /// Stock button event handler.
        /// </summary>
        private void stocksButton_Click(object sender, EventArgs e)
        {
            menuClick("stocks", stocksClicked);
        }
        /// <summary>
        /// Market button event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void marketButton_Click(object sender, EventArgs e)
        {
            menuClick("market", marketClicked);
        }
        /// <summary>
        /// News button event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void newsButton_Click(object sender, EventArgs e)
        {
            menuClick("news", newsClicked);
        }
        /// <summary>
        /// Portfolio button event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void portfolioButton_Click(object sender, EventArgs e)
        {
            menuClick("portfolio", portfolioClicked);
        }

        private void dragPanel_Paint(object sender, PaintEventArgs e)
        {
            
        }

        /// <summary>
        /// Drag panel mouse listener (mouse click down).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void dragPanel_MouseDown(object sender, MouseEventArgs e)
        {
            dragging = true;  
            startPoint = new Point(e.X, e.Y);
        }

        /// <summary>
        /// Drag panel mouse listener (mouse click down).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void dragPanel_MouseUp(object sender, MouseEventArgs e)
        {
            dragging = false;
        }

        /// <summary>
        /// Drag panel mouse listener (mouse moved).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void dragPanel_MouseMove(object sender, MouseEventArgs e)
        {
            if (dragging)
            {
                Point p = PointToScreen(e.Location);
                Location = new Point(p.X - this.startPoint.X, p.Y - this.startPoint.Y);
            }
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
                //Set window state to full screen
                WindowState = FormWindowState.Maximized;
                //Change color of maximize button
                maximizeLabel.ForeColor = Color.Firebrick;
                //set drag panel width
                dragPanel.Width = MainWindow.ActiveForm.Width;
                //Set maximized boolean to true
                maximized = true;
            }
            //Window is already maximized
            else
            {
                //Change state to window
                WindowState = FormWindowState.Normal;
                //Change color of maximize button
                maximizeLabel.ForeColor = Color.DarkGray;
                //Set maximized boolean to false
                maximized = false;
            }
            refreshContentPanels();
        }
        /// <summary>
        /// Mouse listener for maximize label (click).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void maximizeLabel_Click(object sender, EventArgs e)
        {
            toggleMaximize();
        }
        /// <summary>
        /// Mouse listener for minimize label (click).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void minimizeLabel_Click(object sender, EventArgs e)
        {
            WindowState = FormWindowState.Minimized;
        }
        /// <summary>
        /// Mouse listener for control label (mouse enter).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void controlLabel_MouseEnter(object sender, EventArgs e)
        {
            Label label = sender as Label;
            label.ForeColor = Color.Black;
        }

        /// <summary>
        /// Mouse listener for control label (mouse leave).
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void controlLabel_MouseLeave(object sender, EventArgs e)
        {
            Label label = sender as Label;
            if (label.Text == "M" && maximized)
            {
                label.ForeColor = Color.Firebrick;
            }
            else
            {
                label.ForeColor = buttonColor;
            }
        }

        /// <summary>
        /// Method to refresh all content panes.
        /// </summary>
        private void refreshContentPanels()
        {
            /*
             * Set width of left parent panel (which contains a panel for 
             * settings and a panel for content).
             * The height is always stretched.
             */
            leftParentPanel.Width = ((mainContentPanel.Width - menu.Width) / 2) - 10;
            //Set width of right panel.
            rightPanel.Width = ((mainContentPanel.Width - menu.Width) / 2) - 10;
            //Set height of right panel.
            leftPanel.Height = mainContentPanel.Height - settings.Height;
            //Set height and width of right and left content.
            leftPanelResults.updateSize();
        }

        private void lseButton_Click(object sender, EventArgs e)
        {
            marketClick("lse", lseClicked);
        }

        private void nyseButton_Click(object sender, EventArgs e)
        {
            marketClick("nyse", nyseClicked);
        }

        private void omxButton_Click(object sender, EventArgs e)
        {
            marketClick("omx", omxClicked);
        }

    }
    
}