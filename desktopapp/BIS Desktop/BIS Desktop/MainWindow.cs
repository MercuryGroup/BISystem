﻿using System;
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
    /// <summary>
    /// Main window for the application.
    /// </summary>
    public partial class MainWindow : Form
    {
        //Initial point for drag panel
        private Point startPoint = new Point(0, 0);
        //Boolean that checks if window is maximized
        public Boolean maximized; //Check if window is maximized
        private Boolean dragging; //Check if window can be dragged
        private Boolean searchFieldHasText;//Check if searchfield has text
        //Panel heights
        private int marketPanelHeight, dragPanelHeight;
        private String currentMarket; //Currently selected market
        private String currentResultType; //Currently selected result type

        private Controller c;
        /*
         * Inner panels that will handle the parsed results 
         * for both left and right panels
         */
        public ResultPanel leftPanelResults, rightPanelResults;
        //Color used for buttons that have been clicked.
       
        private ThreadHandler th;
        public MainWindow()
        {
            c = new Controller(); 
            //Initialize all boolean values
            dragging = false;
            maximized = false;
            searchFieldHasText = false;
            //Initialize market panel height
            marketPanelHeight = 40;
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
            //Set custom color for controls
            leftPanel.BackColor = Color.Gray;
            rightPanel.BackColor = Color.Gray;
            leftParentPanel.BackColor = Color.Gray;
            mainContentPanel.BackColor = Color.Gray;
            menu.BackColor = c.mercuryBeige;
            searchField.BackColor = c.mercuryGrey;
            dragPanel.BackColor = c.highlightWhite;
            marketPanel.BackColor = c.highlightWhite;

            //Reset buttons
            resetMenuButtons();
            extraPanel.BackColor = c.mercuryBeige;
            disableMarketButtons();
            //Refresh all content panels
            refreshContentPanels();
            //Add event handlers to all necessary panels
            dragPanel.MouseDown += new MouseEventHandler(dragPanel_MouseDown);
            dragPanel.MouseMove += new MouseEventHandler(dragPanel_MouseMove);
            dragPanel.MouseUp += new MouseEventHandler(dragPanel_MouseUp);
            this.SizeChanged += MainWindow_SizeChanged;
            //Set size of drag panel
            dragPanelHeight = dragPanel.Height;
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
                maximizeLabel.ForeColor = c.mercuryRed;
                //set drag panel width
                dragPanel.Width = MainWindow.ActiveForm.Width;
                //Set maximized boolean to true
                maximized = true;
                //Refresh content panels
                refreshContentPanels();
            }
        }

        /// <summary>
        /// Event that triggers whenever a menu button is clicked.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="resultType_"></param>
        private void menuClick(Object sender, String resultType_)
        {
            Button button_ = sender as Button;
            resetMenuButtons();
            button_.BackColor = c.mercuryBlue;
            //button_.ForeColor = Color.White;
            currentResultType = resultType_;
            if (currentMarket == null)
            {
                resetMarketButtons();
                currentMarket = "lse";
                lseButton.BackColor = c.mercuryBlue;
                lseButton.ForeColor = Color.White;
            }
            loadResult(leftPanelResults, currentResultType, currentMarket, currentMarket, this);
        }

        /// <summary>
        /// Event that triggers whenever a market buttons is clicked.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="market_"></param>
        private void marketClick(Object sender, String market_)
        {
            Button button_ = sender as Button;
            resetMarketButtons();
            button_.BackColor = c.mercuryBlue;
            currentMarket = market_;
            loadResult(leftPanelResults, currentResultType, currentMarket, currentMarket, this);
        }

        /// <summary>
        /// Load result to the calling panel.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="contentType"></param>
        /// <param name="resultCategory"></param>
        public void loadResult(object sender, String resultType, String resultSource, String Market, object mainWindow)
        {
            //Instantiate current panel
            ResultPanel panel = sender as ResultPanel;
            
            //Set current panel to loading (if the panel isn't already loading content)
            if (!panel.getLoading())
            {
                refreshContentPanels();
                //Clear current panel
                panel.Controls.Clear();
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
                loadingPanel.BackColor = Color.LightGray;
                //Set text aligntment of text inside label
                loadingLabel.TextAlign = ContentAlignment.TopCenter;
                //Add label to loading panel
                loadingPanel.Controls.Add(loadingLabel);
                //Add to current panel
                panel.Controls.Add(loadingPanel);
                //Save the loading panel as the current content
                panel.setContent(loadingPanel);
                panel.setLoading(true);
                //Createnew instance of class for threading
                th = new ThreadHandler();
                th.fetchResult(panel, resultType, resultSource, Market, mainWindow);
                refreshContentPanels();
            }
        }
        /// <summary>
        /// Loads news results to the loading panel.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="resultType"></param>
        /// <param name="mainWindow"></param>
        /// <param name="n"></param>
        public void loadNewsResult(object sender, String resultType, object mainWindow, News n)
        {
            //Instantiate current panel
            ResultPanel panel = sender as ResultPanel;

            //Set current panel to loading (if the panel isn't already loading content)
            if (!panel.getLoading())
            {
                //Clear current panel
                panel.Controls.Clear();
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
                int yDiff = (panel.Height / 2 - rightPanel.Height / 2);
                loadingLabel.Location = new Point(x, y + yDiff);

                //Set font for loading label
                loadingLabel.Font = new Font("Segoe UI", 15, FontStyle.Bold);
                //Set label to base its size on the content
                loadingLabel.AutoSize = true;
                //Add loading text to label
                loadingLabel.Text = "Loading...";
                //Set backcolor of loading label
                loadingPanel.BackColor = c.loading;
                //Set text aligntment of text inside label
                loadingLabel.TextAlign = ContentAlignment.TopCenter;
                //Add label to loading panel
                loadingPanel.Controls.Add(loadingLabel);
                //Add to current panel
                panel.Controls.Add(loadingPanel);
                //Save the loading panel as the current content
                panel.setContent(loadingPanel);
                panel.setLoading(true);
                //Createnew instance of class for threading
                th = new ThreadHandler();
                th.fetchNewsResult(panel, resultType, mainWindow, n);
            }
        }

        /// <summary>
        /// Sets all menu buttons to unclicked.
        /// </summary>
        private void resetMenuButtons()
        {
            //Set boolean values to false
            //Change color of buttons back to gray
            marketButton.BackColor = c.mercuryGrey;
            newsButton.BackColor = c.mercuryGrey;
            stocksButton.BackColor = c.mercuryGrey;
            portfolioButton.BackColor = c.mercuryGrey;
        }
        /// <summary>
        /// Sets all market buttons to unclicked.
        /// </summary>
        private void resetMarketButtons()
        {
            marketPanel.Height = marketPanelHeight;
            //Change color of buttons back to gray
            lseButton.BackColor = c.mercuryGrey;
            nyseButton.BackColor = c.mercuryGrey;
            omxButton.BackColor = c.mercuryGrey;
        }

        /// <summary>
        /// Market button event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void marketButton_Click(object sender, EventArgs e)
        {
            enableMarketButtons();
            menuClick(sender, "marketinfo");
        }

        /// <summary>
        /// Stock button event handler.
        /// </summary>
        private void stocksButton_Click(object sender, EventArgs e)
        {
            enableMarketButtons();
            menuClick(sender, "stocks");
        }

        /// <summary>
        /// News button event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void newsButton_Click(object sender, EventArgs e)
        {
            enableMarketButtons();
            menuClick(sender, "news");
        }

        /// <summary>
        /// Portfolio button event handler.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void portfolioButton_Click(object sender, EventArgs e)
        {
            menuClick(sender, "portfolio");
            disableMarketButtons();
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
            if (dragging && maximized==false)
            {
                Point p = PointToScreen(e.Location);
                Location = new Point(p.X - this.startPoint.X, p.Y - this.startPoint.Y);
            }
        }
        /// <summary>
        /// Exits the program.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
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
                maximizeLabel.ForeColor = c.mercuryRed;
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
                label.ForeColor = c.mercuryGrey;
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
            
            if (currentResultType == "marketinfo")
            {
                leftParentPanel.Width = mainContentPanel.Width - menu.Width;
                rightPanel.Width = 0;
                rightPanelResults.updateSize();
            }
            else
            {
                leftParentPanel.Width = ((mainContentPanel.Width - menu.Width) / 2)+1;
                //Set width of right panel.
                rightPanel.Width = ((mainContentPanel.Width - menu.Width) / 2);
                rightPanelResults.updateSize();
            }
            //Set height of right panel.
            leftPanel.Height = mainContentPanel.Height - marketPanel.Height;
            //Set height and width of right and left content.
            leftPanelResults.updateSize();
        }
        /// <summary>
        /// Disables all market buttons and hides the market button panel.
        /// </summary>
        private void disableMarketButtons()
        {
            leftPanel.Height = leftParentPanel.Height;
            marketPanel.Height = 0;
            lseButton.Enabled = false;
            nyseButton.Enabled = false;
            omxButton.Enabled = false;
        }
        /// <summary>
        /// Enables all market buttons and shows the market button panel.
        /// </summary>
        private void enableMarketButtons()
        {
            lseButton.Enabled = true;
            nyseButton.Enabled = true;
            omxButton.Enabled = true;
            marketPanel.Height = marketPanelHeight;
            leftPanel.Height = leftParentPanel.Height - marketPanel.Height;
        }

        /// <summary>
        /// Event handler for market button.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void lseButton_Click(object sender, EventArgs e)
        {
            marketClick(sender, "lse");
        }

        /// <summary>
        /// Event handler for market button.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void nyseButton_Click(object sender, EventArgs e)
        {
            marketClick(sender, "nyse");
        }

        /// <summary>
        /// Event handler for market button.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void omxButton_Click(object sender, EventArgs e)
        {
            marketClick(sender, "omx");
        }

        /// <summary>
        /// Changes parameters of the search field when it is focused.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void searchField_Focus(object sender, EventArgs e){
            if (!searchFieldHasText)
            {
                searchField.ForeColor = Color.White;
                searchField.Text = "";
            }
        }

        /// <summary>
        /// Triggers whenver a key i pressed when searchfield is focused.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void searchField_KeyPressed(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                currentResultType = "list";
                refreshContentPanels();
                resetMenuButtons();
                loadResult(leftPanelResults, "search", searchField.Text,"", this);
                disableMarketButtons();
                searchField.Text = "";
                mainContentPanel.Focus();
            }
            else if (e.KeyCode == Keys.Escape)
            {
                searchField.Text = "";
                mainContentPanel.Focus();
            }
            
        }

        /// <summary>
        /// Triggers whenever focused is removed from the earch field.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void searchField_Exit(object sender, EventArgs e)
        {
            
            if (searchField.Text == "")
            {
                searchFieldHasText = false;
                searchField.Text = "Search";
                searchField.ForeColor = Color.Gray;
            }
            else
            {
                searchFieldHasText = true;
            }
        }
        
    }
    
}