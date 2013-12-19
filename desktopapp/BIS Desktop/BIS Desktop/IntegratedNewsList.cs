using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    public class IntegratedNewsList : FlowLayoutPanel
    {
        private String Symbol; // String containing the Symbol of the news
        private String Market; // String containing the Market of the news
        private Controller c; // Controller class
        private JsonHandler jh; // JsonHandler class
        private News displayNews = null; // The current news dispalyed in the list
        private List<News> newsList; // List containing the news
        private int newsLabelHeight; // int for setting the height of the news labels
        private int newsLabelWidth; // int for setting the width of the news labels
        private int panelWidth; // int for setting the width of the news panel
        private int panelHeigth; // int for setting the height of the news panel
        private int listItemClicked = -1; // int for checking which news item in the list is clicked
        private int maxButtons = 8; // int for setting maximum number of buttons on the page
        private Boolean listsNotEmpty = true;  // boolean for checking if the lists are empty
        private Label[,] newsLabels; // 2d array holding all the news labels
        private FlowLayoutPanel[] newsPanels; // FlowLayout panel array for holding the news panels
        private FlowLayoutPanel mainPanel; // main panel used when adding the NewsReader class
        /// <summary>
        /// Class for creating a FlowLayoutout panel containing a list of news
        /// </summary>
        /// <param name="s"></param>
        /// <param name="m"></param>
        public IntegratedNewsList(String s, String m)
        {
            // set market and symbol
            Market = m;
            Symbol = s;
            // initilize classes
            c = new Controller();
            jh = new JsonHandler();
            // set the panels margin
            this.Margin = new Padding(1);
            // get hte news from the particular symbol
            newsList = jh.getSingleNews(Symbol, Market);
            // if the news list is not empty, sort it after date of published 
            if (newsList.Count != 0)
            {
                // initilize buttons
                newsList = c.sortNewsList(newsList, "pubDate", true);
                initilizeNewsList();
            }
            else
            {
                listsNotEmpty = false;
            }
        }
        /// <summary>
        /// Initilize the news lsit
        /// </summary>
        private void initilizeNewsList()
        {
            // cleear the panel
            this.Controls.Clear();
            // create new main panel and set sizes and autoscroll
            mainPanel = new FlowLayoutPanel();
            mainPanel.Width = panelWidth;
            mainPanel.Height = panelHeigth;
            mainPanel.AutoScroll = true;
            // check displayNews, i.e if a news item is clicked
            if (displayNews != null)
            {
                // add new NewsReader
                NewsReader np = new NewsReader(displayNews, false);
                np.setSize(panelWidth - 20, 300);
                mainPanel.Controls.Add(np);
            }
            int stop;
            // calculate the stop of the loop
            if (maxButtons > newsList.Count)
            {
                stop = newsList.Count;
            }
            else
            {
                stop = maxButtons;
            }
            // initilize newsLabels
            newsLabels = new Label[stop, 3];
            int j = 0;
            for (int i = 0; i < stop; i++)
            {
                // get news
                News n = newsList[i];
                // create the news labels
                Label symbolLabel = createLabel(n.symbol, n, j);
                Label titleLabel = createLabel(n.title, n, j);
                Label updatedLabel = createLabel(c.getDate(n.pubDate).ToString(), n, j);
                // add the news labels to the 2d array
                newsLabels[j, 0] = symbolLabel;
                newsLabels[j, 1] = titleLabel;
                newsLabels[j, 2] = updatedLabel;
                j++;
            }
            // initilize the newsPanels and add the labels to it
            newsPanels = new FlowLayoutPanel[stop]; 
            for (int i = 0; i < newsLabels.GetLength(0); i++)
            {
                FlowLayoutPanel newsPanel = new FlowLayoutPanel();
                newsPanel.FlowDirection = System.Windows.Forms.FlowDirection.LeftToRight;
                newsPanel.Height = 40;
                newsPanel.BackColor = c.highlightWhite;
                newsPanel.Width = panelWidth-25;

                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsPanel.Controls.Add(newsLabels[i, k]);
                }

                newsPanels[i] = newsPanel;
            }
            // add buttons to the mainPanel
            foreach (FlowLayoutPanel p in newsPanels)
            {
                mainPanel.Controls.Add(p);
            }
            // add main panel
            this.Controls.Add(mainPanel);
        }
        /// <summary>
        /// Method for creating new news labels
        /// </summary>
        /// <param name="text"></param>
        /// <param name="n"></param>
        /// <param name="j"></param>
        /// <returns> A Label</returns>
        private Label createLabel(String text, News n, int j)
        {
            Label label = new Label();
            label.Text = text;
            label.TextAlign = ContentAlignment.MiddleCenter;
            label.Font = c.mercuryFont;
            label.Height = newsLabelHeight;
            label.Width = newsLabelWidth - 25/3;
            label.Margin = new Padding(0);
            // check if the item is clicked and set colors accordlingly
            if (j == listItemClicked)
            {
                label.BackColor = c.mercuryBlue;
                label.ForeColor = Color.White;
            }
            else
            {
                label.BackColor = Color.FromArgb(70, c.mercuryGrey);
                label.ForeColor = Color.Black;
            }
            // add Click, MouseEnter and MouseLeave methods
            label.Click += (sender, e) => { news_clicked(sender, e, n, j); };
            label.MouseEnter += (sender, e) => { highlightList_MouseEnter(sender, e, j); };
            label.MouseLeave += (sender, e) => { highlightList_MouseLeave(sender, e, j); };
            return label;
        }
        /// <summary>
        /// Method triggered when news item is clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="n"></param>
        /// <param name="num"></param>
        private void news_clicked(object sender, EventArgs e, News n, int num)
        {
            // set list listItemClicked, displayNews and rebuild the panel
            listItemClicked = num;
            displayNews = n;
            initilizeNewsList();
        }
        /// <summary>
        /// Method triggered when mouse enters the news item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="j"></param>
        private void highlightList_MouseEnter(object sender, System.EventArgs e, int j)
        {
            // if it´s not already clicked change color
            if (listItemClicked != j)
            {
                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsLabels[j, k].BackColor = Color.FromArgb(100, c.mercuryGrey);
                }
            }
        }
        /// <summary>
        /// Method triggered when mouse leaves the news item
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="j"></param>
        private void highlightList_MouseLeave(object sender, System.EventArgs e, int j)
        {
            // if it´s not already clicked change color
            if (listItemClicked != j)
            {
                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsLabels[j, k].BackColor = Color.FromArgb(70, c.mercuryGrey);
                }
            }
        }
        /// <summary>
        /// Method used for setting or resetting the size of the panel and its content
        /// </summary>
        /// <param name="W"></param>
        /// <param name="H"></param>
        public void setSize(int W, int H)
        {
            panelWidth = W-10;
            panelHeigth = H;
            this.Width = panelWidth;
            this.Height = H;
            // if the list is not curently empty reload it
            if (listsNotEmpty)
            {
                newsLabelWidth = (panelWidth / 3) - 3;
                newsLabelHeight = 45;
                initilizeNewsList();
            }
        }
    }
}
