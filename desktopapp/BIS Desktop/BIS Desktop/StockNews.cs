using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    public class StockNews : FlowLayoutPanel
    {
        private String Symbol;
        private String Market;

        private Controller c;
        private JsonHandler jh;
        private News displayNews = null;
        private List<News> newsList;

        private int newsLabelHeight;
        private int newsLabelWidth;
        private int panelWidth;
        private int panelHeigth;
        private int listItemClicked = -1;

        private int maxButtons = 8;

        private Boolean listsNotEmpty = true;  // boolean for checking if the lists are empty

        private Label[,] newsLabels;
        private FlowLayoutPanel[] newsPanels;

        private FlowLayoutPanel mainPanel;

        public StockNews(String s, String m)
        {

            Market = m;
            Symbol = s;

            c = new Controller();
            jh = new JsonHandler();

            this.Margin = new Padding(1);
            newsList = jh.getSingleNews(Symbol, Market);

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

        private void initilizeNewsList()
        {
            this.Controls.Clear();
            // create new "news buttons" fill the array 
            mainPanel = new FlowLayoutPanel();
            mainPanel.Width = panelWidth;
            mainPanel.Height = panelHeigth;
            mainPanel.AutoScroll = true;

            if (displayNews != null)
            {
                NewsPanel np = new NewsPanel(displayNews, false);
                np.setSize(panelWidth - 20, 300);
                mainPanel.Controls.Add(np);
            }

            int stop;
            if (maxButtons > newsList.Count)
            {
                stop = newsList.Count;
            }
            else
            {
                stop = maxButtons;
            }

            newsLabels = new Label[stop, 3];

            int j = 0;
            for (int i = 0; i < stop; i++)
            {
                News n = newsList[i];

                Label symbolLabel = createLabel(n.symbol, n, j);
                Label titleLabel = createLabel(n.title, n, j);
                Label updatedLabel = createLabel(c.getDate(n.pubDate).ToString(), n, j);

                newsLabels[j, 0] = symbolLabel;
                newsLabels[j, 1] = titleLabel;
                newsLabels[j, 2] = updatedLabel;

                j++;
            }

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

            // add buttons
            foreach (FlowLayoutPanel p in newsPanels)
            {
                mainPanel.Controls.Add(p);
            }

            this.Controls.Add(mainPanel);
        }

        private Label createLabel(String text, News n, int j)
        {
            Label label = new Label();
            label.Text = text;
            label.TextAlign = ContentAlignment.MiddleCenter;
            label.Font = c.mercuryFont;
            label.Height = newsLabelHeight;
            label.Width = newsLabelWidth - 25/3;
            label.Margin = new Padding(0);
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
            label.Click += (sender, e) => { news_clicked(sender, e, n, j); };
            label.MouseEnter += (sender, e) => { highlightList_MouseEnter(sender, e, j); };
            label.MouseLeave += (sender, e) => { highlightList_MouseLeave(sender, e, j); };

            return label;
        }

        private void news_clicked(object sender, EventArgs e, News n, int num)
        {

            listItemClicked = num;

            displayNews = n;
            initilizeNewsList();

        }

        private void highlightList_MouseEnter(object sender, System.EventArgs e, int j)
        {
            if (listItemClicked != j)
            {
                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsLabels[j, k].BackColor = Color.FromArgb(100, c.mercuryGrey);
                }
            }
        }

        private void highlightList_MouseLeave(object sender, System.EventArgs e, int j)
        {
            if (listItemClicked != j)
            {
                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsLabels[j, k].BackColor = Color.FromArgb(70, c.mercuryGrey);
                }
            }
        }


        public void setSize(int W, int H)
        {
            panelWidth = W-10;
            panelHeigth = H;
            this.Width = panelWidth;
            this.Height = H;
            if (listsNotEmpty)
            {
                newsLabelWidth = (panelWidth / 3) - 3;
                newsLabelHeight = 45;

                initilizeNewsList();
            }
        }
    }
}
