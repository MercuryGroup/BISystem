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

 
        private List<News> newsList;

        private int newsLabelHeight;
        private int newsLabelWidth;
        private int panelWidth; 

        private Label[,] newsLabels;
        private FlowLayoutPanel[] newsPanels;

        public StockNews(String s, String m)
        {

            Market = m; 
            Symbol = s;

            c = new Controller();
            jh = new JsonHandler();

            List<News> allNews = jh.getAllNews(); 
            newsList = filterNews(allNews); 

            initilizeNewsList(); 

            foreach(FlowLayoutPanel p in newsPanels)
            {
                this.Controls.Add(p); 
            }
         
        }


        private List<News> filterNews(List<News> allStocks)
        {
            List<News> news = new List<News>();
            foreach (News temp in allStocks)
            {
                if (temp.symbol == Symbol) 
                {
                    news.Add(temp);
                }
            }
            return news;
        }


        private void initilizeNewsList()
        {

            newsLabels = new Label[newsList.Count, 3]; 
            // create new "news buttons" fill the array 
            for (int i = 0; i < newsList.Count; i++)
            {

                News n = newsList[i];

                Label symbolLabel = new Label();
                symbolLabel.Text = n.symbol;
                symbolLabel.TextAlign = ContentAlignment.MiddleCenter;
                symbolLabel.Font = c.mercuryFont;
                symbolLabel.Height = newsLabelHeight;
                symbolLabel.Width = newsLabelWidth;
                symbolLabel.Margin = new Padding(0);
                symbolLabel.BackColor = Color.White;
                symbolLabel.Click += (sender, e) => { news_clicked(sender, e, n); };
                symbolLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e); };
                symbolLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e); };

                Label titleLabel = new Label();
                titleLabel.Text = n.title;
                titleLabel.TextAlign = ContentAlignment.MiddleCenter;
                titleLabel.Font = c.mercuryFont;
                titleLabel.Height = newsLabelHeight;
                titleLabel.Width = newsLabelWidth;
                titleLabel.Margin = new Padding(0);
                titleLabel.BackColor = Color.White;
                titleLabel.Click += (sender, e) => { news_clicked(sender, e, n); };
                titleLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e); };
                titleLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e); };

                Label updatedLabel = new Label();
                updatedLabel.Text = c.getDate(n.pubDate).ToString();
                updatedLabel.TextAlign = ContentAlignment.MiddleCenter;
                updatedLabel.Font = c.mercuryFont;
                updatedLabel.Height = newsLabelHeight;
                updatedLabel.Width = newsLabelWidth;
                updatedLabel.Margin = new Padding(0);
                updatedLabel.BackColor = Color.White;
                updatedLabel.Click += (sender, e) => { news_clicked(sender, e, n); };
                updatedLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e); };
                updatedLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e); };

                newsLabels[i, 0] = symbolLabel;
                newsLabels[i, 1] = titleLabel;
                newsLabels[i, 2] = updatedLabel;
            }

            newsPanels = new FlowLayoutPanel[newsList.Count];

            for (int i = 0; i < newsLabels.GetLength(0); i++)
            {
                FlowLayoutPanel newsPanel = new FlowLayoutPanel();
                newsPanel.FlowDirection = System.Windows.Forms.FlowDirection.LeftToRight;
                newsPanel.Height = newsLabelHeight;
                newsPanel.Width = panelWidth;

                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsPanel.Controls.Add(newsLabels[i, k]);
                }

                newsPanels[i] = newsPanel;
            }
        }

        public void setSize(int W, int H)
        {
            this.Width = W;
            this.Height = H;

            newsLabelWidth = W / 3;
            newsLabelHeight = H / newsList.Count;

            initilizeNewsList(); 

        }

        private void news_clicked(object sender, EventArgs e, News n)
        {

        }

        private void highlightStock_MouseEnter(object sender, EventArgs e)
        {

        }

        private void highlightStock_MouseLeave(object sender, EventArgs e)
        {

        }
      
    }
}
