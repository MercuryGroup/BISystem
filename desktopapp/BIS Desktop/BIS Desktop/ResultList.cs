using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Diagnostics;
using System.Drawing; 


namespace BIS_Desktop
{
    public class ResultList : FlowLayoutPanel
    {
        private Button[] btns; // Array of buttons
        private JsonHandler jh;
        private Label infoLabel;
        private String DataButtonClicked; 
      
        public ResultList(String Data, String Market){

            DataButtonClicked = Data; 
            this.AutoScroll = true;
            this.HorizontalScroll.Enabled = false;
            this.HorizontalScroll.Visible = false;
            this.AutoScrollPosition = new Point(this.VerticalScroll.Maximum);

            jh = new JsonHandler();

            switch (Data.ToLower())
            {
                case "market":
                    generateNewsList(Market); 
                    break;
                case "stocks":
                    generateStockList(Market); 
                    break;
                case "news":
                    btns = new Button[0]; 

                    generateNewsList(Market);
                    break; 
                case "portfolio":
                    generateStockList(Market);
                    break; 
            } 

        }

       

        private void generateNewsList(String Market)
        {
            List<News> news = jh.getNews(Market);

              
            btns = new Button[news.Count];

            for (int i = 0; i < news.Count; i++)
            {
                News n = news[i];


               btns[i] = new Button();
               btns[i].Text = n.title;
               btns[i].AutoSize = false;
               btns[i].Width = this.Width;
               btns[i].TabStop = false;
               btns[i].FlatStyle = FlatStyle.Flat;
               btns[i].FlatAppearance.BorderSize = 0;
               btns[i].Margin = new Padding(0);

               // add event handler
               btns[i].Click += (sender, e) => { news_clicked(sender, e, n); };


               if (i % 2 == 1)
               {
                    btns[i].BackColor = Color.WhiteSmoke;
               }
               else
               {
                    btns[i].BackColor = Color.LightBlue;
               }


                    this.Controls.Add(btns[i]);
                }

        }


        private void generateStockList(String Market)
        {

            List<Stock> stocks = jh.getStocks(Market);
            
            infoLabel = new Label();

            infoLabel.Text = "Symbol Name Latest Change Percent Open Value";
            infoLabel.BackColor = Color.WhiteSmoke;
            infoLabel.AutoSize = false;
            infoLabel.Width = this.Width;
            infoLabel.TabStop = false;
            infoLabel.Margin = new Padding(0);


            this.Controls.Add(infoLabel); 

            btns = new Button[stocks.Count];

            for (int i = 0; i < stocks.Count; i++ )
            {
                Stock s = stocks[i]; 
                   
                btns[i] = new Button();
                btns[i].Text = s.Symbol + " " + s.Name + " " + s.Latest + " " + s.Change + " " + s.Percent + " " + s.OpenVal;
                btns[i].TextAlign = ContentAlignment.MiddleLeft; 
                btns[i].AutoSize = false;
                btns[i].Width = this.Width;
                btns[i].TabStop = false;
                btns[i].FlatStyle = FlatStyle.Flat;
                btns[i].FlatAppearance.BorderSize = 0;
                btns[i].Margin = new Padding(0);

                // add event handler
                btns[i].Click += (sender, e) => {stock_clicked(sender, e, s); };


                if (i % 2 == 1)
                {
                    btns[i].BackColor = Color.White;
                }

                else
                {
                    btns[i].BackColor = Color.LightBlue;
                }

                
                    this.Controls.Add(btns[i]);
                }


            }


        public String indentText(String text, int Width)
        {

            Graphics grpx = Graphics.FromImage(new Bitmap(1, 1));

            String[] splitArray = text.Split(' ');

            var total = (int)grpx.MeasureString(splitArray.ToString(), btns[0].Font).Width;

            var lengthZero = (int)grpx.MeasureString(splitArray[0], btns[0].Font).Width;
            var lengthOne = (int)grpx.MeasureString(splitArray[1], btns[0].Font).Width;
            var lengthTwo = (int)grpx.MeasureString(splitArray[2], btns[0].Font).Width;
            var lengthThree = (int)grpx.MeasureString(splitArray[3], btns[0].Font).Width;
            var lengthFour = (int)grpx.MeasureString(splitArray[4], btns[0].Font).Width;
            var lengthFive = (int)grpx.MeasureString(splitArray[5], btns[0].Font).Width;
            var lengthSix = (int)grpx.MeasureString(splitArray[6], btns[0].Font).Width;

            String indentZero = new String(' ', ((Width) / 7)- lengthZero);
            String indentOne = new String(' ', ((Width ) / 7) - lengthOne);
            String indentTwo = new String(' ', ((Width ) / 7) - lengthTwo);
            String indentThree = new String(' ', ((Width ) / 7) - lengthThree);
            String indentFour = new String(' ', ((Width ) / 7) - lengthFour);
            String indentFive = new String(' ', ((Width ) / 7) - lengthFive);
            String indentSix = new String(' ', ((Width ) / 7) - lengthSix);

            String newText = splitArray[0] + indentOne + splitArray[1] + indentTwo + splitArray[2] + indentThree + splitArray[3] + indentFour +
            splitArray[4] + indentFive + splitArray[5] + indentSix + splitArray[6]; 

            return newText;
        }


       private void stock_clicked(object sender, System.EventArgs e, Stock s)
       {
            Button b = sender as Button;
            Console.WriteLine(s.Name);
       }

       private void news_clicked(object sender, System.EventArgs e, News n)
       {
           Button b = sender as Button;
           Console.WriteLine(n.description);
       }

       public void setSize(int W, int H)
       {
           this.Height = H;
           this.Width = W;

           if (this.DataButtonClicked.ToLower() == "stocks" || this.DataButtonClicked.ToLower() == "portfolio")
           {
               updateButton(W);
               updateLabel(W);
           }
           else if (this.DataButtonClicked.ToLower() == "market")
           {
               updateButton(W);
           }
         
       }

       private void updateLabel(int W)
       {
           infoLabel.Width = W;
           infoLabel.Height = 35;
           infoLabel.TextAlign = ContentAlignment.MiddleLeft;

           //Graphics grpx = Graphics.FromImage(new Bitmap(1, 1));
           //var Width = (int)grpx.MeasureString(infoLabel.ToString(), btns[0].Font).Width;
           //String Text = infoLabel.Text;
           //indentText(Text, this.Width);
       }

       private void updateButton(int W)
       {
           foreach (Button b in btns)
           {
               b.Width = W;
               b.Height = 35;

              // Graphics grpx = Graphics.FromImage(new Bitmap(1, 1));
               //var Width = (int)grpx.MeasureString(b.Text, btns[0].Font).Width;
              // String Text = b.Text;
              // b.Text = indentText(Text, this.Width);
           }
       }
    }
}
