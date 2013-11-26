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
        private MainWindow mw;
        private Boolean buttonClicked = false;
        private String[][] twoDimText; 

        public ResultList(String Data, String Market, object o){

            mw = o as MainWindow;

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

            List<Stock> stocks = jh.getAllStocks(Market);
            
            infoLabel = new Label();

            infoLabel.Text = "Symbol Name Latest Change Percent Open Value";
            infoLabel.BackColor = System.Drawing.ColorTranslator.FromHtml("#B8B8B8");
            infoLabel.AutoSize = false;
            infoLabel.Width = this.Width;
            infoLabel.TabStop = false;
            infoLabel.Margin = new Padding(0);

            this.Controls.Add(infoLabel); 

            btns = new Button[stocks.Count];
            twoDimText = new String[stocks.Count][]; 

            for (int i = 0; i < stocks.Count; i++ )
            {
                Stock s = stocks[i]; 
                   
                btns[i] = new Button();
                btns[i].Text = s.Symbol + " " + s.Name + " " + s.Latest + " " + s.Change + " " + s.Percent + " " +s.Volume+" "+ s.OpenVal;
                btns[i].TextAlign = ContentAlignment.MiddleLeft; 
                btns[i].AutoSize = false;
                btns[i].Width = this.Width;
                btns[i].TabStop = false;
                btns[i].FlatStyle = FlatStyle.Flat;
                btns[i].FlatAppearance.BorderSize = 0;
                btns[i].Margin = new Padding(0);

                // add event handler
                btns[i].Click += (sender, e) => {stock_clicked(sender, e, s); };

                
                this.Controls.Add(btns[i]);

                string text = btns[i].Text;
                String[] splitText = text.Split(' '); 
                twoDimText[i] = splitText; 
            }

            setButtonColors(btns); 


      }


        public String indentText(int Width)
        {

            Graphics grpx = Graphics.FromImage(new Bitmap(1, 1));

            for (int j = 0; j < twoDimText.Length; j++){


                int longest = 0;

                for (int i = 0; i < twoDimText[j].Length; i++)
                {

                  
                   // var wordsWidth = (int)grpx.MeasureString(twoDimText[i][j], btns[0].Font).Width;
                   // Console.WriteLine(twoDimText[i][j]);

                   // if (wordsWidth > longest)
                    //{
                   //     longest = wordsWidth;
                   ///}

                }

            }

             
             
            
    

            String newText = "";
       

                return newText;
        }


        private void setButtonColors(Button[] buttons)
        {
            for (int i = 0; i < buttons.Length; i++ )

                if (i % 2 == 1)
                {
                    btns[i].BackColor = System.Drawing.ColorTranslator.FromHtml("#A2A2A2");
                    btns[i].ForeColor = Color.Black;
                }

                else
                {
                    btns[i].BackColor = Color.LightGray;
                    btns[i].ForeColor = Color.Black;
                }
        }


       private void stock_clicked(object sender, System.EventArgs e, Stock s)
       {
           Button b = sender as Button;

           if (buttonClicked == true)
           {
               setButtonColors(btns);
               b.BackColor = mw.mercuryBlue;
               b.ForeColor = Color.White;
               buttonClicked = true;
           }
           else
           {
               b.BackColor = mw.mercuryBlue;
               b.ForeColor = Color.White;
               buttonClicked = true;
           }
           
           ResultPanel temp = mw.rightPanelResults as ResultPanel;          
           mw.loadResult(temp, "info", s.Symbol, mw); 
            
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

           if (this.DataButtonClicked == "stocks" || this.DataButtonClicked == "portfolio")
           {
               updateButton(W);
               updateLabel(W);
               indentText(W);
           }
           else if (this.DataButtonClicked == "market")
           {
               updateButton(W);
           }
         
       }

       private void updateLabel(int W)
       {
           infoLabel.Width = W;
           infoLabel.Height = 35;
           infoLabel.TextAlign = ContentAlignment.MiddleLeft;
       }

       private void updateButton(int W)
       {
           foreach (Button b in btns)
           {
               b.Width = W;
               b.Height = 35; 
           }
       }
    }
}
