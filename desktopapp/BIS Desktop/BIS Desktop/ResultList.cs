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
        

     

        // size variables
        private int panelWidth;
        private int panelHeigth;
        private int buttonHeight = 35;

        private int sideStart;// sideStart and sideStop provides the current side span for the numberButtons
        private int sideStop; 

        private Button[] btns; // Array of buttons
        private JsonHandler jh;
        private Label infoLabel;


        // Labels for traversing the list of buttons
        private Label first;
        private Label last; 
        private Label next;
        private Label previous;
        private FlowLayoutPanel nextPreviousPanel;

        private int currentStockIndex; // a int for controling which index of stocks is currently displayed in the list

        private String DataButtonClicked; // variable for determening which data we are listing
        
        private Boolean buttonClicked = false;


        private List<Stock> filteredStockList; // list with the stocks filtered after market
        
        private List<News> newsList; // list with ALL news from jsonhandler
        private List<Stock> allStocksList; // list with ALL stocks from jsonhandler

        private int numberOfButtonsPerPage = 15; // number of buttons listet per page, change depening on screen size

        // classes
        private MainWindow mw;
        private Controller c; 

        public ResultList(String Data, String Market, object o){

            mw = o as MainWindow;

            /*
             * TO DO !!! ändra mängden på numberOfButtonsPerPage bereonde på storleken på panelen""" sad SOdhAKLFJADÖJFLÄFJADLF 
             * FOOOONTS!! 
            */


            DataButtonClicked = Data;

            // initilize all components except the list
            inilizeComponents();

            // initilize classes
            jh = new JsonHandler();
            c = new Controller();
     
            switch (DataButtonClicked)
            {
                case "market":
                    contentAdder(0);
                    break;

                case "stocks":
                    allStocksList = jh.getAllStocks();
                    filteredStockList = filterStocks(allStocksList, Market);
                    contentAdder(0); 
                    break;

                case "news":
                    newsList = jh.getNews(Market);
                    contentAdder(0);
                    break; 

                case "portfolio":
                    contentAdder(0);
                    break; 
            } 

        }

        /*
         * filterStocks, returns a filtered list of Stock,
         * which market matches the functions argument Market
         */
         
        private List<Stock> filterStocks(List<Stock> allStocks, String Market)
        {
            List<Stock> stocks = new List<Stock>();
            foreach (Stock s in allStocks)
            {

                if (s.Market =="NYSE") // nyse couse we get it from db, later change to Market
                {
                    stocks.Add(s);
                }
            }
            return stocks;
        }



        private void contentAdder(int start)
        {
            this.Controls.Clear();
            int stop;

            switch (DataButtonClicked)
            {
                case "market":

                    // calculate which index in the list to show
                    if (start + numberOfButtonsPerPage <= newsList.Count)
                    {
                        stop = start + numberOfButtonsPerPage;
                    }
                    else
                    {
                        stop = newsList.Count;
                    }

                    // initilize buttons
                    initilizeNewsList(start, stop);

                    // add buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }


                    initilizeNumberButtons(start, stop); 
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "stocks":

                    // calculate which index in the list to show
                    if (start + numberOfButtonsPerPage <= filteredStockList.Count)
                    {
                        stop = start + numberOfButtonsPerPage;
                    }
                    else
                    {
                        stop = filteredStockList.Count;
                    }

                    // set next and previous the enabled or not depending on the size of the list
                    if (filteredStockList.Count < numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }
                    else if (start - numberOfButtonsPerPage < 0)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }
                    else if(start + numberOfButtonsPerPage > filteredStockList.Count)
                    {

                        first.Enabled = true;
                        next.Enabled = false;
                        previous.Enabled = true;
                        last.Enabled = false; 
                    }
                    else
                    {
                        first.Enabled = true;
                        next.Enabled = true;
                        previous.Enabled = true;
                        last.Enabled = true;
                    }

                    // initilize buttons
                    initilizeStockList(start, stop);

                    // add info label
                    this.Controls.Add(infoLabel);

                    // add buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }

                    initilizeNumberButtons(start, stop); 
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "news":

                    // calculate which index in the lsit to show
                    if (start + numberOfButtonsPerPage <= newsList.Count)
                    {
                        stop = start + numberOfButtonsPerPage;

                    }else{

                        stop = newsList.Count;
                    }

                    if (stop <= newsList.Count)
                    {
                        previous.Enabled = true;
                        next.Enabled = false;

                    }
                    else if (start < numberOfButtonsPerPage)
                    {
                        first.Enabled = true;
                        next.Enabled = false;
                        previous.Enabled = true;
                        last.Enabled = false; 

                    }
                    else
                    {
                        first.Enabled = true;
                        next.Enabled = true;
                        previous.Enabled = true;
                        last.Enabled = true;
                    }

                    // initilize buttons
                    initilizeNewsList(start, stop);

                    //contentAdder buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }

                    initilizeNumberButtons(start, stop); 
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "portfolio":
                    break;

            }     

        }

        private void initilizeNumberButtons(int start, int stop)
        {

            int temp = 0; 

            if (DataButtonClicked == "market")
            {
                temp = newsList.Count / numberOfButtonsPerPage;
            }
            else if (DataButtonClicked == "stocks")
            {
                temp = filteredStockList.Count / numberOfButtonsPerPage;
            }
            else if (DataButtonClicked == "news")
            {
                temp = newsList.Count / numberOfButtonsPerPage;
            }
            else if (DataButtonClicked == "portfolio")
            {
                temp = filteredStockList.Count / numberOfButtonsPerPage;
            }

            nextPreviousPanel.Controls.Clear();

            nextPreviousPanel.Controls.Add(first);
            nextPreviousPanel.Controls.Add(previous);         

            for (int i = 1; i < 11; i++)
            {      
                Label number = new Label();
                string text = i.ToString();
                Console.WriteLine("text: " + text); 
                number.Text = text;
                number.Size = new System.Drawing.Size(30,20);
                number.Click += (sender, e) => { numberLabel_clicked(sender, e, i); };
                number.MouseEnter += (sender, e) => { highlight_MouseEnter(sender, e); };
                number.MouseLeave += (sender, e) => { highlight_MouseLeave(sender, e); };
                nextPreviousPanel.Controls.Add(number);
                
            }

            nextPreviousPanel.Controls.Add(next);
            nextPreviousPanel.Controls.Add(last);
        }

        private void initilizeStockList(int start, int stop)
        {
     
            // create btns array
            btns = new Button[stop - start];

            // variable for iterating the btns[]
            int j = 0;

            // create new "stock buttons" fill the array 
            for (int i = start; i < stop; i++)
            {         
                Stock s = filteredStockList[i];

                btns[j] = new Button();
                btns[j].Text = s.Symbol + " " + s.Name + " " + s.Latest + " " + s.Change + " " + s.Percent + " " + s.Volume + " " + s.OpenVal;
                btns[j].TextAlign = ContentAlignment.MiddleCenter;
                btns[j].AutoSize = false;
                btns[j].Width = panelWidth;
                btns[j].Height = buttonHeight;
                btns[j].TabStop = false;
                btns[j].FlatStyle = FlatStyle.Flat;
                btns[j].FlatAppearance.BorderSize = 0;
                btns[j].Margin = new Padding(0);
   

                // add event handler
                btns[j].Click += (sender, e) => { stock_clicked(sender, e, s); };
                j++; 
            }
            // set color to buttons
            setButtonColors(btns);

            // reset number buttons
            initilizeNumberButtons(start, stop);
        }

        private void initilizeNewsList(int start, int stop)
        {
          
            // create btns array
            btns = new Button[stop - start];

            // variable for iterating the btns[]
            int j = 0;

            // create new "news buttons" fill the array 
            for (int i = start; i < stop; i++)
            {
     
               News n = newsList[i];

               btns[j] = new Button();
               btns[j].Text = n.title;
               btns[j].AutoSize = false;
               btns[j].Width = panelWidth;
               btns[j].Height = buttonHeight;
               btns[j].TabStop = false;
               btns[j].FlatStyle = FlatStyle.Flat;
               btns[j].FlatAppearance.BorderSize = 0;
               btns[j].Margin = new Padding(0);

               // add event handler
               btns[j].Click += (sender, e) => { news_clicked(sender, e, n); };

               j++; 
            }
            // set color to buttons
            setButtonColors(btns);

            // reset number buttons
            initilizeNumberButtons(start, stop);

        }

        private void setButtonColors(Button[] buttons)
        {
            for (int i = 0; i < buttons.Length; i++)

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


       private void numberLabel_clicked(object sender, System.EventArgs e, int i)
       {
           Console.WriteLine("clicked");
       }

       private void highlight_MouseEnter(object sender, System.EventArgs e)
       {
           Label L = sender as Label;
           L.ForeColor = c.mercuryBlue; 
       }

       private void highlight_MouseLeave(object sender, System.EventArgs e)
       {
           Label L = sender as Label;
           L.ForeColor = Color.Black; 
       }

       private void stock_clicked(object sender, System.EventArgs e, Stock s)
       {
           Button b = sender as Button;

           if (buttonClicked == true)
           {
               setButtonColors(btns);
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               buttonClicked = true;
           }
           else
           {
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               buttonClicked = true;
           }
           
           ResultPanel temp = mw.rightPanelResults as ResultPanel;          
           mw.loadResult(temp, "info", s.Symbol, mw); 
            
       }

       private void news_clicked(object sender, System.EventArgs e, News n)
       {
           Button b = sender as Button;
            
       }
       
       private void last_clicked(object sender, System.EventArgs e)
       {
           int temp; 

           switch(DataButtonClicked)
           {
                   
               case "stocks":

                   temp = filteredStockList.Count % numberOfButtonsPerPage; 

                   if (temp == 0)
                   {
                       contentAdder(filteredStockList.Count - numberOfButtonsPerPage); 
                   }
                   else
                   {
                       Console.WriteLine("TEMPOOOO" + temp); 
                       contentAdder(filteredStockList.Count - temp);
                   }          
                  break;

               case "news":

                  temp = newsList.Count % numberOfButtonsPerPage;

                  if (temp == 0)
                  {
                      contentAdder(newsList.Count - numberOfButtonsPerPage);
                  }
                  else
                  {
                      contentAdder(newsList.Count - temp);
                  }
                  break;

               case "market":

                  temp = newsList.Count % numberOfButtonsPerPage; 

                  if (temp == 0)
                  {
                      contentAdder(newsList.Count - numberOfButtonsPerPage);
                  }
                  else
                  {
                      contentAdder(newsList.Count - temp);
                  }
                  break;

               case "portfolio":

                  temp = filteredStockList.Count % numberOfButtonsPerPage;

                  if (temp == 0)
                  {
                      contentAdder(filteredStockList.Count - numberOfButtonsPerPage);
                  }
                  else
                  {
                      contentAdder(filteredStockList.Count - temp);
                  }
                  break;

               

           } 
             
           contentAdder(sideStop - numberOfButtonsPerPage);
       }

       private void first_clicked(object sender, System.EventArgs e)
       {
           contentAdder(0);
       }

       private void previous_clicked(object sender, System.EventArgs e)
       {
           currentStockIndex = currentStockIndex - numberOfButtonsPerPage;
           contentAdder(currentStockIndex);
       }

       private void next_clicked(object sender, System.EventArgs e)
       {
           currentStockIndex = currentStockIndex + numberOfButtonsPerPage;
           contentAdder(currentStockIndex);
       }

       public void setSize(int W, int H)
       {
           // store the sizes
           panelWidth = W;
           panelHeigth = H;

           // set panel size
           this.Height = H;
           this.Width = W;

           // set nextPreviousPanel width
           nextPreviousPanel.Width = this.Width;
         

           if (this.DataButtonClicked == "stocks" || this.DataButtonClicked == "portfolio")
           {
               foreach (Button b in btns)
               {
                   b.Width = W;
                   b.Height = buttonHeight;
               }

               infoLabel.Width = W;
               infoLabel.Height = buttonHeight;
               infoLabel.TextAlign = ContentAlignment.MiddleLeft;
               //indentText(W);
           }

           else if (this.DataButtonClicked == "market" || this.DataButtonClicked == "news")
           {
               foreach (Button b in btns)
               {
                   b.Width = W;
                   b.Height = buttonHeight;
               }
           }

       }

        private void inilizeComponents(){

            // create the infoLabel for stocks list 
            infoLabel = new Label();
            infoLabel.Text = "Symbol Name Latest Change Percent Open Value";
            infoLabel.TextAlign = ContentAlignment.MiddleCenter;
            infoLabel.BackColor = System.Drawing.ColorTranslator.FromHtml("#B8B8B8");
            infoLabel.AutoSize = false;
            infoLabel.Width = this.Width;
            infoLabel.TabStop = false;
            infoLabel.Margin = new Padding(0);


            // next and previous buttons for traversing the list
            first = new Label();
            last = new Label(); 
            next = new Label();
            previous = new Label();

            last.Text = ">>";
            last.AutoSize = false;
            last.Size = new System.Drawing.Size(30, 20);
            last.FlatStyle = FlatStyle.Flat;
            last.Margin = new Padding(0);
            last.Click += (sender, e) => { last_clicked(sender, e); };
            last.MouseEnter += (sender, e) => { highlight_MouseEnter(sender, e); };
            last.MouseLeave += (sender, e) => { highlight_MouseLeave(sender, e); };

            first.Text = "<<";
            first.AutoSize = false;
            first.Size = new System.Drawing.Size(30, 20);
            first.FlatStyle = FlatStyle.Flat;
            first.Margin = new Padding(0);
            first.Click += (sender, e) => { first_clicked(sender, e); };
            first.MouseEnter += (sender, e) => { highlight_MouseEnter(sender, e); };
            first.MouseLeave += (sender, e) => { highlight_MouseLeave(sender, e); };

            next.Text = ">";
            next.AutoSize = false;
            next.Size = new System.Drawing.Size(20, 20);
            next.FlatStyle = FlatStyle.Flat;
            next.Margin = new Padding(0);
            next.Click += (sender, e) => { next_clicked(sender, e); };
            next.MouseEnter += (sender, e) => { highlight_MouseEnter(sender, e); };
            next.MouseLeave += (sender, e) => { highlight_MouseLeave(sender, e); };

            previous.Text = "<";
            previous.AutoSize = false;
            previous.Size = new System.Drawing.Size(20, 20);
            previous.FlatStyle = FlatStyle.Flat;
            previous.Margin = new Padding(0);
            previous.Click += (sender, e) => { previous_clicked(sender, e); };
            previous.MouseEnter += (sender, e) => { highlight_MouseEnter(sender, e); };
            previous.MouseLeave += (sender, e) => { highlight_MouseLeave(sender, e); };

            nextPreviousPanel = new FlowLayoutPanel();
      
        }


       public void indentText(int Width)
       {
           int symbol = 0;
           int name = 0;
           int latest = 0;
           int change = 0;
           int percent = 0;
           int volume = 0;
           int openval = 0;

           Graphics grpx = Graphics.FromImage(new Bitmap(1, 1));

           for (int i = 0; i < 8; i++)
           {

               foreach (Stock s in filteredStockList)
               {
                   switch (i)
                   {

                       case 1:
                           var symbolsWidth = s.Symbol.Length;
                           if (symbolsWidth > symbol)
                           {
                               symbol = symbolsWidth;
                           }
                           break;

                       case 2:
                           var nameWidth = s.Name.Length;
                           if (nameWidth > name)
                           {
                               name = nameWidth;
                           }
                           break;

                       case 3:

                           var latestWidth = (int)grpx.MeasureString(s.Latest, btns[0].Font).Width;
                           if (latestWidth > latest)
                           {
                               latest = latestWidth;
                           }
                           break;

                       case 4:

                           var changeWidth = (int)grpx.MeasureString(s.Change, btns[0].Font).Width;
                           if (changeWidth > change)
                           {
                               change = changeWidth;
                           }
                           break;

                       case 5:

                           var percentWidth = (int)grpx.MeasureString(s.Percent, btns[0].Font).Width;
                           if (percentWidth > percent)
                           {
                               percent = percentWidth;
                           }
                           break;

                       case 6:

                           var volumeWidth = (int)grpx.MeasureString(s.Volume, btns[0].Font).Width;
                           if (volumeWidth > volume)
                           {
                               volume = volumeWidth;
                           }
                           break;

                       case 7:

                           var openValWidth = (int)grpx.MeasureString(s.OpenVal, btns[0].Font).Width;
                           if (openValWidth > openval)
                           {
                               openval = openValWidth;
                           }
                           break;
                   }


               }



           }


           String symbolIndent, nameIndent, latestIndent, changeIndent, percentIndent, volumeIndent, openvalIndent;

           int j = 0;
           foreach (Stock s in filteredStockList)
           {

               var currentSymbolWidth = s.Symbol.Length;
               symbolIndent = s.Symbol + new string(' ', (symbol - currentSymbolWidth) + 5);

               var currentNameWidth = s.Name.Length;
               nameIndent = s.Name + new string('.', (name - currentNameWidth) + 5);
               Console.WriteLine("currentSymbolWidth " + (name - currentNameWidth) + 5);

               var currentLatestWidth = (int)grpx.MeasureString(s.Latest, btns[0].Font).Width;
               latestIndent = s.Latest + new string(' ', (latest - currentLatestWidth) + 1);

               var currentChangeWidth = (int)grpx.MeasureString(s.Change, btns[0].Font).Width;
               changeIndent = s.Change + new string(' ', (change - currentChangeWidth) + 1);

               var currentPercentWidth = (int)grpx.MeasureString(s.Percent, btns[0].Font).Width;
               percentIndent = s.Percent + new string(' ', (percent - currentPercentWidth) + 1);

               var currentVolumeWidth = (int)grpx.MeasureString(s.Percent, btns[0].Font).Width;
               volumeIndent = s.Volume + new string(' ', (percent - currentVolumeWidth) + 1);

               var currentOpenvalWidth = (int)grpx.MeasureString(s.OpenVal, btns[0].Font).Width;
               openvalIndent = s.OpenVal + new string(' ', (openval - currentOpenvalWidth) + 1);

               btns[j].Text = symbolIndent + nameIndent + latestIndent + changeIndent + percentIndent + volumeIndent + openvalIndent;
               btns[j].TextAlign = ContentAlignment.MiddleLeft;

               j++;
           }

       }

    }
}
