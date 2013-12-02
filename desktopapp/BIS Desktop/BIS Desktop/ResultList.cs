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

        private Label[] infoLabels; // array of info labels
        private Button[] btns; // Array of buttons
        private TableLayoutPanel infoPanel; // Panel with labels, with info for the stock list


        // Labels for traversing the list of buttons
        private Label first;
        private Label last; 
        private Label next;
        private Label previous;
        private FlowLayoutPanel nextPreviousPanel;

        private String DataButtonClicked; // variable for determening which data we are listing
        
        private Boolean listButtonClicked = false; // boolean for controling the colors of the buttonslist
        private Boolean labelClicked = false; // boolean for controling the colors of the infoLabels when clicked
        private Boolean leaveColor; // boolean for controlling the colors of the infoLabels when mouse enters or leaves them

        private List<Stock> filteredStockList; // list with the stocks filtered after market
        
        private List<News> newsList; // list with ALL news from jsonhandler
        private List<Stock> allStocksList; // list with ALL stocks from jsonhandler

        private int numberOfButtonsPerPage = 10; // number of buttons listet per page, change depening on screen size
        private int maxNumberOfPages; // maximum number of pages 
        private int currentSide;  // the current side we are standing on, used for previuos and next labels

        // classes
        private MainWindow mw;
        private Controller c;
        private JsonHandler jh;

        public ResultList(String Data, String Market, object o){
            
            // initilize classes
            mw = o as MainWindow;
            jh = new JsonHandler();
            c = new Controller(); 
            
            /*
             * TO DO !!! ändra mängden på numberOfButtonsPerPage bereonde på storleken på panelen""" sad SOdhAKLFJADÖJFLÄFJADLF 
             * FOOOONTS!! 
            */

            DataButtonClicked = Data;

            // initilize all components except the list
            inilizeComponents();

            switch (DataButtonClicked)
            {
                case "market":
                    maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                    break;

                case "stocks":
                    allStocksList = jh.getAllStocks();
                    filteredStockList = filterStocks(allStocksList, Market);
                    maxNumberOfPages = filteredStockList.Count / numberOfButtonsPerPage;

                    break;

                case "news":
                    newsList = jh.getNews(Market);
                    maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                    break; 

                case "portfolio":
                    
                    break; 
            }

            contentAdder(1); 

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

        private void calcMaxNumberOfPages()
        {
            switch (DataButtonClicked)
            {
                case "market":
                    maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                    break;

                case "stocks":
                    maxNumberOfPages = filteredStockList.Count / numberOfButtonsPerPage;
                    break;

                case "news":
                    maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                    break;

                case "portfolio":
                    maxNumberOfPages = filteredStockList.Count / numberOfButtonsPerPage;
                    break;
            } 

        }



        private void contentAdder(int side)
        {
            // calculate max number of pages
            calcMaxNumberOfPages();


            // if we were standing on the last page when maximizing the window side can be larger than max number of pages
            if (side > maxNumberOfPages)
            {
                side = maxNumberOfPages; 
            }

            currentSide = side; 
            this.Controls.Clear();
            
            switch (DataButtonClicked)
            {
                case "market":
                    
                    int marketStop;
                    int marketStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (side * numberOfButtonsPerPage <= newsList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        marketStop = side * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        marketStart = marketStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal 
                        marketStop = newsList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        marketStart = newsList.Count % maxNumberOfPages;  
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (newsList.Count < numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (side * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (side == maxNumberOfPages)
                    {

                        first.Enabled = true;
                        next.Enabled = false;
                        previous.Enabled = true;
                        last.Enabled = false;
                    }
                    
                    // else if none of the above match me must be somewhere in the middle of the list and therefore every label is set to enabled == true
                    else
                    {
                        first.Enabled = true;
                        next.Enabled = true;
                        previous.Enabled = true;
                        last.Enabled = true;
                    }
       

                    // initilize buttons
                    initilizeNewsList(marketStart, marketStop);

                    // add buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }

                    // reset number buttons
                    initilizeNumberButtons(side, marketStart, marketStop);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "stocks":

                    int stockStop;
                    int stockStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (side * numberOfButtonsPerPage <= filteredStockList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        stockStop = side * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        stockStart = stockStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal 
                        stockStop = filteredStockList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        stockStart = filteredStockList.Count % maxNumberOfPages;  
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (filteredStockList.Count < numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (side * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (side == maxNumberOfPages)
                    {

                        first.Enabled = true;
                        next.Enabled = false;
                        previous.Enabled = true;
                        last.Enabled = false;
                    }
                    
                    // else if none of the above match me must be somewhere in the middle of the list and therefore every label is set to enabled == true
                    else
                    {
                        first.Enabled = true;
                        next.Enabled = true;
                        previous.Enabled = true;
                        last.Enabled = true;
                    }
       

                    // initilize buttons
                    initilizeStockList(stockStart, stockStop);

                    // add buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }


                    // reset number buttons
                    initilizeNumberButtons(side, stockStart, stockStop);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "news":

                    int newsStop;
                    int newsStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (side * numberOfButtonsPerPage <= newsList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        newsStop = side * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        newsStart = newsStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal 
                        newsStop = newsList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        newsStart = newsList.Count % maxNumberOfPages;  
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (newsList.Count < numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (side * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (side == maxNumberOfPages)
                    {

                        first.Enabled = true;
                        next.Enabled = false;
                        previous.Enabled = true;
                        last.Enabled = false;
                    }
                    
                    // else if none of the above match me must be somewhere in the middle of the list and therefore every label is set to enabled == true
                    else
                    {
                        first.Enabled = true;
                        next.Enabled = true;
                        previous.Enabled = true;
                        last.Enabled = true;
                    }
       

                    // initilize buttons
                    initilizeNewsList(newsStart, newsStop);

                    // add buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }

                    // reset number buttons
                    initilizeNumberButtons(side, newsStart, newsStop);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "portfolio":

                    int portfolioStop;
                    int portfolioStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (side * numberOfButtonsPerPage <= newsList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        portfolioStop = side * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        portfolioStart = portfolioStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal 
                        portfolioStop = newsList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        portfolioStart = newsList.Count % maxNumberOfPages;  
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (newsList.Count < numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (side * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (side == maxNumberOfPages)
                    {

                        first.Enabled = true;
                        next.Enabled = false;
                        previous.Enabled = true;
                        last.Enabled = false;
                    }
                    
                    // else if none of the above match me must be somewhere in the middle of the list and therefore every label is set to enabled == true
                    else
                    {
                        first.Enabled = true;
                        next.Enabled = true;
                        previous.Enabled = true;
                        last.Enabled = true;
                    }
       

                    // initilize buttons
                    initilizeNewsList(portfolioStart, portfolioStop);

                    // add buttons
                    foreach (Button b in btns)
                    {
                        this.Controls.Add(b);
                    }

                    // reset number buttons
                    initilizeNumberButtons(side, portfolioStart, portfolioStop);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

            }     

        }

        private void initilizeStockList(int start, int stop)
        {
            // add the info labels
            this.Controls.Add(infoPanel);

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
                btns[j].Font = mw.Font; 
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
               btns[j].TextAlign = ContentAlignment.MiddleCenter;
               btns[j].Font = mw.Font;
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

        }

        private void initilizeNumberButtons(int side, int start, int stop)
        {

            // clear the panel
            nextPreviousPanel.Controls.Clear();

            // re add the first and previous labels
            nextPreviousPanel.Controls.Add(first);
            nextPreviousPanel.Controls.Add(previous);

            // ints that will be assigned the values of which number labels to build
            int numberStop;
            int numberStart;

            // if the side is more than or equal to the max number of pages minus 10, 10 beeing the number of numberlabels we display. 
            if (side >= maxNumberOfPages - 10)
            {

                // numberStart is equal to maxNumberOfpages minus 10
                numberStart = maxNumberOfPages - 10;

                // and numberStop is equal to maxnumberOfPages plus 1
                numberStop = maxNumberOfPages + 1;

            }

            else
            {
                // else everything is normal and we increase both start and stop
                numberStop = side + 10;
                numberStart = side;
            }


            // loop the number labels intervall
            for (int i = numberStart; i < numberStop; i++)
            {
                // create and add the labels
                Label number = new Label();
                string text = i.ToString();
                number.Text = text;
                number.Font = mw.Font;
                number.Size = new System.Drawing.Size(30, 20);

                // it is imposible to pass i as i would set to numberStop instead of current i val, therefore we send the 
                // captured value in text instead
                number.Click += (sender, e) => { numberLabel_clicked(sender, e, text); };

                number.MouseEnter += (sender, e) => { highlightNum_MouseEnter(sender, e); };
                number.MouseLeave += (sender, e) => { highlightNum_MouseLeave(sender, e); };
                nextPreviousPanel.Controls.Add(number);

                // set the current side to c.mercuryBlue
                if (i == side)
                {
                    number.ForeColor = c.mercuryRed; 
                }

            }

            nextPreviousPanel.Controls.Add(next);
            nextPreviousPanel.Controls.Add(last);
        }

        private void setLabelColors(Label[] labels)
        {
            foreach (Label l in labels)
            {
                l.BackColor = c.mercuryGrey;
                l.ForeColor = Color.White;
            }        

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
   
       private void highlightLabel_MouseEnter(object sender, System.EventArgs e)
       {
           Console.WriteLine("enter"); 
           Label L = sender as Label;
           L.BackColor = Color.Black;
       }

       private void highlightLabel_MouseLeave(object sender, System.EventArgs e)
       {
           Console.WriteLine("leave");
           Label L = sender as Label;
           L.BackColor = c.mercuryGrey;
                   
       }

       private void highlightNum_MouseEnter(object sender, System.EventArgs e)
       {
           
           Label L = sender as Label;
           L.ForeColor = c.mercuryRed;
       }

       private void highlightNum_MouseLeave(object sender, System.EventArgs e)
       {

           Label L = sender as Label;
           int temp = 0; 

           try
           {
                temp = int.Parse(L.Text);
             
           }
           catch (Exception ee)
           {
               L.ForeColor = Color.Black;
           }
                
            
           if (temp == currentSide)
           {
                L.ForeColor = c.mercuryRed;
           }
           else
           {
               L.ForeColor = Color.Black; 
           }                 
           
       }

       private void numberLabel_clicked(object sender, System.EventArgs e, String i)
       {
           var num = int.Parse(i);
           contentAdder(num);
       }
       
       private void last_clicked(object sender, System.EventArgs e)
       {
           // go to last side
           contentAdder(maxNumberOfPages); 
       }

       private void first_clicked(object sender, System.EventArgs e)
       {
           // go to side 1
           contentAdder(1);
       }

       private void previous_clicked(object sender, System.EventArgs e)
       {
           // increse side by 1
           contentAdder(currentSide - 1);
       }

       private void next_clicked(object sender, System.EventArgs e)
       {
           // decrese side by 1
           contentAdder(currentSide + 1);
       }

       private void infoLabel_clicked(object sender, System.EventArgs e, String sortAfter)
       {

           leaveColor = false; 
           Label b = sender as Label;

           Console.WriteLine("clicked"); 
           if (labelClicked == true)
           {
               setLabelColors(infoLabels);
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               labelClicked = true;
           }
           else
           {
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               labelClicked = true;
           }
           
           //filteredStockList = Controller.sort(filteredStockList, sortAfter);
           contentAdder(1);
       }

       private void stock_clicked(object sender, System.EventArgs e, Stock s)
       {
           Button b = sender as Button;

           if (listButtonClicked == true)
           {
               setButtonColors(btns);
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               listButtonClicked = true;
           }
           else
           {
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               listButtonClicked = true;
           }

           ResultPanel temp = mw.rightPanelResults as ResultPanel;
           mw.loadResult(temp, "info", s.Symbol, mw);

       }

       private void news_clicked(object sender, System.EventArgs e, News n)
       {
           Button b = sender as Button;
       }

       public void setSize(int W, int H)
       {
           // store the sizes
           panelWidth = W;
           panelHeigth = H;

           // reset panel size
           this.Height = H;
           this.Width = W;

           // reset nextPreviousPanel width
           nextPreviousPanel.Width = this.Width;

           // reset number of buttons per page
           numberOfButtonsPerPage = (H / buttonHeight) - 2;
         

           if (this.DataButtonClicked == "stocks" || this.DataButtonClicked == "portfolio")
           {
               foreach (Button b in btns)
               {
                   b.Width = W;
                   b.Height = buttonHeight;
               }

               infoPanel.Width = W;
               infoPanel.Height = buttonHeight;
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

           contentAdder(currentSide); 

       }

        private void inilizeComponents(){

            // create the infoLabel for stocks list
            infoPanel = new TableLayoutPanel();

            infoLabels = new Label[7]; 

            Label SymbolLabel = new Label();
            SymbolLabel.Text = "Symbol";
            SymbolLabel.TextAlign = ContentAlignment.MiddleCenter;
            SymbolLabel.Font = mw.Font;
            SymbolLabel.BackColor = c.mercuryGrey;
            SymbolLabel.ForeColor = Color.White;
            SymbolLabel.Size = new Size(60, 35);
            SymbolLabel.TabStop = false;
            SymbolLabel.Margin = new Padding(0);

            SymbolLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, SymbolLabel.Text); };
            SymbolLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            SymbolLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            Label NameLabel = new Label();
            NameLabel.Text = "Name";
            NameLabel.TextAlign = ContentAlignment.MiddleCenter;
            NameLabel.Font = mw.Font;
            NameLabel.BackColor = c.mercuryGrey;
            NameLabel.ForeColor = Color.White;
            NameLabel.Size = new Size(60, 35);
            NameLabel.TabStop = false;
            NameLabel.Margin = new Padding(0);

            NameLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, NameLabel.Text); };
            NameLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            NameLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            Label LatestLabel = new Label();
            LatestLabel.Text = "Latest";
            LatestLabel.TextAlign = ContentAlignment.MiddleCenter;
            LatestLabel.Font = mw.Font;
            LatestLabel.BackColor = c.mercuryGrey;
            LatestLabel.ForeColor = Color.White;
            LatestLabel.Size = new Size(60, 35);
            LatestLabel.TabStop = false;
            LatestLabel.Margin = new Padding(0);

            LatestLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, LatestLabel.Text); };
            LatestLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            LatestLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            Label ChangeLabel = new Label();
            ChangeLabel.Text = "Change";
            ChangeLabel.TextAlign = ContentAlignment.MiddleCenter;
            ChangeLabel.Font = mw.Font;
            ChangeLabel.BackColor = c.mercuryGrey;
            ChangeLabel.ForeColor = Color.White;
            ChangeLabel.Size = new Size(60, 35);
            ChangeLabel.TabStop = false;
            ChangeLabel.Margin = new Padding(0);

            ChangeLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, ChangeLabel.Text); };
            ChangeLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            ChangeLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            Label PercentLabel = new Label();
            PercentLabel.Text = "Percent";
            PercentLabel.TextAlign = ContentAlignment.MiddleCenter;
            PercentLabel.Font = mw.Font;
            PercentLabel.BackColor = c.mercuryGrey;
            PercentLabel.ForeColor = Color.White;
            PercentLabel.Size = new Size(60, 35);
            PercentLabel.TabStop = false;
            PercentLabel.Margin = new Padding(0);

            PercentLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, PercentLabel.Text); };
            PercentLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            PercentLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            Label VolumeLabel = new Label();
            VolumeLabel.Text = "Volume";
            VolumeLabel.TextAlign = ContentAlignment.MiddleCenter;
            VolumeLabel.Font = mw.Font;
            VolumeLabel.BackColor = c.mercuryGrey;
            VolumeLabel.ForeColor = Color.White;
            VolumeLabel.Size = new Size(60, 35);
            VolumeLabel.TabStop = false;
            VolumeLabel.Margin = new Padding(0);

            VolumeLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, VolumeLabel.Text); };
            VolumeLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            VolumeLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            Label OpenValueLabel = new Label();
            OpenValueLabel.Text = "Open Value";
            OpenValueLabel.TextAlign = ContentAlignment.MiddleCenter;
            OpenValueLabel.Font = mw.Font;
            OpenValueLabel.BackColor = c.mercuryGrey;
            OpenValueLabel.ForeColor = Color.White;
            OpenValueLabel.Size = new Size(60, 35);
            OpenValueLabel.TabStop = false;
            OpenValueLabel.Margin = new Padding(0);

            OpenValueLabel.Click += (sender, e) => { infoLabel_clicked(sender, e, OpenValueLabel.Text); };
            OpenValueLabel.MouseEnter += (sender, e) => { highlightLabel_MouseEnter(sender, e); };
            OpenValueLabel.MouseLeave += (sender, e) => { highlightLabel_MouseLeave(sender, e); };

            // add the labels to the panel
            infoPanel.Controls.Add(SymbolLabel, 1, 1);
            infoPanel.Controls.Add(NameLabel, 3, 1);
            infoPanel.Controls.Add(LatestLabel, 5, 1);
            infoPanel.Controls.Add(ChangeLabel, 7, 1);
            infoPanel.Controls.Add(PercentLabel, 9, 1);
            infoPanel.Controls.Add(VolumeLabel, 11, 1);
            infoPanel.Controls.Add(OpenValueLabel, 13, 1);

            // add the labels to the array
            infoLabels[0] = SymbolLabel;
            infoLabels[1] = NameLabel;
            infoLabels[2] = LatestLabel;
            infoLabels[3] = ChangeLabel;
            infoLabels[4] = PercentLabel;
            infoLabels[5] = VolumeLabel;
            infoLabels[6] = OpenValueLabel;



            // next and previous buttons for traversing the list
            first = new Label();
            last = new Label(); 
            next = new Label();
            previous = new Label();

            last.Text = ">>";
            last.Font = mw.Font;
            last.AutoSize = false;
            last.Size = new System.Drawing.Size(30, 20);
            last.FlatStyle = FlatStyle.Flat;
            last.Margin = new Padding(0);
            last.Click += (sender, e) => { last_clicked(sender, e); };
            last.MouseEnter += (sender, e) => { highlightNum_MouseEnter(sender, e); };
            last.MouseLeave += (sender, e) => { highlightNum_MouseLeave(sender, e); };

            first.Text = "<<";
            first.Font = mw.Font;
            first.AutoSize = false;
            first.Size = new System.Drawing.Size(30, 20);
            first.FlatStyle = FlatStyle.Flat;
            first.Margin = new Padding(0);
            first.Click += (sender, e) => { first_clicked(sender, e); };
            first.MouseEnter += (sender, e) => { highlightNum_MouseEnter(sender, e); };
            first.MouseLeave += (sender, e) => { highlightNum_MouseLeave(sender, e); };

            next.Text = ">";
            next.Font = mw.Font;
            next.AutoSize = false;
            next.Size = new System.Drawing.Size(20, 20);
            next.FlatStyle = FlatStyle.Flat;
            next.Margin = new Padding(0);
            next.Click += (sender, e) => { next_clicked(sender, e); };
            next.MouseEnter += (sender, e) => { highlightNum_MouseEnter(sender, e); };
            next.MouseLeave += (sender, e) => { highlightNum_MouseLeave(sender, e); };

            previous.Text = "<";
            previous.Font = mw.Font;
            previous.AutoSize = false;
            previous.Size = new System.Drawing.Size(20, 20);
            previous.FlatStyle = FlatStyle.Flat;
            previous.Margin = new Padding(0);
            previous.Click += (sender, e) => { previous_clicked(sender, e); };
            previous.MouseEnter += (sender, e) => { highlightNum_MouseEnter(sender, e); };
            previous.MouseLeave += (sender, e) => { highlightNum_MouseLeave(sender, e); };

            nextPreviousPanel = new FlowLayoutPanel();
            nextPreviousPanel.Height = 20;
      
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
