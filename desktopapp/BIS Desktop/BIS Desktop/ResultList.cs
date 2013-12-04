using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Diagnostics;


namespace BIS_Desktop
{
    public class ResultList : FlowLayoutPanel
    {
       

        // size variables
        private int panelWidth;
        private int panelHeigth;
        private int buttonHeight = 40;
        
        private int stockLabelWidth; // the width of the stock labels
        private int newsLabelWidth; // the width of the news labels

        private int numberOfButtonsPerPage = 10; // number of buttons listet per page, change depening on screen size
        private int maxNumberOfPages; // maximum number of pages 
        private int currentSide;  // the current side we are standing on, used for previuos and next labels


        // Boolean for controlling the infoButtons sort functions, ie sort decendeing or acending
        private Boolean SymbolInfoClicked = false;
        private Boolean NameInfoClicked = false;
        private Boolean LatestInfoClicked = false;
        private Boolean ChangeInfoClicked = false;
        private Boolean PercentInfoClicked = false;
        private Boolean OpenValInfoClicked = false;

        private Button[] stockInfoButtons; // array of stock info buttons
        private Button[] newsInfoButtons; // array of news info buttons

        private Label[,] stockLabels; // 2d Array of stockLabels
        private Label[,] newsLabels; // 2d Array of newsLabels

        private FlowLayoutPanel[] stockPanels; // array for stockPanels with labels
        private FlowLayoutPanel[] newsPanels; // array for newsPanels with labels

        private TableLayoutPanel stockInfoPanel; // Panel with labels, with info for the stock list
        private TableLayoutPanel newsInfoPanel; // panel with labels, with info for the news list

        // Labels for traversing the list of buttons
        private Label first;
        private Label last; 
        private Label next;
        private Label previous;
        private FlowLayoutPanel nextPreviousPanel;

        private String DataButtonClicked; // variable for determening which data we are listing
        
        private Boolean listButtonClicked = false; // boolean for controling the colors of the buttonslist
        private Boolean labelClicked = false; // boolean for controling the colors of the infoLabels when clicke

        private List<Stock> filteredStockList; // list with the stocks filtered after market
        
        private List<News> newsList; // list with ALL news from jsonhandler
        private List<Stock> allStocksList; // list with ALL stocks from jsonhandler

     

        // classes
        private MainWindow mw;
        private Controller c;
        private JsonHandler jh;

        public ResultList(String Data, String Market, object o){

            // initilize classes
            mw = o as MainWindow;
            jh = new JsonHandler();
            c = new Controller(); 

            DataButtonClicked = Data;

            this.BackColor = c.mercuryBeige;
            // initilize all components except the list
            inilizeComponents();

            switch (DataButtonClicked)
            {
                case "market":
                    break;

                case "stocks":
                    allStocksList = jh.getAllStocks();
                    filteredStockList = filterStocks(allStocksList, Market);
                    break;

                case "news":
                    newsList = jh.getAllNews(Market);
                    break; 

                case "portfolio":
                    
                    break; 
            }

            currentSide = 1;
            contentAdder(currentSide); 

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
                    if (newsList.Count % numberOfButtonsPerPage == 0)
                    {
                        maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                    }
                    else
                    {
                        maxNumberOfPages = (newsList.Count / numberOfButtonsPerPage) + 1;
                    }
                    
                    break;

                case "stocks":
                    if (filteredStockList.Count % numberOfButtonsPerPage == 0)
                    {
                        maxNumberOfPages = filteredStockList.Count / numberOfButtonsPerPage;
                    }
                    else
                    {
                        maxNumberOfPages = (filteredStockList.Count / numberOfButtonsPerPage) + 1;
                    }
                    break;

                case "news":
                    if (newsList.Count % numberOfButtonsPerPage == 0)
                    {
                        maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                    }
                    else
                    {
                        maxNumberOfPages = (newsList.Count / numberOfButtonsPerPage) + 1;
                    }
                    break;

                case "portfolio":
                    if (filteredStockList.Count % numberOfButtonsPerPage == 0)
                    {
                        maxNumberOfPages = filteredStockList.Count / numberOfButtonsPerPage;
                    }
                    else
                    {
                        maxNumberOfPages = (filteredStockList.Count / numberOfButtonsPerPage) + 1;
                    }
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
                    if (currentSide * numberOfButtonsPerPage <= newsList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        marketStop = currentSide * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        marketStart = marketStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal to the count of the list
                        marketStop = newsList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        marketStart = marketStop - (filteredStockList.Count % numberOfButtonsPerPage);  
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (maxNumberOfPages == 1)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (currentSide * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (currentSide == maxNumberOfPages)
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
                    foreach (Panel p in newsPanels)
                    {
                        this.Controls.Add(p);
                    }

                    // reset number buttons
                    initilizeNumberButtons(currentSide);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "stocks":

                    int stockStop;
                    int stockStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (currentSide * numberOfButtonsPerPage <= filteredStockList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        stockStop = currentSide * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        stockStart = stockStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal to the count of the list
                        stockStop = filteredStockList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        stockStart = stockStop - (filteredStockList.Count % numberOfButtonsPerPage);  
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (maxNumberOfPages == 1)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (currentSide * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (currentSide == maxNumberOfPages)
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
                    foreach (Panel p in stockPanels)
                    {
                        this.Controls.Add(p); 
                    }


                    // reset number buttons
                    Console.WriteLine("side: " + currentSide + "  stockStart: " + stockStart + "  stockStop: " + stockStop); 
                    initilizeNumberButtons(currentSide);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "news":

                    int newsStop;
                    int newsStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (currentSide * numberOfButtonsPerPage <= newsList.Count)
                    {
                        // stop equals side times number of buttons per page
                        newsStop = currentSide * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        newsStart = newsStop - numberOfButtonsPerPage;
                    }
                    else
                    {
                        // else stop is equal to the count of the list
                        newsStop = newsList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        newsStart = newsStop - (newsList.Count % numberOfButtonsPerPage);
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (maxNumberOfPages == 1)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (currentSide * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (currentSide == maxNumberOfPages)
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
                    foreach (Panel p in newsPanels)
                    {
                        this.Controls.Add(p);
                    }

                    // reset number buttons
                    Console.WriteLine("side: " + currentSide + "  newsStart: " + newsStart + "  newsStop: " + newsStop); 
                    initilizeNumberButtons(currentSide);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

                case "portfolio":

                    int portfolioStop;
                    int portfolioStart;

                    // if the side times the number of buttons per page is less or equal to the size of the list
                    if (currentSide * numberOfButtonsPerPage <= newsList.Count)
                    {  
                        // stop equals side times number of buttons per page
                        portfolioStop = currentSide * numberOfButtonsPerPage;

                        // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                        portfolioStart = portfolioStop - numberOfButtonsPerPage;  
                    }
                    else
                    {
                        // else stop is equal to the count of the list
                        portfolioStop = newsList.Count;

                        // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                        portfolioStart = portfolioStop - (filteredStockList.Count % numberOfButtonsPerPage);
                    }

                    // if the list is less the the preffered number of buttons per page than set all buttons to enabled == false
                    if (maxNumberOfPages == 1)
                    {
                        first.Enabled = false;
                        previous.Enabled = false;
                        next.Enabled = false;
                        last.Enabled = false;
                    }

                    // if we are on the first side we set fist and previous to enabled == false
                    else if (currentSide * numberOfButtonsPerPage == numberOfButtonsPerPage)
                    {
                        first.Enabled = false;
                        next.Enabled = true;
                        previous.Enabled = false;
                        last.Enabled = true;

                    }

                    // if we are on the last side we set next and last to enabled == false
                    else if (currentSide == maxNumberOfPages)
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
                    foreach (Panel p in newsPanels)
                    {
                        this.Controls.Add(p);
                    }

                    // reset number buttons
                    initilizeNumberButtons(currentSide);

                    // add panel
                    this.Controls.Add(nextPreviousPanel);

                    break;

            }     

        }

        private void initilizeStockList(int start, int stop)
        {
            // add the info labels
            this.Controls.Add(stockInfoPanel);

            // create btns array
            stockLabels = new Label[stop - start, 6];

            // variable for iterating the btns[]
            int j = 0;

            // create new "stock buttons" fill the array 
            for (int i = start; i < stop; i++)
            { 
   
                Stock s = filteredStockList[i];

                Label symbolLabel = new Label();
                symbolLabel.Text = s.Symbol;
                symbolLabel.TextAlign = ContentAlignment.MiddleCenter;
                symbolLabel.Font = mw.Font;
                symbolLabel.Height = buttonHeight;
                symbolLabel.Width = stockLabelWidth; 
                symbolLabel.Margin = new Padding(0);
                symbolLabel.BackColor = Color.White; 
                symbolLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
                symbolLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
                symbolLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); }; 

                Label nameLabel = new Label();
                nameLabel.Text = s.Name;
                nameLabel.TextAlign = ContentAlignment.MiddleCenter;
                nameLabel.Font = mw.Font;
                nameLabel.Height = buttonHeight;
                nameLabel.Width = stockLabelWidth; 
                nameLabel.Margin = new Padding(0);
                nameLabel.BackColor = Color.White; 
                nameLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
                nameLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
                nameLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); }; 

                Label latestLabel = new Label();
                latestLabel.Text = s.Latest;
                latestLabel.TextAlign = ContentAlignment.MiddleCenter;
                latestLabel.Font = mw.Font;
                latestLabel.Height = buttonHeight;
                latestLabel.Width = stockLabelWidth; 
                latestLabel.Margin = new Padding(0);
                latestLabel.BackColor = Color.White; 
                latestLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
                latestLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
                latestLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); }; 

                Label changeLabel = new Label();
                changeLabel.Text = s.Change;
                changeLabel.TextAlign = ContentAlignment.MiddleCenter;
                changeLabel.Font = mw.Font;
                changeLabel.Height = buttonHeight;
                changeLabel.Width = stockLabelWidth; 
                changeLabel.Margin = new Padding(0);
                changeLabel.BackColor = Color.White;

                if (changeLabel.Text[0] == '-')
                {
                    changeLabel.ForeColor = Color.Red;
                }
                else if (changeLabel.Text[0] == '+')
                {
                    changeLabel.ForeColor = Color.Green;
                }

                changeLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
                changeLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
                changeLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); }; 


                Label percentLabel = new Label();
                percentLabel.Text = s.Percent;
                percentLabel.TextAlign = ContentAlignment.MiddleCenter;
                percentLabel.Font = mw.Font;
                percentLabel.Height = buttonHeight;
                percentLabel.Width = stockLabelWidth; 
                percentLabel.Margin = new Padding(0);
                percentLabel.BackColor = Color.White;

                if (percentLabel.Text[0] == '-')
                {
                    percentLabel.ForeColor = Color.Red;
                }
                else if (percentLabel.Text[0] == '+')
                {
                    percentLabel.ForeColor = Color.Green;
                }
             
                
                percentLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
                percentLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
                percentLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); }; 

                Label openLabel = new Label();
                openLabel.Text = s.OpenVal;
                openLabel.TextAlign = ContentAlignment.MiddleCenter;
                openLabel.Font = mw.Font;
                openLabel.Height = buttonHeight;
                openLabel.Width = stockLabelWidth; 
                openLabel.Margin = new Padding(0);
                openLabel.BackColor = Color.White; 
                openLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
                openLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
                openLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); };


                stockLabels[j, 0] = symbolLabel; 
                stockLabels[j, 1] = nameLabel;
                stockLabels[j, 2] = latestLabel;
                stockLabels[j, 3] = changeLabel;
                stockLabels[j, 4] = percentLabel;
                stockLabels[j, 5] = openLabel; 
  
                j++; 
             
            }

            //setStockLabelColors(); 

            stockPanels = new FlowLayoutPanel[numberOfButtonsPerPage]; 

            for (int i = 0; i < stockLabels.GetLength(0); i++)
            {
                FlowLayoutPanel stockPanel = new FlowLayoutPanel();
                stockPanel.FlowDirection = System.Windows.Forms.FlowDirection.LeftToRight;
                stockPanel.Height = buttonHeight;
                stockPanel.Width = panelWidth; 

                for (int k = 0; k < stockLabels.GetLength(1); k++)
                {
                    stockPanel.Controls.Add(stockLabels[i, k]);
                }

                stockPanels[i] = stockPanel; 
            }
          
        }

        private void initilizeNewsList(int start, int stop)
        {

            this.Controls.Add(newsInfoPanel); 

            // create btns array
            newsLabels = new Label[stop - start, 3];

            // variable for iterating the btns[]
            int j = 0;

            // create new "news buttons" fill the array 
            for (int i = start; i < stop; i++)
            {
     
               News n = newsList[i];

               Label symbolLabel = new Label();
               symbolLabel.Text = n.symbol;
               symbolLabel.TextAlign = ContentAlignment.MiddleCenter;
               symbolLabel.Font = mw.Font;
               symbolLabel.Height = buttonHeight;
               symbolLabel.Width = newsLabelWidth;
               symbolLabel.Margin = new Padding(0);
               symbolLabel.BackColor = Color.White;
               symbolLabel.Click += (sender, e) => { news_clicked(sender, e, n); };
               symbolLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
               symbolLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); };

               Label titleLabel = new Label();
               titleLabel.Text = n.title;
               titleLabel.TextAlign = ContentAlignment.MiddleCenter;
               titleLabel.Font = mw.Font;
               titleLabel.Height = buttonHeight;
               titleLabel.Width = newsLabelWidth;
               titleLabel.Margin = new Padding(0);
               titleLabel.BackColor = Color.White;
               titleLabel.Click += (sender, e) => { news_clicked(sender, e, n); };
               titleLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
               titleLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); };

               Label updatedLabel = new Label();
               updatedLabel.Text = n.pubDate;
               updatedLabel.TextAlign = ContentAlignment.MiddleCenter;
               updatedLabel.Font = mw.Font;
               updatedLabel.Height = buttonHeight;
               updatedLabel.Width = newsLabelWidth;
               updatedLabel.Margin = new Padding(0);
               updatedLabel.BackColor = Color.White;
               updatedLabel.Click += (sender, e) => { news_clicked(sender, e, n); };
               updatedLabel.MouseEnter += (sender, e) => { highlightStock_MouseEnter(sender, e, j); };
               updatedLabel.MouseLeave += (sender, e) => { highlightStock_MouseLeave(sender, e, j); };


               newsLabels[j, 0] = symbolLabel;
               newsLabels[j, 1] = titleLabel;
               newsLabels[j, 2] = updatedLabel;

               j++; 
            }

            newsPanels = new FlowLayoutPanel[numberOfButtonsPerPage];

            for (int i = 0; i < newsLabels.GetLength(0); i++)
            {
                FlowLayoutPanel newsPanel = new FlowLayoutPanel();
                newsPanel.FlowDirection = System.Windows.Forms.FlowDirection.LeftToRight;
                newsPanel.Height = buttonHeight;
                newsPanel.Width = panelWidth;

                for (int k = 0; k < newsLabels.GetLength(1); k++)
                {
                    newsPanel.Controls.Add(newsLabels[i, k]);
                }

                newsPanels[i] = newsPanel; 
            }
          
    

        }

        private void initilizeNumberButtons(int side)
        {

            // clear the panel
            nextPreviousPanel.Controls.Clear();

            // re add the first and previous labels
            nextPreviousPanel.Controls.Add(first);
            nextPreviousPanel.Controls.Add(previous);

            // ints that will be assigned the values of which number labels to build
            int numberStop;
            int numberStart;

            if(maxNumberOfPages < 10)
            {
                numberStart = 1;
                numberStop = maxNumberOfPages + 1;
                
            }       
            
            // if the side is more than or equal to the max number of pages minus 10, 10 beeing the number of numberlabels we display. 
            else if (side >= maxNumberOfPages - 10)
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

            Console.WriteLine("numberStart: " + numberStart); 
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
                    number.ForeColor = c.mercuryBlue; 
                }

            }

            nextPreviousPanel.Controls.Add(next);
            nextPreviousPanel.Controls.Add(last);
        }


        private void setStockLabelColors()
        {
       
            for (int i = 0; i <= stockLabels.GetLength(0); i++)
            {
                for (int k = 0; k <= stockLabels.GetLength(1); k++)
                {
                    if (i % 2 == 1)
                    {
                        stockLabels[i, k].BackColor = Color.White;
                        stockLabels[i, k].ForeColor = Color.Black;
                    }
                    else
                    {
                        stockLabels[i, k].BackColor = Color.White;
                        stockLabels[i, k].ForeColor = Color.Black;
                    }
                
                }
                         
            }
        
    }


        private void setInfoColors(Button[] infoButtons)
        {
            foreach (Button l in infoButtons)
            {
                l.BackColor = c.mercuryGrey;
                l.ForeColor = Color.White;
            }        

        }

        private void setButtonColors(Button[] buttons)
        {
            for (int i = 0; i < buttons.Length; i++)
            {
                if (i % 2 == 1)
                {
                    buttons[i].BackColor = Color.White;
                    buttons[i].ForeColor = Color.Black;
                }

                else
                {
                    buttons[i].BackColor = Color.White;
                    buttons[i].ForeColor = Color.Black;
                }
            }               
              
        }

       private void highlightStock_MouseEnter(object sender, System.EventArgs e, int j)
       {
           

       }

       private void highlightStock_MouseLeave(object sender, System.EventArgs e, int j)
       {
         
       }

       private void highlightNum_MouseEnter(object sender, System.EventArgs e)
       {
           Label L = sender as Label;
           L.ForeColor = Color.LightBlue;
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

       private void newsInfoButton_clicked(object sender, System.EventArgs e, String sortAfter)
       {
            Button b = sender as Button;

           if (labelClicked == true)
           {
               setInfoColors(newsInfoButtons);
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

           var temp = b.Text;

           switch (temp)
           {
               case "Symbol":

                   if (SymbolInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       SymbolInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       SymbolInfoClicked = true;
                   }

                   break; 

               case "Title":

                   if (NameInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       NameInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       NameInfoClicked = true;
                   }

                   break;

               case "Updated":
                   if (LatestInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       LatestInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       LatestInfoClicked = true;
                   }

                   break; 
           }

           contentAdder(1);
       }

       private void stockInfoButton_clicked(object sender, System.EventArgs e, String sortAfter)
       {
           Button b = sender as Button;

           if (labelClicked == true)
           {
               setInfoColors(stockInfoButtons);
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

           var temp = b.Text;

           switch (temp)
           {
               case "Symbol":
                   if(SymbolInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       SymbolInfoClicked = false; 
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       SymbolInfoClicked = true;
                   }
                        
                   break;

               case "Name":
                   if (NameInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       NameInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       NameInfoClicked = true;
                   }

                   break;

               case "Latest":
                   if (LatestInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       LatestInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       LatestInfoClicked = true;
                   }

                   break;

               case "Change":
                   if (ChangeInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       ChangeInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       ChangeInfoClicked = true;
                   }

                   break;

               case "Percent":
                   if (PercentInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       PercentInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       PercentInfoClicked = true;
                   }

                   break;

               case "Opening value":
                   if (OpenValInfoClicked)
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, true);
                       OpenValInfoClicked = false;
                   }

                   else
                   {
                       filteredStockList = c.sortStockList(filteredStockList, sortAfter, false);
                       OpenValInfoClicked = true;
                   }

                   break;
 
           }
           
           contentAdder(1);
       }

       private void stock_clicked(object sender, System.EventArgs e, Stock s, int num)
       {
           ResultPanel temp = mw.rightPanelResults as ResultPanel;
           mw.loadResult(temp, "info", s.Symbol, mw);
       }

       private void news_clicked(object sender, System.EventArgs e, News n)
       {
            //String news = n.symbol + "#" + n.title +"#"+ n.link +"#"+ n.description +"#"+ n.guid +"#"+ n.pubDate;
            ResultPanel temp = mw.rightPanelResults as ResultPanel;
            mw.loadNewsResult(temp, "newsReader", mw, n);
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

           if (this.DataButtonClicked == "stocks" || this.DataButtonClicked == "portfolio")
           {

               foreach (FlowLayoutPanel p in stockPanels)
               {
                   p.Width = W;
                   p.Height = buttonHeight; 
               }


               stockLabelWidth = W / 6;

               foreach (Button btn in stockInfoButtons)
               {
                   btn.Width = stockLabelWidth;
               }

               stockInfoPanel.Width = W;
               stockInfoPanel.Height = buttonHeight; 

               // reset number of buttons per page
               if ((H / buttonHeight) - 2  < filteredStockList.Count)
                {                     
                    numberOfButtonsPerPage = (H / buttonHeight) - 4;

                }
                    
           }

           else if (this.DataButtonClicked == "market" || this.DataButtonClicked == "news")
           {

               newsLabelWidth = W / 3; 

               foreach (Button btn in newsInfoButtons)
               {
                   btn.Width = newsLabelWidth;
               }

               newsInfoPanel.Width = W;
               newsInfoPanel.Height = buttonHeight; 

               // reset number of buttons per page
               if ((H / buttonHeight) - 2 < newsList.Count)
               {
                   numberOfButtonsPerPage = (H / buttonHeight) - 4;
               }
           }

           panelWidth = W;
           newsInfoPanel.Width = W;
           newsInfoPanel.Height = buttonHeight;

           contentAdder(currentSide); 

       }

       private void inilizeComponents()
       {

           newsInfoPanel = new TableLayoutPanel();
           
           Button NewsNameButton = new Button();
           NewsNameButton.Text = "Symbol";
           NewsNameButton.TextAlign = ContentAlignment.MiddleCenter;
           NewsNameButton.Font = mw.Font;
           NewsNameButton.BackColor = c.mercuryGrey;
           NewsNameButton.ForeColor = Color.White;
           NewsNameButton.Height = 35;
           NewsNameButton.TabStop = false;
           NewsNameButton.Margin = new Padding(0);
           NewsNameButton.FlatStyle = FlatStyle.Flat;
           NewsNameButton.Click += (sender, e) => { newsInfoButton_clicked(sender, e, "symbol"); };

           Button NewsTitleButton = new Button();
           NewsTitleButton.Text = "Title";
           NewsTitleButton.TextAlign = ContentAlignment.MiddleCenter;
           NewsTitleButton.Font = mw.Font;
           NewsTitleButton.BackColor = c.mercuryGrey;
           NewsTitleButton.ForeColor = Color.White;
           NewsTitleButton.Height = 35;
           NewsTitleButton.TabStop = false;
           NewsTitleButton.Margin = new Padding(0);
           NewsTitleButton.FlatStyle = FlatStyle.Flat;
           NewsTitleButton.Click += (sender, e) => { newsInfoButton_clicked(sender, e, "title"); };

           Button NewsUpdatedButton = new Button();
           NewsUpdatedButton.Text = "Updated";
           NewsUpdatedButton.TextAlign = ContentAlignment.MiddleCenter;
           NewsUpdatedButton.Font = mw.Font;
           NewsUpdatedButton.BackColor = c.mercuryGrey;
           NewsUpdatedButton.ForeColor = Color.White;
           NewsUpdatedButton.Height = 35;
           NewsUpdatedButton.TabStop = false;
           NewsUpdatedButton.Margin = new Padding(0);
           NewsUpdatedButton.FlatStyle = FlatStyle.Flat;
           NewsUpdatedButton.Click += (sender, e) => { newsInfoButton_clicked(sender, e, "updated"); };

           newsInfoPanel.Controls.Add(NewsNameButton, 1, 1);
           newsInfoPanel.Controls.Add(NewsTitleButton, 2, 1);
           newsInfoPanel.Controls.Add(NewsUpdatedButton, 3, 1);

           newsInfoButtons = new Button[3]; 

           // add the labels to the array
           newsInfoButtons[0] = NewsUpdatedButton;
           newsInfoButtons[1] = NewsTitleButton;
           newsInfoButtons[2] = NewsNameButton;
    
           // create the infoLabel for stocks list
           stockInfoPanel = new TableLayoutPanel();

           stockInfoButtons = new Button[6];

           Button SymbolButton = new Button();
           SymbolButton.Text = "Symbol";
           SymbolButton.TextAlign = ContentAlignment.MiddleCenter;
           SymbolButton.Font = mw.Font;
           SymbolButton.BackColor = c.mercuryGrey;
           SymbolButton.ForeColor = Color.White;
           SymbolButton.Height = 35;
           SymbolButton.TabStop = false;
           SymbolButton.Margin = new Padding(0);
           SymbolButton.FlatStyle = FlatStyle.Flat;

           SymbolButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, SymbolButton.Text); };
           SymbolButton.FlatAppearance.MouseOverBackColor = Color.Black;
           SymbolButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           Button NameButton = new Button();
           NameButton.Text = "Name";
           NameButton.TextAlign = ContentAlignment.MiddleCenter;
           NameButton.Font = mw.Font;
           NameButton.BackColor = c.mercuryGrey;
           NameButton.ForeColor = Color.White;
           NameButton.Height = 35;
           NameButton.TabStop = false;
           NameButton.Margin = new Padding(0);
           NameButton.FlatStyle = FlatStyle.Flat;

           NameButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, NameButton.Text); };
           NameButton.FlatAppearance.MouseOverBackColor = Color.Black;
           NameButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           Button LatestButton = new Button();
           LatestButton.Text = "Latest";
           LatestButton.TextAlign = ContentAlignment.MiddleCenter;
           LatestButton.Font = mw.Font;
           LatestButton.BackColor = c.mercuryGrey;
           LatestButton.ForeColor = Color.White;
           LatestButton.Height = 35;
           LatestButton.TabStop = false;
           LatestButton.Margin = new Padding(0);
           LatestButton.FlatStyle = FlatStyle.Flat;

           LatestButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, LatestButton.Text); };
           LatestButton.FlatAppearance.MouseOverBackColor = Color.Black;
           LatestButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           Button ChangeButton = new Button();
           ChangeButton.Text = "Change";
           ChangeButton.TextAlign = ContentAlignment.MiddleCenter;
           ChangeButton.Font = mw.Font;
           ChangeButton.BackColor = c.mercuryGrey;
           ChangeButton.ForeColor = Color.White;
           ChangeButton.Height = 35;
           ChangeButton.TabStop = false;
           ChangeButton.Margin = new Padding(0);
           ChangeButton.FlatStyle = FlatStyle.Flat;

           ChangeButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, ChangeButton.Text); };
           ChangeButton.FlatAppearance.MouseOverBackColor = Color.Black;
           ChangeButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           Button PercentButton = new Button();
           PercentButton.Text = "Percent";
           PercentButton.TextAlign = ContentAlignment.MiddleCenter;
           PercentButton.Font = mw.Font;
           PercentButton.BackColor = c.mercuryGrey;
           PercentButton.ForeColor = Color.White;
           PercentButton.Height = 35;
           PercentButton.TabStop = false;
           PercentButton.Margin = new Padding(0);
           PercentButton.FlatStyle = FlatStyle.Flat;

           PercentButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, PercentButton.Text); };
           PercentButton.FlatAppearance.MouseOverBackColor = Color.Black;
           PercentButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           Button VolumeButton = new Button();
           VolumeButton.Text = "Volume";
           VolumeButton.TextAlign = ContentAlignment.MiddleCenter;
           VolumeButton.Font = mw.Font;
           VolumeButton.BackColor = c.mercuryGrey;
           VolumeButton.ForeColor = Color.White;
           VolumeButton.Height = 35;
           VolumeButton.TabStop = false;
           VolumeButton.Margin = new Padding(0);
           VolumeButton.FlatStyle = FlatStyle.Flat;

           VolumeButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, VolumeButton.Text); };
           VolumeButton.FlatAppearance.MouseOverBackColor = Color.Black;
           VolumeButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           Button OpenValueButton = new Button();
           OpenValueButton.Text = "Opening";
           OpenValueButton.TextAlign = ContentAlignment.MiddleCenter;
           OpenValueButton.Font = mw.Font;
           OpenValueButton.BackColor = c.mercuryGrey;
           OpenValueButton.ForeColor = Color.White;
           OpenValueButton.Height = 35;
           OpenValueButton.TabStop = false;
           OpenValueButton.Margin = new Padding(0);
           OpenValueButton.FlatStyle = FlatStyle.Flat;

           OpenValueButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "OpenVal"); };
           OpenValueButton.FlatAppearance.MouseOverBackColor = Color.Black;
           OpenValueButton.FlatAppearance.MouseDownBackColor = c.mercuryRed;

           // add the labels to the panel
           stockInfoPanel.Controls.Add(SymbolButton, 1, 1);
           stockInfoPanel.Controls.Add(NameButton, 3, 1);
           stockInfoPanel.Controls.Add(LatestButton, 5, 1);
           stockInfoPanel.Controls.Add(ChangeButton, 7, 1);
           stockInfoPanel.Controls.Add(PercentButton, 9, 1);
           stockInfoPanel.Controls.Add(OpenValueButton, 13, 1);

           // add the labels to the array
           stockInfoButtons[0] = SymbolButton;
           stockInfoButtons[1] = NameButton;
           stockInfoButtons[2] = LatestButton;
           stockInfoButtons[3] = ChangeButton;
           stockInfoButtons[4] = PercentButton;
           stockInfoButtons[5] = OpenValueButton;



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
    }
}
