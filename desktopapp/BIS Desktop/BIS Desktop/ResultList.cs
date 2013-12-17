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
        private int numberOfNumLabel;// holds the number of number labels on the page, used for calculation the with of the nextPreviousPanel
        private int listItemClicked = -1; // number of the list buttons currently selected
        // Boolean for controlling the infoButtons sort functions, ie sort decendeing or acending
        private Boolean SymbolInfoClicked = false;
        private Boolean NameInfoClicked = false;
        private Boolean LatestInfoClicked = false;
        private Boolean ChangeInfoClicked = false;
        private Boolean PercentInfoClicked = false;
        private Boolean OpenValInfoClicked = false;
        private Boolean labelClicked = false; // boolean for controling the colors of the infoLabels when clicked
        private Boolean listsEmpty = false;  // boolean for checking if the lists are empty
        private MercuryButton[] stockInfoButtons; // array of stock info buttons
        private MercuryButton[] newsInfoButtons; // array of news info buttons
        private Label[,] listLabels; // 2d Array of stockLabels
        private FlowLayoutPanel[] listPanels; // array for stockPanels with labels
        private FlowLayoutPanel nextPreviousPanel; // panel for holding the the list traverser 
        private TableLayoutPanel stockInfoPanel; // Panel with labels, with info for the stock list
        private TableLayoutPanel newsInfoPanel; // panel with labels, with info for the news list
        // Labels for traversing the list of buttons
        private Label filler; 
        private Label first;
        private Label last; 
        private Label next;
        private Label previous;
        private String DataButtonClicked; // variable for determening which data we are listing
        private List<Stock> allStocksList; // list with ALL stocks from jsonhandler
        private List<Stock> filteredStockList; // list with the stocks filtered after market
        private List<News> allNewsList; // list with ALL stocks from jsonhandler
        private List<News> newsList; // list with ALL news from jsonhandler
        // classes
        private MainWindow mw;
        private Controller c;
        private JsonHandler jh;
        /// <summary>
        /// resultList loads a list with content depending on what button is pressed, i.e. market, stocks, news, portfolio or search
        /// </summary>
        /// <param name="Data"></param>
        /// <param name="Market"></param>
        /// <param name="o"></param>
        public ResultList(String Data, String Market, object o){
            // initilize classes
            mw = o as MainWindow;
            jh = new JsonHandler();
            c = new Controller(); 
            //this.Controls.Add()
            DataButtonClicked = Data;
            // set background color
            this.BackColor = c.highlightWhite;          
            // initilize all components except the list
            inilizeComponents();
            // check databutton and prodcede accordingly
            switch (DataButtonClicked)
            {
                case "market":
                    // if market is clicked we load all news and filter for selected market
                    allNewsList = jh.getAllNews();
                    newsList = c.filterNews(allNewsList, Market);
                    if (newsList.Count == 0)
                    {
                        listsEmpty = true; 
                    }
                    break;
                case "stocks":
                    // if stocks is clicked we load all stocks and filter for selected market
                    allStocksList = jh.getAllStocks();
                    filteredStockList = c.filterStocks(allStocksList, Market); 
                    if (filteredStockList.Count == 0)
                    {
                        listsEmpty = true;
                    }
                    break;
                case "news":
                    // if news is clicked we load all news
                    allNewsList = jh.getAllNews();
                    newsList = c.filterNews(allNewsList, Market);
                    if (newsList.Count == 0)
                    {
                        listsEmpty = true;
                    }
                    break;
                case "portfolio":
                    // if portfolio is clicked we load from the portfolio file
                    filteredStockList = c.loadFromPortfolio();
                    if (filteredStockList.Count == 0)
                    {
                        listsEmpty = true;
                    }
                    break; 
                case "search":
                    // if a search inputed we load all stocks and search them for matches and load those
                    allStocksList = jh.getAllStocks();
                    filteredStockList = c.search(allStocksList, Market);                  
                    if (filteredStockList.Count == 0)
                    {
                        listsEmpty = true;
                    }
                    break; 
            }
            // if the list isn´t empty we procede
            if (!listsEmpty)
            {
                currentSide = 1;
                contentAdder(currentSide);
            }           
        }
        /// <summary>
        /// Method for calculating the maximum number of pages, depending on list size.
        /// </summary>
        private void calcMaxNumberOfPages()
        {
            // Again, we check DataButtonClicked
            if (DataButtonClicked == "news" || DataButtonClicked == "market")
            {
                // if the length of the list modulus the number of buttons per page equals 0
                if (newsList.Count % numberOfButtonsPerPage == 0)
                {
                    // the max number of pages = the length of the list divided by number buttons per page
                    maxNumberOfPages = newsList.Count / numberOfButtonsPerPage;
                }
                else
                {
                    // else, the max number of pages equals the length of the list divided by number buttons per page plus one
                    maxNumberOfPages = (newsList.Count / numberOfButtonsPerPage) + 1;
                }
            }
            else if(DataButtonClicked == "stocks" || DataButtonClicked == "portfolio" || DataButtonClicked == "search")
            {
                if (filteredStockList.Count % numberOfButtonsPerPage == 0)
                {
                    maxNumberOfPages = filteredStockList.Count / numberOfButtonsPerPage;
                }
                else
                {
                    maxNumberOfPages = (filteredStockList.Count / numberOfButtonsPerPage) + 1;
                }
            }           
        }
        /// <summary>
        /// Method for setting stepper buttons enableb true or false
        /// </summary>
        private  void setSideStepperButtons()
        {
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
        }
        /// <summary>
        /// Method for calculating start and stop of variables used for traversing the list
        /// </summary>
        /// <param name="sizeOfList"></param>
        /// <returns>returns array with two values, 0 == start and 1 == stop</returns>
        public int[] calcStartStop(int sizeOfList) 
        {
            int Start = 0;
            int Stop = 0; 
            // if the side times the number of buttons per page is less or equal to the size of the list
            if (currentSide * numberOfButtonsPerPage <= sizeOfList)
            {
                // stop equals side times number of buttons per page
                Stop = currentSide * numberOfButtonsPerPage;
                // since stop is the side * numberOfButtons per side we set start to stop minus number of buttons per side. 
                Start = Stop - numberOfButtonsPerPage;
            }
            else
            {
                // else stop is equal to the count of the list
                Stop = sizeOfList;
                // since stop here is less than numberOfButtonsPerPage than start is equal to the size of the news list modulus the maxNumberOfPages
                Start = Stop - (sizeOfList % numberOfButtonsPerPage);
            }
            int[] array = { Start, Stop };
            return array; 
        }
        /// <summary>
        /// Method for updating the panels content
        /// </summary>
        /// <param name="side"></param>
        private void contentAdder(int side)
        {
            // calculate max number of pages
            calcMaxNumberOfPages();
            // if we were standing on the last page when maximizing the window side can be larger than max number of pages
            if (side > maxNumberOfPages)
            {
                side = maxNumberOfPages;
            }
            // set current side to side
            currentSide = side;
            // clear the panel
            this.Controls.Clear();
            if (DataButtonClicked == "news" || DataButtonClicked == "market")
            {
                // calculate start and stop of the list
                int[] temp = calcStartStop(newsList.Count);
                int Start = temp[0];
                int Stop = temp[1]; 
                // enable stepper buttons
                setSideStepperButtons();
                // initilize buttons
                initilizeNewsList(Start, Stop);
                // add buttons
                foreach (Panel p in listPanels)
                {
                    this.Controls.Add(p);
                }
                // reset number buttons
                initilizeNumberButtons(currentSide);
                // add panel
                this.Controls.Add(nextPreviousPanel);
            }
            else if (DataButtonClicked == "stocks" || DataButtonClicked == "portfolio" || DataButtonClicked == "search")
            {
                // calculate start and stop of the list
                int[] temp = calcStartStop(filteredStockList.Count);
                int Start = temp[0];
                int Stop = temp[1]; 
                // enable stepper buttons
                setSideStepperButtons();
                // initilize buttons
                initilizeStockList(Start, Stop);
                // add buttons
                foreach (Panel p in listPanels)
                {
                    this.Controls.Add(p);
                }
                // reset number buttons
                initilizeNumberButtons(currentSide);
                // add panel
                this.Controls.Add(nextPreviousPanel);
            }
        }
        /// <summary>
        /// Method for initilizing the stock list
        /// </summary>
        /// <param name="start"></param>
        /// <param name="stop"></param>       
        private void initilizeStockList(int start, int stop)
        {
            // add the info labels
            this.Controls.Add(stockInfoPanel);
            // create btns array
            listLabels = new Label[stop - start, 6];          
            // int for keeping check of labels
            int j = 0;
            // create new "stock buttons" fill the array
            for (int i = start; i < stop; i++)
            { 
                // create labels
                Stock s = filteredStockList[i];
                Label symbolLabel = createStockLabel(s.Symbol, s, j);
                Label nameLabel = createStockLabel(s.Name, s, j);
                Label latestLabel = createStockLabel(s.Latest, s, j);
                latestLabel.Width = stockLabelWidth - 1;
                Label changeLabel = createStockLabel(s.Change, s, j);
                changeLabel.Width = stockLabelWidth - 1;
                if (listItemClicked == j)
                {
                    changeLabel.ForeColor = Color.White;
                }
                else
                {
                    // set colors on the change label to green or red depending if the value is + or -
                    if (changeLabel.Text[0] == '-')
                    {
                        changeLabel.ForeColor = Color.Red;
                    }
                    else if (changeLabel.Text[0] == '+')
                    {
                        changeLabel.ForeColor = Color.Green;
                    }
                }
                Label percentLabel = createStockLabel(s.Percent, s, j);
                percentLabel.Width = stockLabelWidth - 1;
                // set colors on the percent label to green or red depending if the value is + or -
                if (listItemClicked == j)
                {
                    percentLabel.ForeColor = Color.White;
                }
                else
                {
                    // set colors on the change label to green or red depending if the value is + or -
                    if (percentLabel.Text[0] == '-')
                    {
                        percentLabel.ForeColor = Color.Red;
                    }
                    else if (percentLabel.Text[0] == '+')
                    {
                        percentLabel.ForeColor = Color.Green;
                    }
                }
                Label openLabel = createStockLabel(s.OpenVal, s, j);
                openLabel.Width = stockLabelWidth - 1;
                // add the labels to the labels 2d array
                listLabels[j, 0] = symbolLabel;
                listLabels[j, 1] = nameLabel;
                listLabels[j, 2] = latestLabel;
                listLabels[j, 3] = changeLabel;
                listLabels[j, 4] = percentLabel;
                listLabels[j, 5] = openLabel;
                //increase j by one
                j++; 
            }
            //initilize listPanels array
            listPanels = new FlowLayoutPanel[numberOfButtonsPerPage];

            for (int i = 0; i < listLabels.GetLength(0); i++)
            {
                // loop columns and create a new panel each time
                FlowLayoutPanel stockPanel = new FlowLayoutPanel();
                stockPanel.FlowDirection = System.Windows.Forms.FlowDirection.LeftToRight;
                stockPanel.Height = buttonHeight;
                stockPanel.BackColor = Color.WhiteSmoke; 
                stockPanel.Width = panelWidth -3;
                for (int k = 0; k < listLabels.GetLength(1); k++)
                {
                    // loop rows and add the labels of the 2d array
                    stockPanel.Controls.Add(listLabels[i, k]);
                }
                // add the panels to the array of panels
                listPanels[i] = stockPanel; 
            }
        }
        /// <summary>
        /// Method for initilizing the news list
        /// </summary>
        /// <param name="start"></param>
        /// <param name="stop"></param>
        private void initilizeNewsList(int start, int stop)
        {
            // add the news info labels
            this.Controls.Add(newsInfoPanel); 
            // create labels array
            listLabels = new Label[stop - start, 3];
            // int for keeping check of labels
            int j = 0;
            // create new "news buttons" fill the array 
            for (int i = start; i < stop; i++)
            {     
               News n = newsList[i];
               // create label
               Label symbolLabel = createNewsLabel(n.symbol, n, j);
               Label titleLabel = createNewsLabel(n.title, n, j);
               Label updatedLabel = createNewsLabel(c.getDate(n.pubDate).ToString(), n, j);
               // add the labels to 2d array of labels
               listLabels[j, 0] = symbolLabel;
               listLabels[j, 1] = titleLabel;
               listLabels[j, 2] = updatedLabel;
               // increase j by one
               j++; 
            }
            //initilize listPanels array 
            listPanels = new FlowLayoutPanel[numberOfButtonsPerPage];
            for (int i = 0; i < listLabels.GetLength(0); i++)
            {
                // loop columns and create a new panel each time
                FlowLayoutPanel newsPanel = new FlowLayoutPanel();
                newsPanel.FlowDirection = System.Windows.Forms.FlowDirection.LeftToRight;
                newsPanel.Height = buttonHeight;
                newsPanel.Width = panelWidth;
                newsPanel.BackColor = Color.WhiteSmoke;
                for (int k = 0; k < listLabels.GetLength(1); k++)
                {
                    // loop rows and add the labels of the 2d array
                    newsPanel.Controls.Add(listLabels[i, k]);
                }
                // add the panels to the array of panels
                listPanels[i] = newsPanel;
            }  
        }
        /// <summary>
        /// Method for initilizing the numbers in the side stepper function
        /// </summary>
        /// <param name="side"></param>
        private void initilizeNumberButtons(int side)
        {
            // clear the panel
            nextPreviousPanel.Controls.Clear();
            // re add the first and previous labels
            nextPreviousPanel.Controls.Add(filler); 
            nextPreviousPanel.Controls.Add(first);
            nextPreviousPanel.Controls.Add(previous);
            // ints that will be assigned the values of which number labels to build
            int numberStop;
            int numberStart;
            // if max is less than 10
            if(maxNumberOfPages < 10)
            {
                // then start is equal to one and stop equal to the number of pages plus one
                numberStart = 1;
                numberStop = maxNumberOfPages + 1;              
            }               
            // if the side is more than or equal to the max number of pages minus 10, 10 beeing the number of numberlabels we display. 
            else if (side >= maxNumberOfPages - 10)
            {
                // numberStart is equal to maxNumberOfpages minus 9
                numberStart = maxNumberOfPages - 9;
                // and numberStop is equal to maxnumberOfPages plus 1
                numberStop = maxNumberOfPages + 1;
            }
            else
            {
                // else everything is normal and we increase both start and stop
                numberStop = side + 10;
                numberStart = side;
            }
            numberOfNumLabel = numberStop; 
            // loop the number labels intervall
            for (int i = numberStart; i < numberStop; i++)
            {
                // create and add the labels
                Label number = new Label();
                string text = i.ToString();
                number.Text = text;
                number.Font = new Font("Segoe UI", 9, FontStyle.Regular);
                number.Size = new System.Drawing.Size(33, 20);
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
            // add next and last to the panel
            nextPreviousPanel.Controls.Add(next);
            nextPreviousPanel.Controls.Add(last);         
        }
        /// <summary>
        /// Method for resetting the color of the info buttons after they have been clicked
        /// </summary>
        /// <param name="infoButtons"></param>
        private void setInfoColors(Button[] infoButtons)
        {
            foreach (Button l in infoButtons)
            {
                // foreach buttons set the standard colors
                l.BackColor = c.mercuryGrey;
                l.ForeColor = Color.White;
            }        
        }
        /// <summary>
        /// Method for resetting the color of the list labels after they have been clicked
        /// </summary>
        private void setListColors()
        {
            //loop columns
            for (int i = 0; i < listLabels.GetLength(0); i++)
            {
               // loop rows
               for (int k = 0; k < listLabels.GetLength(1); k++)
                {
                    //set colors
                    if ((k == 3 && listLabels[i, k].Text[0] == '-') || (k == 4 && listLabels[i, k].Text[0] == '-'))
                    {
                        listLabels[i, k].BackColor = Color.FromArgb(70, c.mercuryGrey);
                        listLabels[i, k].ForeColor = Color.Red; 
                    }
                    else if ((k == 3 && listLabels[i, k].Text[0] == '+') || (k == 4 && listLabels[i, k].Text[0] == '+'))
                    {
                        listLabels[i, k].BackColor = Color.FromArgb(70, c.mercuryGrey);
                        listLabels[i, k].ForeColor = Color.Green; 
                    }
                    else
                    {
                        listLabels[i, k].BackColor = Color.FromArgb(70, c.mercuryGrey);
                        listLabels[i, k].ForeColor = Color.Black; 
                    } 
                }
            }  
        }
        /// <summary>
        /// Method triggered when mouse enters any of the list items
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="j"></param>
        private void highlightList_MouseEnter(object sender, System.EventArgs e, int j)
        {
            // check if its not allready clicked
            if (listItemClicked != j)
            {
                // loop rows
                for (int k = 0; k < listLabels.GetLength(1); k++)
                {
                    // set colors
                    listLabels[j, k].BackColor = Color.FromArgb(100, c.mercuryGrey);
                }
            }     
        }
        /// <summary>
        /// Method triggered when mouse leaves any of the list items
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="j"></param>
        private void highlightList_MouseLeave(object sender, System.EventArgs e, int j)
        {
            // check if its not allready clicked
            if (listItemClicked != j)
            {
                // loop rows
                for (int k = 0; k < listLabels.GetLength(1); k++)
                {
                    // set colors
                    listLabels[j, k].BackColor = Color.FromArgb(70, c.mercuryGrey);
                }
            }     
        }
        /// <summary>
        /// Method triggered when mouse enters any of the labels of the side stepper
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void highlightNum_MouseEnter(object sender, System.EventArgs e)
        {
            // get label and set font size 10
            Label L = sender as Label;
            L.Font = new Font("Segoe UI", 10, FontStyle.Regular);
        }
        /// <summary>
        /// Method triggered when mouse leaves any of the labels of the side stepper
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void highlightNum_MouseLeave(object sender, System.EventArgs e)
        {
            // get label and set font size 9
            Label L = sender as Label;
            L.Font = new Font("Segoe UI", 9, FontStyle.Regular);     
        }
        /// <summary>
        /// Method triggered when a number label in the side stepper is clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="i"></param>
        private void numberLabel_clicked(object sender, System.EventArgs e, String i)
        {
            // parse the string to int and reload the panel
            try
            {
                var num = int.Parse(i);
                contentAdder(num);
            }
            catch (Exception ee) { }           
        }
        /// <summary>
        /// Method triggered when a last label in the side stepper is clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void last_clicked(object sender, System.EventArgs e)
        {
            // go to last side
            contentAdder(maxNumberOfPages); 
        }
        /// <summary>
        /// Method triggered when a first label in the side stepper is clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void first_clicked(object sender, System.EventArgs e)
        {
            // go to first side
            contentAdder(1);
        }
        /// <summary>
        /// Method triggered when a previous label in the side stepper is clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void previous_clicked(object sender, System.EventArgs e)
        {
            // increse side by 1
            contentAdder(currentSide - 1);
        }
        /// <summary>
        /// Method triggered when a next label in the side stepper is clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void next_clicked(object sender, System.EventArgs e)
        {
            // decrese side by 1
            contentAdder(currentSide + 1);
        }
        /// <summary>
        /// Method triggered when a news info button is clicked 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="sortAfter"></param>
        private void newsInfoButton_clicked(object sender, System.EventArgs e, String sortAfter)
        {
            Button b = sender as Button;
            // if any button is allready clicked we have to reset every other buttons before we set the "clicked colors"
            if (labelClicked == true)
            {
                setInfoColors(newsInfoButtons);
                b.BackColor = c.mercuryBlue;
                b.ForeColor = Color.White;
                labelClicked = true;
            }
            // if no buttons is previously clicked we just change the colors
            else
            {
                b.BackColor = c.mercuryBlue;
                b.ForeColor = Color.White;
                labelClicked = true;
            }
            // check which button was clicked and sort accordingly
            var temp = b.Text;
            switch (temp)
            {
                case "Symbol":
                   if (SymbolInfoClicked)
                   {
                       newsList = c.sortNewsList(newsList, sortAfter, true);
                       SymbolInfoClicked = false;
                   }
                   else
                   {
                       newsList = c.sortNewsList(newsList, sortAfter, false);
                       SymbolInfoClicked = true;
                   }
                   break; 
               case "Title":
                   if (NameInfoClicked)
                   {
                       newsList = c.sortNewsList(newsList, sortAfter, true);
                       NameInfoClicked = false;
                   }
                   else
                   {
                       newsList = c.sortNewsList(newsList, sortAfter, false);
                       NameInfoClicked = true;
                   }
                   break;
               case "Published":
                   if (LatestInfoClicked)
                   {
                       newsList = c.sortNewsList(newsList, sortAfter, true);
                       LatestInfoClicked = false;
                   }
                   else
                   {
                       newsList = c.sortNewsList(newsList, sortAfter, false);
                       LatestInfoClicked = true;
                   }
                   break; 
            }
            // reload the panel
            contentAdder(1);
        }
        /// <summary>
        /// Method triggered when a stock info button clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="sortAfter"></param>
       private void stockInfoButton_clicked(object sender, System.EventArgs e, String sortAfter)
       {
           Button b = sender as Button;
           // if any button is allready clicked we have to reset every other buttons before we set the "clicked colors"
           if (labelClicked == true)
           {
               setInfoColors(stockInfoButtons);
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               labelClicked = true;
           }
           // if no buttons is previously clicked we just change the colors
           else
           {
               b.BackColor = c.mercuryBlue;
               b.ForeColor = Color.White;
               labelClicked = true;
           }
           // check which button was clicked and sort accordingly
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
               case "Company":
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
               case "Latest €":
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
               case "Change €":
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
               case "Opening €":
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
           // reload the panel
           contentAdder(1);
       }
        /// <summary>
        /// Method triggered when a stock in list clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="s"></param>
        /// <param name="num"></param>
       private void stock_clicked(object sender, System.EventArgs e, Stock s, int num)
       {
           // set colors
           setListColors(); 
           for (int k = 0; k < listLabels.GetLength(1); k++)
           {
               listLabels[num, k].BackColor = c.mercuryBlue;
               listLabels[num, k].ForeColor = Color.White;
           }
           // load rightPanelResul as resultPanel
           listItemClicked = num;
           ResultPanel temp = mw.rightPanelResults as ResultPanel;
           mw.loadResult(temp, "stockinfo", s.Symbol, s.Market, mw);
       }
        /// <summary>
        /// Method triggered when a news in list clicked
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        /// <param name="n"></param>
        private void news_clicked(object sender, System.EventArgs e, News n, int num)
        {
            // set colors
            setListColors();
            for (int k = 0; k < listLabels.GetLength(1); k++)
            {
                listLabels[num, k].BackColor = c.mercuryBlue;
                listLabels[num, k].ForeColor = Color.White;
            }
            listItemClicked = num;
            // load rightPanelResul as resultPanel
            ResultPanel temp = mw.rightPanelResults as ResultPanel;
            NewsReader newsPanel = new NewsReader(n, true);
            temp.Controls.Clear();
            temp.setContent(newsPanel);
            temp.updateSize();
            temp.Controls.Add(newsPanel);
       }
    /// <summary>
    /// Method that sets the size of components
    /// </summary>
    /// <param name="W"></param>
    /// <param name="H"></param>
       public void setSize(int W, int H)
       {
           // store the sizes
           panelWidth = W;
           panelHeigth = H;
           // reset panel size
           this.Height = H;
           this.Width = W;
           // reset nextPreviousPanel width
           nextPreviousPanel.Width = W;
           // check DataButtonClicked
           if (this.DataButtonClicked == "stocks" || this.DataButtonClicked == "portfolio" || this.DataButtonClicked == "search")
           {
               // the width of stock labels equals W / 6 since we have 6 labels
               stockLabelWidth = W / 6;
               foreach (MercuryButton btn in stockInfoButtons)
               {
                   // set their size -6 to fit the padding
                   btn.Width = stockLabelWidth - 6;            
               }
               // set the info buttons panel width
               stockInfoPanel.Width = W;
               stockInfoPanel.Height = buttonHeight; 
               // reset number of buttons per page
               if ((H / buttonHeight) - 4  < filteredStockList.Count)
               {   
                   numberOfButtonsPerPage = (H / buttonHeight) - 4;
               }            
           }
           // check DataButtonClicked
           else if (this.DataButtonClicked == "news" || this.DataButtonClicked == "market")
           {
               // the width of news labels equals W / 3 since we have 3 labels
               newsLabelWidth = W / 3;
               foreach (MercuryButton btn in newsInfoButtons)
               {
                   // set their size -6 to fit the padding
                    btn.Width = newsLabelWidth - 6;
               }
               // set the info panel width
               newsInfoPanel.Width = W;
               newsInfoPanel.Height = buttonHeight; 
               // reset number of buttons per page
               if ((H / buttonHeight) - 4 < newsList.Count)
               {
                   numberOfButtonsPerPage = (H / buttonHeight) - 4;
               }
           }
           panelWidth = W;
           nextPreviousPanel.Width = panelWidth;
           filler.Width = (panelWidth - (106 + (numberOfNumLabel * 35))) / 2;
           // if the lists aren´t empty we reload the panel
           if (!listsEmpty)
           {
                contentAdder(currentSide); 
           }   
       }
        /// <summary>
        ///  Method tat initilizes the components
        /// </summary>
       private void inilizeComponents()
       {          
           newsInfoPanel = new TableLayoutPanel();
           // create new mercuryButtons for the sorting the list of stocks
           MercuryButton NewsNameButton = new MercuryButton("Symbol", "");
           NewsNameButton.Height = 35;
           NewsNameButton.Click += (sender, e) => { newsInfoButton_clicked(sender, e, "symbol"); };
           MercuryButton NewsTitleButton = new MercuryButton("Title", "");
           NewsTitleButton.Height = 35;
           NewsTitleButton.Click += (sender, e) => { newsInfoButton_clicked(sender, e, "title"); };
           MercuryButton NewsUpdatedButton = new MercuryButton("Published", "");
           NewsUpdatedButton.Height = 35;
           NewsUpdatedButton.Click += (sender, e) => { newsInfoButton_clicked(sender, e, "pubDate"); };
           // add them to panel
           newsInfoPanel.Controls.Add(NewsNameButton, 1, 1);
           newsInfoPanel.Controls.Add(NewsTitleButton, 2, 1);
           newsInfoPanel.Controls.Add(NewsUpdatedButton, 3, 1);
           newsInfoButtons = new MercuryButton[3];   
           // add the labels to the array
           newsInfoButtons[0] = NewsUpdatedButton;
           newsInfoButtons[1] = NewsTitleButton;
           newsInfoButtons[2] = NewsNameButton;
           // create the infoLabel for stocks list
           stockInfoPanel = new TableLayoutPanel();
           stockInfoButtons = new MercuryButton[6];
           // create new mercuryButtons for the sorting the list of stocks
           MercuryButton SymbolButton = new MercuryButton("Symbol", "");
           SymbolButton.Height = 35;
           SymbolButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "Symbol"); };
           MercuryButton NameButton = new MercuryButton("Company", "");  
           NameButton.Height = 35;
           NameButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "Name"); };
           MercuryButton LatestButton = new MercuryButton("Latest €", "");
           LatestButton.Height = 35;
           LatestButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "Latest"); };
           MercuryButton ChangeButton = new MercuryButton("Change €", "");
           ChangeButton.Height = 35;
           ChangeButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "Change"); };
           MercuryButton PercentButton = new MercuryButton("Percent", "");
           PercentButton.Height = 35;
           PercentButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "Percent"); };
           MercuryButton OpenValueButton = new MercuryButton("Opening €", "");
           OpenValueButton.Height = 35;
           OpenValueButton.Click += (sender, e) => { stockInfoButton_clicked(sender, e, "OpenVal"); };
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
           // filler, last, next previous and last labels for traversing the list
           filler = new Label();
           filler.Margin = new Padding(0);
           filler.FlatStyle = FlatStyle.Flat;
           last = createSideStepperButton(">>");
           last.Size = new System.Drawing.Size(33, 20);
           last.Click += (sender, e) => { last_clicked(sender, e); };
           first = createSideStepperButton("<<");
           first.Size = new System.Drawing.Size(33, 20);
           first.Click += (sender, e) => { first_clicked(sender, e); };
           next = createSideStepperButton(">");
           next.Click += (sender, e) => { next_clicked(sender, e); };
           previous = createSideStepperButton("<");
           previous.Click += (sender, e) => { previous_clicked(sender, e); };       
           nextPreviousPanel = new FlowLayoutPanel();
           nextPreviousPanel.Height = 25;
       }       
        /// <summary>
        /// Method for creating news label for the list
        /// </summary>
        /// <param name="text"></param>
        /// <param name="n"></param>
        /// <param name="j"></param>
        /// <returns>news label</returns>
       private Label createNewsLabel(String text, News n, int j)
       {
           Label newsLabel = new Label();
           newsLabel.Text = text;
           newsLabel.TextAlign = ContentAlignment.MiddleCenter;
           newsLabel.Font = c.mercuryFont;
           newsLabel.Height = buttonHeight;
           newsLabel.Width = newsLabelWidth - 2;
           newsLabel.Margin = new Padding(0);
           if (listItemClicked == j)
           {
               newsLabel.BackColor = c.mercuryBlue;
               newsLabel.ForeColor = Color.White;
           }
           else
           {
               newsLabel.BackColor = Color.FromArgb(70, c.mercuryGrey);
               newsLabel.ForeColor = Color.Black;
           }
           newsLabel.Click += (sender, e) => { news_clicked(sender, e, n, j); };
           newsLabel.MouseEnter += (sender, e) => { highlightList_MouseEnter(sender, e, j); };
           newsLabel.MouseLeave += (sender, e) => { highlightList_MouseLeave(sender, e, j); };
           return newsLabel;
       }   

    /// <summary>
    /// Method for creating stocklabels for the list
    /// </summary>
    /// <param name="text"></param>
    /// <param name="s"></param>
    /// <param name="j"></param>
    /// <returns> stock label</returns>
       private Label createStockLabel(String text, Stock s, int j)
       {
           Label stockLabel = new Label();
           stockLabel.Text = text;
           stockLabel.TextAlign = ContentAlignment.MiddleCenter;
           stockLabel.Font = c.mercuryFont;
           stockLabel.Height = buttonHeight;
           stockLabel.Width = stockLabelWidth;
           stockLabel.Margin = new Padding(0);
           if (listItemClicked == j)
           {
               stockLabel.BackColor = c.mercuryBlue;
               stockLabel.ForeColor = Color.White;
           }
           else
           {
               stockLabel.BackColor = Color.FromArgb(70, c.mercuryGrey);
               stockLabel.ForeColor = Color.Black; 
           }
           stockLabel.Click += (sender, e) => { stock_clicked(sender, e, s, j); };
           stockLabel.MouseEnter += (sender, e) => { highlightList_MouseEnter(sender, e, j); };
           stockLabel.MouseLeave += (sender, e) => { highlightList_MouseLeave(sender, e, j); };
           return stockLabel;
       }   
        /// <summary>
        /// Method for creating new labels for the "side stepper" 
        /// </summary>
        /// <param name="text"></param>
        /// <returns>side step label</returns>
        private Label createSideStepperButton(String text){
            Label sideStep = new Label();
            sideStep.Text = text;
            sideStep.Font = new Font("Segoe UI", 9, FontStyle.Regular);
            sideStep.AutoSize = false;
            sideStep.Size = new System.Drawing.Size(20, 20);
            sideStep.FlatStyle = FlatStyle.Flat;
            sideStep.Margin = new Padding(0);
            sideStep.MouseEnter += (sender, e) => { highlightNum_MouseEnter(sender, e); };
            sideStep.MouseLeave += (sender, e) => { highlightNum_MouseLeave(sender, e); };
            return sideStep; 
        }
    }
}
