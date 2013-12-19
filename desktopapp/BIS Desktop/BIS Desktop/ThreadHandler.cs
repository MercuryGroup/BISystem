using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Threading;

namespace BIS_Desktop
{
    /// <summary>
    /// Handles all additional threads that loads new results.
    /// </summary>
    class ThreadHandler
    {
        Thread t;//Thread used by thread handler
        /// <summary>
        /// Starts a new threaad that sends back results to a panel.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="resultType"></param>
        /// <param name="source"></param>
        /// <param name="Market"></param>
        /// <param name="mainWindow"></param>
        public void fetchResult(Object sender, String resultType, String source, String Market, object mainWindow)
        {
            if (t != null)
            {
                t.Abort();
            }
            t = new Thread(() => fetchResultThread(sender, resultType, source, null, Market, mainWindow));
            t.Start();
        }

        /// <summary>
        /// Starts a new thread that sends back news results to a panel.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="resultType"></param>
        /// <param name="mainWindow"></param>
        /// <param name="n"></param>
        public void fetchNewsResult(Object sender, String resultType, object mainWindow, News n)
        {
            //Set thread to load new content
            t = new Thread(() => fetchResultThread(sender, resultType, "", n, "", mainWindow));
            t.Start();
        }
        /// <summary>
        /// Method that handles all thread requests.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="resultType"></param>
        /// <param name="source"></param>
        /// <param name="n"></param>
        /// <param name="Market"></param>
        /// <param name="mainWindow"></param>
        private void fetchResultThread(Object sender, String resultType, String source, News n, String Market, object mainWindow)
        {    
            ResultPanel panel = sender as ResultPanel;
            //Return result list of stocks
            if (resultType == "stocks")
            {
                try
                {
                    //Initialize new result list
                    ResultList list = new ResultList(resultType, source, mainWindow);
                    //Invoke panel to clear its contents
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                    panel.Controls.Clear();
                    //Set new contents of panel
                    panel.setContent(list);
                    //Update size of panel
                    panel.updateSize();
                    //Invoke panel to add contents to panel
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Add(list)));
                }
                //Catch all exceptions
                catch(Exception e){}
             
            }
            //Return result list of portfolio items
            else if (resultType == "portfolio")
            {
                try
                {
                ResultList list = new ResultList(resultType, source, mainWindow);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(list);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(list)));
                }
                catch(Exception e){}

            }
            //Return result list of search results
            else if (resultType == "search")
            {
                try
                {
                ResultList list = new ResultList(resultType, source, mainWindow);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(list);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(list)));
                }
                catch(Exception e){}

            }
            //Return info display of selected stock
            else if (resultType == "stockinfo")
            {
                 try
                 {
                 InfoDisplay info = new InfoDisplay("stock", source, Market);
                 panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                 panel.Controls.Clear();
                 panel.setContent(info);
                 panel.updateSize();
                 panel.Invoke((MethodInvoker)(() => panel.Controls.Add(info)));
                 }
                catch (Exception e){}
                
            }
            //Return info display of selected market
            else if (resultType == "marketinfo")
            {
                try
                {
                InfoDisplay info = new InfoDisplay("market", source, Market);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(info);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(info)));
                }
                catch (Exception e){}

            }
            //Return news list
            else if (resultType == "news")
            {
                try
                {
                ResultList newsDisplay = new ResultList(resultType, source, mainWindow);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(newsDisplay);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(newsDisplay)));
                }
                catch (Exception e){}

            }
            //Set loading of panel to false
            panel.setLoading(false);
        }
    }
}
