using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Threading;

namespace BIS_Desktop
{
    class ThreadHandler
    {
        Thread t;
        public void fetchResult(Object sender, String resultType, String source, String Market, object mainWindow)
        {
            t = new Thread(() => fetchResultThread(sender, resultType, source, null, Market, mainWindow));
            t.Start();
        }

        public void fetchNewsResult(Object sender, String resultType, object mainWindow, News n)
        {
            t = new Thread(() => fetchResultThread(sender, resultType, "", n, "", mainWindow));
            t.Start();
        }

        private void fetchResultThread(Object sender, String resultType, String source, News n, String Market, object mainWindow)
        {
            Console.WriteLine(resultType);        
            ResultPanel panel = sender as ResultPanel;
            if (resultType == "stocks")
            {
               // try
                //{
                    ResultList list = new ResultList(resultType, source, mainWindow);
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                    panel.Controls.Clear();
                    panel.setContent(list);
                    panel.updateSize();
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Add(list)));
                //}
                //catch(Exception e)
               // {
                //    Console.WriteLine("Exception in ThreadHandler: " + e.Data);
                //}
             
            }
            else if (resultType == "portfolio")
            {
                // try
                //{
                ResultList list = new ResultList(resultType, source, mainWindow);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(list);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(list)));
                //}
                //catch(Exception e)
                // {
                //    Console.WriteLine("Exception in ThreadHandler: " + e.Data);
                //}

            }

            else if (resultType == "stockinfo")
            {
              //  try
               // {
                    //InfoDisplay info = new InfoDisplay(market, mw);
                    InfoDisplay info = new InfoDisplay("stock", source, Market);
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                    panel.Controls.Clear();
                    panel.setContent(info);
                    panel.updateSize();
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Add(info)));
                //}
                //catch (Exception e)
                //{
                 //   Console.WriteLine("Exception in ThreadHandler: " + e.Data);
                //}
                
            }
            else if (resultType == "marketinfo")
            {
                //  try
                // {
                //InfoDisplay info = new InfoDisplay(market, mw);
                InfoDisplay info = new InfoDisplay("market", source, Market);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(info);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(info)));
                //}
                //catch (Exception e)
                //{
                //   Console.WriteLine("Exception in ThreadHandler: " + e.Data);
                //}

            }

            else if (resultType == "news")
            {
                //  try
                // {
                ResultList newsDisplay = new ResultList(resultType, source, mainWindow);
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(newsDisplay);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(newsDisplay)));
                //}
                //catch (Exception e)
                //{
                //   Console.WriteLine("Exception in ThreadHandler: " + e.Data);
                //}

            }


            else if (resultType == "newsReader")
            {
                //  try
                // {

                    NewsPanel newsPanel = new NewsPanel(n);
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                    panel.Controls.Clear();
                    panel.setContent(newsPanel);
                    panel.updateSize();
                    panel.Invoke((MethodInvoker)(() => panel.Controls.Add(newsPanel)));

                
                //}
                //catch (Exception e)
                //{
                //   Console.WriteLine("Exception in ThreadHandler: " + e.Data);
                //}

            }

            panel.setLoading(false);
        }
    }
}
