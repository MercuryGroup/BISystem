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
        public void fetchResult(Object sender, String resultType, String market)
        {
            t = new Thread(() => fetchResultThread(sender, resultType, market));
            t.Start();
        }
        private void fetchResultThread(Object sender, String resultType, String market)
        {
            ResultPanel panel = sender as ResultPanel;
            if (resultType == "stocks")
            {
                ResultList list = new ResultList(resultType, market);
                panel.Invoke((MethodInvoker) (() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(list);
                panel.updateSize();
                panel.Invoke((MethodInvoker) (() => panel.Controls.Add(list)));
            }
            else if (resultType == "info")
            {
                InfoDisplay info = new InfoDisplay();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Clear()));
                panel.Controls.Clear();
                panel.setContent(info);
                panel.updateSize();
                panel.Invoke((MethodInvoker)(() => panel.Controls.Add(info)));

            }
            
            panel.setLoading(false);
        }
    }
}
