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
        public void fetchResult(Object sender, String resultType, String source)
        {
            ResultPanel panel = sender as ResultPanel;
            if (resultType == "list")
            {
                ResultList list = new ResultList("stocks", "lse");
                panel.Controls.Clear();
                
                panel.setContent(list);
                panel.updateSize();
                panel.Controls.Add(list);
                
            }
            else if (resultType == "info")
            {
                panel.Controls.Clear();
                
            }
            panel.setLoading(false);
            
        }
    }
}
