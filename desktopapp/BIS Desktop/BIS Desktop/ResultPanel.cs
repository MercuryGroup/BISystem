﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace BIS_Desktop
{
    
    public class ResultPanel : Panel
    {
        private bool loading;
        private Panel content;
        public ResultPanel()
        {
            loading = false;
            Dock = DockStyle.Fill;

            content = new Panel();
            content.Dock = DockStyle.Fill;
            
            Controls.Add(content);
        }
        public void setLoading(bool loading_)
        {
            loading = loading_;
        }
        public bool getLoading()
        {
            return loading;
        }
        public void updateSize()
        {
            if (content is ResultList)
            {
                ResultList temp = content as ResultList;
                temp.setSize(this.Width, this.Height);
                Console.WriteLine("List");
            }
            
            
            Console.WriteLine(content.Width + " " + this.Width);
            
            
        }
        public Panel getContent()
        {
            return content;
        }
        public void setContent(Panel content_)
        {
            content = content_;
        }
    }
    
}