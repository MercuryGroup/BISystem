using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    class NewsPanel : Panel
    {
        private RichTextBox richTextBox; // rich text box for diplaying all the text

        private News n; // The current news instance displayed

        public NewsPanel(Object o)
        {
            n = o as News; 
  
            // initialize components
            InitializeComponents();


        }
 
        private void InitializeComponents()
        {
            richTextBox = new RichTextBox();
            richTextBox.Text = n.title + "\n" + n.description+"\n"+ n.link; 
        } 


    }
}
