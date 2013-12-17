using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    class NewsReader :  Panel
    {

        private News n; // The current news instance displayed
        private Controller c; 

        private int panelHeight; 
        private int panelWidth;

        private RichTextBox titleRichTextBox; 
        private RichTextBox descritionRichTextBox;
        private Label linkLabel;
        private Label line;
        private Label filler;
        private Boolean fromNewsList; 
        private FlowLayoutPanel panel; 

        public NewsReader(News news, Boolean b)
        {
            n = news;
            c = new Controller();
            fromNewsList = b; 

            this.BackColor = Color.White; 
            // initialize components
            InitializeComponents();
 
        }

        private void InitializeComponents()
        {

            titleRichTextBox = createTextBox(n.title);
            titleRichTextBox.SelectAll();
            titleRichTextBox.SelectionFont = new Font("Segoe UI", 16, FontStyle.Regular);
            titleRichTextBox.Height = 35; 

            descritionRichTextBox = createTextBox("\n" + n.description);
            descritionRichTextBox.SelectAll();
            descritionRichTextBox.SelectionFont = c.mercuryFont; 
            descritionRichTextBox.Height = 225;

            linkLabel = new Label();
            linkLabel.Text = "Read the whole article here";
            linkLabel.BackColor = Color.White; 
            linkLabel.TextAlign = ContentAlignment.MiddleCenter;
            linkLabel.ForeColor = Color.Blue; 
            linkLabel.Width = panelWidth;
            linkLabel.Height = 35;
            linkLabel.Font = c.mercuryFont;
            linkLabel.Click += (sender, e) => { linkLabel_clicked(sender, e); };

            line = new Label();
            line.Width = panelWidth - ((panelWidth / 10) * 2);
            line.Height = 1; 
            line.BackColor = Color.Black;
            line.Anchor = AnchorStyles.None;
            line.Left = (panelWidth - panelWidth) / 2;

            filler = new Label();
            filler.Width = (panelWidth - (line.Width)) / 2;

            panel = new FlowLayoutPanel();
            panel.Width = panelWidth - 1;
            panel.Height = 150 + descritionRichTextBox.Height;
            panel.BackColor = Color.White; 
            panel.Anchor = AnchorStyles.None;
            panel.BorderStyle = System.Windows.Forms.BorderStyle.None;
            panel.AutoScroll = false;
            if (fromNewsList)
            {
                panel.Top = 60;
                panel.Left = (panelWidth - panelWidth) / 2; 
            }
           
            panel.Controls.Add(titleRichTextBox);
            if (n.description != "")
            {
                panel.Controls.Add(descritionRichTextBox);
            }
            panel.Controls.Add(linkLabel);
            panel.Controls.Add(filler); 
            panel.Controls.Add(line); 
        
        }
        private RichTextBox createTextBox(String text)
        {
            RichTextBox rtb = new RichTextBox();
            rtb.Text = text; 
            rtb.SelectAll();
            rtb.SelectionAlignment = HorizontalAlignment.Center;
            rtb.BackColor = Color.White;
            rtb.Width = panelWidth - 10;
            rtb.Top = 0;
            rtb.BorderStyle = BorderStyle.None;
            rtb.ReadOnly = true;
            rtb.WordWrap = true;
            rtb.ScrollBars = RichTextBoxScrollBars.None;

            return rtb; 

        }

        private void linkLabel_clicked(object sender, EventArgs e)
        {
            try
            {
                System.Diagnostics.Process.Start(n.link);
                linkLabel.ForeColor = Color.Purple; 
            }
            catch(Exception ee) { }
        }


        public void setSize(int W, int H)
        {
            
            panelHeight = H;
            panelWidth = W-10;

            this.Width = panelWidth;
            this.Height = panelHeight;

            this.Controls.Clear(); 

            InitializeComponents();

            this.Controls.Add(panel);             

        }
    }
}
