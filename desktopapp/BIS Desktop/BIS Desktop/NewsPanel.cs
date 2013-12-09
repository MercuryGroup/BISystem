using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    class NewsPanel :  Panel
    {
       // public RichTextBox richTextBox; // rich text box for diplaying all the text

        private News n; // The current news instance displayed
        private Controller c; 

        private int panelHeight; 
        private int panelWidth;

        private RichTextBox titleRichTextBox; 
        private RichTextBox descritionRichTextBox;
        private Label linkLabel;
        private Label line;
        private Label filler; 

        private FlowLayoutPanel panel; 

        public NewsPanel(News news)
        {
            n = news;
            c = new Controller();

            this.BackColor = Color.White; 
            // initialize components
            InitializeComponents();
 
        }

        private void InitializeComponents()
        {

            titleRichTextBox = new RichTextBox(); 
            titleRichTextBox.Text = n.title; 
            titleRichTextBox.SelectAll();
            titleRichTextBox.SelectionFont = new Font("Segoe UI", 16, FontStyle.Regular); 
            titleRichTextBox.SelectionAlignment = HorizontalAlignment.Center;
            titleRichTextBox.BackColor = Color.White;
            titleRichTextBox.Height = 35; 
            titleRichTextBox.Width = panelWidth;
            titleRichTextBox.Top = 0;
            titleRichTextBox.BorderStyle = BorderStyle.None;
            titleRichTextBox.ReadOnly = true;
            titleRichTextBox.WordWrap = true;
            titleRichTextBox.ScrollBars = RichTextBoxScrollBars.None; 


            descritionRichTextBox = new RichTextBox();
            descritionRichTextBox.Text = "\n" + n.description;
            descritionRichTextBox.SelectAll();
            descritionRichTextBox.SelectionAlignment = HorizontalAlignment.Center;
            descritionRichTextBox.SelectionFont = c.mercuryFont; 
            descritionRichTextBox.BackColor = Color.White;
            
            using (Graphics g = CreateGraphics())
            {
                
                descritionRichTextBox.Height = (int)g.MeasureString(descritionRichTextBox.Text,
                    descritionRichTextBox.Font, descritionRichTextBox.Width).Height - 50;
            
            }

            descritionRichTextBox.Width = panelWidth - 10;
            descritionRichTextBox.Top = 0;
            descritionRichTextBox.BorderStyle = BorderStyle.None;
            descritionRichTextBox.ReadOnly = true;
            descritionRichTextBox.WordWrap = true;
            descritionRichTextBox.ScrollBars = RichTextBoxScrollBars.None;

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
            panel.Top = 40; // panelHeight - (panelHeight + 200);
            panel.Left = (panelWidth - panelWidth) / 2; 

            panel.Controls.Add(titleRichTextBox);
            panel.Controls.Add(descritionRichTextBox);
            panel.Controls.Add(linkLabel);
            panel.Controls.Add(filler); 
            panel.Controls.Add(line); 
        
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
            panelWidth = W;

            this.Width = panelWidth;
            this.Height = panelHeight;

            this.Controls.Clear(); 

            InitializeComponents();

            this.Controls.Add(panel);             

        }

    }
}
