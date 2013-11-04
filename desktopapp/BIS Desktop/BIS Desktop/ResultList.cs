using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;



namespace BIS_Desktop
{
    public class ResultList : FlowLayoutPanel
    {
        private Button[] btns; // Array of buttons

        public ResultList(String Source){

            // List = JsonHandler(Source) 
          

            List<String> Listoftexts = new List<String>();

            Listoftexts.Add("LON.STK.EXCH");
            Listoftexts.Add("LMS CAPITAL");
            Listoftexts.Add("LLOYDS GRP");
            Listoftexts.Add("LEGAL&GEN");
            Listoftexts.Add("LAW.DEB.CORP");
            Listoftexts.Add("LAVENDON GROUP");
            Listoftexts.Add("LAND SECS");

 
            btns = new Button[Listoftexts.Count];
            for(int i = 0; i < Listoftexts.Count; i++)
            {
                btns[i] = new Button();
                btns[i].Text = Listoftexts[i];
                btns[i].AutoSize = false;
                btns[i].Width = this.Width;
                btns[i].TabStop = false;
                btns[i].FlatStyle = FlatStyle.Flat;
                btns[i].FlatAppearance.BorderSize = 0;
                btns[i].Margin = new Padding(0);

                // add event handler
                btns[i].Click += new EventHandler(this.button_Click);


                if (i % 2 == 1)
                {
                    btns[i].BackColor = Color.SkyBlue;
                }
                else
                {
                    btns[i].BackColor = Color.WhiteSmoke;
                }

               
                this.Controls.Add(btns[i]);
                this.AutoScroll = true;
                this.HorizontalScroll.Enabled = false;
                this.HorizontalScroll.Visible = false;
                this.AutoScrollPosition = new Point(this.VerticalScroll.Maximum);

            }
            
        }

       private void button_Click(object sender, System.EventArgs e)
       {
           Button b = sender as Button;
           String text = b.Text;
           Console.WriteLine(text);
       }

       public void setSize(int W, int H)
       {
           this.Height = H;
           this.Width = W;
           updateButton();
       }

       private void updateButton()
       {
           foreach(Button b in btns)
           {
               b.Width = this.Width; 
           }
       }
    }
}
