using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;



namespace BIS_Desktop
{
    public class Class1 : Panel
    {
    
        public Class1(){


            List<String> Listoftexts = new List<string>();

            Listoftexts.Add("LON.STK.EXCH");
            Listoftexts.Add("LMS CAPITAL");
            Listoftexts.Add("LLOYDS GRP");
            Listoftexts.Add("LEGAL&GEN");
            Listoftexts.Add("LAW.DEB.CORP");
            Listoftexts.Add("LAVENDON GROUP");
            Listoftexts.Add("LAND SECS");
            

            for (int i = 0; i < Listoftexts.Count; i++)
            {

                Button fieldButton = new Button();
                fieldButton.Location = new Point(160, (40 + ((i - 2) * 20)));
                fieldButton.Tag = 2;
                fieldButton.Text = Listoftexts[i];
                fieldButton.AutoSize = false;
                fieldButton.FlatStyle = FlatStyle.Flat;
                this.Controls.Add(fieldButton);
         
                if (i % 2 == 1)
                {
                    fieldButton.BackColor = Color.LightBlue;
                }
                else
                {
                    fieldButton.BackColor = Color.White;
                }

            }


        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            this.ResumeLayout(false);

        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }
       
    }
}
