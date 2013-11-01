using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace BIS_Desktop
{
    public partial class Form1 : Form
    {
        private Boolean marketClicked, stocksClicked;
        Class1 leftPanelValue;
        public Form1()
        {
            marketClicked = false;
            stocksClicked = false;
            leftPanelValue = new Class1();
            InitializeComponent();
        }

        private void menu_Paint(object sender, PaintEventArgs e)
        {

        }
        private void menuClick(String button, Boolean toggled)
        {
            Color buttonColor = Color.LightGray;
            if (!toggled)
            {
                resetMenuButtons();
                buttonColor = Color.CornflowerBlue;
                leftPanel.Controls.Add(leftPanelValue);
                leftPanelValue.Size = leftPanel.Size;
                //Temp

            }
            if(leftPanel.Contains(leftPanelValue))
            {
                leftPanel.Controls.Remove(leftPanelValue);
            }
            leftPanel.Controls.Add(leftPanelValue);
            switch (button)
            {
                case "market":
                    marketButton.BackColor = buttonColor;
                    break;
                case "stocks":
                    stocksButton.BackColor = buttonColor;
                    break;
                case "news":
                    break;
                case "portfolio":
                    break;
                default:
                    break;
            }
        }
        private void resetMenuButtons()
        {
            marketClicked = false;
            stocksClicked = false;
            marketButton.BackColor = Color.LightGray;
            stocksButton.BackColor = Color.LightGray;
        }

        private void stocksButton_Click(object sender, EventArgs e)
        {
            
            if (!stocksClicked)
            {
                menuClick("stocks", stocksClicked);
                stocksClicked = true;
            }
        }
        private void marketButton_Click(object sender, EventArgs e)
        {
            
            if (!marketClicked)
            {
                menuClick("market", marketClicked);
                marketClicked = true;
            }
        }
    }
}
