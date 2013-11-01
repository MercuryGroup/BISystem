namespace BIS_Desktop
{
    partial class MainWindow
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.menu = new System.Windows.Forms.Panel();
            this.portfolioButton = new System.Windows.Forms.Button();
            this.newsButton = new System.Windows.Forms.Button();
            this.stocksButton = new System.Windows.Forms.Button();
            this.marketButton = new System.Windows.Forms.Button();
            this.settings = new System.Windows.Forms.Panel();
            this.leftPanel = new System.Windows.Forms.Panel();
            this.rightPanel = new System.Windows.Forms.Panel();
            this.menu.SuspendLayout();
            this.SuspendLayout();
            // 
            // menu
            // 
            this.menu.BackColor = System.Drawing.Color.White;
            this.menu.Controls.Add(this.portfolioButton);
            this.menu.Controls.Add(this.newsButton);
            this.menu.Controls.Add(this.stocksButton);
            this.menu.Controls.Add(this.marketButton);
            this.menu.Location = new System.Drawing.Point(4, 35);
            this.menu.Name = "menu";
            this.menu.Size = new System.Drawing.Size(168, 634);
            this.menu.TabIndex = 0;
            this.menu.Paint += new System.Windows.Forms.PaintEventHandler(this.menu_Paint);
            // 
            // portfolioButton
            // 
            this.portfolioButton.BackColor = System.Drawing.Color.LightGray;
            this.portfolioButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.portfolioButton.Location = new System.Drawing.Point(-1, 343);
            this.portfolioButton.Name = "portfolioButton";
            this.portfolioButton.Size = new System.Drawing.Size(154, 82);
            this.portfolioButton.TabIndex = 0;
            this.portfolioButton.Text = "Portfolio";
            this.portfolioButton.UseVisualStyleBackColor = false;
            this.portfolioButton.Click += new System.EventHandler(this.portfolioButton_Click);
            // 
            // newsButton
            // 
            this.newsButton.BackColor = System.Drawing.Color.LightGray;
            this.newsButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.newsButton.Location = new System.Drawing.Point(-1, 262);
            this.newsButton.Name = "newsButton";
            this.newsButton.Size = new System.Drawing.Size(154, 82);
            this.newsButton.TabIndex = 0;
            this.newsButton.Text = "News";
            this.newsButton.UseVisualStyleBackColor = false;
            this.newsButton.Click += new System.EventHandler(this.newsButton_Click);
            // 
            // stocksButton
            // 
            this.stocksButton.BackColor = System.Drawing.Color.LightGray;
            this.stocksButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.stocksButton.Location = new System.Drawing.Point(-1, 181);
            this.stocksButton.Name = "stocksButton";
            this.stocksButton.Size = new System.Drawing.Size(154, 82);
            this.stocksButton.TabIndex = 0;
            this.stocksButton.Text = "Stocks";
            this.stocksButton.UseVisualStyleBackColor = false;
            this.stocksButton.Click += new System.EventHandler(this.stocksButton_Click);
            // 
            // marketButton
            // 
            this.marketButton.BackColor = System.Drawing.Color.LightGray;
            this.marketButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.marketButton.Location = new System.Drawing.Point(-1, 100);
            this.marketButton.Name = "marketButton";
            this.marketButton.Size = new System.Drawing.Size(154, 82);
            this.marketButton.TabIndex = 0;
            this.marketButton.Text = "Market";
            this.marketButton.UseVisualStyleBackColor = false;
            this.marketButton.Click += new System.EventHandler(this.marketButton_Click);
            // 
            // settings
            // 
            this.settings.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(255)))), ((int)(((byte)(128)))));
            this.settings.Location = new System.Drawing.Point(178, 35);
            this.settings.Name = "settings";
            this.settings.Size = new System.Drawing.Size(567, 51);
            this.settings.TabIndex = 1;
            // 
            // leftPanel
            // 
            this.leftPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.leftPanel.Location = new System.Drawing.Point(178, 92);
            this.leftPanel.Name = "leftPanel";
            this.leftPanel.Size = new System.Drawing.Size(567, 577);
            this.leftPanel.TabIndex = 2;
            // 
            // rightPanel
            // 
            this.rightPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(128)))));
            this.rightPanel.Location = new System.Drawing.Point(751, 35);
            this.rightPanel.Margin = new System.Windows.Forms.Padding(0);
            this.rightPanel.Name = "rightPanel";
            this.rightPanel.Size = new System.Drawing.Size(511, 634);
            this.rightPanel.TabIndex = 3;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1264, 681);
            this.Controls.Add(this.rightPanel);
            this.Controls.Add(this.settings);
            this.Controls.Add(this.menu);
            this.Controls.Add(this.leftPanel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "Form1";
            this.Padding = new System.Windows.Forms.Padding(1);
            this.Text = "Form1";
            this.menu.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel menu;
        private System.Windows.Forms.Panel settings;
        private System.Windows.Forms.Panel leftPanel;
        private System.Windows.Forms.Panel rightPanel;
        private System.Windows.Forms.Button marketButton;
        private System.Windows.Forms.Button stocksButton;
        private System.Windows.Forms.Button newsButton;
        private System.Windows.Forms.Button portfolioButton;
    }
}

