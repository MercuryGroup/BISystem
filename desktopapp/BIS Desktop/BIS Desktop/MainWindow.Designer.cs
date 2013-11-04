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
            this.dragPanel = new System.Windows.Forms.Panel();
            this.minimizeLabel = new System.Windows.Forms.Label();
            this.closeLabel = new System.Windows.Forms.Label();
            this.maximizeLabel = new System.Windows.Forms.Label();
            this.menu.SuspendLayout();
            this.dragPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // menu
            // 
            this.menu.BackColor = System.Drawing.Color.White;
            this.menu.Controls.Add(this.portfolioButton);
            this.menu.Controls.Add(this.newsButton);
            this.menu.Controls.Add(this.stocksButton);
            this.menu.Controls.Add(this.marketButton);
            this.menu.Location = new System.Drawing.Point(0, 40);
            this.menu.Name = "menu";
            this.menu.Size = new System.Drawing.Size(201, 732);
            this.menu.TabIndex = 0;
            this.menu.Paint += new System.Windows.Forms.PaintEventHandler(this.menu_Paint);
            // 
            // portfolioButton
            // 
            this.portfolioButton.BackColor = System.Drawing.Color.LightGray;
            this.portfolioButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.portfolioButton.Font = new System.Drawing.Font("Calibri", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.portfolioButton.Location = new System.Drawing.Point(-7, 396);
            this.portfolioButton.Name = "portfolioButton";
            this.portfolioButton.Size = new System.Drawing.Size(185, 95);
            this.portfolioButton.TabIndex = 0;
            this.portfolioButton.Text = "Portfolio";
            this.portfolioButton.UseVisualStyleBackColor = false;
            this.portfolioButton.Click += new System.EventHandler(this.portfolioButton_Click);
            // 
            // newsButton
            // 
            this.newsButton.BackColor = System.Drawing.Color.LightGray;
            this.newsButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.newsButton.Font = new System.Drawing.Font("Calibri", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.newsButton.Location = new System.Drawing.Point(-7, 302);
            this.newsButton.Name = "newsButton";
            this.newsButton.Size = new System.Drawing.Size(185, 95);
            this.newsButton.TabIndex = 0;
            this.newsButton.Text = "News";
            this.newsButton.UseVisualStyleBackColor = false;
            this.newsButton.Click += new System.EventHandler(this.newsButton_Click);
            // 
            // stocksButton
            // 
            this.stocksButton.BackColor = System.Drawing.Color.LightGray;
            this.stocksButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.stocksButton.Font = new System.Drawing.Font("Calibri", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.stocksButton.Location = new System.Drawing.Point(-7, 209);
            this.stocksButton.Name = "stocksButton";
            this.stocksButton.Size = new System.Drawing.Size(185, 95);
            this.stocksButton.TabIndex = 0;
            this.stocksButton.Text = "Stocks";
            this.stocksButton.UseVisualStyleBackColor = false;
            this.stocksButton.Click += new System.EventHandler(this.stocksButton_Click);
            // 
            // marketButton
            // 
            this.marketButton.BackColor = System.Drawing.Color.LightGray;
            this.marketButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.marketButton.Font = new System.Drawing.Font("Calibri", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.marketButton.Location = new System.Drawing.Point(-7, 115);
            this.marketButton.Name = "marketButton";
            this.marketButton.Size = new System.Drawing.Size(185, 95);
            this.marketButton.TabIndex = 0;
            this.marketButton.Text = "Market";
            this.marketButton.UseVisualStyleBackColor = false;
            this.marketButton.Click += new System.EventHandler(this.marketButton_Click);
            // 
            // settings
            // 
            this.settings.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(255)))), ((int)(((byte)(128)))));
            this.settings.Location = new System.Drawing.Point(208, 40);
            this.settings.Name = "settings";
            this.settings.Size = new System.Drawing.Size(661, 59);
            this.settings.TabIndex = 1;
            // 
            // leftPanel
            // 
            this.leftPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.leftPanel.Location = new System.Drawing.Point(208, 106);
            this.leftPanel.Name = "leftPanel";
            this.leftPanel.Size = new System.Drawing.Size(661, 666);
            this.leftPanel.TabIndex = 2;
            // 
            // dragPanel
            // 
            this.dragPanel.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.dragPanel.Controls.Add(this.minimizeLabel);
            this.dragPanel.Controls.Add(this.closeLabel);
            this.dragPanel.Controls.Add(this.maximizeLabel);
            this.dragPanel.Location = new System.Drawing.Point(0, 0);
            this.dragPanel.Margin = new System.Windows.Forms.Padding(0);
            this.dragPanel.Name = "dragPanel";
            this.dragPanel.Size = new System.Drawing.Size(1282, 32);
            this.dragPanel.TabIndex = 3;
            this.dragPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.dragPanel_Paint);
            // 
            // minimizeLabel
            // 
            this.minimizeLabel.AutoSize = true;
            this.minimizeLabel.Font = new System.Drawing.Font("Calibri", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.minimizeLabel.ForeColor = System.Drawing.Color.Silver;
            this.minimizeLabel.Location = new System.Drawing.Point(1192, 3);
            this.minimizeLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.minimizeLabel.Name = "minimizeLabel";
            this.minimizeLabel.Size = new System.Drawing.Size(17, 19);
            this.minimizeLabel.TabIndex = 4;
            this.minimizeLabel.Text = "_";
            // 
            // closeLabel
            // 
            this.closeLabel.AutoSize = true;
            this.closeLabel.Font = new System.Drawing.Font("Calibri", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.closeLabel.ForeColor = System.Drawing.Color.Silver;
            this.closeLabel.Location = new System.Drawing.Point(1250, 9);
            this.closeLabel.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.closeLabel.Name = "closeLabel";
            this.closeLabel.Padding = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.closeLabel.Size = new System.Drawing.Size(27, 17);
            this.closeLabel.TabIndex = 5;
            this.closeLabel.Text = "X";
            this.closeLabel.Click += new System.EventHandler(this.closeLabel_Click);
            // 
            // maximizeLabel
            // 
            this.maximizeLabel.AutoSize = true;
            this.maximizeLabel.Font = new System.Drawing.Font("Calibri Light", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.maximizeLabel.ForeColor = System.Drawing.Color.Silver;
            this.maximizeLabel.Location = new System.Drawing.Point(1215, 9);
            this.maximizeLabel.Margin = new System.Windows.Forms.Padding(0);
            this.maximizeLabel.Name = "maximizeLabel";
            this.maximizeLabel.Padding = new System.Windows.Forms.Padding(6, 0, 6, 0);
            this.maximizeLabel.Size = new System.Drawing.Size(32, 17);
            this.maximizeLabel.TabIndex = 4;
            this.maximizeLabel.Text = "M";
            this.maximizeLabel.TextAlign = System.Drawing.ContentAlignment.TopRight;
            this.maximizeLabel.Click += new System.EventHandler(this.maximizeLabel_Click);
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1280, 720);
            this.Controls.Add(this.dragPanel);
            this.Controls.Add(this.settings);
            this.Controls.Add(this.menu);
            this.Controls.Add(this.leftPanel);
            this.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "MainWindow";
            this.Text = "Form1";
            this.menu.ResumeLayout(false);
            this.dragPanel.ResumeLayout(false);
            this.dragPanel.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel menu;
        private System.Windows.Forms.Panel settings;
        private System.Windows.Forms.Panel leftPanel;
        private System.Windows.Forms.Button marketButton;
        private System.Windows.Forms.Button stocksButton;
        private System.Windows.Forms.Button newsButton;
        private System.Windows.Forms.Button portfolioButton;
        private System.Windows.Forms.Panel dragPanel;
        private System.Windows.Forms.Label maximizeLabel;
        private System.Windows.Forms.Label closeLabel;
        private System.Windows.Forms.Label minimizeLabel;
    }
}

