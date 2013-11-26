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
            this.marketButton = new System.Windows.Forms.Button();
            this.stocksButton = new System.Windows.Forms.Button();
            this.newsButton = new System.Windows.Forms.Button();
            this.portfolioButton = new System.Windows.Forms.Button();
            this.menu = new System.Windows.Forms.Panel();
            this.searchField = new System.Windows.Forms.TextBox();
            this.closeLabel = new System.Windows.Forms.Label();
            this.maximizeLabel = new System.Windows.Forms.Label();
            this.minimizeLabel = new System.Windows.Forms.Label();
            this.dragPanel = new System.Windows.Forms.Panel();
            this.windowLabel = new System.Windows.Forms.Label();
            this.extraPanel = new System.Windows.Forms.Panel();
            this.mainContentPanel = new System.Windows.Forms.Panel();
            this.rightPanel = new System.Windows.Forms.Panel();
            this.leftParentPanel = new System.Windows.Forms.Panel();
            this.marketPanel = new System.Windows.Forms.Panel();
            this.nyseButton = new System.Windows.Forms.Button();
            this.lseButton = new System.Windows.Forms.Button();
            this.omxButton = new System.Windows.Forms.Button();
            this.leftPanel = new System.Windows.Forms.Panel();
            this.menu.SuspendLayout();
            this.dragPanel.SuspendLayout();
            this.mainContentPanel.SuspendLayout();
            this.leftParentPanel.SuspendLayout();
            this.marketPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // marketButton
            // 
            this.marketButton.BackColor = System.Drawing.Color.LightGray;
            this.marketButton.FlatAppearance.BorderSize = 0;
            this.marketButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.marketButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.marketButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.marketButton.Font = new System.Drawing.Font("Segoe UI", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.marketButton.ForeColor = System.Drawing.Color.White;
            this.marketButton.Location = new System.Drawing.Point(0, 114);
            this.marketButton.Name = "marketButton";
            this.marketButton.Size = new System.Drawing.Size(185, 70);
            this.marketButton.TabIndex = 0;
            this.marketButton.Text = "Market";
            this.marketButton.UseVisualStyleBackColor = false;
            this.marketButton.Click += new System.EventHandler(this.marketButton_Click);
            // 
            // stocksButton
            // 
            this.stocksButton.BackColor = System.Drawing.Color.LightGray;
            this.stocksButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.stocksButton.FlatAppearance.BorderSize = 0;
            this.stocksButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.stocksButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.stocksButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.stocksButton.Font = new System.Drawing.Font("Segoe UI", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.stocksButton.ForeColor = System.Drawing.Color.White;
            this.stocksButton.Location = new System.Drawing.Point(0, 191);
            this.stocksButton.Name = "stocksButton";
            this.stocksButton.Size = new System.Drawing.Size(185, 70);
            this.stocksButton.TabIndex = 1;
            this.stocksButton.Text = "Stocks";
            this.stocksButton.UseVisualStyleBackColor = false;
            this.stocksButton.Click += new System.EventHandler(this.stocksButton_Click);
            // 
            // newsButton
            // 
            this.newsButton.BackColor = System.Drawing.Color.LightGray;
            this.newsButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.newsButton.FlatAppearance.BorderSize = 0;
            this.newsButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.newsButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.newsButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.newsButton.Font = new System.Drawing.Font("Segoe UI", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.newsButton.ForeColor = System.Drawing.Color.White;
            this.newsButton.Location = new System.Drawing.Point(0, 268);
            this.newsButton.Name = "newsButton";
            this.newsButton.Size = new System.Drawing.Size(185, 70);
            this.newsButton.TabIndex = 2;
            this.newsButton.Text = "News";
            this.newsButton.UseVisualStyleBackColor = false;
            this.newsButton.Click += new System.EventHandler(this.newsButton_Click);
            // 
            // portfolioButton
            // 
            this.portfolioButton.BackColor = System.Drawing.Color.LightGray;
            this.portfolioButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.portfolioButton.FlatAppearance.BorderSize = 0;
            this.portfolioButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.portfolioButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.portfolioButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.portfolioButton.Font = new System.Drawing.Font("Segoe UI", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.portfolioButton.ForeColor = System.Drawing.Color.White;
            this.portfolioButton.Location = new System.Drawing.Point(0, 345);
            this.portfolioButton.Name = "portfolioButton";
            this.portfolioButton.Size = new System.Drawing.Size(185, 70);
            this.portfolioButton.TabIndex = 3;
            this.portfolioButton.Text = "Portfolio";
            this.portfolioButton.UseVisualStyleBackColor = false;
            this.portfolioButton.Click += new System.EventHandler(this.portfolioButton_Click);
            // 
            // menu
            // 
            this.menu.BackColor = System.Drawing.Color.White;
            this.menu.Controls.Add(this.searchField);
            this.menu.Controls.Add(this.portfolioButton);
            this.menu.Controls.Add(this.newsButton);
            this.menu.Controls.Add(this.stocksButton);
            this.menu.Controls.Add(this.marketButton);
            this.menu.Dock = System.Windows.Forms.DockStyle.Left;
            this.menu.Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.menu.Location = new System.Drawing.Point(0, 0);
            this.menu.Margin = new System.Windows.Forms.Padding(0);
            this.menu.Name = "menu";
            this.menu.Padding = new System.Windows.Forms.Padding(3);
            this.menu.Size = new System.Drawing.Size(195, 688);
            this.menu.TabIndex = 0;
            // 
            // searchField
            // 
            this.searchField.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.searchField.Font = new System.Drawing.Font("Segoe UI", 12F);
            this.searchField.ForeColor = System.Drawing.Color.Gray;
            this.searchField.Location = new System.Drawing.Point(29, 0);
            this.searchField.MaxLength = 50;
            this.searchField.Multiline = true;
            this.searchField.Name = "searchField";
            this.searchField.Size = new System.Drawing.Size(130, 30);
            this.searchField.TabIndex = 7;
            this.searchField.Text = "Search";
            this.searchField.Enter += new System.EventHandler(this.searchField_Focus);
            this.searchField.KeyDown += new System.Windows.Forms.KeyEventHandler(this.searchField_KeyPressed);
            this.searchField.Leave += new System.EventHandler(this.searchField_Exit);
            // 
            // closeLabel
            // 
            this.closeLabel.AutoSize = true;
            this.closeLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.closeLabel.Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.closeLabel.ForeColor = System.Drawing.Color.DarkGray;
            this.closeLabel.Location = new System.Drawing.Point(1248, 0);
            this.closeLabel.Margin = new System.Windows.Forms.Padding(0);
            this.closeLabel.Name = "closeLabel";
            this.closeLabel.Padding = new System.Windows.Forms.Padding(6, 7, 10, 1);
            this.closeLabel.Size = new System.Drawing.Size(32, 25);
            this.closeLabel.TabIndex = 5;
            this.closeLabel.Text = "X";
            this.closeLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.closeLabel.Click += new System.EventHandler(this.closeLabel_Click);
            this.closeLabel.MouseEnter += new System.EventHandler(this.controlLabel_MouseEnter);
            this.closeLabel.MouseLeave += new System.EventHandler(this.controlLabel_MouseLeave);
            // 
            // maximizeLabel
            // 
            this.maximizeLabel.AutoSize = true;
            this.maximizeLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.maximizeLabel.Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.maximizeLabel.ForeColor = System.Drawing.Color.DarkGray;
            this.maximizeLabel.Location = new System.Drawing.Point(1216, 0);
            this.maximizeLabel.Margin = new System.Windows.Forms.Padding(0);
            this.maximizeLabel.Name = "maximizeLabel";
            this.maximizeLabel.Padding = new System.Windows.Forms.Padding(6, 7, 6, 1);
            this.maximizeLabel.Size = new System.Drawing.Size(32, 25);
            this.maximizeLabel.TabIndex = 6;
            this.maximizeLabel.Text = "M";
            this.maximizeLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.maximizeLabel.Click += new System.EventHandler(this.maximizeLabel_Click);
            this.maximizeLabel.MouseEnter += new System.EventHandler(this.controlLabel_MouseEnter);
            this.maximizeLabel.MouseLeave += new System.EventHandler(this.controlLabel_MouseLeave);
            // 
            // minimizeLabel
            // 
            this.minimizeLabel.AutoSize = true;
            this.minimizeLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.minimizeLabel.Font = new System.Drawing.Font("Segoe UI", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.minimizeLabel.ForeColor = System.Drawing.Color.DarkGray;
            this.minimizeLabel.Location = new System.Drawing.Point(1187, 0);
            this.minimizeLabel.Margin = new System.Windows.Forms.Padding(0);
            this.minimizeLabel.Name = "minimizeLabel";
            this.minimizeLabel.Padding = new System.Windows.Forms.Padding(6, 2, 6, 0);
            this.minimizeLabel.Size = new System.Drawing.Size(29, 23);
            this.minimizeLabel.TabIndex = 7;
            this.minimizeLabel.Text = "_";
            this.minimizeLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.minimizeLabel.Click += new System.EventHandler(this.minimizeLabel_Click);
            this.minimizeLabel.MouseEnter += new System.EventHandler(this.controlLabel_MouseEnter);
            this.minimizeLabel.MouseLeave += new System.EventHandler(this.controlLabel_MouseLeave);
            // 
            // dragPanel
            // 
            this.dragPanel.BackColor = System.Drawing.Color.White;
            this.dragPanel.Controls.Add(this.windowLabel);
            this.dragPanel.Controls.Add(this.extraPanel);
            this.dragPanel.Controls.Add(this.minimizeLabel);
            this.dragPanel.Controls.Add(this.maximizeLabel);
            this.dragPanel.Controls.Add(this.closeLabel);
            this.dragPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.dragPanel.Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.dragPanel.Location = new System.Drawing.Point(0, 0);
            this.dragPanel.Margin = new System.Windows.Forms.Padding(0);
            this.dragPanel.Name = "dragPanel";
            this.dragPanel.Size = new System.Drawing.Size(1280, 32);
            this.dragPanel.TabIndex = 3;
            // 
            // windowLabel
            // 
            this.windowLabel.AutoSize = true;
            this.windowLabel.BackColor = System.Drawing.Color.Transparent;
            this.windowLabel.Enabled = false;
            this.windowLabel.Font = new System.Drawing.Font("Segoe UI", 12F);
            this.windowLabel.ForeColor = System.Drawing.Color.Black;
            this.windowLabel.Location = new System.Drawing.Point(201, 4);
            this.windowLabel.Name = "windowLabel";
            this.windowLabel.Size = new System.Drawing.Size(270, 21);
            this.windowLabel.TabIndex = 10;
            this.windowLabel.Text = "Mercury Business Intelligence System";
            // 
            // extraPanel
            // 
            this.extraPanel.Enabled = false;
            this.extraPanel.Location = new System.Drawing.Point(0, 0);
            this.extraPanel.Name = "extraPanel";
            this.extraPanel.Size = new System.Drawing.Size(195, 32);
            this.extraPanel.TabIndex = 11;
            // 
            // mainContentPanel
            // 
            this.mainContentPanel.BackColor = System.Drawing.Color.White;
            this.mainContentPanel.Controls.Add(this.rightPanel);
            this.mainContentPanel.Controls.Add(this.leftParentPanel);
            this.mainContentPanel.Controls.Add(this.menu);
            this.mainContentPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.mainContentPanel.Location = new System.Drawing.Point(0, 32);
            this.mainContentPanel.Margin = new System.Windows.Forms.Padding(0);
            this.mainContentPanel.Name = "mainContentPanel";
            this.mainContentPanel.Size = new System.Drawing.Size(1280, 688);
            this.mainContentPanel.TabIndex = 4;
            // 
            // rightPanel
            // 
            this.rightPanel.BackColor = System.Drawing.Color.ForestGreen;
            this.rightPanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.rightPanel.Font = new System.Drawing.Font("Segoe UI", 9.75F);
            this.rightPanel.Location = new System.Drawing.Point(804, 0);
            this.rightPanel.Margin = new System.Windows.Forms.Padding(5, 0, 0, 0);
            this.rightPanel.Name = "rightPanel";
            this.rightPanel.Size = new System.Drawing.Size(476, 688);
            this.rightPanel.TabIndex = 4;
            // 
            // leftParentPanel
            // 
            this.leftParentPanel.BackColor = System.Drawing.SystemColors.Highlight;
            this.leftParentPanel.Controls.Add(this.marketPanel);
            this.leftParentPanel.Controls.Add(this.leftPanel);
            this.leftParentPanel.Dock = System.Windows.Forms.DockStyle.Left;
            this.leftParentPanel.Location = new System.Drawing.Point(195, 0);
            this.leftParentPanel.Margin = new System.Windows.Forms.Padding(0, 0, 5, 0);
            this.leftParentPanel.Name = "leftParentPanel";
            this.leftParentPanel.Size = new System.Drawing.Size(517, 688);
            this.leftParentPanel.TabIndex = 3;
            // 
            // marketPanel
            // 
            this.marketPanel.BackColor = System.Drawing.Color.White;
            this.marketPanel.Controls.Add(this.nyseButton);
            this.marketPanel.Controls.Add(this.lseButton);
            this.marketPanel.Controls.Add(this.omxButton);
            this.marketPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.marketPanel.Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.marketPanel.Location = new System.Drawing.Point(0, 0);
            this.marketPanel.Margin = new System.Windows.Forms.Padding(0);
            this.marketPanel.Name = "marketPanel";
            this.marketPanel.Padding = new System.Windows.Forms.Padding(6);
            this.marketPanel.Size = new System.Drawing.Size(517, 40);
            this.marketPanel.TabIndex = 1;
            // 
            // nyseButton
            // 
            this.nyseButton.BackColor = System.Drawing.Color.LightGray;
            this.nyseButton.Enabled = false;
            this.nyseButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.nyseButton.FlatAppearance.BorderSize = 0;
            this.nyseButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.nyseButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.nyseButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.nyseButton.Font = new System.Drawing.Font("Segoe UI", 10F);
            this.nyseButton.ForeColor = System.Drawing.Color.White;
            this.nyseButton.Location = new System.Drawing.Point(179, 0);
            this.nyseButton.Margin = new System.Windows.Forms.Padding(3, 3, 3, 6);
            this.nyseButton.Name = "nyseButton";
            this.nyseButton.Size = new System.Drawing.Size(80, 30);
            this.nyseButton.TabIndex = 6;
            this.nyseButton.Text = "NYSE";
            this.nyseButton.UseVisualStyleBackColor = false;
            this.nyseButton.Click += new System.EventHandler(this.nyseButton_Click);
            // 
            // lseButton
            // 
            this.lseButton.BackColor = System.Drawing.Color.LightGray;
            this.lseButton.Enabled = false;
            this.lseButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.lseButton.FlatAppearance.BorderSize = 0;
            this.lseButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.lseButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.lseButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.lseButton.Font = new System.Drawing.Font("Segoe UI", 10F);
            this.lseButton.ForeColor = System.Drawing.Color.White;
            this.lseButton.Location = new System.Drawing.Point(6, 0);
            this.lseButton.Margin = new System.Windows.Forms.Padding(3, 3, 3, 6);
            this.lseButton.Name = "lseButton";
            this.lseButton.Size = new System.Drawing.Size(80, 30);
            this.lseButton.TabIndex = 4;
            this.lseButton.Text = "LSE";
            this.lseButton.UseVisualStyleBackColor = false;
            this.lseButton.Click += new System.EventHandler(this.lseButton_Click);
            // 
            // omxButton
            // 
            this.omxButton.BackColor = System.Drawing.Color.LightGray;
            this.omxButton.Enabled = false;
            this.omxButton.FlatAppearance.BorderColor = System.Drawing.Color.White;
            this.omxButton.FlatAppearance.BorderSize = 0;
            this.omxButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(220)))), ((int)(((byte)(53)))), ((int)(((byte)(2)))));
            this.omxButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Black;
            this.omxButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.omxButton.Font = new System.Drawing.Font("Segoe UI", 10F);
            this.omxButton.ForeColor = System.Drawing.Color.White;
            this.omxButton.Location = new System.Drawing.Point(92, 0);
            this.omxButton.Margin = new System.Windows.Forms.Padding(3, 3, 3, 6);
            this.omxButton.Name = "omxButton";
            this.omxButton.Size = new System.Drawing.Size(80, 30);
            this.omxButton.TabIndex = 5;
            this.omxButton.Text = "OMX";
            this.omxButton.UseVisualStyleBackColor = false;
            this.omxButton.Click += new System.EventHandler(this.omxButton_Click);
            // 
            // leftPanel
            // 
            this.leftPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.leftPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.leftPanel.Font = new System.Drawing.Font("Segoe UI", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.leftPanel.Location = new System.Drawing.Point(0, 52);
            this.leftPanel.Name = "leftPanel";
            this.leftPanel.Size = new System.Drawing.Size(517, 636);
            this.leftPanel.TabIndex = 2;
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1280, 720);
            this.Controls.Add(this.mainContentPanel);
            this.Controls.Add(this.dragPanel);
            this.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "MainWindow";
            this.Text = "Form1";
            this.menu.ResumeLayout(false);
            this.menu.PerformLayout();
            this.dragPanel.ResumeLayout(false);
            this.dragPanel.PerformLayout();
            this.mainContentPanel.ResumeLayout(false);
            this.leftParentPanel.ResumeLayout(false);
            this.marketPanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button marketButton;
        private System.Windows.Forms.Button stocksButton;
        private System.Windows.Forms.Button newsButton;
        private System.Windows.Forms.Button portfolioButton;
        private System.Windows.Forms.Panel menu;
        private System.Windows.Forms.Label closeLabel;
        private System.Windows.Forms.Label maximizeLabel;
        private System.Windows.Forms.Label minimizeLabel;
        private System.Windows.Forms.Panel dragPanel;
        private System.Windows.Forms.Panel mainContentPanel;
        private System.Windows.Forms.Panel leftParentPanel;
        private System.Windows.Forms.Panel marketPanel;
        private System.Windows.Forms.Panel rightPanel;
        private System.Windows.Forms.Panel leftPanel;
        private System.Windows.Forms.Button lseButton;
        private System.Windows.Forms.Button nyseButton;
        private System.Windows.Forms.Button omxButton;
        private System.Windows.Forms.TextBox searchField;
        private System.Windows.Forms.Label windowLabel;
        private System.Windows.Forms.Panel extraPanel;
    }
}

