using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;

namespace BIS_Desktop
{
    /// <summary>
    /// Generic panel that is used for displaying all types of results in the program.
    /// </summary>
    public class ResultPanel : Panel
    {
        //Determines whether or not the panel is loading.
        private bool loading;
        //Content of the panel
        private Panel content;
        /// <summary>
        /// Constructor
        /// </summary>
        public ResultPanel()
        {
            //Set loading to false;
            loading = false;
            //Set result panel to fill
            Dock = DockStyle.Fill;
            //Initialize new panel
            content = new Panel();
            //Set content to fill the result panel
            content.Dock = DockStyle.Fill;
            //Add content to result panel
            Controls.Add(content);
        }
        /// <summary>
        /// Sets loading to either true or false.
        /// </summary>
        /// <param name="loading_"></param>
        public void setLoading(bool loading_)
        {
            loading = loading_;
        }

        /// <summary>
        /// Returns loading boolean.
        /// </summary>
        /// <returns></returns>
        public bool getLoading()
        {
            return loading;
        }
        /// <summary>
        /// Updates the result panel and its contents.
        /// </summary>
        public void updateSize()
        {
            //Resultlist
            if (content is ResultList)
            {
                ResultList temp = content as ResultList;
                temp.setSize(this.Width, this.Height);
            }
            //Info display
            else if (content is InfoDisplay)
            {
                InfoDisplay temp = content as InfoDisplay;
                temp.setSize(this.Width, this.Height);
            }
            //News reader
            else if (content is NewsReader)
            {
                NewsReader temp = content as NewsReader;
                temp.setSize(this.Width, this.Height);
            }
            //Integrated news list
            else if (content is IntegratedNewsList)
            {
                IntegratedNewsList temp = content as IntegratedNewsList;
                temp.setSize(this.Width, this.Height);
            }
            //Sets size of result panel
            content.Width = this.Width;
            content.Height = this.Height;
        }
        /// <summary>
        /// Returns content of result panel.
        /// </summary>
        /// <returns></returns>
        public Panel getContent()
        {
            return content;
        }
        /// <summary>
        /// Sets content of result panel.
        /// </summary>
        /// <param name="content_"></param>
        public void setContent(Panel content_)
        {
            content = content_;
        }
    }
    
}
