
package com.example.mercbisandroid;

import org.afree.chart.AFreeChart;
import org.afree.graphics.geom.RectShape;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.widget.ImageView;


/**
 * @author
 *		
 */
public class ChartView extends ImageView {
    private Bitmap              bitmap;
    private RectShape           rectArea;
    private Canvas              canvas;
    private AFreeChart          chart;

    public ChartView( Context context, AttributeSet attributeSet ){
        super(context, attributeSet);
    }

    public ChartView( Context context ){
        super(context);
        intChart();
    }

    private void intChart() {
        if (getResources().getConfiguration().orientation == Configuration.ORIENTATION_LANDSCAPE) {
            bitmap = Bitmap.createBitmap(700, 500, Bitmap.Config.ARGB_8888);
            rectArea = new RectShape(0.0, 0.0, 700, 500);
        }
        else {
            bitmap = Bitmap.createBitmap(700, 500, Bitmap.Config.ARGB_8888);
            rectArea = new RectShape(0.0, 0.0, 700, 500);
        }
    }

    public void drawChart( AFreeChart chart ) {
        canvas = new Canvas(bitmap);
        this.chart = chart;             
        this.chart.draw(canvas, rectArea);
        setImageBitmap(bitmap);
    }

    @Override
    protected void onDraw( Canvas canvas ){
        super.onDraw(canvas);               
    }
}