package com.example.mercbisandroid;

import org.afree.chart.AFreeChart;
import org.afree.graphics.geom.RectShape;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.widget.ImageView;

public class ChartView extends ImageView {
	
    private Bitmap              bitmap;
    private RectShape           rectArea;
    private Canvas              canvas;
    private AFreeChart          chart;

    public ChartView( Context context, AttributeSet attributeSet ) {
        super(context, attributeSet);
    }

    public ChartView( Context stockGraphFragment ){
        super(stockGraphFragment);
        intChart();
    }

	private void intChart(){
        //Setting different width and height based on the orientation.
        if (getResources().getConfiguration().orientation == Configuration.ORIENTATION_LANDSCAPE) {
            bitmap = Bitmap.createBitmap(600, 300, Bitmap.Config.ARGB_8888);
            rectArea = new RectShape(0.0, 0.0, 600, 300);
        }
        
        else {
            bitmap = Bitmap.createBitmap(600, 300, Bitmap.Config.ARGB_8888);
            rectArea = new RectShape(0.0, 0.0, 600, 300);
        }
    }

    public void drawChart( AFreeChart chart ) {
        canvas = new Canvas(bitmap);
        this.chart = chart;             
        this.chart.draw(canvas, rectArea);
        setImageBitmap(bitmap);
    }

    @Override
    protected void onDraw( Canvas canvas ) {
        super.onDraw(canvas);               
    }
}