package com.example.mercbisandroid;

import java.util.ArrayList;

import android.app.Activity;
import android.os.AsyncTask;
import android.os.Bundle;

public class StockActivity extends Activity {
	
	AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> DetailedStockArray;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {         

       super.onCreate(savedInstanceState);    
       setContentView(R.layout.activity_stock);
       DetailedStockArray = new DetailedStockThread().execute();
       //rest of the code
   }

}
