package com.example.mercbisandroid;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;

public class MainActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }
    
    public boolean onOptionsItemSelected(MenuItem item){
    	Intent marketIntent = new Intent(this, MarketActivity.class);
    	Intent portfolioIntent = new Intent(this, PortfolioActivity.class);
    	switch (item.getItemId()) {
/*    		case R.id.stocks:
    			startActivity(intent);
    			return true;*/
    		case R.id.market:
    			startActivity(marketIntent);
    			return true;
    		case R.id.portfolio:
    			startActivity(portfolioIntent);
    			return true;
    		default:
    			return super.onOptionsItemSelected(item);
    	}
    }

   




}
