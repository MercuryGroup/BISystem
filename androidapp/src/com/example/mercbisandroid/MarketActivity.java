package com.example.mercbisandroid;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;

public class MarketActivity extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_market);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		return true;
	}

	public boolean onOptionsItemSelected(MenuItem item){
		Intent stocksIntent = new Intent(this, MainActivity.class);
		Intent portfolioIntent = new Intent(this, PortfolioActivity.class);
		switch (item.getItemId()) {
		case R.id.stocks:
			startActivity(stocksIntent);
			return true;
		case R.id.portfolio:
			startActivity(portfolioIntent);
		default:
			return super.onOptionsItemSelected(item);
		
			
		}
	}
	
	
}
