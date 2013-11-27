package com.merc.webservice.rest.jersey.JAXRS_BISystem.Resources;

import java.util.Calendar;

import javax.inject.Singleton;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ektorp.CouchDbConnector;
import org.ektorp.ViewQuery;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.DatabaseHandler;

/**
 * Root resource (exposed at "markets" path)
 * 
 * Created: 2013-10-31. Modified: 2013-11-27.
 * 
 * @author Robin Larsson
 * @version 0.9
 */
@Path("/markets")
@Singleton
@Produces(MediaType.APPLICATION_JSON)
public class MarketsResource {
    private DatabaseHandler dbHandler;
    private CouchDbConnector dbConnector;
    
    private ViewQuery queryAllMarketStockDayData = new ViewQuery();
    private ViewQuery queryMarketStockDayData = new ViewQuery();
    private ViewQuery queryAllMarketIndexDayData = new ViewQuery();
    private ViewQuery queryMarketIndexDayData = new ViewQuery();
    private ViewQuery queryAllMarketStockLatestData = new ViewQuery(); 
    
    private Calendar calendar = null;

    /**
     * Creates a new instance of MarketsResource.
     */
    public MarketsResource() {
	/* Creating a connection to the CouchDB database */
	this.dbHandler = new DatabaseHandler();
	this.dbConnector = this.dbHandler.getConnector();
    }

    // /**
    // * Returns the available market symbols.
    // *
    // * @return String that will be returned as a application/json response.
    // */
    // @GET
    // @Path("symbols")
    // public String getAvailableStockSymbols() {
    // return "{\"MarketSymbols\": \"Got it!\"}";
    // }
    
    /**
     * Returns a day of stock data for all the stock market exchanges.
     * 
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/stocks/all")
    public String getAllMarketStockDayData() {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
//	System.out.println("Start: " + Long.toString(
//		currentTime - (86400 *1000)));
//	System.out.println("End: " + Long.toString(
//		currentTime));
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryAllMarketStockDayData,
		"_design/bi",
		"lse, nyse, omx",
		Long.toString( // Start time, a day before current time
			calendar.getTimeInMillis() - (86400 * 1000)),
		Long.toString( // End time, current time
			calendar.getTimeInMillis()),
		false, true);
    }

    /**
     * Returns a day of stock data for a stock market exchange {@code symbol}.
     * 
     * @param symbol
     *            Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/stocks/{symbol}")
    public String getMarketStockDayData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
//	System.out.println("Start: " + Long.toString(
//		currentTime - (86400 *1000)));
//	System.out.println("End: " + Long.toString(
//		currentTime));
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryMarketStockDayData,
		"_design/bi",
		symbol.toLowerCase(),
		Long.toString( // Start time, a day before current time
			calendar.getTimeInMillis() - (86400 * 1000)),
		Long.toString( // End time, current time
			calendar.getTimeInMillis()),
		false, false);
    }
    
    /**
     * Returns a day of market index data exchange for all the
     * stock market exchanges.
     * 
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/index/all")
    public String getAllMarketIndexDayData() {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
//	System.out.println("Start: " + Long.toString(
//			currentTime - (86400 *1000)));
//	System.out.println("End: " + Long.toString(
//			currentTime));
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryAllMarketIndexDayData,
		"_design/bi",
		"lse_market, nyse_market, omx_market",
		Long.toString( // Start time, a day before current time
			calendar.getTimeInMillis() - (86400 * 1000)),
		Long.toString( // End time, current time
			calendar.getTimeInMillis()),
		false, true);
    }

    /**
     * Returns a day of market index data exchange for a stock market exchange
     * {@code symbol}.
     * 
     * @param symbol
     *            Stock market exchange index symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/index/{symbol}")
    public String getMarketIndexDayData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
//	System.out.println("Start: " + Long.toString(
//			currentTime - (86400 *1000)));
//	System.out.println("End: " + Long.toString(
//			currentTime));
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryMarketIndexDayData,
		"_design/bi",
		symbol.toLowerCase().concat("_market"),
		Long.toString( // Start time, a day before current time
			calendar.getTimeInMillis() - (86400 * 1000)),
		Long.toString( // End time, current time
			calendar.getTimeInMillis()),
		false, false);
    }
    
    /**
     * Returns the latest stock data from all the markets.
     * 
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("/latest/all")
    public String getAllMarketStockLatestData() {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryAllMarketStockLatestData,
		"_design/bi",
		"lse_latest, nyse_latest, omx_latest",
		true);
    }
}
