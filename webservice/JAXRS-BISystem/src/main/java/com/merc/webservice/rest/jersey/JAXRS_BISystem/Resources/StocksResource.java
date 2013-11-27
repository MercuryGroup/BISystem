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
import org.ektorp.support.View;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.DatabaseHandler;

/**
 * Root resource (exposed at "stocks" path)
 * 
 * Created: 2013-10-31. Modified: 2013-11-25.
 * 
 * @author Robin Larsson
 * @version 0.9
 */
@Path("/stocks")
@Singleton
@Produces(MediaType.APPLICATION_JSON)
public class StocksResource {
    private DatabaseHandler dbHandler;
    private CouchDbConnector dbConnector;

    private ViewQuery queryStockDayData = new ViewQuery();
    private ViewQuery queryStockWeekData = new ViewQuery();
    private ViewQuery queryStockMonthData = new ViewQuery();
    
    private Calendar calendar = null;
    
    /**
     * Creates a new instance of StocksResource.
     */
    public StocksResource() {
	/* Creating a connection to the CouchDB database */
	this.dbHandler = new DatabaseHandler();
	this.dbConnector = this.dbHandler.getConnector();
    }

    /**
     * Returns a day of stock data for {@code symbol}.
     * 
     * @param symbol
     *            Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/{symbol}")
    public String getDayStockData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
//	System.out.println("Day [\""
//		.concat(symbol.toUpperCase())
//		.concat("\",\"")
//		.concat(Long.toString(currentTime - (86400 * 1000)))
//		.concat("\"]")
//		.concat("[\"".concat(symbol.toUpperCase()).concat("\",\"")
//			.concat(Long.toString(currentTime)).concat("\"]")));
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(
		calendar.getTimeInMillis() - (86400 * 1000))
		.concat("\""));
	startKey.append(']');

	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(
		calendar.getTimeInMillis()).concat("\""));
	endKey.append(']');

	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryStockDayData,
		"_design/bi",
		"lse_stock, nyse_stock, omx_stock",
		/*
		 * Start time, a day before current time
		 */
		startKey.toString(),
		/*
		 * End time, current time
		 */
		endKey.toString(),
		true, true);
    }

    /**
     * Returns a week of stock data for {@code symbol}.
     * 
     * @param symbol
     *            Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("week/{symbol}")
    public String getWeekStockData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
//	System.out.println("Week [\""
//		.concat(symbol.toUpperCase())
//		.concat("\",\"")
//		.concat(Long.toString(currentTime - (604800 * 1000)))
//		.concat("\"]")
//		.concat("[\"".concat(symbol.toUpperCase()).concat("\",\"")
//			.concat(Long.toString(currentTime)).concat("\"]")));
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(
		calendar.getTimeInMillis() - (604800 * 1000)).concat(
		"\""));
	startKey.append(']');

	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(
		calendar.getTimeInMillis()).concat("\""));
	endKey.append(']');

	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryStockWeekData,
		"_design/bi",
		"lse_stock, nyse_stock, omx_stock",
		/*
		 * Start time, a week before current time
		 */
		startKey.toString(),
		/*
		 * End time, current time
		 */
		endKey.toString(),
		true, true);
    }

    /**
     * Returns a month of stock data for {@code symbol}.
     * 
     * @param symbol
     *            Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("month/{symbol}")
    public String getMonthStockData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
//	long currentTime = System.currentTimeMillis();
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);

	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(
		calendar.getTimeInMillis() + (2592000 * 1000)).concat(
		"\""));
	startKey.append(']');
	
//	System.out.println(startKey.toString());

	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(
		calendar.getTimeInMillis()).concat("\""));
	endKey.append(']');

	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryStockMonthData,
		"_design/bi",
		"lse_stock, nyse_stock, omx_stock",
		/*
		 * Start time, a month before current time
		 */
		startKey.toString(),
		/*
		 * End time, current time
		 */
		endKey.toString(),
		true, true);
    }
}
