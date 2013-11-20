package com.merc.webservice.rest.jersey.JAXRS_BISystem.Resources;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ektorp.CouchDbConnector;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.DatabaseHandler;

/**
 * Root resource (exposed at "stocks" path)
 * 
 * Created: 2013-10-31. Modified: 2013-11-20.
 * 
 * @author Robin Larsson
 * @version 0.9
 */
@Path("/stocks")
@Produces(MediaType.APPLICATION_JSON)
public class StocksResource {
    private CouchDbConnector dbConnector;

    /**
     * Creates a new instance of StocksResource.
     */
    public StocksResource() {
	/* Creating a connection to the CouchDB database */
	this.dbConnector = new DatabaseHandler().getConnector();
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
	long currentTime = System.currentTimeMillis();
//	System.out.println("Day [\""
//		.concat(symbol.toUpperCase())
//		.concat("\",\"")
//		.concat(Long.toString(currentTime - (86400 * 1000)))
//		.concat("\"]")
//		.concat("[\"".concat(symbol.toUpperCase()).concat("\",\"")
//			.concat(Long.toString(currentTime)).concat("\"]")));

	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(currentTime - (86400 * 1000))
		.concat("\""));
	startKey.append(']');

	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(currentTime).concat("\""));
	endKey.append(']');

	return DatabaseHandler.retrieveJSONData(this.dbConnector, "_design/bi",
		"lse_stock, nyse_stock, omx_stock",
		/*
		 * Start time, a day before current time
		 */
		startKey.toString(),
		/*
		 * End time, current time
		 */
		endKey.toString(),
		true);
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
	long currentTime = System.currentTimeMillis();
//	System.out.println("Week [\""
//		.concat(symbol.toUpperCase())
//		.concat("\",\"")
//		.concat(Long.toString(currentTime - (604800 * 1000)))
//		.concat("\"]")
//		.concat("[\"".concat(symbol.toUpperCase()).concat("\",\"")
//			.concat(Long.toString(currentTime)).concat("\"]")));
	
	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(currentTime - (604800 * 1000)).concat(
		"\""));
	startKey.append(']');

	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(currentTime).concat("\""));
	endKey.append(']');

	return DatabaseHandler.retrieveJSONData(this.dbConnector, "_design/bi",
		"lse_stock, nyse_stock, omx_stock",
		/*
		 * Start time, a week before current time
		 */
		startKey.toString(),
		/*
		 * End time, current time
		 */
		endKey.toString(),
		true);
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
	long currentTime = System.currentTimeMillis();

	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(currentTime + (2592000 * 1000)).concat(
		"\""));
	startKey.append(']');
	
//	System.out.println(startKey.toString());

	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(currentTime).concat("\""));
	endKey.append(']');

	return DatabaseHandler.retrieveJSONData(this.dbConnector, "_design/bi",
		"lse_stock, nyse_stock, omx_stock",
		/*
		 * Start time, a month before current time
		 */
		startKey.toString(),
		/*
		 * End time, current time
		 */
		endKey.toString(),
		true);
    }
}
