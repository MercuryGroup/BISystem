package com.merc.webservice.rest.jersey.JAXRS_BISystem.Resource;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ektorp.CouchDbConnector;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.DesignDocModels.Stocks;
import com.merc.webservice.rest.jersey.JAXRS_BISystem.DesignDocModels.Markets;
import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handler.DatabaseHandler;

/**
 * Root resource (exposed at "markets" path)
 * 
 * Modified: 2013-11-09.
 * 
 * @author Robin Larsson
 * @version 0.5
 */
@Path("/markets")
@Produces(MediaType.APPLICATION_JSON)
public class MarketsResource {
    private CouchDbConnector dbConnector;

    public MarketsResource() {
	/* Creating a connection to the CouchDB database */
	this.dbConnector = new DatabaseHandler().getConnector();
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
     * Returns a day of stock data for a stock market exchange {@code symbol}.
     * 
     * @param symbol
     *            Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/stocks/{symbol}")
    public String getMarketStockDayData(@PathParam("symbol") String symbol) {
	// List<String> symbols = Arrays.asList(symbol.split(","));
	// return "{\"MarketData\":" + symbols.toString() + "}";
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 */
	return DatabaseHandler.retrieveJSONData(this.dbConnector, "_design/bi",
		symbol.toLowerCase(),
		Long.toString( // Start time, a day before current time
			System.currentTimeMillis() - (86400 * 1000)),
		Long.toString( // End time, current time
			System.currentTimeMillis()), Stocks.class);
	// return DatabaseHandler.retrieveRawJSONData(this.dbConnector,
	// "_design/bi", symbol.toLowerCase());
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
	// List<String> symbols = Arrays.asList(symbol.split(","));
	// return "{\"MarketData\":" + symbols.toString() + "}";
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 */
	return DatabaseHandler.retrieveJSONData(this.dbConnector, "_design/bi",
		symbol.toLowerCase().concat("_market"),
		Long.toString( // Start time, a day before current time
			System.currentTimeMillis() - (86400 * 1000)),
		Long.toString( // End time, current time
			System.currentTimeMillis()), Markets.class);
//	return DatabaseHandler.retrieveRawJSONData(this.dbConnector,
//		"_design/bi", symbol.toLowerCase().concat("_market"));
    }
}
