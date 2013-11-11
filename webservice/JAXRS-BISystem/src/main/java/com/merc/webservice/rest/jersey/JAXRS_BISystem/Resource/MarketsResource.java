package com.merc.webservice.rest.jersey.JAXRS_BISystem;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ektorp.CouchDbConnector;

/**
 * Root resource (exposed at "markets" path)
 * 
 * Modified: 2013-11-07.
 * @author Robin Larsson
 * @version 0.5
 */
@Path("/markets")
@Produces(MediaType.APPLICATION_JSON)
public class MarketsResource {
    private CouchDbConnector dbConnector;
    
    public MarketsResource() {
	/* Creating a connection to the CouchDB database */
	dbConnector = new DatabaseHandler().getConnector();
    }
//    /**
//     * Returns the available market symbols.
//     * 
//     * @return String that will be returned as a application/json response.
//     */
//    @GET
//    @Path("symbols")
//    public String getAvailableStockSymbols() {
//	return "{\"MarketSymbols\": \"Got it!\"}";
//    }

    /**
     * Returns the available stock market data for "symbol".
     * @param symbol A single stock market symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("stocks/{symbol}")
    public String getAvailableMarketData(@PathParam("symbol") String symbol) {
//	List<String> symbols = Arrays.asList(symbol.split(","));
//	return "{\"MarketData\":" + symbols.toString() + "}";
	/*
	 * Retrieves and returns data from the CouchDB database
	 * based on the supplied parameters.
	 */
	return DatabaseHandler.retrieveRawJSONData(this.dbConnector,
		"_design/bi", symbol.toLowerCase());
    }
    
    /**
     * Returns the available stock market index data for "symbol".
     * 
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("index/{symbol}")
    public String getAvailableMarketIndexData(@PathParam("symbol") String symbol) {
//	List<String> symbols = Arrays.asList(symbol.split(","));
//	return "{\"MarketData\":" + symbols.toString() + "}";
	/*
	 * Retrieves and returns data from the CouchDB database
	 * based on the supplied parameters.
	 */
	return DatabaseHandler.retrieveRawJSONData(this.dbConnector,
		"_design/bi", symbol.toLowerCase().concat("_market"));
    }
}
