package com.merc.webservice.rest.jersey.JAXRS_BISystem.Resource;

import java.util.List;
import java.util.Arrays;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ektorp.CouchDbConnector;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handler.DatabaseHandler;

/**
 * Root resource (exposed at "stocks" path)
 * 
 * Modified: 2013-11-09.
 * 
 * @author Robin Larsson
 * @version 0.5
 */
@Path("/stocks")
@Produces(MediaType.APPLICATION_JSON)
public class StocksResource {
    private CouchDbConnector dbConnector;

    public StocksResource() {
	/* Creating a connection to the CouchDB database */
	this.dbConnector = new DatabaseHandler().getConnector();
    }
//    /**
//     * Returns the available stock symbols.
//     * 
//     * @return String that will be returned as a application/json response.
//     */
//    @GET
//    @Path("symbols")
//    public String getAvailableStockSymbols() {
//	return "{\"StockSymbols\": \"Got it!\"}";
//    }

    /**
     * Returns a day of stock data for {@code symbol}.
     * 
     * @param symbol Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("day/{symbol}")
    public String getDayStockData(@PathParam("symbol") String symbol) {
	List<String> symbols = Arrays.asList(symbol.split(","));
	return "{\"StockData\":" + symbols.toString() + "}";
    }
    
    /**
     * Returns a week of stock data for {@code symbol}.
     * 
     * @param symbol Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("week/{symbol}")
    public String getWeekStockData(@PathParam("symbol") String symbol) {
	List<String> symbols = Arrays.asList(symbol.split(","));
	return "{\"StockData\":" + symbols.toString() + "}";
    }
    
    /**
     * Returns a month of stock data for {@code symbol}.
     * 
     * @param symbol Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("month/{symbol}")
    public String getMonthStockData(@PathParam("symbol") String symbol) {
	List<String> symbols = Arrays.asList(symbol.split(","));
	return "{\"StockData\":" + symbols.toString() + "}";
    }
}
