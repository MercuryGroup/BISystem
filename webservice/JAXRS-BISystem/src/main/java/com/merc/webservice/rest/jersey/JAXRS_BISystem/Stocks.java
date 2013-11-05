package com.merc.webservice.rest.jersey.JAXRS_BISystem;

import java.util.List;
import java.util.Arrays;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * Root resource (exposed at "stocks" path)
 */
@Path("/stocks")
@Produces(MediaType.APPLICATION_JSON)
public class Stocks {
	/**
     * Returns the available stock symbols.
     *
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("symbols")
    public String getAvailableStockSymbols() {
        return "{\"StockSymbols\": \"Got it!\"}";
    }
    
	/**
     * Returns the available stock data for "symbol".
     *
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("{symbol}")
    public String getAvailableStockData(@PathParam("symbol") String symbol) {
    	List<String> symbols = Arrays.asList(symbol.split(","));
        return "{\"StockData\":" + symbols.toString() + "}";
    }
}
