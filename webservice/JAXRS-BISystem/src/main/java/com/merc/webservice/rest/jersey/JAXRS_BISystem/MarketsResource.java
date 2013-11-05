package com.merc.webservice.rest.jersey.JAXRS_BISystem;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * Root resource (exposed at "markets" path)
 */
@Path("/markets")
@Produces(MediaType.APPLICATION_JSON)
public class MarketsResource {
	/**
     * Returns the available market symbols.
     *
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("symbols")
    public String getAvailableStockSymbols() {
        return "{\"MarketSymbols\": \"Got it!\"}";
    }

	/**
     * Returns the available market data for "symbol".
     *
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("{symbol}")
    public String getAvailableMarketData(@PathParam("symbol") String symbol) {
    	List<String> symbols = Arrays.asList(symbol.split(","));
        return "{\"MarketData\":" + symbols.toString() + "}";
    }
}
