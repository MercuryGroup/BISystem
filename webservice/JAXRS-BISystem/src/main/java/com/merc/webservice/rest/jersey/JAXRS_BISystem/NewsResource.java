package com.merc.webservice.rest.jersey.JAXRS_BISystem;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

/**
 * Root resource (exposed at "news" path)
 */
@Path("/news")
@Produces(MediaType.APPLICATION_JSON)
public class NewsResource {
    /**
     * Returns the available news for "symbol".
     *
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("{symbol}")
    public String getAvailableNewsData(@PathParam("symbol") String symbol) {
    	List<String> symbols = Arrays.asList(symbol.split(","));
        return "{\"NewsData\":" + symbols.toString() + "}";
    }
}
