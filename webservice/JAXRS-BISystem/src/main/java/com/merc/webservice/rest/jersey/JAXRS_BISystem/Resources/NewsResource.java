package com.merc.webservice.rest.jersey.JAXRS_BISystem.Resources;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ektorp.CouchDbConnector;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.DesignDocModels.News;
import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.DatabaseHandler;

/**
 * Root resource (exposed at "news" path)
 * 
 * Modified: 2013-11-14.
 * 
 * @author Robin Larsson
 * @version 0.5
 */
@Path("/news")
@Produces(MediaType.APPLICATION_JSON)
public class NewsResource {
    private CouchDbConnector dbConnector;

    /**
     * Creates a new instance of NewsResource.
     */
    public NewsResource() {
	/* Creating a connection to the CouchDB database */
	this.dbConnector = new DatabaseHandler().getConnector();
    }

    /**
     * Returns the available stock news for {@code symbol}.
     * 
     * @param symbol Stock market exchange- or stock market index symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("all/stocks/{symbol}")
    public String getAvailableStockNewsData(@PathParam("symbol") String symbol) {
//	List<String> symbols = Arrays.asList(symbol.split(","));
//	return "{\"NewsData\":" + symbols.toString() + "}";
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 */
	return DatabaseHandler.retrieveJSONData(this.dbConnector, "_design/bi",
		symbol.toLowerCase(), News.class);
    }
}
