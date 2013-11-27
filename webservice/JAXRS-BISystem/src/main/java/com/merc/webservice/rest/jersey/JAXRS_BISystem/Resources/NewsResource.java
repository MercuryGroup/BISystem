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
 * Root resource (exposed at "news" path)
 * 
 * Created: 2013-10-31. Modified: 2013-11-27.
 * 
 * @author Robin Larsson
 * @version 0.9
 */
@Path("/news")
@Singleton
@Produces(MediaType.APPLICATION_JSON)
public class NewsResource {
    private DatabaseHandler dbHandler;
    private CouchDbConnector dbConnector;
    
    private ViewQuery queryNewsStockDayData = new
	    ViewQuery();
    private ViewQuery queryNewsIndexDayData = new
	    ViewQuery();
    
    private Calendar calendar = null;

    /**
     * Creates a new instance of NewsResource.
     */
    public NewsResource() {
	/* Creating a connection to the CouchDB database */
	this.dbHandler = new DatabaseHandler();
	this.dbConnector = this.dbHandler.getConnector();
    }

    /**
     * Returns a day of stock news for {@code symbol}.
     * 
     * @param symbol Stock market exchange symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("/stocks/day/{symbol}")
    public String getNewsStockDayData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(
		symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(
		calendar.getTimeInMillis() - (86400 *1000)).concat("\""));
	startKey.append(']');
	
	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(
		symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(
		calendar.getTimeInMillis()).concat("\""));
	endKey.append(']');
	
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryNewsStockDayData,
		"_design/bi",
		"news",
		startKey.toString() ,
		endKey.toString(),
		true, false);
    }
    
    /**
     * Returns a day of market index news for {@code symbol}.
     * 
     * @param symbol Stock market exchange index symbol.
     * @return String that will be returned as a application/json response.
     */
    @GET
    @Path("/index/day/{symbol}")
    public String getNewsIndexDayData(@PathParam("symbol") String symbol) {
	/*
	 * Retrieves and returns data from the CouchDB database based on the
	 * supplied parameters.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	calendar = Calendar.getInstance();
	calendar.set(Calendar.HOUR_OF_DAY, 0);
	calendar.set(Calendar.MINUTE, 0);
	calendar.set(Calendar.SECOND, 0);
	calendar.set(Calendar.MILLISECOND, 0);
	
	StringBuilder startKey = new StringBuilder();
	startKey.append('[');
	startKey.append("\"".concat(
		symbol.toUpperCase()));
	startKey.append("\",\"");
	startKey.append(Long.toString(
		calendar.getTimeInMillis() - (86400 *1000)).concat("\""));
	startKey.append(']');
	
	StringBuilder endKey = new StringBuilder();
	endKey.append('[');
	endKey.append("\"".concat(
		symbol.toUpperCase()));
	endKey.append("\",\"");
	endKey.append(Long.toString(
		calendar.getTimeInMillis()).concat("\""));
	endKey.append(']');
	
	return this.dbHandler.retrieveJSONData(this.dbConnector,
		queryNewsIndexDayData,
		"_design/bi",
		"news_market",
		startKey.toString(),
		endKey.toString(),
		true, false);
    }
}
