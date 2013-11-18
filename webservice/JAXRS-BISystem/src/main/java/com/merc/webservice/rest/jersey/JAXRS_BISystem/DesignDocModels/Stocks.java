package com.merc.webservice.rest.jersey.JAXRS_BISystem.DesignDocModels;

import org.ektorp.support.CouchDbDocument;

/**
 * Model for stock-based design documents, retrieved from CouchDB.
 * @author Robin Larsson
 * @version 0.5
 */
public class Stocks extends CouchDbDocument {
    private String _id;
    private String change;
    private String latest;
    private String market;
    private String name;
    private String openVal;
    private String percent;
    private String symbol;
    private String type;
    private String updated;
    /**
     * @return the _id
     */
    public String get_id() {
        return _id;
    }
    /**
     * @param _id the _id to set
     */
    public void set_id(String _id) {
        this._id = _id;
    }
    /**
     * @return the change
     */
    public String getChange() {
        return change;
    }
    /**
     * @param change the change to set
     */
    public void setChange(String change) {
        this.change = change;
    }
    /**
     * @return the latest
     */
    public String getLatest() {
        return latest;
    }
    /**
     * @param latest the latest to set
     */
    public void setLatest(String latest) {
        this.latest = latest;
    }
    /**
     * @return the market
     */
    public String getMarket() {
        return market;
    }
    /**
     * @param market the market to set
     */
    public void setMarket(String market) {
        this.market = market;
    }
    /**
     * @return the name
     */
    public String getName() {
        return name;
    }
    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }
    /**
     * @return the openVal
     */
    public String getOpenVal() {
        return openVal;
    }
    /**
     * @param openVal the openVal to set
     */
    public void setOpenVal(String openVal) {
        this.openVal = openVal;
    }
    /**
     * @return the percent
     */
    public String getPercent() {
        return percent;
    }
    /**
     * @param percent the percent to set
     */
    public void setPercent(String percent) {
        this.percent = percent;
    }
    /**
     * @return the symbol
     */
    public String getSymbol() {
        return symbol;
    }
    /**
     * @param symbol the symbol to set
     */
    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }
    /**
     * @return the type
     */
    public String getType() {
        return type;
    }
    /**
     * @param type the type to set
     */
    public void setType(String type) {
        this.type = type;
    }
    /**
     * @return the updated
     */
    public String getUpdated() {
        return updated;
    }
    /**
     * @param updated the updated to set
     */
    public void setUpdated(String updated) {
        this.updated = updated;
    }
}
