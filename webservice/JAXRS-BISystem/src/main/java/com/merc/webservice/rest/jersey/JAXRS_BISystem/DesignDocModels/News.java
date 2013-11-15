package com.merc.webservice.rest.jersey.JAXRS_BISystem.DesignDocModels;

import org.ektorp.support.CouchDbDocument;

/**
 * Model for news-based design documents, retrieved from CouchDB.
 * @author Robin Larsson
 * @version 0.5
 */
public class News extends CouchDbDocument {
    private String _id;
    private String title;
    private String link;
    private String description;
    private String guid;
    private String pubDate;
    private String type;
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
     * @return the title
     */
    public String getTitle() {
        return title;
    }
    /**
     * @param title the title to set
     */
    public void setTitle(String title) {
        this.title = title;
    }
    /**
     * @return the link
     */
    public String getLink() {
        return link;
    }
    /**
     * @param link the link to set
     */
    public void setLink(String link) {
        this.link = link;
    }
    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }
    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }
    /**
     * @return the guid
     */
    public String getGuid() {
        return guid;
    }
    /**
     * @param guid the guid to set
     */
    public void setGuid(String guid) {
        this.guid = guid;
    }
    /**
     * @return the pubDate
     */
    public String getPubDate() {
        return pubDate;
    }
    /**
     * @param pubDate the pubDate to set
     */
    public void setPubDate(String pubDate) {
        this.pubDate = pubDate;
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
    
    
}
