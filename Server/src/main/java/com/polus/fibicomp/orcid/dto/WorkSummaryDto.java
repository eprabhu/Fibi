package com.polus.fibicomp.orcid.dto;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "put-code",
    "created-date",
    "last-modified-date",
    "source",
    "title",
    "external-ids",
    "url",
    "type",
    "publication-date",
    "visibility",
    "path",
    "display-index",
    "journal-title"
})
public class WorkSummaryDto {

	@JsonProperty("put-code")
    private Integer putCode;

    @JsonProperty("created-date")
    private CreatedDateDto createdDate;

    @JsonProperty("last-modified-date")
    private LastModifiedDateDto lastModifiedDate;

    @JsonProperty("source")
    private SourceDto source;

    @JsonProperty("title")
    private TitleDto title;

    @JsonProperty("external-ids")
    private ExternalIdsDto externalIds;

    @JsonProperty("url")
    private UrlDto url;

    @JsonProperty("type")
    private String type;

    @JsonProperty("publication-date")
    private PublicationDateDto publicationDate;

    @JsonProperty("visibility")
    private String visibility;

    @JsonProperty("path")
    private String path;

    @JsonProperty("display-index")
    private String displayIndex;

    @JsonProperty("journal-title")
    private JournalTitleDto journalTitle;

    @JsonProperty("put-code")
	public Integer getPutCode() {
		return putCode;
	}

    @JsonProperty("put-code")
	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

    @JsonProperty("created-date")
	public CreatedDateDto getCreatedDate() {
		return createdDate;
	}

    @JsonProperty("created-date")
	public void setCreatedDate(CreatedDateDto createdDate) {
		this.createdDate = createdDate;
	}

    @JsonProperty("last-modified-date")
	public LastModifiedDateDto getLastModifiedDate() {
		return lastModifiedDate;
	}

    @JsonProperty("last-modified-date")
	public void setLastModifiedDate(LastModifiedDateDto lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
	}

    @JsonProperty("source")
	public SourceDto getSource() {
		return source;
	}

    @JsonProperty("source")
	public void setSource(SourceDto source) {
		this.source = source;
	}

    @JsonProperty("title")
	public TitleDto getTitle() {
		return title;
	}

    @JsonProperty("title")
	public void setTitle(TitleDto title) {
		this.title = title;
	}

    @JsonProperty("external-ids")
	public ExternalIdsDto getExternalIds() {
		return externalIds;
	}

    @JsonProperty("external-ids")
	public void setExternalIds(ExternalIdsDto externalIds) {
		this.externalIds = externalIds;
	}

    @JsonProperty("url")
    public UrlDto getUrl() {
		return url;
	}

    @JsonProperty("url")
	public void setUrl(UrlDto url) {
		this.url = url;
	}

	@JsonProperty("type")
	public String getType() {
		return type;
	}

    @JsonProperty("type")
	public void setType(String type) {
		this.type = type;
	}

    @JsonProperty("publication-date")
	public PublicationDateDto getPublicationDate() {
		return publicationDate;
	}

    @JsonProperty("publication-date")
	public void setPublicationDate(PublicationDateDto publicationDate) {
		this.publicationDate = publicationDate;
	}

    @JsonProperty("visibility")
	public String getVisibility() {
		return visibility;
	}

    @JsonProperty("visibility")
	public void setVisibility(String visibility) {
		this.visibility = visibility;
	}

    @JsonProperty("path")
	public String getPath() {
		return path;
	}

    @JsonProperty("path")
	public void setPath(String path) {
		this.path = path;
	}

	@JsonProperty("display-index")
	public String getDisplayIndex() {
		return displayIndex;
	}

	@JsonProperty("display-index")
	public void setDisplayIndex(String displayIndex) {
		this.displayIndex = displayIndex;
	}

	@JsonProperty("journal-title")
	public JournalTitleDto getJournalTitle() {
		return journalTitle;
	}

	@JsonProperty("journal-title")
	public void setJournalTitle(JournalTitleDto journalTitle) {
		this.journalTitle = journalTitle;
	}

}
