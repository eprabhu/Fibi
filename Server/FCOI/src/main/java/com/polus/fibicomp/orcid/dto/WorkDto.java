package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "created-date",
    "last-modified-date",
    "source",
    "put-code",
    "path",
    "title",
    "journal-title",
    "short-description",
    "citation",
    "type",
    "publication-date",
    "external-ids",
    "url",
    "contributors",
    "language-code",
    "country",
    "visibility"
})
public class WorkDto {

	@JsonProperty("created-date")
    private CreatedDateDto createdDate;

    @JsonProperty("last-modified-date")
    private LastModifiedDateDto lastModifiedDate;

    @JsonProperty("source")
    private SourceDto source;

    @JsonProperty("put-code")
    private Integer putCode;

    @JsonProperty("path")
    private String path;

    @JsonProperty("title")
    private TitleDto title;

    @JsonProperty("journal-title")
    private JournalTitleDto journalTitle;

    @JsonProperty("short-description")
    private String shortDescription;

    @JsonProperty("citation")
    private CitationDto citation;

    @JsonProperty("type")
    private String type;

    @JsonProperty("publication-date")
    private PublicationDateDto publicationDate;

    @JsonProperty("external-ids")
    private ExternalIdsDto externalIds;

    @JsonProperty("url")
    private UrlDto url;

    @JsonProperty("contributors")
    private ContributorsDto contributors;

    @JsonProperty("language-code")
    private String languageCode;

    @JsonProperty("country")
    private CountryDto country;

    @JsonProperty("visibility")
    private String visibility;

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

    @JsonProperty("put-code")
	public Integer getPutCode() {
		return putCode;
	}

    @JsonProperty("put-code")
	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

    @JsonProperty("path")
	public String getPath() {
		return path;
	}

    @JsonProperty("path")
	public void setPath(String path) {
		this.path = path;
	}

	@JsonProperty("title")
	public TitleDto getTitle() {
		return title;
	}

	@JsonProperty("title")
	public void setTitle(TitleDto title) {
		this.title = title;
	}

	@JsonProperty("journal-title")
	public JournalTitleDto getJournalTitle() {
		return journalTitle;
	}

	@JsonProperty("journal-title")
	public void setJournalTitle(JournalTitleDto journalTitle) {
		this.journalTitle = journalTitle;
	}

	@JsonProperty("short-description")
	public String getShortDescription() {
		return shortDescription;
	}

	@JsonProperty("short-description")
	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}

	@JsonProperty("citation")
	public CitationDto getCitation() {
		return citation;
	}

	@JsonProperty("citation")
	public void setCitation(CitationDto citation) {
		this.citation = citation;
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

	@JsonProperty("contributors")
	public ContributorsDto getContributors() {
		return contributors;
	}

	@JsonProperty("contributors")
	public void setContributors(ContributorsDto contributors) {
		this.contributors = contributors;
	}

	@JsonProperty("language-code")
	public String getLanguageCode() {
		return languageCode;
	}

	@JsonProperty("language-code")
	public void setLanguageCode(String languageCode) {
		this.languageCode = languageCode;
	}

	@JsonProperty("country")
	public CountryDto getCountry() {
		return country;
	}

	@JsonProperty("country")
	public void setCountry(CountryDto country) {
		this.country = country;
	}

	@JsonProperty("visibility")
	public String getVisibility() {
		return visibility;
	}

	@JsonProperty("visibility")
	public void setVisibility(String visibility) {
		this.visibility = visibility;
	}

}
