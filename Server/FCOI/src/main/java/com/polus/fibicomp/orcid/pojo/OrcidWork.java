package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "ORCID_WORK")
public class OrcidWork implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PUT_CODE")
	private Integer putCode;

	@Column(name = "ORCID_WORK_STATUS_CODE")
	private String orcidWorkStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_FK4"), name = "ORCID_WORK_STATUS_CODE", referencedColumnName = "ORCID_WORK_STATUS_CODE", insertable = false, updatable = false)
	private OrcidWorkStatus orcidWorkStatus;

	@Column(name = "SOURCE")
	private String source;

	@Column(name = "PATH")
	private String path;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "SUB_TITLE")
	private String subTitle;

	@Column(name = "TRANSLATED_TITLE")
	private String translatedTitle;

	@Column(name = "TRANSLATED_TITLE_LOCALE_CODE")
	private String translatedLocaleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_FK2"), name = "TRANSLATED_TITLE_LOCALE_CODE", referencedColumnName = "LOCALE_CODE", insertable = false, updatable = false)
	private Locale translatedLocale;

	@Column(name = "JOURNAL_TITLE")
	private String journalTitle;

	@Column(name = "SHORT_DESCRIPTION")
	private String shortDescription;

	@Column(name = "CITATION_VALUE")
	private String citationValue;

	@Column(name = "ORCID_WORK_TYPE_CODE")
	private String workTypeCode;

	@Column(name = "URL")
	private String url;

	@Column(name = "LOCALE_CODE")
	private String localeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_FK1"), name = "LOCALE_CODE", referencedColumnName = "LOCALE_CODE", insertable = false, updatable = false)
	private Locale locale;

	@Column(name = "COUNTRY_CODE")
	private String countryCode;

	@Column(name = "VISIBILITY")
	private String visibility;

	@Column(name = "CREATE_DATE")
	private Timestamp createDate;

	@Column(name = "LAST_MODIFIED_DATE")
	private Timestamp lastModifiedDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "orcidWork", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<OrcidWorkContributor> orcidWorkContributors;

	@JsonManagedReference
	@OneToMany(mappedBy = "orcidWork", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<OrcidWorkExternalIdentifier> orcidWorkExternalIdentifiers;

	@Column(name = "ORCID_WORK_CATEGORY_CODE")
	private String orcidWorkCategoryCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
    @JoinColumns(foreignKey = @ForeignKey(name = "ORCID_WORK_FK3"), value = { 
    	@JoinColumn(name = "ORCID_WORK_TYPE_CODE", referencedColumnName = "ORCID_WORK_TYPE_CODE", insertable = false, updatable = false), 
    	@JoinColumn(name = "ORCID_WORK_CATEGORY_CODE", referencedColumnName = "ORCID_WORK_CATEGORY_CODE", insertable = false, updatable = false) })
    private OrcidWorkType orcidWorkType;

	@Column(name = "ORCID_WORK_CITATION_TYPE_CODE")
	private String citationTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_FK5"), name = "ORCID_WORK_CITATION_TYPE_CODE", referencedColumnName = "ORCID_WORK_CITATION_TYPE_CODE ", insertable = false, updatable = false)
	private OrcidWorkCitationType citationType;

	@Column(name = "PUBLICATION_YEAR")
	private String publicationYear;

	@Column(name = "PUBLICATION_MONTH")
	private String publicationMonth;

	@Column(name = "PUBLICATION_DAY")
	private String publicationDay;

	@Transient
	private String countryName;

	public Integer getPutCode() {
		return putCode;
	}

	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

	public String getOrcidWorkStatusCode() {
		return orcidWorkStatusCode;
	}

	public void setOrcidWorkStatusCode(String orcidWorkStatusCode) {
		this.orcidWorkStatusCode = orcidWorkStatusCode;
	}

	public OrcidWorkStatus getOrcidWorkStatus() {
		return orcidWorkStatus;
	}

	public void setOrcidWorkStatus(OrcidWorkStatus orcidWorkStatus) {
		this.orcidWorkStatus = orcidWorkStatus;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getSubTitle() {
		return subTitle;
	}

	public void setSubTitle(String subTitle) {
		this.subTitle = subTitle;
	}

	public String getTranslatedTitle() {
		return translatedTitle;
	}

	public void setTranslatedTitle(String translatedTitle) {
		this.translatedTitle = translatedTitle;
	}

	public String getTranslatedLocaleCode() {
		return translatedLocaleCode;
	}

	public void setTranslatedLocaleCode(String translatedLocaleCode) {
		this.translatedLocaleCode = translatedLocaleCode;
	}

	public Locale getTranslatedLocale() {
		return translatedLocale;
	}

	public void setTranslatedLocale(Locale translatedLocale) {
		this.translatedLocale = translatedLocale;
	}

	public String getJournalTitle() {
		return journalTitle;
	}

	public void setJournalTitle(String journalTitle) {
		this.journalTitle = journalTitle;
	}

	public String getShortDescription() {
		return shortDescription;
	}

	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}

	public String getCitationValue() {
		return citationValue;
	}

	public void setCitationValue(String citationValue) {
		this.citationValue = citationValue;
	}

	public String getWorkTypeCode() {
		return workTypeCode;
	}

	public void setWorkTypeCode(String workTypeCode) {
		this.workTypeCode = workTypeCode;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getLocaleCode() {
		return localeCode;
	}

	public void setLocaleCode(String localeCode) {
		this.localeCode = localeCode;
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public String getVisibility() {
		return visibility;
	}

	public void setVisibility(String visibility) {
		this.visibility = visibility;
	}

	public Timestamp getCreateDate() {
		return createDate;
	}

	public void setCreateDate(Timestamp createDate) {
		this.createDate = createDate;
	}

	public Timestamp getLastModifiedDate() {
		return lastModifiedDate;
	}

	public void setLastModifiedDate(Timestamp lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<OrcidWorkContributor> getOrcidWorkContributors() {
		return orcidWorkContributors;
	}

	public void setOrcidWorkContributors(List<OrcidWorkContributor> orcidWorkContributors) {
		this.orcidWorkContributors = orcidWorkContributors;
	}

	public List<OrcidWorkExternalIdentifier> getOrcidWorkExternalIdentifiers() {
		return orcidWorkExternalIdentifiers;
	}

	public void setOrcidWorkExternalIdentifiers(List<OrcidWorkExternalIdentifier> orcidWorkExternalIdentifiers) {
		this.orcidWorkExternalIdentifiers = orcidWorkExternalIdentifiers;
	}

	public String getOrcidWorkCategoryCode() {
		return orcidWorkCategoryCode;
	}

	public void setOrcidWorkCategoryCode(String orcidWorkCategoryCode) {
		this.orcidWorkCategoryCode = orcidWorkCategoryCode;
	}

	public OrcidWorkType getOrcidWorkType() {
		return orcidWorkType;
	}

	public void setOrcidWorkType(OrcidWorkType orcidWorkType) {
		this.orcidWorkType = orcidWorkType;
	}

	public String getCountryName() {
		return countryName;
	}

	public void setCountryName(String countryName) {
		this.countryName = countryName;
	}

	public String getCitationTypeCode() {
		return citationTypeCode;
	}

	public void setCitationTypeCode(String citationTypeCode) {
		this.citationTypeCode = citationTypeCode;
	}

	public void setCitationType(OrcidWorkCitationType citationType) {
		this.citationType = citationType;
	}

	public OrcidWorkCitationType getCitationType() {
		return citationType;
	}

	public String getPublicationYear() {
		return publicationYear;
	}

	public void setPublicationYear(String publicationYear) {
		this.publicationYear = publicationYear;
	}

	public String getPublicationMonth() {
		return publicationMonth;
	}

	public void setPublicationMonth(String publicationMonth) {
		this.publicationMonth = publicationMonth;
	}

	public String getPublicationDay() {
		return publicationDay;
	}

	public void setPublicationDay(String publicationDay) {
		this.publicationDay = publicationDay;
	}

}
