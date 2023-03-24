package com.polus.fibicomp.scopusintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "SCOPUS")
@EntityListeners(AuditingEntityListener.class)
public class Scopus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SCOPUS_ID")
	private String scopusId;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "COVER_DATE")
	private String coverDate;

	@Column(name = "COVER_DISPLAY_DATE")
	private String coverDisplayDate;

	@Column(name = "SOURCE_TITLE")
	private String sourceTitle;

	@Column(name = "ISSN")
	private String issn;

	@Column(name = "SOURCE_TYPE")
	private String sourceType;

	@Column(name = "CITATIONS")
	private Integer citations;

	@Column(name = "REFERENCE")
	private String reference;

	@Column(name = "DOI")
	private String doi;

	@Column(name = "PUBLICATION_TYPE")
	private String publicationType;

	@Column(name = "PUB_MEDIA_ID")
	private String pubMedId;

	@Column(name = "CREATOR")
	private String creator;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonManagedReference
	@OneToMany(mappedBy = "scopus", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ScopusAuthors> scopusAuthors;

	@JsonManagedReference
	@OneToMany(mappedBy = "scopus", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ScopusAffiliation> scopusAffiliations;

	@JsonManagedReference
	@OneToMany(mappedBy = "scopus", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ScopusMetrics> scopusMetrics;

	@Transient
	private String authors;

	@Transient
	private Integer noOfAuthors;

	@Transient
	private String affiliations;

	@Transient
	private Integer noOfAffiliations;

	public String getScopusId() {
		return scopusId;
	}

	public void setScopusId(String scopusId) {
		this.scopusId = scopusId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getSourceTitle() {
		return sourceTitle;
	}

	public void setSourceTitle(String sourceTitle) {
		this.sourceTitle = sourceTitle;
	}

	public String getIssn() {
		return issn;
	}

	public void setIssn(String issn) {
		this.issn = issn;
	}

	public String getSourceType() {
		return sourceType;
	}

	public void setSourceType(String sourceType) {
		this.sourceType = sourceType;
	}

	public Integer getCitations() {
		return citations;
	}

	public void setCitations(Integer citations) {
		this.citations = citations;
	}

	public String getReference() {
		return reference;
	}

	public void setReference(String reference) {
		this.reference = reference;
	}

	public String getDoi() {
		return doi;
	}

	public void setDoi(String doi) {
		this.doi = doi;
	}

	public String getPublicationType() {
		return publicationType;
	}

	public void setPublicationType(String publicationType) {
		this.publicationType = publicationType;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getPubMedId() {
		return pubMedId;
	}

	public void setPubMedId(String pubMedId) {
		this.pubMedId = pubMedId;
	}

	public List<ScopusAuthors> getScopusAuthors() {
		return scopusAuthors;
	}

	public void setScopusAuthors(List<ScopusAuthors> scopusAuthors) {
		this.scopusAuthors = scopusAuthors;
	}

	public List<ScopusAffiliation> getScopusAffiliations() {
		return scopusAffiliations;
	}

	public void setScopusAffiliations(List<ScopusAffiliation> scopusAffiliations) {
		this.scopusAffiliations = scopusAffiliations;
	}

	public List<ScopusMetrics> getScopusMetrics() {
		return scopusMetrics;
	}

	public void setScopusMetrics(List<ScopusMetrics> scopusMetrics) {
		this.scopusMetrics = scopusMetrics;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getCoverDisplayDate() {
		return coverDisplayDate;
	}

	public void setCoverDisplayDate(String coverDisplayDate) {
		this.coverDisplayDate = coverDisplayDate;
	}

	public String getCoverDate() {
		return coverDate;
	}

	public void setCoverDate(String coverDate) {
		this.coverDate = coverDate;
	}

	public String getAuthors() {
		if (scopusAuthors != null && !scopusAuthors.isEmpty()) {
			this.setNoOfAuthors(scopusAuthors.size());
			StringBuilder result = new StringBuilder();
			for (ScopusAuthors author : scopusAuthors) {
		        result.append(author.getAuthorName());
		        result.append(", ");
		    }
			authors = result.length() > 0 ? result.substring(0, result.length() - 2): "";
		}
		return authors;
	}

	public void setAuthors(String authors) {
		this.authors = authors;
	}

	public Integer getNoOfAuthors() {
		return noOfAuthors;
	}

	public void setNoOfAuthors(Integer noOfAuthors) {
		this.noOfAuthors = noOfAuthors;
	}

	public String getAffiliations() {
		if (scopusAffiliations != null && !scopusAffiliations.isEmpty()) {
			this.setNoOfAffiliations(scopusAffiliations.size());
			StringBuilder result = new StringBuilder();
			for (ScopusAffiliation affiliation : scopusAffiliations) {
		        result.append(affiliation.getName());
		        result.append(", ");
		    }
			affiliations = result.length() > 0 ? result.substring(0, result.length() - 2): "";
		}
		return affiliations;
	}

	public void setAffiliations(String affiliations) {
		this.affiliations = affiliations;
	}

	public Integer getNoOfAffiliations() {
		return noOfAffiliations;
	}

	public void setNoOfAffiliations(Integer noOfAffiliations) {
		this.noOfAffiliations = noOfAffiliations;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
}
