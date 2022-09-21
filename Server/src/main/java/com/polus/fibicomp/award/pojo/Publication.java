package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "PUBLICATION")
public class Publication implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PUBLICATION_ID")
	private Integer publicationId;

	@Column(name = "TYPE")
	private String publicationType;

	@Column(name = "STATUS")
	private String publicationStatus;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "YEAR")
	private String year;

	@Column(name = "VOLUME")
	private String volume;

	@Column(name = "PAGE")
	private String page;

	@Column(name = "EDITION")
	private String edition;

	@Column(name = "SCHOOL_OF_AUTHOR")
	private String schoolOfAuthor;

	@Column(name = "ISSN")
	private String issn;

	@Column(name = "REVIEW_STATUS")
	private String reviewStatus;

	@Column(name = "AUTHORS")
	private String author;

	@Column(name = "AUTHORGANIZATION")
	private String authorOrganisation;

	@Column(name = "URL")
	private String url;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "NAME_OF_JOURNAL")
	private String nameOfJournal;

	public Integer getPublicationId() {
		return publicationId;
	}

	public void setPublicationId(Integer publicationId) {
		this.publicationId = publicationId;
	}

	public String getPublicationType() {
		return publicationType;
	}

	public void setPublicationType(String publicationType) {
		this.publicationType = publicationType;
	}

	public String getPublicationStatus() {
		return publicationStatus;
	}

	public void setPublicationStatus(String publicationStatus) {
		this.publicationStatus = publicationStatus;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public String getVolume() {
		return volume;
	}

	public void setVolume(String volume) {
		this.volume = volume;
	}

	public String getPage() {
		return page;
	}

	public void setPage(String page) {
		this.page = page;
	}

	public String getEdition() {
		return edition;
	}

	public void setEdition(String edition) {
		this.edition = edition;
	}

	public String getSchoolOfAuthor() {
		return schoolOfAuthor;
	}

	public void setSchoolOfAuthor(String schoolOfAuthor) {
		this.schoolOfAuthor = schoolOfAuthor;
	}

	public String getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public String getAuthorOrganisation() {
		return authorOrganisation;
	}

	public void setAuthorOrganisation(String authorOrganisation) {
		this.authorOrganisation = authorOrganisation;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
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

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getNameOfJournal() {
		return nameOfJournal;
	}

	public void setNameOfJournal(String nameOfJournal) {
		this.nameOfJournal = nameOfJournal;
	}

	public String getIssn() {
		return issn;
	}

	public void setIssn(String issn) {
		this.issn = issn;
	}

}
