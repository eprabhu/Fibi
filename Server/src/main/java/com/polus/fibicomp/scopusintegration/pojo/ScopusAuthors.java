package com.polus.fibicomp.scopusintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "SCOPUS_AUTHOR")
@EntityListeners(AuditingEntityListener.class)
public class ScopusAuthors implements Serializable {

	private static final long serialVersionUID = 1L;

//	@Id
//	@Column(name = "AUTHOR_ID")
//	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AUTHOR")
//	@SequenceGenerator(name = "SEQ_AUTHOR", sequenceName = "SEQ_AUTHOR", allocationSize = 1)
//	private Integer authorId;
	
	@Id
	@Column(name = "AUTH_ID")
	private String scopusAuthorId;

	@Id
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "SCOPUS_AUTHOR_FK1"), name = "SCOPUS_ID", referencedColumnName = "SCOPUS_ID")
	private Scopus scopus;

	@Column(name = "AUTHOR_URL")
	private String authorUrl;

	@Column(name = "GIVEN_NAME")
	private String givenName;

	@Column(name = "AUTHOR_NAME")
	private String authorName;

	@Column(name = "SUR_NAME")
	private String surName;

	@Column(name = "INITIAL")
	private String initial;

	@Column(name = "AUTHOR_FLAG")
	private String authorFlag;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public String getAuthorUrl() {
		return authorUrl;
	}

	public void setAuthorUrl(String authorUrl) {
		this.authorUrl = authorUrl;
	}

	public String getGivenName() {
		return givenName;
	}

	public void setGivenName(String givenName) {
		this.givenName = givenName;
	}

	public String getAuthorName() {
		return authorName;
	}

	public void setAuthorName(String authorName) {
		this.authorName = authorName;
	}

	public String getSurName() {
		return surName;
	}

	public void setSurName(String surName) {
		this.surName = surName;
	}

	public String getInitial() {
		return initial;
	}

	public void setInitial(String initial) {
		this.initial = initial;
	}

	public Scopus getScopus() {
		return scopus;
	}

	public void setScopus(Scopus scopus) {
		this.scopus = scopus;
	}

	public String getAuthorFlag() {
		return authorFlag;
	}

	public void setAuthorFlag(String authorFlag) {
		this.authorFlag = authorFlag;
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

	public String getScopusAuthorId() {
		return scopusAuthorId;
	}

	public void setScopusAuthorId(String scopusAuthorId) {
		this.scopusAuthorId = scopusAuthorId;
	}
}
