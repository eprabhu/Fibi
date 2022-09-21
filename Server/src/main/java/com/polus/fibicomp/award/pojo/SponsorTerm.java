package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "SPONSOR_TERM")
public class SponsorTerm implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SPONSOR_TERM_ID")
	private Integer sponsorTermId;

	@Column(name = "SPONSOR_TERM_CODE")
	private String sponsorTermCode;

	@Column(name = "SPONSOR_TERM_TYPE_CODE")
	private String sponsorTermTypeCode;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_TERM_FK"), name = "SPONSOR_TERM_TYPE_CODE", referencedColumnName = "SPONSOR_TERM_TYPE_CODE", insertable = false, updatable = false)
	private SponsorTermType sponsorTermType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getSponsorTermCode() {
		return sponsorTermCode;
	}

	public void setSponsorTermCode(String sponsorTermCode) {
		this.sponsorTermCode = sponsorTermCode;
	}

	public String getSponsorTermTypeCode() {
		return sponsorTermTypeCode;
	}

	public void setSponsorTermTypeCode(String sponsorTermTypeCode) {
		this.sponsorTermTypeCode = sponsorTermTypeCode;
	}

	public SponsorTermType getSponsorTermType() {
		return sponsorTermType;
	}

	public void setSponsorTermType(SponsorTermType sponsorTermType) {
		this.sponsorTermType = sponsorTermType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getSponsorTermId() {
		return sponsorTermId;
	}

	public void setSponsorTermId(Integer sponsorTermId) {
		this.sponsorTermId = sponsorTermId;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
