package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SPONSOR_TERM_TYPE")
public class SponsorTermType  implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "SPONSOR_TERM_TYPE_CODE")
	private String sponsorTermTypeCode;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@JsonManagedReference
	@OneToMany(mappedBy = "sponsorTermType", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<SponsorTerm> sponsorTerms;

	public String getSponsorTermTypeCode() {
		return sponsorTermTypeCode;
	}

	public void setSponsorTermTypeCode(String sponsorTermTypeCode) {
		this.sponsorTermTypeCode = sponsorTermTypeCode;
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

	public List<SponsorTerm> getSponsorTerms() {
		return sponsorTerms;
	}

	public void setSponsorTerms(List<SponsorTerm> sponsorTerms) {
		this.sponsorTerms = sponsorTerms;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
