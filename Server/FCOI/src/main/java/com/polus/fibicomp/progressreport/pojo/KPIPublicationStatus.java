package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "KPI_PUBLICATION_STATUS")
public class KPIPublicationStatus implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "PUBLICATION_STATUS_CODE")
	private String publicationStatusCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Date updateTimestamp;

	@JsonIgnore	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public String getPublicationStatusCode() {
		return publicationStatusCode;
	}

	public void setPublicationStatusCode(String publicationStatusCode) {
		this.publicationStatusCode = publicationStatusCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Date getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Date updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}
}
