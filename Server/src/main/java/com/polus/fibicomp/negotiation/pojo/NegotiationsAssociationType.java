package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "NEGOTIATION_ASSOCIATION_TYPE")
public class NegotiationsAssociationType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ASSOCIATION_TYPE_CODE")
	private String associationTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@JsonIgnore
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getAssociationTypeCode() {
		return associationTypeCode;
	}

	public void setAssociationTypeCode(String associationTypeCode) {
		this.associationTypeCode = associationTypeCode;
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

}
