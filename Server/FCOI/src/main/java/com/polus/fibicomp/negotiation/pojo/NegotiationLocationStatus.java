package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "NEGOTIATION_LOCATION_STATUS")
public class NegotiationLocationStatus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "LOCATION_STATUS_CODE")
	private String locationStatusCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

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

	public String getLocationStatusCode() {
		return locationStatusCode;
	}

	public void setLocationStatusCode(String locationStatusCode) {
		this.locationStatusCode = locationStatusCode;
	}

}
