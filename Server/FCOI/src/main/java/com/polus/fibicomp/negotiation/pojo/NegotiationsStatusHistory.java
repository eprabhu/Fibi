package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "NEGOTIATION_STATUS_HISTRY")
public class NegotiationsStatusHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "NEGO_STATUS_HISTRY_ID")
	private int negotiationStatusHistoryId;
	
	@Column(name = "NEGOTIATION_ID")
	private int negotiationId;
	
	@Column(name="NEGOTIATION_STATUS_CODE")
	private String negotiationStatusCode;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	public int getNegotiationStatusHistoryId() {
		return negotiationStatusHistoryId;
	}

	public void setNegotiationStatusHistoryId(int negotiationStatusHistoryId) {
		this.negotiationStatusHistoryId = negotiationStatusHistoryId;
	}

	public int getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(int negotiationId) {
		this.negotiationId = negotiationId;
	}

	public String getNegotiationStatusCode() {
		return negotiationStatusCode;
	}

	public void setNegotiationStatusCode(String negotiationStatusCode) {
		this.negotiationStatusCode = negotiationStatusCode;
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
