package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "NEGOTIATION_NEGOTIATOR_HISTRY")
public class NegotiationsNegotiatorHistory implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "NEGO_NEGOTIATOR_HISTRY_ID")
	private Integer negotiationsNegotiatorHistoryId;
	
	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;
	
	@Column(name="NEGOTIATOR_PERSON_ID")
	private String negotiatorPersonId;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getNegotiatorPersonId() {
		return negotiatorPersonId;
	}

	public void setNegotiatorPersonId(String negotiatorPersonId) {
		this.negotiatorPersonId = negotiatorPersonId;
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

	public Integer getNegotiationsNegotiatorHistoryId() {
		return negotiationsNegotiatorHistoryId;
	}

	public void setNegotiationsNegotiatorHistoryId(Integer negotiationsNegotiatorHistoryId) {
		this.negotiationsNegotiatorHistoryId = negotiationsNegotiatorHistoryId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}
	
}
