package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "ORCID_WEBHOOK_ACTION_TYPES")
public class OrcidWebHookActionTypes implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WEBHOOK_ACTION_TYPE_CODE")
	private String webhookActionTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getWebhookActionTypeCode() {
		return webhookActionTypeCode;
	}

	public void setWebhookActionTypeCode(String webhookActionTypeCode) {
		this.webhookActionTypeCode = webhookActionTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

}
