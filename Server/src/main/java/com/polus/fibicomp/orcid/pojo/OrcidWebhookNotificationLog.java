package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "ORCID_WEBHOOK_NOTIFY_LOG")
public class OrcidWebhookNotificationLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ORCID_WEBHOOK_NOTIFY_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ORCID_WBHK_NTFY_LOG_ID_GNRTR")
	@SequenceGenerator(name="ORCID_WBHK_NTFY_LOG_ID_GNRTR", sequenceName = "ORCID_WBHK_NTFY_LOG_ID_GNRTR", allocationSize=1)
	private Integer orcidWebhookLogId;

	@Column(name = "ORCID_ID")
	private String orcidId;

	@Column(name = "WEBHOOK_ACTION_TYPE_CODE")
	private String webHookActionTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WEBHOOK_NOTIFY_LOG_FK1"), name = "WEBHOOK_ACTION_TYPE_CODE", referencedColumnName = "WEBHOOK_ACTION_TYPE_CODE", insertable = false, updatable = false)
	private OrcidWebHookActionTypes webHookActionType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getOrcidWebhookLogId() {
		return orcidWebhookLogId;
	}

	public void setOrcidWebhookLogId(Integer orcidWebhookLogId) {
		this.orcidWebhookLogId = orcidWebhookLogId;
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

	public String getWebHookActionTypeCode() {
		return webHookActionTypeCode;
	}

	public void setWebHookActionTypeCode(String webHookActionTypeCode) {
		this.webHookActionTypeCode = webHookActionTypeCode;
	}

	public OrcidWebHookActionTypes getWebHookActionType() {
		return webHookActionType;
	}

	public void setWebHookActionType(OrcidWebHookActionTypes webHookActionType) {
		this.webHookActionType = webHookActionType;
	}

	public String getOrcidId() {
		return orcidId;
	}

	public void setOrcidId(String orcidId) {
		this.orcidId = orcidId;
	}

}
