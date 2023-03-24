package com.polus.fibicomp.agreements.pojo;

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
@Table(name = "AGREEMENT_ACTION_LOG")
public class AgreementActionLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AGRMNT_ACTION_LOG_ID_GENERATOR")
	@SequenceGenerator(name="AGRMNT_ACTION_LOG_ID_GENERATOR", sequenceName = "AGRMNT_ACTION_LOG_ID_GENERATOR", allocationSize=1)
	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ACTION_LOG_FK2"), name = "AGREEMENT_REQUEST_ID", referencedColumnName = "AGREEMENT_REQUEST_ID", insertable = false, updatable = false)
	private AgreementHeader agreementHeader;

	@Column(name = "ACTION_TYPE_CODE")
	private String actionTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_ACTION_LOG_FK1"), name = "ACTION_TYPE_CODE", referencedColumnName = "ACTION_TYPE_CODE", insertable = false, updatable = false)
	private AgreementActionType agreementActionType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public AgreementHeader getAgreementHeader() {
		return agreementHeader;
	}

	public void setAgreementHeader(AgreementHeader agreementHeader) {
		this.agreementHeader = agreementHeader;
	}

	public String getActionTypeCode() {
		return actionTypeCode;
	}

	public void setActionTypeCode(String actionTypeCode) {
		this.actionTypeCode = actionTypeCode;
	}

	public AgreementActionType getAgreementActionType() {
		return agreementActionType;
	}

	public void setAgreementActionType(AgreementActionType agreementActionType) {
		this.agreementActionType = agreementActionType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
