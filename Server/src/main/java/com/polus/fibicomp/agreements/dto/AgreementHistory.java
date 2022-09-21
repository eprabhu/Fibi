package com.polus.fibicomp.agreements.dto;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.agreements.pojo.AgreementNote;

public class AgreementHistory {

	private String message;

	private Timestamp updateTimestamp;

	private String updateUserFullName;

	private String actionTypeCode;

	private Integer actionLogId;

	private List<AgreementNote> agreementNotes;

	public AgreementHistory() {
		agreementNotes = new ArrayList<>();
	}

	public String getActionTypeCode() {
		return actionTypeCode;
	}

	public void setActionTypeCode(String actionTypeCode) {
		this.actionTypeCode = actionTypeCode;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public List<AgreementNote> getAgreementNotes() {
		return agreementNotes;
	}

	public void setAgreementNotes(List<AgreementNote> agreementNotes) {
		this.agreementNotes = agreementNotes;
	}

}
