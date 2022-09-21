package com.polus.fibicomp.fastintegration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "SAP_FEED_USER_ACTIONS")
public class SapFeedUserAction {

	@Id
	@Column(name = "USER_ACTION_CODE")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "USER_ACTION_CODE_ID_GENERATOR")
	@SequenceGenerator(name = "USER_ACTION_CODE_ID_GENERATOR", sequenceName = "USER_ACTION_CODE_ID_GENERATOR", allocationSize = 1)
	private String userActionCode;

	@Column(name = "USER_ACTION")
	private String userAction;

	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "INVOICE_DESCRIPTION")
	private String invoiceDescription;

	@Column(name = "COMMENT")
	private String comment;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getUserActionCode() {
		return userActionCode;
	}

	public void setUserActionCode(String userActionCode) {
		this.userActionCode = userActionCode;
	}

	public String getUserAction() {
		return userAction;
	}

	public void setUserAction(String userAction) {
		this.userAction = userAction;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
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

	public String getInvoiceDescription() {
		return invoiceDescription;
	}

	public void setInvoiceDescription(String invoiceDescription) {
		this.invoiceDescription = invoiceDescription;
	}

}
