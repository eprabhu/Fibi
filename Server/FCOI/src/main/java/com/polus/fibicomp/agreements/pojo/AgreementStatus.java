package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AGREEMENT_STATUS")
public class AgreementStatus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AGREEMENT_STATUS_CODE")
	private String agreementStatusCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "CAN_ASSOCIATE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean canAssociate;

	public String getAgreementStatusCode() {
		return agreementStatusCode;
	}

	public void setAgreementStatusCode(String agreementStatusCode) {
		this.agreementStatusCode = agreementStatusCode;
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

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public Boolean getCanAssociate() {
		return canAssociate;
	}

	public void setCanAssociate(Boolean canAssociate) {
		this.canAssociate = canAssociate;
	}

}
