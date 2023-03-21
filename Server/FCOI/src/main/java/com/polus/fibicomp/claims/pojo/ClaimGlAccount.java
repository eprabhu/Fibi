package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_GL_ACCOUNT_CODE")
public class ClaimGlAccount implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GL_ACCOUNT_CODE")
	private String glAccountCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;
	
	@Column(name = "IS_CONTROLLED_GL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isControlledGl;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getGlAccountCode() {
		return glAccountCode;
	}

	public void setGlAccountCode(String glAccountCode) {
		this.glAccountCode = glAccountCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

	public Boolean getIsControlledGl() {
		return isControlledGl;
	}

	public void setIsControlledGl(Boolean isControlledGl) {
		this.isControlledGl = isControlledGl;
	}
}
