package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "MANPOWER_USER_ACTIONS")
public class ManpowerUserAction implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "MANPOWER_USER_ACTION_CODE")
	private String manpowerUserActionCode;

	@Column(name = "MANPOWER_USER_ACTION")
	private String manpowerUserAction;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public String getManpowerUserActionCode() {
		return manpowerUserActionCode;
	}

	public void setManpowerUserActionCode(String manpowerUserActionCode) {
		this.manpowerUserActionCode = manpowerUserActionCode;
	}

	public String getManpowerUserAction() {
		return manpowerUserAction;
	}

	public void setManpowerUserAction(String manpowerUserAction) {
		this.manpowerUserAction = manpowerUserAction;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
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
