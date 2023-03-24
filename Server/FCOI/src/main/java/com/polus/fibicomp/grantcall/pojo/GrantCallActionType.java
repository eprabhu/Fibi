package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "GRANTCALL_ACTION_TYPE")
public class GrantCallActionType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACTION_TYPE_CODE")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ACTION_TYPE_CODE_GENERATOR")
	@SequenceGenerator(name="ACTION_TYPE_CODE_GENERATOR", sequenceName = "ACTION_TYPE_CODE_GENERATOR", allocationSize=1)
	private Integer actionTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getActionTypeCode() {
		return actionTypeCode;
	}

	public void setActionTypeCode(Integer actionTypeCode) {
		this.actionTypeCode = actionTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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
