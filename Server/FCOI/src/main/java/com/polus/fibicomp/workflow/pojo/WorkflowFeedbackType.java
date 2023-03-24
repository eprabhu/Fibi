package com.polus.fibicomp.workflow.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.person.pojo.PersonRoleType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "WORKFLOW_FEEDBACK_TYPE")
public class WorkflowFeedbackType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FEEDBACK_TYPE_CODE")
	private String feedbackTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "ROLE_TYPE_CODE")
	private Integer roleTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_FEEDBACK_TYPE_FK1"), name = "ROLE_TYPE_CODE", referencedColumnName = "ROLE_TYPE_CODE", insertable = false, updatable = false)
	private PersonRoleType personRoleType;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public String getFeedbackTypeCode() {
		return feedbackTypeCode;
	}

	public void setFeedbackTypeCode(String feedbackTypeCode) {
		this.feedbackTypeCode = feedbackTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getRoleTypeCode() {
		return roleTypeCode;
	}

	public void setRoleTypeCode(Integer roleTypeCode) {
		this.roleTypeCode = roleTypeCode;
	}

	public PersonRoleType getPersonRoleType() {
		return personRoleType;
	}

	public void setPersonRoleType(PersonRoleType personRoleType) {
		this.personRoleType = personRoleType;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
