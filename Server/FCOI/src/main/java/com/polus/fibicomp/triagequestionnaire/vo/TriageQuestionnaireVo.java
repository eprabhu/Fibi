package com.polus.fibicomp.triagequestionnaire.vo;

import com.polus.fibicomp.triagequestionnaire.pojo.TriageHeader;

public class TriageQuestionnaireVo {

	private Integer triageHeaderId;

	private Integer moduleCode;

	private Integer subModuleCode;

	private String moduleItemKey;

	private String subModuleItemKey;

	private String personId;

	private String updateUser;

	private TriageHeader triageHeader;

	public Integer getTriageHeaderId() {
		return triageHeaderId;
	}

	public void setTriageHeaderId(Integer triageHeaderId) {
		this.triageHeaderId = triageHeaderId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public TriageHeader getTriageHeader() {
		return triageHeader;
	}

	public void setTriageHeader(TriageHeader triageHeader) {
		this.triageHeader = triageHeader;
	}

}
