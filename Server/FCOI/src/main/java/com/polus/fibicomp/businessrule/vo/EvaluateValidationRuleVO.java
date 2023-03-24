package com.polus.fibicomp.businessrule.vo;

import java.util.List;

import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;

public class EvaluateValidationRuleVO {

	private Integer moduleCode;
	private Integer subModuleCode;
	private Integer ruleId;
	private String moduleItemKey;
	private String updateUser;
	private String logginPersonId;
	private String moduleItemId;
	private String acType;
	private String comments;
	private String subModuleItemKey;
	
	private List<WorkFlowResultDto> WorkFlowResultList;
	
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
	
	public String getUpdateUser() {
		return updateUser;
	}
	
	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}
	
	public String getLogginPersonId() {
		return logginPersonId;
	}
	
	public void setLogginPersonId(String logginPersonId) {
		this.logginPersonId = logginPersonId;
	}
	
	public Integer getRuleId() {
		return ruleId;
	}
	
	public void setRuleId(Integer ruleId) {
		this.ruleId = ruleId;
	}

	public List<WorkFlowResultDto> getWorkFlowResultList() {
		return WorkFlowResultList;
	}

	public void setWorkFlowResultList(List<WorkFlowResultDto> workFlowResultList) {
		WorkFlowResultList = workFlowResultList;
	}

	public String getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(String moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}
	
}
