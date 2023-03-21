package com.polus.fibicomp.businessrule.metarule.vo;

import java.util.List;

import com.polus.fibicomp.businessrule.metarule.dto.MetaRuleDto;
import com.polus.fibicomp.businessrule.metarule.pojo.MetaRule;

public class MetaRuleVO {

	private MetaRule metaRule;

	private String nodeCondition;

	private String unitNumber;

	private Integer moduleCode;

	private Integer subModuleCode;

	private String parentNodeNumber;

	private String metaRuleType;

	private Boolean metaRuleAvailable;

	private Boolean isRootNode;

	private Integer metaRuleId;

	private Integer metaRuleDetailId;

	private List<MetaRuleDto> metaRuleDtos;

	public MetaRule getMetaRule() {
		return metaRule;
	}

	public void setMetaRule(MetaRule metaRule) {
		this.metaRule = metaRule;
	}

	public String getNodeCondition() {
		return nodeCondition;
	}

	public void setNodeCondition(String nodeCondition) {
		this.nodeCondition = nodeCondition;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
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

	public String getParentNodeNumber() {
		return parentNodeNumber;
	}

	public void setParentNodeNumber(String parentNodeNumber) {
		this.parentNodeNumber = parentNodeNumber;
	}

	public String getMetaRuleType() {
		return metaRuleType;
	}

	public void setMetaRuleType(String metaRuleType) {
		this.metaRuleType = metaRuleType;
	}

	public Boolean getMetaRuleAvailable() {
		return metaRuleAvailable;
	}

	public void setMetaRuleAvailable(Boolean metaRuleAvailable) {
		this.metaRuleAvailable = metaRuleAvailable;
	}

	public Boolean getIsRootNode() {
		return isRootNode;
	}

	public void setIsRootNode(Boolean isRootNode) {
		this.isRootNode = isRootNode;
	}

	public Integer getMetaRuleId() {
		return metaRuleId;
	}

	public void setMetaRuleId(Integer metaRuleId) {
		this.metaRuleId = metaRuleId;
	}

	public Integer getMetaRuleDetailId() {
		return metaRuleDetailId;
	}

	public void setMetaRuleDetailId(Integer metaRuleDetailId) {
		this.metaRuleDetailId = metaRuleDetailId;
	}

	public List<MetaRuleDto> getMetaRuleDtos() {
		return metaRuleDtos;
	}

	public void setMetaRuleDtos(List<MetaRuleDto> metaRuleDtos) {
		this.metaRuleDtos = metaRuleDtos;
	}

}
