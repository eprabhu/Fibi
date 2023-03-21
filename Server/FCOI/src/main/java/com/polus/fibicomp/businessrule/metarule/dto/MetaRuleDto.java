package com.polus.fibicomp.businessrule.metarule.dto;

import java.util.List;

public class MetaRuleDto {

	private Integer metaRuleId;
	private Integer metaRuleDetailId;
	private Integer nodeNumber;
	private Integer ruleId;
	private String ruleName;
	private Integer nextNode;
	private Integer nodeIfTrue;
	private Integer nodeIfFalse;
	private String nodeCondition;
	private Integer parentNodeNumber;
	private List<MetaRuleDto> childNodes;

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

	public Integer getNodeNumber() {
		return nodeNumber;
	}

	public void setNodeNumber(Integer nodeNumber) {
		this.nodeNumber = nodeNumber;
	}

	public Integer getRuleId() {
		return ruleId;
	}

	public void setRuleId(Integer ruleId) {
		this.ruleId = ruleId;
	}

	public String getRuleName() {
		return ruleName;
	}

	public void setRuleName(String ruleName) {
		this.ruleName = ruleName;
	}

	public Integer getNextNode() {
		return nextNode;
	}

	public void setNextNode(Integer nextNode) {
		this.nextNode = nextNode;
	}

	public Integer getNodeIfTrue() {
		return nodeIfTrue;
	}

	public void setNodeIfTrue(Integer nodeIfTrue) {
		this.nodeIfTrue = nodeIfTrue;
	}

	public Integer getNodeIfFalse() {
		return nodeIfFalse;
	}

	public void setNodeIfFalse(Integer nodeIfFalse) {
		this.nodeIfFalse = nodeIfFalse;
	}

	public String getNodeCondition() {
		return nodeCondition;
	}

	public void setNodeCondition(String nodeCondition) {
		this.nodeCondition = nodeCondition;
	}

	public Integer getParentNodeNumber() {
		return parentNodeNumber;
	}

	public void setParentNodeNumber(Integer parentNodeNumber) {
		this.parentNodeNumber = parentNodeNumber;
	}

	public List<MetaRuleDto> getChildNodes() {
		return childNodes;
	}

	public void setChildNodes(List<MetaRuleDto> childNodes) {
		this.childNodes = childNodes;
	}

}
