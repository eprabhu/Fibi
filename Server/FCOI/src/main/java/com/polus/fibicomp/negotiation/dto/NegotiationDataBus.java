package com.polus.fibicomp.negotiation.dto;

public class NegotiationDataBus {
	private String outputDataFormat;
	private Integer moduleCode;
	private Integer sequenceNumber;
	private Integer negotiationsId;
	private String committeAction;
	private String actionCode;
	private String moduleItemKey;
	
	public String getOutputDataFormat() {
		return outputDataFormat;
	}
	public void setOutputDataFormat(String outputDataFormat) {
		this.outputDataFormat = outputDataFormat;
	}
	public Integer getModuleCode() {
		return moduleCode;
	}
	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}
	public Integer getSequenceNumber() {
		return sequenceNumber;
	}
	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}
	public String getCommitteAction() {
		return committeAction;
	}
	public void setCommitteAction(String committeAction) {
		this.committeAction = committeAction;
	}
	public String getActionCode() {
		return actionCode;
	}
	public void setActionCode(String actionCode) {
		this.actionCode = actionCode;
	}
	public Integer getNegotiationsId() {
		return negotiationsId;
	}
	public void setNegotiationsId(Integer negotiationsId) {
		this.negotiationsId = negotiationsId;
	}
	public String getModuleItemKey() {
		return moduleItemKey;
	}
	public void setModuleItemKey(String moduleKey) {
		this.moduleItemKey = moduleKey;
	}
	
}
