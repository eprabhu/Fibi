package com.polus.fibicomp.correspondence.dto;

public class CorrespondenceDataBus {
	private String outputDataFormat;
	private Integer moduleCode;
	private Integer sequenceNumber;
	private String moduleItemKey;
	private String committeAction;
	private String actionCode;

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

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getCommitteAction() {
		return committeAction;
	}

	public void setCommitteAction(String committeAction) {
		this.committeAction = committeAction;
	}

	public String getOutputDataFormat() {
		return outputDataFormat;
	}

	public void setOutputDataFormat(String outputDataFormat) {
		this.outputDataFormat = outputDataFormat;
	}

	public String getActionCode() {
		return actionCode;
	}

	public void setActionCode(String actionCode) {
		this.actionCode = actionCode;
	}
}
