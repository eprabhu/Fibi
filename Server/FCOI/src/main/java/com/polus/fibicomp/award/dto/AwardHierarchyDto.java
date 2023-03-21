package com.polus.fibicomp.award.dto;

import java.util.List;

import com.polus.fibicomp.award.pojo.AwardStatus;

/**
 * AwardHierarchy DTO with award hierarchy details
 *
 */
public class AwardHierarchyDto {

	private String awardNumber;

	private String parentAwardNumber;

	private String awardId;

	private String principalInvestigator;

	private String accountNumber;

	private boolean isSelected;

	private Integer StatusCode;

	private String colorCode;

	private int level;

	private List<AwardHierarchyDto> children;

	private String rootAwardNumber;

	private String name;

	private boolean isOpen;

	private AwardStatus awardStatus;

	private String awardSequenceStatus;

	private Integer workflowAwardStatusCode;

	private Integer awardDocumentTypeCode;

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getParentAwardNumber() {
		return parentAwardNumber;
	}

	public void setParentAwardNumber(String parentAwardNumber) {
		this.parentAwardNumber = parentAwardNumber;
	}

	public String getAwardId() {
		return awardId;
	}

	public void setAwardId(String awardId) {
		this.awardId = awardId;
	}

	public String getPrincipalInvestigator() {
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public boolean isSelected() {
		return isSelected;
	}

	public void setSelected(boolean isSelected) {
		this.isSelected = isSelected;
	}

	public Integer getStatusCode() {
		return StatusCode;
	}

	public void setStatusCode(Integer statusCode) {
		StatusCode = statusCode;
	}

	public String getColorCode() {
		return colorCode;
	}

	public void setColorCode(String colorCode) {
		this.colorCode = colorCode;
	}

	public int getLevel() {
		return level;
	}

	public void setLevel(int level) {
		this.level = level;
	}

	public List<AwardHierarchyDto> getChildren() {
		return children;
	}

	public void setChildren(List<AwardHierarchyDto> children) {
		this.children = children;
	}

	public String getRootAwardNumber() {
		return rootAwardNumber;
	}

	public void setRootAwardNumber(String rootAwardNumber) {
		this.rootAwardNumber = rootAwardNumber;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isOpen() {
		return isOpen;
	}

	public void setOpen(boolean isOpen) {
		this.isOpen = isOpen;
	}

	public AwardStatus getAwardStatus() {
		return awardStatus;
	}

	public void setAwardStatus(AwardStatus awardStatus) {
		this.awardStatus = awardStatus;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public Integer getWorkflowAwardStatusCode() {
		return workflowAwardStatusCode;
	}

	public void setWorkflowAwardStatusCode(Integer workflowAwardStatusCode) {
		this.workflowAwardStatusCode = workflowAwardStatusCode;
	}

	public Integer getAwardDocumentTypeCode() {
		return awardDocumentTypeCode;
	}

	public void setAwardDocumentTypeCode(Integer awardDocumentTypeCode) {
		this.awardDocumentTypeCode = awardDocumentTypeCode;
	}

}
