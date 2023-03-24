package com.polus.fibicomp.view;

public class ResearchSummaryView {

	private String id;

	private String researchType;

	private Long researchCount;

	private Integer totalAmount;

	private Long sortOrder;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getResearchType() {
		return researchType;
	}

	public void setResearchType(String researchType) {
		this.researchType = researchType;
	}

	public Long getResearchCount() {
		return researchCount;
	}

	public void setResearchCount(Long researchCount) {
		this.researchCount = researchCount;
	}

	public Integer getTotalAmount() {
		return totalAmount;
	}

	public void setTotalAmount(Integer totalAmount) {
		this.totalAmount = totalAmount;
	}

	public Long getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Long sortOrder) {
		this.sortOrder = sortOrder;
	}

}
