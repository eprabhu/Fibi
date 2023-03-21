package com.polus.fibicomp.print.dto;

import java.util.List;

public class IpPrintParameter {

	private String commentType;
	private  List<String>  ipComments;
	
	private String specialReviewType;
	private String approvalType;
	private String protocolNumber;
	private String applicationDate;
	private String approvalDate;
	private String expirationDate;
	private String specialReviewComment;
	
	private String researchAreaCode;
	private String researchAreaType;
	private String researchArea;
	private String subResearchArea;
	

	private String designation;
	private String fullName;
	private String percentageOfEffort;
	private String units;
	private String role;
	private String organization;
	private String personType;

	public String getCommentType() {
		return commentType;
	}
	public void setCommentType(String commentType) {
		this.commentType = commentType;
	}
	public List<String> getIpComments() {
		return ipComments;
	}
	public void setIpComments(List<String> ipComments) {
		this.ipComments = ipComments;
	}
	public String getSpecialReviewType() {
		return specialReviewType;
	}
	public void setSpecialReviewType(String specialReviewType) {
		this.specialReviewType = specialReviewType;
	}
	public String getApprovalType() {
		return approvalType;
	}
	public void setApprovalType(String approvalType) {
		this.approvalType = approvalType;
	}
	public String getProtocolNumber() {
		return protocolNumber;
	}
	public void setProtocolNumber(String protocolNumber) {
		this.protocolNumber = protocolNumber;
	}
	public String getApplicationDate() {
		return applicationDate;
	}
	public void setApplicationDate(String applicationDate) {
		this.applicationDate = applicationDate;
	}
	public String getApprovalDate() {
		return approvalDate;
	}
	public void setApprovalDate(String approvalDate) {
		this.approvalDate = approvalDate;
	}
	public String getExpirationDate() {
		return expirationDate;
	}
	public void setExpirationDate(String expirationDate) {
		this.expirationDate = expirationDate;
	}
	public String getSpecialReviewComment() {
		return specialReviewComment;
	}
	public void setSpecialReviewComment(String specialReviewComment) {
		this.specialReviewComment = specialReviewComment;
	}
	public String getResearchAreaCode() {
		return researchAreaCode;
	}
	public void setResearchAreaCode(String researchAreaCode) {
		this.researchAreaCode = researchAreaCode;
	}
	public String getResearchAreaType() {
		return researchAreaType;
	}
	public void setResearchAreaType(String researchAreaType) {
		this.researchAreaType = researchAreaType;
	}
	public String getResearchArea() {
		return researchArea;
	}
	public void setResearchArea(String researchArea) {
		this.researchArea = researchArea;
	}
	public String getSubResearchArea() {
		return subResearchArea;
	}
	public void setSubResearchArea(String subResearchArea) {
		this.subResearchArea = subResearchArea;
	}
	public String getDesignation() {
		return designation;
	}
	public void setDesignation(String designation) {
		this.designation = designation;
	}
	public String getFullName() {
		return fullName;
	}
	public void setFullName(String fullName) {
		this.fullName = fullName;
	}
	public String getPercentageOfEffort() {
		return percentageOfEffort;
	}
	public void setPercentageOfEffort(String percentageOfEffort) {
		this.percentageOfEffort = percentageOfEffort;
	}
	public String getUnits() {
		return units;
	}
	public void setUnits(String units) {
		this.units = units;
	}
	public String getRole() {
		return role;
	}
	public void setRole(String role) {
		this.role = role;
	}
	public String getOrganization() {
		return organization;
	}
	public void setOrganization(String organization) {
		this.organization = organization;
	}
	public String getPersonType() {
		return personType;
	}
	public void setPersonType(String personType) {
		this.personType = personType;
	}
	public IpPrintParameter(String commentType, List<String> ipComments) {
		super();
		this.commentType = commentType;
		this.ipComments = ipComments;
	}
	public IpPrintParameter(String specialReviewType, String approvalType, String protocolNumber,
			String applicationDate, String approvalDate, String expirationDate, String specialReviewComment) {
		super();
		this.specialReviewType = specialReviewType;
		this.approvalType = approvalType;
		this.protocolNumber = protocolNumber;
		this.applicationDate = applicationDate;
		this.approvalDate = approvalDate;
		this.expirationDate = expirationDate;
		this.specialReviewComment = specialReviewComment;
	}
	public IpPrintParameter(String researchAreaCode, String researchAreaType, String researchArea,
			String subResearchArea) {
		super();
		this.researchAreaCode = researchAreaCode;
		this.researchAreaType = researchAreaType;
		this.researchArea = researchArea;
		this.subResearchArea = subResearchArea;
	}
	public IpPrintParameter() {
		super();
	}

}
