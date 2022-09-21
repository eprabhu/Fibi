package com.polus.fibicomp.print.dto;

import java.util.List;

import com.polus.fibicomp.persontraining.pojo.PersonTraining;

public class ProposalPrintParameter {

	private String fullName;
	private String percentageOfEffort;
	private String units;
	private String role;
	private String projectTeamMemberName;
	private String projectRole;
	private String projectStartDate;
	private String projectEndDate;
	private String isProjectActive;
	private String percentageCharged;
	private String researchAreaCode;
	private String researchAreaType;
	private String researchArea;
	private String subResearchArea;
	private String sponsorName;
	private String sponsorType;
	private String fundStartDate;
	private String fundEndDate;
	private String fundAmount;
	private String fundingStatus;
	private String fundingSource;
	private String specialReviewType;
	private String approvalType;
	private String protocolNumber;
	private String applicationDate;
	private String approvalDate;
	private String expirationDate;
	private String specialReviewComment;
	private String questionAndAnswers;
	private String isActive;
	private String activePeriod;
	private String designation;
	// For declaration of other funding support
	private String rolePlayed;
	private String projectTitle;
	private String percentageEffort;
	private String grantCallName;
	private String currencyCode;
	// For Milestone
	private String researchMilestone;
	private String startMonth;
	private String duration;
	private String personType;
	private String organization;
    private String startDate;
    private String endDate;
    private String certificationStatus;
    private List<PersonTraining> trainings;
    private String commentType;
    private List<String>  proposalComments;
	private String personId;

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

	public String getProjectTeamMemberName() {
		return projectTeamMemberName;
	}

	public void setProjectTeamMemberName(String projectTeamMemberName) {
		this.projectTeamMemberName = projectTeamMemberName;
	}

	public String getProjectRole() {
		return projectRole;
	}

	public void setProjectRole(String projectRole) {
		this.projectRole = projectRole;
	}

	public String getIsProjectActive() {
		return isProjectActive;
	}

	public void setIsProjectActive(String isProjectActive) {
		this.isProjectActive = isProjectActive;
	}

	public String getPercentageCharged() {
		return percentageCharged;
	}

	public void setPercentageCharged(String percentageCharged) {
		this.percentageCharged = percentageCharged;
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

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getSponsorType() {
		return sponsorType;
	}

	public void setSponsorType(String sponsorType) {
		this.sponsorType = sponsorType;
	}

	public String getFundAmount() {
		return fundAmount;
	}

	public void setFundAmount(String fundAmount) {
		this.fundAmount = fundAmount;
	}

	public String getFundingStatus() {
		return fundingStatus;
	}

	public void setFundingStatus(String fundingStatus) {
		this.fundingStatus = fundingStatus;
	}

	public String getFundStartDate() {
		return fundStartDate;
	}

	public void setFundStartDate(String fundStartDate) {
		this.fundStartDate = fundStartDate;
	}

	public String getFundEndDate() {
		return fundEndDate;
	}

	public void setFundEndDate(String fundEndDate) {
		this.fundEndDate = fundEndDate;
	}

	public String getFundingSource() {
		return fundingSource;
	}

	public void setFundingSource(String fundingSource) {
		this.fundingSource = fundingSource;
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

	public String getQuestionAndAnswers() {
		return questionAndAnswers;
	}

	public void setQuestionAndAnswers(String questionAndAnswers) {
		this.questionAndAnswers = questionAndAnswers;
	}

	public ProposalPrintParameter(String questionAndAnswers) {
		this.questionAndAnswers = questionAndAnswers;
	}

	public String getProjectStartDate() {
		return projectStartDate;
	}

	public void setProjectStartDate(String projectStartDate) {
		this.projectStartDate = projectStartDate;
	}

	public String getProjectEndDate() {
		return projectEndDate;
	}

	public void setProjectEndDate(String projectEndDate) {
		this.projectEndDate = projectEndDate;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public String getActivePeriod() {
		return activePeriod;
	}

	public void setActivePeriod(String activePeriod) {
		this.activePeriod = activePeriod;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}

	public ProposalPrintParameter() {
		super();
	}

	public ProposalPrintParameter(String researchAreaCode, String researchAreaType, String researchArea,
			String subResearchArea) {
		this.researchAreaCode = researchAreaCode;
		this.researchAreaType = researchAreaType;
		this.researchArea = researchArea;
		this.subResearchArea = subResearchArea;
	}

	public ProposalPrintParameter(String specialReviewType, String approvalType, String protocolNumber,
			String applicationDate, String approvalDate, String expirationDate, String specialReviewComment) {
		this.specialReviewType = specialReviewType;
		this.approvalType = approvalType;
		this.protocolNumber = protocolNumber;
		this.applicationDate = applicationDate;
		this.approvalDate = approvalDate;
		this.expirationDate = expirationDate;
		this.specialReviewComment = specialReviewComment;
	}

	public String getRolePlayed() {
		return rolePlayed;
	}

	public void setRolePlayed(String rolePlayed) {
		this.rolePlayed = rolePlayed;
	}

	public String getProjectTitle() {
		return projectTitle;
	}

	public void setProjectTitle(String projectTitle) {
		this.projectTitle = projectTitle;
	}

	public String getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(String percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public String getResearchMilestone() {
		return researchMilestone;
	}

	public void setResearchMilestone(String researchMilestone) {
		this.researchMilestone = researchMilestone;
	}

	public String getStartMonth() {
		return startMonth;
	}

	public void setStartMonth(String startMonth) {
		this.startMonth = startMonth;
	}

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}

	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		this.personType = personType;
	}

	public String getOrganization() {
		return organization;
	}

	public void setOrganization(String organization) {
		this.organization = organization;
	}

	public String getStartDate() {
		return startDate;
	}

	public void setStartDate(String startDate) {
		this.startDate = startDate;
	}

	public String getEndDate() {
		return endDate;
	}

	public void setEndDate(String endDate) {
		this.endDate = endDate;
	}

	public String getCertificationStatus() {
		return certificationStatus;
	}

	public void setCertificationStatus(String certificationStatus) {
		this.certificationStatus = certificationStatus;
	}

	public List<PersonTraining> getTrainings() {
		return trainings;
	}

	public void setTrainings(List<PersonTraining> trainings) {
		this.trainings = trainings;
	}

	public String getCommentType() {
		return commentType;
	}

	public void setCommentType(String commentType) {
		this.commentType = commentType;
	}

	public List<String> getProposalComments() {
		return proposalComments;
	}

	public void setProposalComments(List<String> proposalComments) {
		this.proposalComments = proposalComments;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}
}
