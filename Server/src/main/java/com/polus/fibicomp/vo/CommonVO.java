package com.polus.fibicomp.vo;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.evaluation.pojo.EvaluationRecommendation;
import com.polus.fibicomp.evaluation.pojo.EvaluationStop;
import com.polus.fibicomp.pojo.FileType;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.report.vo.Report;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.roles.pojo.RoleType;

public class CommonVO {

	private String userName;

	private String password;

	private Integer pageNumber;

	private String sortBy;

	private String reverse;

	private String tabIndex;

	private String inputData;

	private String property1;

	private String property2;

	private String property3;

	private String property4;

	private String property5;

	private String property6;

	private String property7;

	private String property8;

	private String property9;

	private Integer currentPage;

	private String personId;

	private String sponsorCode;

	private String pieChartIndex;

	private String researchSummaryIndex;

	private String donutChartIndex;

	private String awardId;

	private String accessToken;

	private String deviceToken;

	private String deviceType;

	@Pattern(regexp = "^[0-9 -]*$", message = "awardNumber must not include special characters.")
	private String awardNumber;

	@Pattern(regexp = "^[0-9 -]*$", message = "selectedAwardNumber must not include special characters.")
	private String selectedAwardNumber;

	private String documentNo;

	private String actionType;

	private Integer questionnaireId;

	private String comments;

	private List<HashMap<String, String>> answerList;

	private String unitNumber;

	private Boolean isUnitAdmin;

	private boolean isProvost;

	private boolean isReviewer;

	private String fullName;

	private List<HashMap<String, Object>> categoryMap;

	private List<HashMap<String, Object>> typeMap;

	private List<HashMap<String, Object>> templateList;

	private Integer categoryCode;

	private Integer serviceTypeCode;

	private String successMsg;

	private List<HashMap<String, Object>> departmentList;

	private List<HashMap<String, Object>> userList;

	private Integer moduleCode;

	private String moduleItemKey;

	private Integer ostprojectId;

	private String tabName;

	private String exportType;

	private String documentHeading;

	private Boolean isAdmin = false;

	private String actionFlag;

	private String newPassword;

	private String updatePasswordMessage;

	private String oldPasswordErrorMessage;

	private String jobCode;

	private Integer grantCallId;

	private Map<String, String> sort = new HashMap<>();

	private List<Sponsor> sponsors;

	private Integer totalSponsors;

	private Report report;

	private Integer proposalId;

	private String property10;

	private String property11;

	private String property12;

	private Boolean isGrantAdmin;

	private Boolean isDEC;

	private Boolean isURC;

	private String userRole;

	private Boolean isResearcher;

	private List<BudgetCategory> budgetCategoryList;

	private List<RoleType> roletypeList;

	private Integer proposalRank;

	private Boolean isIRBSecretariat;

	private Boolean isHOD;

	private Boolean isOrttDirector;

	private boolean canCreateGrantCall;

	private String serviceRequestTabName;

	private Integer recommendationCode;

	private Integer proposalStatusCode;

	private String activityTypeCode;

	private Boolean isDownload;

	private String advancedSearch = "L";

	private List<EvaluationRecommendation> evaluationRecommendation;

	@Pattern(regexp = "^[0-9a-zA-Z]*$", message = "Account Number must not include special characters.")
	private String accountNumber;

	private Timestamp property13;

	private Timestamp property14;

	@Pattern(regexp = "^[0-9a-zA-Z ._-]*$", message = "lookUpTableName must not include special characters.")
	private String lookUpTableName;

	@Pattern(regexp = "^[0-9a-zA-Z ._-]*$", message = "lookUpTableColumnName must not include special characters.")
	private String lookUpTableColumnName;

	private String searchString;

	private List<String> budgetCategoryCodes;

	private String challengeAreaCode;

	private String sponsorTypeCode;

	private Boolean isSingleQuestionnairePrint;

	private String remoteUser;

	@Pattern(regexp = "^[EP]*$", message = "type must include E and P.")
	private String type;

	private Integer subModuleCode;

	private String subModuleItemKey;

	private List<String> activityTypeCodes;

	private List<EvaluationStop> evaluationStopList;

	private List<Role> roles;

	private Boolean isGraduateStudent = false;

	private String loginPersonUnitNumber;

	private List<Integer> sectionCodes;

	private String costElementCode;

	private Integer level;

	private Boolean includeClosedGrantCall = Boolean.FALSE;

	private Integer awardPersonId;

	private String researchTypeCode;

	private String researchTypeAreaCode;

	private String check;

	private List<String> rightName;
	
	private List<FileType> fileTypes;

	private Boolean isEnableAddtoAddressBook = Boolean.FALSE;

	private Boolean triageQuestionnaireRequired = false;

	private String manpowerRequestType;
	
	private String sponsorGroupId;

	private List<String> sponsorCodes;

	private String startDate;

	private String endDate;

	private String descentFlag;

	public void setRightName(List<String> rightName) {
		this.rightName = rightName;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public String getTabIndex() {
		return tabIndex;
	}

	public void setTabIndex(String tabIndex) {
		this.tabIndex = tabIndex;
	}

	public String getInputData() {
		return inputData;
	}

	public void setInputData(String inputData) {
		this.inputData = inputData;
	}

	public String getProperty1() {
		return property1;
	}

	public void setProperty1(String property1) {
		this.property1 = property1;
	}

	public String getProperty2() {
		return property2;
	}

	public void setProperty2(String property2) {
		this.property2 = property2;
	}

	public String getProperty3() {
		return property3;
	}

	public void setProperty3(String property3) {
		this.property3 = property3;
	}

	public String getProperty4() {
		return property4;
	}

	public void setProperty4(String property4) {
		this.property4 = property4;
	}

	public String getProperty6() {
		return property6;
	}

	public void setProperty6(String property6) {
		this.property6 = property6;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public String getProperty8() {
		return property8;
	}

	public void setProperty8(String property8) {
		this.property8 = property8;
	}

	public String getProperty9() {
		return property9;
	}

	public void setProperty9(String property9) {
		this.property9 = property9;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getPieChartIndex() {
		return pieChartIndex;
	}

	public void setPieChartIndex(String pieChartIndex) {
		this.pieChartIndex = pieChartIndex;
	}

	public String getResearchSummaryIndex() {
		return researchSummaryIndex;
	}

	public void setResearchSummaryIndex(String researchSummaryIndex) {
		this.researchSummaryIndex = researchSummaryIndex;
	}

	public String getDonutChartIndex() {
		return donutChartIndex;
	}

	public void setDonutChartIndex(String donutChartIndex) {
		this.donutChartIndex = donutChartIndex;
	}

	public String getAwardId() {
		return awardId;
	}

	public void setAwardId(String awardId) {
		this.awardId = awardId;
	}

	public String getAccessToken() {
		return accessToken;
	}

	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}

	public String getDeviceToken() {
		return deviceToken;
	}

	public void setDeviceToken(String deviceToken) {
		this.deviceToken = deviceToken;
	}

	public String getDeviceType() {
		return deviceType;
	}

	public void setDeviceType(String deviceType) {
		this.deviceType = deviceType;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getSelectedAwardNumber() {
		return selectedAwardNumber;
	}

	public void setSelectedAwardNumber(String selectedAwardNumber) {
		this.selectedAwardNumber = selectedAwardNumber;
	}

	public String getDocumentNo() {
		return documentNo;
	}

	public void setDocumentNo(String documentNo) {
		this.documentNo = documentNo;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public Integer getQuestionnaireId() {
		return questionnaireId;
	}

	public void setQuestionnaireId(Integer questionnaireId) {
		this.questionnaireId = questionnaireId;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public List<HashMap<String, String>> getAnswerList() {
		return answerList;
	}

	public void setAnswerList(List<HashMap<String, String>> answerList) {
		this.answerList = answerList;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Boolean getIsUnitAdmin() {
		return isUnitAdmin;
	}

	public void setIsUnitAdmin(Boolean isUnitAdmin) {
		this.isUnitAdmin = isUnitAdmin;
	}

	public boolean isProvost() {
		return isProvost;
	}

	public void setProvost(boolean isProvost) {
		this.isProvost = isProvost;
	}

	public boolean isReviewer() {
		return isReviewer;
	}

	public void setReviewer(boolean isReviewer) {
		this.isReviewer = isReviewer;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public List<HashMap<String, Object>> getCategoryMap() {
		return categoryMap;
	}

	public void setCategoryMap(List<HashMap<String, Object>> categoryMap) {
		this.categoryMap = categoryMap;
	}

	public List<HashMap<String, Object>> getTypeMap() {
		return typeMap;
	}

	public void setTypeMap(List<HashMap<String, Object>> typeMap) {
		this.typeMap = typeMap;
	}

	public List<HashMap<String, Object>> getTemplateList() {
		return templateList;
	}

	public void setTemplateList(List<HashMap<String, Object>> templateList) {
		this.templateList = templateList;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public Integer getServiceTypeCode() {
		return serviceTypeCode;
	}

	public void setServiceTypeCode(Integer serviceTypeCode) {
		this.serviceTypeCode = serviceTypeCode;
	}

	public String getSuccessMsg() {
		return successMsg;
	}

	public void setSuccessMsg(String successMsg) {
		this.successMsg = successMsg;
	}

	public List<HashMap<String, Object>> getDepartmentList() {
		return departmentList;
	}

	public void setDepartmentList(List<HashMap<String, Object>> departmentList) {
		this.departmentList = departmentList;
	}

	public List<HashMap<String, Object>> getUserList() {
		return userList;
	}

	public void setUserList(List<HashMap<String, Object>> userList) {
		this.userList = userList;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getOstprojectId() {
		return ostprojectId;
	}

	public void setOstprojectId(Integer ostprojectId) {
		this.ostprojectId = ostprojectId;
	}

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public String getActionFlag() {
		return actionFlag;
	}

	public void setActionFlag(String actionFlag) {
		this.actionFlag = actionFlag;
	}

	public String getNewPassword() {
		return newPassword;
	}

	public void setNewPassword(String newPassword) {
		this.newPassword = newPassword;
	}

	public String getUpdatePasswordMessage() {
		return updatePasswordMessage;
	}

	public void setUpdatePasswordMessage(String updatePasswordMessage) {
		this.updatePasswordMessage = updatePasswordMessage;
	}

	public String getOldPasswordErrorMessage() {
		return oldPasswordErrorMessage;
	}

	public void setOldPasswordErrorMessage(String oldPasswordErrorMessage) {
		this.oldPasswordErrorMessage = oldPasswordErrorMessage;
	}

	public String getProperty5() {
		return property5;
	}

	public void setProperty5(String property5) {
		this.property5 = property5;
	}

	public String getJobCode() {
		return jobCode;
	}

	public void setJobCode(String jobCode) {
		this.jobCode = jobCode;
	}

	public List<Sponsor> getSponsors() {
		return sponsors;
	}

	public void setSponsors(List<Sponsor> sponsors) {
		this.sponsors = sponsors;
	}

	public Integer getTotalSponsors() {
		return totalSponsors;
	}

	public void setTotalSponsors(Integer totalSponsors) {
		this.totalSponsors = totalSponsors;
	}

	public Report getReport() {
		return report;
	}

	public void setReport(Report report) {
		this.report = report;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getProperty10() {
		return property10;
	}

	public void setProperty10(String property10) {
		this.property10 = property10;
	}

	public Boolean getIsGrantAdmin() {
		return isGrantAdmin;
	}

	public void setIsGrantAdmin(Boolean isGrantAdmin) {
		this.isGrantAdmin = isGrantAdmin;
	}

	public Boolean getIsDEC() {
		return isDEC;
	}

	public void setIsDEC(Boolean isDEC) {
		this.isDEC = isDEC;
	}

	public Boolean getIsURC() {
		return isURC;
	}

	public void setIsURC(Boolean isURC) {
		this.isURC = isURC;
	}

	public String getUserRole() {
		return userRole;
	}

	public void setUserRole(String userRole) {
		this.userRole = userRole;
	}

	public Boolean getIsResearcher() {
		return isResearcher;
	}

	public void setIsResearcher(Boolean isResearcher) {
		this.isResearcher = isResearcher;
	}

	public List<BudgetCategory> getBudgetCategoryList() {
		return budgetCategoryList;
	}

	public void setBudgetCategoryList(List<BudgetCategory> budgetCategoryList) {
		this.budgetCategoryList = budgetCategoryList;
	}

	public List<RoleType> getRoletypeList() {
		return roletypeList;
	}

	public void setRoletypeList(List<RoleType> roletypeList) {
		this.roletypeList = roletypeList;
	}

	public Integer getProposalRank() {
		return proposalRank;
	}

	public void setProposalRank(Integer proposalRank) {
		this.proposalRank = proposalRank;
	}

	public Boolean getIsIRBSecretariat() {
		return isIRBSecretariat;
	}

	public void setIsIRBSecretariat(Boolean isIRBSecretariat) {
		this.isIRBSecretariat = isIRBSecretariat;
	}

	public String getProperty11() {
		return property11;
	}

	public void setProperty11(String property11) {
		this.property11 = property11;
	}

	public Boolean getIsHOD() {
		return isHOD;
	}

	public void setIsHOD(Boolean isHOD) {
		this.isHOD = isHOD;
	}

	public Boolean getIsOrttDirector() {
		return isOrttDirector;
	}

	public void setIsOrttDirector(Boolean isOrttDirector) {
		this.isOrttDirector = isOrttDirector;
	}

	public String getServiceRequestTabName() {
		return serviceRequestTabName;
	}

	public void setServiceRequestTabName(String serviceRequestTabName) {
		this.serviceRequestTabName = serviceRequestTabName;
	}

	public boolean getCanCreateGrantCall() {
		return canCreateGrantCall;
	}

	public void setCanCreateGrantCall(boolean canCreateGrantCall) {
		this.canCreateGrantCall = canCreateGrantCall;
	}

	public Integer getProposalStatusCode() {
		return proposalStatusCode;
	}

	public void setProposalStatusCode(Integer proposalStatusCode) {
		this.proposalStatusCode = proposalStatusCode;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public String getProperty12() {
		return property12;
	}

	public void setProperty12(String property12) {
		this.property12 = property12;
	}

	public List<EvaluationRecommendation> getEvaluationRecommendation() {
		return evaluationRecommendation;
	}

	public void setEvaluationRecommendation(List<EvaluationRecommendation> evaluationRecommendation) {
		this.evaluationRecommendation = evaluationRecommendation;
	}

	public Integer getRecommendationCode() {
		return recommendationCode;
	}

	public void setRecommendationCode(Integer recommendationCode) {
		this.recommendationCode = recommendationCode;
	}

	public String getAdvancedSearch() {
		return advancedSearch;
	}

	public void setAdvancedSearch(String advancedSearch) {
		this.advancedSearch = advancedSearch;
	}

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public Timestamp getProperty13() {
		return property13;
	}

	public void setProperty13(Timestamp property13) {
		this.property13 = property13;
	}

	public Timestamp getProperty14() {
		return property14;
	}

	public void setProperty14(Timestamp property14) {
		this.property14 = property14;
	}

	public Boolean getIsAdmin() {
		return isAdmin;
	}

	public void setIsAdmin(Boolean isAdmin) {
		this.isAdmin = isAdmin;
	}

	public String getLookUpTableName() {
		return lookUpTableName;
	}

	public void setLookUpTableName(String lookUpTableName) {
		this.lookUpTableName = lookUpTableName;
	}

	public String getLookUpTableColumnName() {
		return lookUpTableColumnName;
	}

	public void setLookUpTableColumnName(String lookUpTableColumnName) {
		this.lookUpTableColumnName = lookUpTableColumnName;
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

	public List<String> getBudgetCategoryCodes() {
		return budgetCategoryCodes;
	}

	public void setBudgetCategoryCodes(List<String> budgetCategoryCodes) {
		this.budgetCategoryCodes = budgetCategoryCodes;
	}

	public String getChallengeAreaCode() {
		return challengeAreaCode;
	}

	public void setChallengeAreaCode(String challengeAreaCode) {
		this.challengeAreaCode = challengeAreaCode;
	}

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public Boolean getIsSingleQuestionnairePrint() {
		return isSingleQuestionnairePrint;
	}

	public void setIsSingleQuestionnairePrint(Boolean isSingleQuestionnairePrint) {
		this.isSingleQuestionnairePrint = isSingleQuestionnairePrint;
	}

	public String getRemoteUser() {
		return remoteUser;
	}

	public void setRemoteUser(String remoteUser) {
		this.remoteUser = remoteUser;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public List<String> getActivityTypeCodes() {
		return activityTypeCodes;
	}

	public void setActivityTypeCodes(List<String> activityTypeCodes) {
		this.activityTypeCodes = activityTypeCodes;
	}

	public List<EvaluationStop> getEvaluationStopList() {
		return evaluationStopList;
	}

	public void setEvaluationStopList(List<EvaluationStop> evaluationStopList) {
		this.evaluationStopList = evaluationStopList;
	}

	public List<Role> getRoles() {
		return roles;
	}

	public void setRoles(List<Role> roles) {
		this.roles = roles;
	}

	public Boolean getIsGraduateStudent() {
		return isGraduateStudent;
	}

	public void setIsGraduateStudent(Boolean isGraduateStudent) {
		this.isGraduateStudent = isGraduateStudent;
	}

	public String getLoginPersonUnitNumber() {
		return loginPersonUnitNumber;
	}

	public void setLoginPersonUnitNumber(String loginPersonUnitNumber) {
		this.loginPersonUnitNumber = loginPersonUnitNumber;
	}

	public List<Integer> getSectionCodes() {
		return sectionCodes;
	}

	public void setSectionCodes(List<Integer> sectionCodes) {
		this.sectionCodes = sectionCodes;
	}

	public String getCostElementCode() {
		return costElementCode;
	}

	public void setCostElementCode(String costElementCode) {
		this.costElementCode = costElementCode;
	}

	public Integer getLevel() {
		return level;
	}

	public void setLevel(Integer level) {
		this.level = level;
	}

	public Boolean getIncludeClosedGrantCall() {
		return includeClosedGrantCall;
	}

	public void setIncludeClosedGrantCall(Boolean includeClosedGrantCall) {
		this.includeClosedGrantCall = includeClosedGrantCall;
	}

	public Integer getAwardPersonId() {
		return awardPersonId;
	}

	public void setAwardPersonId(Integer awardPersonId) {
		this.awardPersonId = awardPersonId;
	}

	public String getResearchTypeCode() {
		return researchTypeCode;
	}

	public void setResearchTypeCode(String researchTypeCode) {
		this.researchTypeCode = researchTypeCode;
	}

	public String getResearchTypeAreaCode() {
		return researchTypeAreaCode;
	}

	public void setResearchTypeAreaCode(String researchTypeAreaCode) {
		this.researchTypeAreaCode = researchTypeAreaCode;
	}

	public String getCheck() {
		return check;
	}

	public void setCheck(String check) {
		this.check = check;
	}

	public List<String> getRightName() {
		return rightName;
	}

	public Boolean getIsEnableAddtoAddressBook() {
		return isEnableAddtoAddressBook;
	}

	public void setIsEnableAddtoAddressBook(Boolean isEnableAddtoAddressBook) {
		this.isEnableAddtoAddressBook = isEnableAddtoAddressBook;
	}

	public String getManpowerRequestType() {
		return manpowerRequestType;
	}

	public void setManpowerRequestType(String manpowerRequestType) {
		this.manpowerRequestType = manpowerRequestType;
  }
  
	public Boolean getTriageQuestionnaireRequired() {
		return triageQuestionnaireRequired;
	}

	public void setTriageQuestionnaireRequired(Boolean triageQuestionnaireRequired) {
		this.triageQuestionnaireRequired = triageQuestionnaireRequired;
	}

	public List<FileType> getFileTypes() {
		return fileTypes;
	}

	public void setFileTypes(List<FileType> fileTypes) {
		this.fileTypes = fileTypes;
	}

	public String getSponsorGroupId() {
		return sponsorGroupId;
	}

	public void setSponsorGroupId(String sponsorGroupId) {
		this.sponsorGroupId = sponsorGroupId;
	}

	public List<String> getSponsorCodes() {
		return sponsorCodes;
	}

	public void setSponsorCodes(List<String> sponsorCodes) {
		this.sponsorCodes = sponsorCodes;
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

	public String getDescentFlag() {
		return descentFlag;
	}

	public void setDescentFlag(String descentFlag) {
		this.descentFlag = descentFlag;
	}

}
