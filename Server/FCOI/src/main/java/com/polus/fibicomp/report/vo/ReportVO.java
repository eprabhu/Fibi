package com.polus.fibicomp.report.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.report.pojo.ReportTemplate;
import com.polus.fibicomp.report.pojo.ReportType;
import com.polus.fibicomp.view.AwardView;
import com.polus.fibicomp.view.ExpenditureByAwardView;

public class ReportVO {

	private Integer grantCallId;

	private Integer proposalCount;

	private String reportName;

	private List<GrantCall> grantIds;

	private List<Proposal> proposals;

	private Map<String, List<Proposal>> applicationsByGrantCallType;

	private Map<String, List<AwardView>> awardByGrantType;

	private Integer awardCount;

	private List<AwardView> awards;

	private String personId;

	private List<AwardView> awardNumbers;

	private String awardNumber;

	private List<ExpenditureByAwardView> expenditureList;

	private Map<String, Object> configFile;

	private String promptMessage;

	private Integer promptCode;

	private String reportTypeId;

	private ReportTemplate reportTemplate;

	private Template template;

	private Integer reportTemplateId;

	private List<ReportTemplate> reportTemplates;

	private List<HashMap<String, Object>> generatedReport;

	private ResponseEntity<byte[]> generatedReportInByte;

	private String queryString;

	private List<String> headers;

	private List<Object[]> datas;

	private String documentHeading;

	private String selectedFields;

	private String conditions;

	private String reportView;

	private ReportType reportType;

	private Integer moduleCode;

	private String joinWhereClause;

	private List<String> reportCriteria;

	private Map<String, String> headerDetails;

	private Integer totalRecords;

	private String message;

	private Boolean isReportAdmin = false;

	private List<Integer> specialIndex = null;

	private Boolean isBirt;

	private String exportType;

	private Map<String, Object> inputPrams;

	private List<Field> inputParamDetails;

	public Boolean getIsBirt() {
		return isBirt;
	}

	public void setIsBirt(Boolean isBirt) {
		this.isBirt = isBirt;
	}

	public Boolean getIsReportAdmin() {
		return isReportAdmin;
	}

	public void setIsReportAdmin(Boolean isReportAdmin) {
		this.isReportAdmin = isReportAdmin;
	}

	public String getReportName() {
		return reportName;
	}

	public void setReportName(String reportName) {
		this.reportName = reportName;
	}

	public List<GrantCall> getGrantIds() {
		return grantIds;
	}

	public void setGrantIds(List<GrantCall> grantIds) {
		this.grantIds = grantIds;
	}

	public List<Proposal> getProposals() {
		return proposals;
	}

	public void setProposals(List<Proposal> proposals) {
		this.proposals = proposals;
	}

	public Integer getProposalCount() {
		return proposalCount;
	}

	public void setProposalCount(Integer proposalCount) {
		this.proposalCount = proposalCount;
	}

	public Integer getAwardCount() {
		return awardCount;
	}

	public void setAwardCount(Integer awardCount) {
		this.awardCount = awardCount;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<AwardView> getAwardNumbers() {
		return awardNumbers;
	}

	public void setAwardNumbers(List<AwardView> awardNumbers) {
		this.awardNumbers = awardNumbers;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public List<AwardView> getAwards() {
		return awards;
	}

	public void setAwards(List<AwardView> awards) {
		this.awards = awards;
	}

	public Map<String, List<Proposal>> getApplicationsByGrantCallType() {
		return applicationsByGrantCallType;
	}

	public void setApplicationsByGrantCallType(Map<String, List<Proposal>> applicationsByGrantCallType) {
		this.applicationsByGrantCallType = applicationsByGrantCallType;
	}

	public List<ExpenditureByAwardView> getExpenditureList() {
		return expenditureList;
	}

	public void setExpenditureList(List<ExpenditureByAwardView> expenditureList) {
		this.expenditureList = expenditureList;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public Map<String, List<AwardView>> getAwardByGrantType() {
		return awardByGrantType;
	}

	public void setAwardByGrantType(Map<String, List<AwardView>> awardByGrantType) {
		this.awardByGrantType = awardByGrantType;
	}

	public Map<String, Object> getConfigFile() {
		return configFile;
	}

	public void setConfigFile(Map<String, Object> configFile) {
		this.configFile = configFile;
	}


	public String getPromptMessage() {
		return promptMessage;
	}

	public void setPromptMessage(String promptMessage) {
		this.promptMessage = promptMessage;
	}

	public Integer getPromptCode() {
		return promptCode;
	}

	public void setPromptCode(Integer promptCode) {
		this.promptCode = promptCode;
	}

	public ReportTemplate getReportTemplate() {
		return reportTemplate;
	}

	public void setReportTemplate(ReportTemplate reportTemplate) {
		this.reportTemplate = reportTemplate;
	}

	public Template getTemplate() {
		return template;
	}

	public void setTemplate(Template template) {
		this.template = template;
	}

	public Integer getReportTemplateId() {
		return reportTemplateId;
	}

	public void setReportTemplateId(Integer reportTemplateId) {
		this.reportTemplateId = reportTemplateId;
	}

	public List<ReportTemplate> getReportTemplates() {
		return reportTemplates;
	}

	public void setReportTemplates(List<ReportTemplate> reportTemplates) {
		this.reportTemplates = reportTemplates;
	}

	public ResponseEntity<byte[]> getGeneratedReportInByte() {
		return generatedReportInByte;
	}

	public void setGeneratedReportInByte(ResponseEntity<byte[]> generatedReportInByte) {
		this.generatedReportInByte = generatedReportInByte;
	}

	public String getQueryString() {
		return queryString;
	}

	public void setQueryString(String queryString) {
		this.queryString = queryString;
	}

	public List<String> getHeaders() {
		return headers;
	}

	public void setHeaders(List<String> headers) {
		this.headers = headers;
	}

	public List<Object[]> getDatas() {
		return datas;
	}

	public void setDatas(List<Object[]> datas) {
		this.datas = datas;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public String getSelectedFields() {
		return selectedFields;
	}

	public void setSelectedFields(String selectedFields) {
		this.selectedFields = selectedFields;
	}

	public String getConditions() {
		return conditions;
	}

	public void setConditions(String conditions) {
		this.conditions = conditions;
	}

	public String getReportView() {
		return reportView;
	}

	public void setReportView(String reportView) {
		this.reportView = reportView;
	}

	public String getReportTypeId() {
		return reportTypeId;
	}

	public void setReportTypeId(String reportTypeId) {
		this.reportTypeId = reportTypeId;
	}

	public ReportType getReportType() {
		return reportType;
	}

	public void setReportType(ReportType reportType) {
		this.reportType = reportType;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public List<HashMap<String, Object>> getGeneratedReport() {
		return generatedReport;
	}

	public void setGeneratedReport(List<HashMap<String, Object>> generatedReport) {
		this.generatedReport = generatedReport;
	}

	public String getJoinWhereClause() {
		return joinWhereClause;
	}

	public void setJoinWhereClause(String joinWhereClause) {
		this.joinWhereClause = joinWhereClause;
	}

	public List<String> getReportCriteria() {
		return reportCriteria;
	}

	public void setReportCriteria(List<String> reportCriteria) {
		this.reportCriteria = reportCriteria;
	}

	public Map<String, String> getHeaderDetails() {
		return headerDetails;
	}

	public void setHeaderDetails(Map<String, String> headerDetails) {
		this.headerDetails = headerDetails;
	}

	public Integer getTotalRecords() {
		return totalRecords;
	}

	public void setTotalRecords(Integer totalRecords) {
		this.totalRecords = totalRecords;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<Integer> getSpecialIndex() {
		return specialIndex;
	}

	public void setSpecialIndex(List<Integer> specialIndex) {
		this.specialIndex = specialIndex;
	}

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

	public List<Field> getInputParamDetails() {
		return inputParamDetails;
	}

	public void setInputParamDetails(List<Field> inputParamDetails) {
		this.inputParamDetails = inputParamDetails;
	}

	public Map<String, Object> getInputPrams() {
		return inputPrams;
	}

	public void setInputPrams(Map<String, Object> inputPrams) {
		this.inputPrams = inputPrams;
	}

}
