package com.polus.fibicomp.questionnaire.dto;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.vo.LookUp;

public class QuestionnaireDataBus {

	private List<HashMap<String, Object>> applicableQuestionnaire;

	private Integer questionnaireId;

	private String moduleItemKey;

	private String moduleSubItemKey;

	private Integer moduleItemCode;

	private Integer moduleSubItemCode;

	private Integer questionnaireAnswerHeaderId;

	private Integer questionnaireAnsAttachmentId;

	private String questionnaireCompleteFlag;

	private String actionUserId;

	private String actionPersonId;

	private String actionPersonName;

	private String acType;

	private String questionnaireName;

	private boolean newQuestionnaireVersion;

	private boolean questionEditted;

	private List<HashMap<String, Object>> questionnaireList;

	private List<HashMap<String, Object>> questionnaireGroup;

	private Map<String, Object> header;

	private QuestionnaireDto questionnaire;

	private List<HashMap<String, Object>> usage;

	private String fileName;

	private String fileContent;

	private Integer length;

	private Integer remaining;

	private String fileTimestamp;

	private String contentType;

	private String personId;

	private String multipartFile;

	private List<HashMap<String, Object>> moduleList;

	private Boolean isInserted;

	private Timestamp updateTimestamp;

	private String copyModuleItemKey;

	private List<Integer> questionnaireNumbers;

	private List<LookUp> lookUpDetails;

	private Integer newQuestionnaireId;
	
	private List<Integer> moduleSubItemCodes;

	private List<HashMap<String, Object>> questionnaireBusinessRules;

	private String ruleId;

	private Boolean rulePassed;
	
	private String questionnaireMode;

	public QuestionnaireDataBus() {
		moduleSubItemCodes = new ArrayList<>();
	}

	public List<HashMap<String, Object>> getApplicableQuestionnaire() {
		return applicableQuestionnaire;
	}

	public void setApplicableQuestionnaire(List<HashMap<String, Object>> applicableQuestionnaire) {
		this.applicableQuestionnaire = applicableQuestionnaire;
	}

	public List<HashMap<String, Object>> getUsage() {
		return usage;
	}

	public void setUsage(List<HashMap<String, Object>> usage) {
		this.usage = usage;
	}

	public Map<String, Object> getHeader() {
		return header;
	}

	public void setHeader(Map<String, Object> header) {
		this.header = header;
	}

	public QuestionnaireDto getQuestionnaire() {
		return questionnaire;
	}

	public void setQuestionnaire(QuestionnaireDto questionnaire) {
		this.questionnaire = questionnaire;
	}

	public List<HashMap<String, Object>> getQuestionnaireList() {
		return questionnaireList;
	}

	public void setQuestionnaireList(List<HashMap<String, Object>> questionnaireList) {
		this.questionnaireList = questionnaireList;
	}

	public List<HashMap<String, Object>> getQuestionnaireGroup() {
		return questionnaireGroup;
	}

	public void setQuestionnaireGroup(List<HashMap<String, Object>> questionnaireGroup) {
		this.questionnaireGroup = questionnaireGroup;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getQuestionnaireName() {
		return questionnaireName;
	}

	public void setQuestionnaireName(String questionnaireName) {
		this.questionnaireName = questionnaireName;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getMultipartFile() {
		return multipartFile;
	}

	public void setMultipartFile(String multipartFile) {
		this.multipartFile = multipartFile;
	}

	public Integer getQuestionnaireId() {
		return questionnaireId;
	}

	public void setQuestionnaireId(Integer questionnaireId) {
		this.questionnaireId = questionnaireId;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleSubItemKey() {
		return moduleSubItemKey;
	}

	public void setModuleSubItemKey(String moduleSubItemKey) {
		this.moduleSubItemKey = moduleSubItemKey;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public Integer getModuleSubItemCode() {
		return moduleSubItemCode;
	}

	public void setModuleSubItemCode(Integer moduleSubItemCode) {
		this.moduleSubItemCode = moduleSubItemCode;
	}

	public Integer getQuestionnaireAnswerHeaderId() {
		return questionnaireAnswerHeaderId;
	}

	public void setQuestionnaireAnswerHeaderId(Integer questionnaireAnswerHeaderId) {
		this.questionnaireAnswerHeaderId = questionnaireAnswerHeaderId;
	}

	public Integer getQuestionnaireAnsAttachmentId() {
		return questionnaireAnsAttachmentId;
	}

	public void setQuestionnaireAnsAttachmentId(Integer questionnaireAnsAttachmentId) {
		this.questionnaireAnsAttachmentId = questionnaireAnsAttachmentId;
	}

	public String getQuestionnaireCompleteFlag() {
		return questionnaireCompleteFlag;
	}

	public void setQuestionnaireCompleteFlag(String questionnaireCompleteFlag) {
		this.questionnaireCompleteFlag = questionnaireCompleteFlag;
	}

	public String getActionUserId() {
		return actionUserId;
	}

	public void setActionUserId(String actionUserId) {
		this.actionUserId = actionUserId;
	}

	public String getActionPersonId() {
		return actionPersonId;
	}

	public void setActionPersonId(String actionPersonId) {
		this.actionPersonId = actionPersonId;
	}

	public String getActionPersonName() {
		return actionPersonName;
	}

	public void setActionPersonName(String actionPersonName) {
		this.actionPersonName = actionPersonName;
	}

	public boolean isNewQuestionnaireVersion() {
		return newQuestionnaireVersion;
	}

	public void setNewQuestionnaireVersion(boolean newQuestionnaireVersion) {
		this.newQuestionnaireVersion = newQuestionnaireVersion;
	}

	public boolean isQuestionEditted() {
		return questionEditted;
	}

	public void setQuestionEditted(boolean questionEditted) {
		this.questionEditted = questionEditted;
	}

	public List<HashMap<String, Object>> getModuleList() {
		return moduleList;
	}

	public void setModuleList(List<HashMap<String, Object>> moduleList) {
		this.moduleList = moduleList;
	}

	public Boolean getIsInserted() {
		return isInserted;
	}

	public void setIsInserted(Boolean isInserted) {
		this.isInserted = isInserted;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getCopyModuleItemKey() {
		return copyModuleItemKey;
	}

	public void setCopyModuleItemKey(String copyModuleItemKey) {
		this.copyModuleItemKey = copyModuleItemKey;
  }
  
	public List<LookUp> getLookUpDetails() {
		return lookUpDetails;
	}

	public void setLookUpDetails(List<LookUp> lookUpDetails) {
		this.lookUpDetails = lookUpDetails;
	}

	public List<Integer> getModuleSubItemCodes() {
		return moduleSubItemCodes;
	}

	public void setModuleSubItemCodes(List<Integer> moduleSubItemCodes) {
		this.moduleSubItemCodes = moduleSubItemCodes;
	}

	public List<Integer> getQuestionnaireNumbers() {
		return questionnaireNumbers;
	}

	public void setQuestionnaireNumbers(List<Integer> questionnaireNumbers) {
		this.questionnaireNumbers = questionnaireNumbers;
	}

	public Integer getNewQuestionnaireId() {
		return newQuestionnaireId;
	}

	public void setNewQuestionnaireId(Integer newQuestionnaireId) {
		this.newQuestionnaireId = newQuestionnaireId;
	}

	public List<HashMap<String, Object>> getQuestionnaireBusinessRules() {
		return questionnaireBusinessRules;
	}

	public void setQuestionnaireBusinessRules(List<HashMap<String, Object>> questionnaireBusinessRules) {
		this.questionnaireBusinessRules = questionnaireBusinessRules;
	}

	public String getRuleId() {
		return ruleId;
	}

	public void setRuleId(String ruleId) {
		this.ruleId = ruleId;
	}

	public Boolean getRulePassed() {
		return rulePassed;
	}

	public void setRulePassed(Boolean rulePassed) {
		this.rulePassed = rulePassed;
	}

	public String getQuestionnaireMode() {
		return questionnaireMode;
	}

	public void setQuestionnaireMode(String questionnaireMode) {
		this.questionnaireMode = questionnaireMode;
	}

}
