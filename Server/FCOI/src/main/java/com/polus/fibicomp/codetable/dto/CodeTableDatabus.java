package com.polus.fibicomp.codetable.dto;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.codetable.pojo.CodeTableConfiguration;

public class CodeTableDatabus {

	private CodeTable codeTable;

	private List<CodeTable> codeTables;

	private Map<String, Object> configFile;

	private List<HashMap<String, Object>> tableData;

	private List<HashMap<String, Object>> primaryValues;

	private String selectedColumnForDownload;

	private String updatedUser;

	private String promptMessage;

	private Integer promptCode; //If 1 then successful else If 0 then error found

	private String fileName;

	private String fileContent;

	private Integer length;

	private Integer remaining;

	private String fileTimestamp;

	private String contentType;

	private String personId;

	private List<CodeTableConfiguration> codeTableConfigurations;

	private CodeTableConfiguration codeTableConfiguration;

	private String tableName;

	private List<String> changedMap;

	public Map<String, Object> getConfigFile() {
		return configFile;
	}

	public void setConfigFile(Map<String, Object> configFile) {
		this.configFile = configFile;
	}

	public List<HashMap<String, Object>> getTableData() {
		return tableData;
	}

	public void setTableData(List<HashMap<String, Object>> tableData) {
		this.tableData = tableData;
	}

	public String getPromptMessage() {
		return promptMessage;
	}

	public void setPromptMessage(String promptMessage) {
		this.promptMessage = promptMessage;
	}

	public String getUpdatedUser() {
		return updatedUser;
	}

	public void setUpdatedUser(String updatedUser) {
		this.updatedUser = updatedUser;
	}

	public Integer getPromptCode() {
		return promptCode;
	}

	public void setPromptCode(Integer promptCode) {
		this.promptCode = promptCode;
	}

	public String getSelectedColumnForDownload() {
		return selectedColumnForDownload;
	}

	public void setSelectedColumnForDownload(String selectedColumnForDownload) {
		this.selectedColumnForDownload = selectedColumnForDownload;
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

	public List<HashMap<String, Object>> getPrimaryValues() {
		return primaryValues;
	}

	public void setPrimaryValues(List<HashMap<String, Object>> primaryValues) {
		this.primaryValues = primaryValues;
	}

	public CodeTable getCodeTable() {
		return codeTable;
	}

	public void setCodeTable(CodeTable codeTable) {
		this.codeTable = codeTable;
	}

	public List<CodeTableConfiguration> getCodeTableConfigurations() {
		return codeTableConfigurations;
	}

	public void setCodeTableConfigurations(List<CodeTableConfiguration> codeTableConfigurations) {
		this.codeTableConfigurations = codeTableConfigurations;
	}

	public CodeTableConfiguration getCodeTableConfiguration() {
		return codeTableConfiguration;
	}

	public void setCodeTableConfiguration(CodeTableConfiguration codeTableConfiguration) {
		this.codeTableConfiguration = codeTableConfiguration;
	}

	public String getTableName() {
		return tableName;
	}

	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	public List<String> getChangedMap() {
		return changedMap;
	}

	public void setChangedMap(List<String> changedMap) {
		this.changedMap = changedMap;
	}

	public List<CodeTable> getCodeTables() {
		return codeTables;
	}

	public void setCodeTables(List<CodeTable> codeTables) {
		this.codeTables = codeTables;
	}
}
