package com.polus.fibicomp.codetable.dto;

import java.util.HashMap;
import java.util.List;

public class CodeTable {

	private String group;

	private String codeTableName;

	private String description;

	private String databaseTableName;

	private List<Fields> fields;

	private List<String> primaryKey;

	private List<HashMap<String, Object>> dependency;

	private String actions;

	private String fileColumnName;

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public String getCodeTableName() {
		return codeTableName;
	}

	public void setCodeTableName(String codeTableName) {
		this.codeTableName = codeTableName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getDatabaseTableName() {
		return databaseTableName;
	}

	public void setDatabaseTableName(String databaseTableName) {
		this.databaseTableName = databaseTableName;
	}

	public List<Fields> getFields() {
		return fields;
	}

	public void setFields(List<Fields> fields) {
		this.fields = fields;
	}

	public List<String> getPrimaryKey() {
		return primaryKey;
	}

	public void setPrimaryKey(List<String> primaryKey) {
		this.primaryKey = primaryKey;
	}

	public List<HashMap<String, Object>> getDependency() {
		return dependency;
	}

	public void setDependency(List<HashMap<String, Object>> dependency) {
		this.dependency = dependency;
	}

	public String getActions() {
		return actions;
	}

	public void setActions(String actions) {
		this.actions = actions;
	}

	public String getFileColumnName() {
		return fileColumnName;
	}

	public void setFileColumnName(String fileColumnName) {
		this.fileColumnName = fileColumnName;
	}

}
