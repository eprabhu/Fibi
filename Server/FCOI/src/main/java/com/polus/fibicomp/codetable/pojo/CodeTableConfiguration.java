package com.polus.fibicomp.codetable.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "CODE_TABLE_CONFIGURATION")
public class CodeTableConfiguration implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TABLE_NAME")
	private String tableName;

	@Column(name = "DISPLAY_NAME")
	private String displayName;

	@Column(name = "CONTENT")
	private String content;

	@Column(name = "GROUP_NAME")
	private String groupName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public CodeTableConfiguration() {
	}

	public CodeTableConfiguration(String tableName, String displayName, String groupName, String description) {
		super();
		this.tableName = tableName;
		this.displayName = displayName;
		this.groupName = groupName;
		this.description = description;
	}

	public String getTableName() {
		return tableName;
	}

	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
