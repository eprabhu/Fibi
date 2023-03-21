package com.polus.fibicomp.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "LOOKUP_WINDOW")
public class LookupWindow implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "LOOKUP_WINDOW_NAME")
	private String lookupWindowName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "TABLE_NAME")
	private String tableName;

	@Column(name = "COLUMN_NAME")
	private String columnName;

	@Column(name = "OTHERS_DISPLAY_COLUMNS")
	private String otherDisplayColumn;

	@Column(name = "DATA_TYPE_CODE")
	private String dataTypeCode;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Transient
	private List<String> userLookup;

	public String getLookupWindowName() {
		return lookupWindowName;
	}

	public void setLookupWindowName(String lookupWindowName) {
		this.lookupWindowName = lookupWindowName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getTableName() {
		return tableName;
	}

	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	public String getColumnName() {
		return columnName;
	}

	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}

	public String getOtherDisplayColumn() {
		return otherDisplayColumn;
	}

	public void setOtherDisplayColumn(String otherDisplayColumn) {
		this.otherDisplayColumn = otherDisplayColumn;
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

	public String getDataTypeCode() {
		return dataTypeCode;
	}

	public void setDataTypeCode(String dataTypeCode) {
		this.dataTypeCode = dataTypeCode;
	}

	public List<String> getUserLookup() {
		return userLookup;
	}

	public void setUserLookup(List<String> userLookup) {
		this.userLookup = userLookup;
	}

}
