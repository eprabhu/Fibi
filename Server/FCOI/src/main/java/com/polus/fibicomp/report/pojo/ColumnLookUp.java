package com.polus.fibicomp.report.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name= "COLUMN_LOOKUP")
public class ColumnLookUp implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COLUMN_ID")
	private Integer columnId;

	@Column(name = "COLUMN_NAME")
	private String columnName;

	@Column(name = "FILTER_TYPE")
	private String filterType;

	@Column(name = "VALUE_FIELD")
	private String valueField;

	@Column(name = "INDEX_FIELD")
	private String indexField;

	@Column(name = "QUERY_TYPE")
	private String queryType;

	@Column(name = "INNER_JOIN")
	private String innerJoin;

	@Column(name = "SUMMABLE")
	private String summable;

	@Column(name = "IS_CURRENCY")
	private String isCurrency;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Transient
	private String displayName;

	@Transient
	private String defaultValue;

	public Integer getColumnId() {
		return columnId;
	}

	public void setColumnId(Integer columnId) {
		this.columnId = columnId;
	}

	public String getColumnName() {
		return columnName;
	}

	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}

	public String getFilterType() {
		return filterType;
	}

	public void setFilterType(String filterType) {
		this.filterType = filterType;
	}

	public String getValueField() {
		return valueField;
	}

	public void setValueField(String valueField) {
		this.valueField = valueField;
	}

	public String getIndexField() {
		return indexField;
	}

	public void setIndexField(String indexField) {
		this.indexField = indexField;
	}

	public String getQueryType() {
		return queryType;
	}

	public void setQueryType(String queryType) {
		this.queryType = queryType;
	}

	public String getInnerJoin() {
		return innerJoin;
	}

	public void setInnerJoin(String innerJoin) {
		this.innerJoin = innerJoin;
	}

	public String getSummable() {
		return summable;
	}

	public void setSummable(String summable) {
		this.summable = summable;
	}

	public String getIsCurrency() {
		return isCurrency;
	}

	public void setIsCurrency(String isCurrency) {
		this.isCurrency = isCurrency;
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

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

}
