package com.polus.fibicomp.vo;

public abstract class EndPointDetails {

	private String tableName;

	private String primaryColumn;

	private String descriptionColumn;

	public EndPointDetails(String tableName, String primaryColumn, String descriptionColumn) {
		this.tableName = tableName;
		this.primaryColumn = primaryColumn;
		this.descriptionColumn = descriptionColumn;
	}

	public String getTableName() {
		return tableName;
	}

	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	public String getPrimaryColumn() {
		return primaryColumn;
	}

	public void setPrimaryColumn(String primaryColumn) {
		this.primaryColumn = primaryColumn;
	}

	public String getDescriptionColumn() {
		return descriptionColumn;
	}

	public void setDescriptionColumn(String descriptionColumn) {
		this.descriptionColumn = descriptionColumn;
	}

}
