package com.polus.fibicomp.report.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name= "REPORT_COLUMNS")
public class ReportColumns implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REPORT_COLUMN_ID")
	private Integer reportColumnId;

	@Column(name = "TYPE_CODE")
	private String typeCode;

	@Column(name = "COLUMN_ID")
	private Integer columId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "REPORT_COLUMNS_FK1"), name = "COLUMN_ID", referencedColumnName = "COLUMN_ID", insertable = false, updatable = false)
	private ColumnLookUp columnLookUp;

	@Column(name = "IS_FILTER")
	private String isFilter;

	@Column(name = "IS_COLUMN")
	private String isColumn;

	@Column(name = "DISPLAY_NAME")
	private String displayName;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getReportColumnId() {
		return reportColumnId;
	}

	public void setReportColumnId(Integer reportColumnId) {
		this.reportColumnId = reportColumnId;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public Integer getColumId() {
		return columId;
	}

	public void setColumId(Integer columId) {
		this.columId = columId;
	}

	public String getIsFilter() {
		return isFilter;
	}

	public void setIsFilter(String isFilter) {
		this.isFilter = isFilter;
	}

	public String getIsColumn() {
		return isColumn;
	}

	public void setIsColumn(String isColumn) {
		this.isColumn = isColumn;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
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

	public ColumnLookUp getColumnLookUp() {
		return columnLookUp;
	}

	public void setColumnLookUp(ColumnLookUp columnLookUp) {
		this.columnLookUp = columnLookUp;
	}

}
