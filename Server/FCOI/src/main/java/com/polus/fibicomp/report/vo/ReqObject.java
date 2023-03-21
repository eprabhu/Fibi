package com.polus.fibicomp.report.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.report.pojo.ReportColumns;

public class ReqObject {

	private String reportTypeId;

	private String exportType;

	private Map<String, Object> inputParams;

	private List<ReportColumns> columns;

	private List<Field> fields;

	public String getReportTypeId() {
		return reportTypeId;
	}

	public void setReportTypeId(String reportTypeId) {
		this.reportTypeId = reportTypeId;
	}

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

	public Map<String, Object> getInputParams() {
		return inputParams;
	}

	public void setInputParams(Map<String, Object> inputParams) {
		this.inputParams = inputParams;
	}

	public List<ReportColumns> getColumns() {
		return columns;
	}

	public void setColumns(List<ReportColumns> columns) {
		this.columns = columns;
	}

	public List<Field> getFields() {
		return fields;
	}

	public void setFields(List<Field> fields) {
		this.fields = fields;
	}

}
