package com.polus.fibicomp.dashboard.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

public class ServiceRequestDashboardVO {
	
	private Integer pageNumber;

	@Pattern(regexp="^$|[\\.0-9a-zA-Z _-]*$", message="sortBy must not include special characters except '.','-' and '_'.")
	private String sortBy;

	@Pattern(regexp="^$|[\\.0-9a-zA-Z _-]*$", message="reverse must not include special characters except '.','-' and '_'.")
	private String reverse;

	@Pattern(regexp="^$|[0-9a-zA-Z -]*$", message="Request ID must not include special characters.")
	private String serviceRequestId;

	@Pattern(regexp="^$|[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Request Subject must not include special characters.")
	private String serviceRequestSubject;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Request Type must not include special characters.") String> moduleCodes;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Request Type must not include special characters.") String> srTypeCodes;

	private List<@Pattern(regexp = "^$|[0-9]*$", message =  "Status must not include special characters.") String> srStatusCodes;

	@Pattern(regexp="^$|[0-9a-zA-Z ()]+$", message="Lead Unit must not include special characters.")
	private String unitName;
	
	private String unitNumber;

	private List</*
					 * @Pattern(regexp = "^$|[0-9]*$", message =
					 * "Request Type must not include special characters.")
					 */String> srPriorities;

	private Integer currentPage;
	
	private Boolean isDownload;

	//@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="tabName must not include special characters except '_'.")
	private String tabName;

	//@Pattern(regexp="^$|[AL]+$", message="advancedSearch must not include special characters.")
	private String advancedSearch = "L";
	
	private Map</*@Pattern(regexp="^$|[.0-9a-zA-Z]+$", message="Sort key must not include special characters except '.'.")*/ String, @Pattern(regexp="^$|[0-9a-zA-Z]+$", message="Sort value must not include special characters.") String> sort = new HashMap<>();

	//@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="procedureName must not include special characters except '_'.")
	private String procedureName;

	//@Pattern(regexp="^$|[0-9a-zA-Z _-]*$", message="documentHeading must not include special characters.")
	private String documentHeading;

	//@Pattern(regexp="^$|[a-zA-Z]*$", message="exportType must not include special characters.")
	private String exportType;

	
	private String personId;

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public String getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(String serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public String getServiceRequestSubject() {
		return serviceRequestSubject;
	}

	public void setServiceRequestSubject(String serviceRequestSubject) {
		this.serviceRequestSubject = serviceRequestSubject;
	}

	public List<String> getModuleCodes() {
		return moduleCodes;
	}

	public void setModuleCodes(List<String> moduleCodes) {
		this.moduleCodes = moduleCodes;
	}

	public List<String> getSrTypeCodes() {
		return srTypeCodes;
	}

	public void setSrTypeCodes(List<String> srTypeCodes) {
		this.srTypeCodes = srTypeCodes;
	}

	public List<String> getSrStatusCodes() {
		return srStatusCodes;
	}

	public void setSrStatusCodes(List<String> srStatusCodes) {
		this.srStatusCodes = srStatusCodes;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public List<String> getSrPriorities() {
		return srPriorities;
	}

	public void setSrPriorities(List<String> srPriorities) {
		this.srPriorities = srPriorities;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public String getAdvancedSearch() {
		return advancedSearch;
	}

	public void setAdvancedSearch(String advancedSearch) {
		this.advancedSearch = advancedSearch;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public String getProcedureName() {
		return procedureName;
	}

	public void setProcedureName(String procedureName) {
		this.procedureName = procedureName;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

}
