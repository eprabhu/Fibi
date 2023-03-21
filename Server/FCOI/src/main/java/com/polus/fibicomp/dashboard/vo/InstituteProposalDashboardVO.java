package com.polus.fibicomp.dashboard.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InstituteProposalDashboardVO {

	private Integer pageNumber;

	private String sortBy;

	private String reverse;

	private String personId;

	private String property1;

	private String property2;

	private List<Integer> property3;

	private List<String> property4;

	private String property5;

	private Integer currentPage;

	private String documentHeading;

	private String exportType;

	private Boolean isCount = false;

	private String tabName;

	private Map<String, String> sort = new HashMap<String, String>();

	private Boolean isDownload;

	private String advancedSearch = "L";

	private String property6;

	private List<Integer> property7;

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

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getProperty1() {
		return property1;
	}

	public void setProperty1(String property1) {
		this.property1 = property1;
	}

	public String getProperty2() {
		return property2;
	}

	public void setProperty2(String property2) {
		this.property2 = property2;
	}

	public List<Integer> getProperty3() {
		return property3;
	}

	public void setProperty3(List<Integer> property3) {
		this.property3 = property3;
	}

	public List<String> getProperty4() {
		return property4;
	}

	public void setProperty4(List<String> property4) {
		this.property4 = property4;
	}

	public String getProperty5() {
		return property5;
	}

	public void setProperty5(String property5) {
		this.property5 = property5;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
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

	public Boolean getIsCount() {
		return isCount;
	}

	public void setIsCount(Boolean isCount) {
		this.isCount = isCount;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public String getAdvancedSearch() {
		return advancedSearch;
	}

	public void setAdvancedSearch(String advancedSearch) {
		this.advancedSearch = advancedSearch;
	}

	public String getProperty6() {
		return property6;
	}

	public void setProperty6(String property6) {
		this.property6 = property6;
	}

	public List<Integer> getProperty7() {
		return property7;
	}

	public void setProperty7(List<Integer> property7) {
		this.property7 = property7;
	}

}
