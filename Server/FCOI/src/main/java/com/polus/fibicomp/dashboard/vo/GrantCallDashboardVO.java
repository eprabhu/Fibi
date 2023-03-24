package com.polus.fibicomp.dashboard.vo;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

public class GrantCallDashboardVO {

	@Pattern(regexp="^$|[\\.0-9a-zA-Z _-]*$", message="sortBy must not include special characters except '.','-' and '_'.")
	private String sortBy;

	private String personId;

	@Pattern(regexp="^[0-9a-zA-Z -]*$", message="Grant Call Id ID must not include special characters.")
	private String property1;

	@Pattern(regexp="^[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Grant call title must not include special characters.")
	private String property2;

	private List<Integer> property3;

	@Pattern(regexp="^$|[0-9a-zA-Z (),.]*$", message="Funding agency must not include special characters.")
	private String property4;

	private List<Integer> property5;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Relevant Fields must not include special characters.") String> property6;
	
	@Pattern(regexp="^$|[0-9a-zA-Z _(),.-]*$", message="Funding Scheme Name must not include special characters.")
	private String property7;

	private Timestamp property13;

	private Timestamp property14;

	private boolean canCreateGrantCall;

	private Integer pageNumber;

	private Integer currentPage;

	private Boolean isCalenderRequired = Boolean.FALSE;

	private Integer grantCallId;
	
	private String documentHeading;

	private String exportType;
	
	private Boolean isGrantCallLinked = false;

	@Pattern(regexp="^[AL]*$", message="advancedSearch must not include special characters.")
	private String advancedSearch = "L";
	
	private Boolean isDownload;
	
	private String tabName;

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	private Map<String, String> sort = new HashMap<>();

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

	public String getProperty4() {
		return property4;
	}

	public void setProperty4(String property4) {
		this.property4 = property4;
	}

	public List<Integer> getProperty5() {
		return property5;
	}

	public void setProperty5(List<Integer> property5) {
		this.property5 = property5;
	}

	public List<String> getProperty6() {
		return property6;
	}

	public void setProperty6(List<String> property6) {
		this.property6 = property6;
	}

	public Timestamp getProperty13() {
		return property13;
	}

	public void setProperty13(Timestamp property13) {
		this.property13 = property13;
	}

	public Timestamp getProperty14() {
		return property14;
	}

	public void setProperty14(Timestamp property14) {
		this.property14 = property14;
	}

	public boolean getCanCreateGrantCall() {
		return canCreateGrantCall;
	}

	public void setCanCreateGrantCall(boolean canCreateGrantCall) {
		this.canCreateGrantCall = canCreateGrantCall;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public Boolean getIsCalenderRequired() {
		return isCalenderRequired;
	}

	public void setIsCalenderRequired(Boolean isCalenderRequired) {
		this.isCalenderRequired = isCalenderRequired;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
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

	public Boolean getIsGrantCallLinked() {
		return isGrantCallLinked;
	}

	public void setIsGrantCallLinked(Boolean isGrantCallLinked) {
		this.isGrantCallLinked = isGrantCallLinked;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public String getAdvancedSearch() {
		return advancedSearch;
	}

	public void setAdvancedSearch(String advancedSearch) {
		this.advancedSearch = advancedSearch;
	}

}
