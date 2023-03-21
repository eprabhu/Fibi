package com.polus.fibicomp.dashboard.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

public class ProposalDashboardVO {

	private Integer pageNumber;

	@Pattern(regexp="^$|[0-9a-zA-Z -]*$", message="Proposal ID must not include special characters.")
	private String property1;

	@Pattern(regexp="^$|[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Title must not include special characters.")
	private String property2;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Proposal Status must not include special characters.") String> property3;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Category must not include special characters.") String> property4;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Researcher Name must not include special characters.")
	private String property5;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Proposal Type must not include special characters.") String> property6;

	@Pattern(regexp="^$|[0-9a-zA-Z *_(),&.]*$", message="Sponsor must not include special characters except '-' and '_'.")
	private String property7;

	@Pattern(regexp="^$|[0-9a-zA-Z ().]*$", message="Lead unit must not include special characters.")
	private String property8;

	@Pattern(regexp="^$|[0-9a-zA-Z (),&]*$", message="Grant Call Title must not include special characters.")
	private String property9;

	@Pattern(regexp="^$|[0-9a-zA-Z -]*$", message="Sponsor proposal ID must not include special characters.")
	private String property10;

	@Pattern(regexp="^$|[0-9a-zA-Z. _-]*$", message="Keyword must not include special characters.")
	private String property11;

	@Pattern(regexp="^$|[0-9a-zA-Z ]*$", message="property12 must not include special characters.")
	private String property12;

	private Integer currentPage;

	@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="personId must not include special characters.")
	private String personId;

	@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="tabName must not include special characters except '-' and '_'.")
	private String tabName;

	private Map<@Pattern(regexp="^$|[a-zA-Z\\.]+$", message="Sort key must not include special characters.") String, @Pattern(regexp="^$|[a-zA-Z]+$", message="Sort value must not include special characters.") String> sort = new HashMap<>();

	@Pattern(regexp="^$|[\\.0-9a-zA-Z _-]*$", message="sortBy must not include special characters except '.','-' and '_'.")
	private String sortBy;

	private Boolean isDownload;

	@Pattern(regexp="^$|[AL]+$", message="advancedSearch must not include special characters.")
	private String advancedSearch = "L";

	@Pattern(regexp="^$|[a-zA-Z]*$", message="exportType must not include special characters.")
	private String exportType;

	@Pattern(regexp="^$|[0-9a-zA-Z _-]*$", message="documentHeading must not include special characters.")
	private String documentHeading;

	private Integer grantCallId;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Researcher Role must not include special characters.") String> property13;

	@Pattern(regexp="^$|[0-9-]*$", message="property14 must not include special characters, alphabets except '-'")
	private String property14;
	
	private String property15;

	private String property16;

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
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

	public List<String> getProperty3() {
		return property3;
	}

	public void setProperty3(List<String> property3) {
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

	public List<String> getProperty6() {
		return property6;
	}

	public void setProperty6(List<String> property6) {
		this.property6 = property6;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public String getProperty9() {
		return property9;
	}

	public void setProperty9(String property9) {
		this.property9 = property9;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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

	public String getProperty10() {
		return property10;
	}

	public void setProperty10(String property10) {
		this.property10 = property10;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
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

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public String getProperty11() {
		return property11;
	}

	public void setProperty11(String property11) {
		this.property11 = property11;
	}

	public String getProperty12() {
		return property12;
	}

	public void setProperty12(String property12) {
		this.property12 = property12;
	}

	public String getProperty8() {
		return property8;
	}

	public void setProperty8(String property8) {
		this.property8 = property8;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public List<String> getProperty13() {
		return property13;
	}

	public void setProperty13(List<String> property13) {
		this.property13 = property13;
	}

	public String getProperty14() {
		return property14;
	}

	public void setProperty14(String property14) {
		this.property14 = property14;
	}

	public String getProperty15() {
		return property15;
	}

	public void setProperty15(String property15) {
		this.property15 = property15;
	}

	public String getProperty16() {
		return property16;
	}

	public void setProperty16(String property16) {
		this.property16 = property16;
	}

}
