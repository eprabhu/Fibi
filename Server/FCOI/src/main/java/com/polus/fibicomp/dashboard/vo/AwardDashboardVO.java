package com.polus.fibicomp.dashboard.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

public class AwardDashboardVO {

	private Integer pageNumber;

	@Pattern(regexp="^$|[0-9a-zA-Z._-]*$", message="Account Number must not include special characters.")
	private String property1;

	@Pattern(regexp="^$|[0-9a-zA-Z ()]+$", message="Lead Unit must not include special characters.")
	private String property2;

	@Pattern(regexp="^$|[0-9a-zA-Z _(),&.*-]+$", message="Sponsor must not include special characters.")
	private String property3;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Researcher Name must not include special characters.")
	private String property4;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Award ID must not include special characters.")
	private String property5;

	@Pattern(regexp="^$|[0-9 -]+$", message="Award Number must not include special characters.")
	private String property6;

	@Pattern(regexp="^$|[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Title must not include special characters.")
	private String property7;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Award Status must not include special characters.") String> property8;

	@Pattern(regexp="^$|[0-9a-zA-Z. _()-/]+$", message="Keyword must not include special characters.")
	private String property9;

	@Pattern(regexp="^$|[0-9a-zA-Z _-]+$", message="Grant call Title must not include special characters.")
	private String property10;

	private Integer currentPage;

	private String personId;

	@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="tabName must not include special characters except '-' and '_'.")
	private String tabName;

	private Map<@Pattern(regexp="^$|[a-zA-Z\\.]+$", message="Sort key must not include special characters.") String, @Pattern(regexp="^$|[a-zA-Z]+$", message="Sort value must not include special characters.") String> sort = new HashMap<>();

	@Pattern(regexp="^$|[\\.a-zA-Z _-]*$", message="sortBy must not include special characters except '.','-' and '_'.")
	private String sortBy;

	private Boolean isDownload;

	@Pattern(regexp="^$|[AL]+$", message="advancedSearch must not include special characters.")
	private String advancedSearch = "L";

	@Pattern(regexp="^$|[a-zA-Z]*$", message="exportType must not include special characters.")
	private String exportType;

	@Pattern(regexp="^$|[0-9a-zA-Z _-]*$", message="documentHeading must not include special characters.")
	private String documentHeading;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Researcher Role must not include special characters.") String> property11;

	@Pattern(regexp="^$|[0-9a-zA-Z -./_()]*$", message="Sponsor Award Number must not include special characters.")
	private String property12;
	
	private List<@Pattern(regexp="^$|[0-9]*$", message="Version Type must not include special characters.") String> property13;
	
	private List<@Pattern(regexp="^$|[0-9]*$", message="Account Type must not include special characters.") String> property14;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Grant Type must not include special characters.") String> property15;

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

	public String getProperty3() {
		return property3;
	}

	public void setProperty3(String property3) {
		this.property3 = property3;
	}

	public String getProperty4() {
		return property4;
	}

	public void setProperty4(String property4) {
		this.property4 = property4;
	}

	public String getProperty5() {
		return property5;
	}

	public void setProperty5(String property5) {
		this.property5 = property5;
	}

	public String getProperty6() {
		return property6;
	}

	public void setProperty6(String property6) {
		this.property6 = property6;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public List<String> getProperty8() {
		return property8;
	}

	public void setProperty8(List<String> property8) {
		this.property8 = property8;
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

	public List<String> getProperty11() {
		return property11;
	}

	public void setProperty11(List<String> property11) {
		this.property11 = property11;
	}

	public String getProperty12() {
		return property12;
	}

	public void setProperty12(String property12) {
		this.property12 = property12;
	}
	
	public List<String> getProperty13() {
		return property13;
	}

	public void setProperty13(List<String> property13) {
		this.property13 = property13;
	}

	public List<String> getProperty14() {
		return property14;
	}

	public void setProperty14(List<String> property14) {
		this.property14 = property14;
	}

	public List<String> getProperty15() {
		return property15;
	}

	public void setProperty15(List<String> property15) {
		this.property15 = property15;
	}


}
