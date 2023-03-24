package com.polus.fibicomp.dashboard.vo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementPeopleType;

public class AgreementDashboardVO {

	private Integer pageNumber;

	private Integer itemsPerPage;

	@Pattern(regexp="^$|[\\.a-zA-Z _-]*$", message="sortBy must not include special characters except '.','-' and '_'.")
	private String sortBy;

	private String reverse;

	private String personId;

	@Pattern(regexp="^$|[0-9]*$", message="Agreement ID must not include special characters.")
	private String property1;

	@Pattern(regexp="^$|[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Title must not include special characters.")
	private String property2;

	private List<String> property3;

	private  List<@Pattern(regexp="^$|[0-9]*$", message="Agreement Type must not include special characters.") String> property4;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Requestor Name must not include special characters.")
	private String property5;

	@Pattern(regexp="^$|[0-9a-zA-Z ]+$", message="Lead Unit must not include special characters.")
	private String property7;

	@Pattern(regexp="^$|[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Organization must not include special characters.")
	private String property8;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Agreement Status must not include special characters.") String> property6;
	
	private Boolean isDownload = false;
	
	private String tabName;

	@Pattern(regexp="^$|[AL]+$", message="advancedSearch must not include special characters.")
	private String advancedSearch = "L";

	private Map<@Pattern(regexp="^$|[a-zA-Z\\.]+$", message="Sort key must not include special characters.") String, @Pattern(regexp="^$|[a-zA-Z]+$", message="Sort value must not include special characters.") String> sort = new HashMap<String, String>();

	private Integer currentPage;

	@Pattern(regexp="^$|[0-9a-zA-Z _-]*$", message="documentHeading must not include special characters.")
	private  String documentHeading;

	@Pattern(regexp="^$|[a-zA-Z]*$", message="exportType must not include special characters.")
	private  String exportType;

	private String categoryCode;

	private String statusCode;

	private Boolean isUnitAdmin = false;

	private List<AgreementHeader> agreementHeaderList;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Negotiator must not include special characters.")
	private String property9;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Principal Investigator must not include special characters.")
	private String property10;

	private String property11;

	private String property12;

	private String property13;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Review Status must not include special characters.") String> property14;

	private List<@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Organization Type must not include special characters.") String> property15;

	@Pattern(regexp="^$|[0-9a-zA-Z _]+$", message="Administrator must not include special characters.")
	private String property16;

	@Pattern(regexp="^$|[0-9 -]*$", message="Submission Start Date must not include special characters.")
	private String property17;

	@Pattern(regexp="^$|[0-9 -]*$", message="Submission End Date must not include special characters.")
	private String property18;

	private List<AgreementPeopleType> agreementPeopleTypes;

	@Pattern(regexp="^$|[0-9 -]*$", message="Project ID must not include special characters.")
	private String property19;
	
	private List<@Pattern(regexp="^$|[0-9]*$", message="Admin Group must not include special characters.") String> property20;

	private String exportHeading;

	public AgreementDashboardVO() {
		property14 = new ArrayList<>();
	}

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

	public List<String> getProperty3() {
		return property3;
	}

	public void setProperty3(List<String> property3) {
		this.property3 = property3;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public String getProperty5() {
		return property5;
	}

	public void setProperty5(String property5) {
		this.property5 = property5;
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

	public void setProperty2(String property2) {
		this.property2 = property2;
	}

	public void setProperty4(List<String> property4) {
		this.property4 = property4;
	}

	public String getProperty2() {
		return property2;
	}

	public List<String> getProperty4() {
		return property4;
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

	public String getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(String categoryCode) {
		this.categoryCode = categoryCode;
	}

	public String getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(String statusCode) {
		this.statusCode = statusCode;
	}

	public Boolean getIsUnitAdmin() {
		return isUnitAdmin;
	}

	public void setIsUnitAdmin(Boolean isUnitAdmin) {
		this.isUnitAdmin = isUnitAdmin;
	}

	public List<AgreementHeader> getAgreementHeaderList() {
		return agreementHeaderList;
	}

	public void setAgreementHeaderList(List<AgreementHeader> agreementHeaderList) {
		this.agreementHeaderList = agreementHeaderList;
	}

	public Integer getItemsPerPage() {
		return itemsPerPage;
	}

	public void setItemsPerPage(Integer itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
	}

	public String getProperty8() {
		return property8;
	}

	public void setProperty8(String property8) {
		this.property8 = property8;
	}

	public String getProperty9() {
		return property9;
	}

	public void setProperty9(String property9) {
		this.property9 = property9;
	}

	public String getProperty10() {
		return property10;
	}

	public void setProperty10(String property10) {
		this.property10 = property10;
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

	public String getProperty13() {
		return property13;
	}

	public void setProperty13(String property13) {
		this.property13 = property13;
	}

	public List<String> getProperty14() {
		return property14;
	}

	public void setProperty14(List<String> property14) {
		this.property14 = property14;
	}

	public List<AgreementPeopleType> getAgreementPeopleTypes() {
		return agreementPeopleTypes;
	}

	public void setAgreementPeopleTypes(List<AgreementPeopleType> agreementPeopleTypes) {
		this.agreementPeopleTypes = agreementPeopleTypes;
	}

	public String getProperty16() {
		return property16;
	}

	public void setProperty16(String property16) {
		this.property16 = property16;
	}

	public String getProperty17() {
		return property17;
	}

	public void setProperty17(String property17) {
		this.property17 = property17;
	}

	public String getProperty18() {
		return property18;
	}

	public void setProperty18(String property18) {
		this.property18 = property18;
	}

	public List<String> getProperty15() {
		return property15;
	}

	public void setProperty15(List<String> property15) {
		this.property15 = property15;
	}

	public String getProperty19() {
		return property19;
	}

	public void setProperty19(String property19) {
		this.property19 = property19;
	}

	public List<String> getProperty20() {
		return property20;
	}

	public void setProperty20(List<String> property20) {
		this.property20 = property20;
	}

	public String getExportHeading() {
		return exportHeading;
	}

	public void setExportHeading(String exportHeading) {
		this.exportHeading = exportHeading;
	}

}
