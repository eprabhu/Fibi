package com.polus.fibicomp.dashboard.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

public class CoiDashboardVO {

	private Integer id;

	private Integer pageNumber;

	private Integer currentPage;

	@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="personId must not include special characters.")
	private String personId;

	@Pattern(regexp="^$|[0-9a-zA-Z _]*$", message="tab name must not include special characters except '-' and '_'.")
	private String tabName;

	private Map<@Pattern(regexp="^$|[a-zA-Z\\.]+$", message="Sort key must not include special characters.") String, @Pattern(regexp="^$|[a-zA-Z]+$", message="Sort value must not include special characters.") String> sort = new HashMap<>();

	private Boolean isDownload;

	@Pattern(regexp="^$|[AL]+$", message="advancedSearch must not include special characters.")
	private String advancedSearch = "L";

	private String exportType;

	private String documentHeading;

	@Pattern(regexp="^$|[0-9 -]*$", message="Disclosure Number must not include special characters.")
	private String property1;

	private String property2;

	@Pattern(regexp="^$|[0-9a-zA-Z ]+$", message="home Unit must not include special characters.")
	private String property3;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Disclosure Status must not include special characters.") String> property4;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Disclosure Category Type must not include special characters.") String> property5;

	private String property6;

	private String property7;

	private String property8;

	private String property9;

	@Pattern(regexp="^$|[0-9]*$", message="Proposal ID must not include special characters.")
	private String property10;

	private String property11;

	@Pattern(regexp="^$|[0-9]*$", message="Award ID must not include special characters.")
	private String property12;

	private String property13;

	private String property14;

	private Boolean property15;

	private String property16;

	private Boolean property17;

	private Boolean property18;

	private Boolean property19;
	
	private String filterType;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Disposition Status must not include special characters.") String> property20;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Review Status must not include special characters.") String> property21;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Conflict Status must not include special characters.") String> property22;

	private String property23;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
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

	public List<String> getProperty4() {
		return property4;
	}

	public void setProperty4(List<String> property4) {
		this.property4 = property4;
	}

	public List<String> getProperty5() {
		return property5;
	}

	public void setProperty5(List<String> property5) {
		this.property5 = property5;
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

	public String getProperty14() {
		return property14;
	}

	public void setProperty14(String property14) {
		this.property14 = property14;
	}

	public Boolean getProperty15() {
		return property15;
	}

	public void setProperty15(Boolean property15) {
		this.property15 = property15;
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

	public String getProperty16() {
		return property16;
	}

	public void setProperty16(String property16) {
		this.property16 = property16;
	}

	public Boolean getProperty17() {
		return property17;
	}

	public void setProperty17(Boolean property17) {
		this.property17 = property17;
	}

	public Boolean getProperty18() {
		return property18;
	}

	public void setProperty18(Boolean property18) {
		this.property18 = property18;
	}

	public Boolean getProperty19() {
		return property19;
	}

	public void setProperty19(Boolean property19) {
		this.property19 = property19;
	}

	public List<String> getProperty20() {
		return property20;
	}

	public void setProperty20(List<String> property20) {
		this.property20 = property20;
	}

	public List<String> getProperty21() {
		return property21;
	}

	public void setProperty21(List<String> property21) {
		this.property21 = property21;
	}

	public List<String> getProperty22() {
		return property22;
	}

	public void setProperty22(List<String> property22) {
		this.property22 = property22;
	}
	
	public String getFilterType() {
		return filterType;
	}

	public void setFilterType(String filterType) {
		this.filterType = filterType;
	}

	public String getProperty23() {
		return property23;
	}

	public void setProperty23(String property23) {
		this.property23 = property23;
	}
}
