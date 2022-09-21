package com.polus.fibicomp.person.timesheetdetail.vo;

import java.util.List;
import java.util.Map;

import javax.validation.constraints.Pattern;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.person.pojo.Person;

public class TimesheetVO {

	private List<Award> awards;

	private String personId;

	private Integer totalRecords;

	private Integer currentPage;

	private Integer itemsPerPage;

	private Integer awardPersonId;

	private Integer awardId;

	private String awardKeyPersonTimesheetType;

    private Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails;

    private List<AwardKeyPersonTimesheet> awardKeyPersonTimesheet;

    private String sortType;

    @Pattern(regexp="^$|[0-9 -]+$", message="Award Number must not include special characters.")
    private String property1;
   
    @Pattern(regexp="^$|[0-9a-zA-Z -./_()]*$", message="Sponsor Award Number must not include special characters.")
    private String property2;
   
    @Pattern(regexp="^$|[\",<>'_()/\\.\\[\\]\\{\\}&#@:0-9a-zA-Z -]*$", message="Title must not include special characters.")
    private String property3;
    
    @Pattern(regexp="^$|[0-9a-zA-Z ()]+$", message="Lead Unit must not include special characters.")
    private String property4;
    
    @Pattern(regexp="^$|[0-9a-zA-Z _(),.*-]+$", message="Sponsor must not include special characters.")
    private String property5;

	private List<@Pattern(regexp="^$|[0-9]*$", message="Award Type must not include special characters.") String> property6;

	private List<AwardType> awardTypes;

	private Boolean maintainTimesheetRightExist = Boolean.FALSE;

	private Person person;

	private String reverse;

	private String sortBy;

	public List<Award> getAwards() {
		return awards;
	}

	public void setAwards(List<Award> awards) {
		this.awards = awards;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public Integer getItemsPerPage() {
		return itemsPerPage;
	}

	public void setItemsPerPage(Integer itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
	}

	public Integer getTotalRecords() {
		return totalRecords;
	}

	public void setTotalRecords(Integer totalRecords) {
		this.totalRecords = totalRecords;
	}

	public Integer getAwardPersonId() {
		return awardPersonId;
	}

	public void setAwardPersonId(Integer awardPersonId) {
		this.awardPersonId = awardPersonId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardKeyPersonTimesheetType() {
		return awardKeyPersonTimesheetType;
	}

	public void setAwardKeyPersonTimesheetType(String awardKeyPersonTimesheetType) {
		this.awardKeyPersonTimesheetType = awardKeyPersonTimesheetType;
	}

	public Map<String, List<AwardKeyPersonTimesheet>> getAwardKeyPersonTimesheetDetails() {
		return awardKeyPersonTimesheetDetails;
	}

	public void setAwardKeyPersonTimesheetDetails(
			Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails) {
		this.awardKeyPersonTimesheetDetails = awardKeyPersonTimesheetDetails;
	}

	public List<AwardKeyPersonTimesheet> getAwardKeyPersonTimesheet() {
		return awardKeyPersonTimesheet;
	}

	public void setAwardKeyPersonTimesheet(List<AwardKeyPersonTimesheet> awardKeyPersonTimesheet) {
		this.awardKeyPersonTimesheet = awardKeyPersonTimesheet;
	}

	public String getSortType() {
		return sortType;
	}

	public void setSortType(String sortType) {
		this.sortType = sortType;
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

	public List<String> getProperty6() {
		return property6;
	}

	public void setProperty6(List<String> property6) {
		this.property6 = property6;
	}

	public List<AwardType> getAwardTypes() {
		return awardTypes;
	}

	public void setAwardTypes(List<AwardType> awardTypes) {
		this.awardTypes = awardTypes;
	}

	public Boolean getMaintainTimesheetRightExist() {
		return maintainTimesheetRightExist;
	}

	public void setMaintainTimesheetRightExist(Boolean maintainTimesheetRightExist) {
		this.maintainTimesheetRightExist = maintainTimesheetRightExist;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

}
