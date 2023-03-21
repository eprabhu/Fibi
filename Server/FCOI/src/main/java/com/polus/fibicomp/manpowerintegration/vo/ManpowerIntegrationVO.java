package com.polus.fibicomp.manpowerintegration.vo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.manpower.pojo.ManpowerUserAction;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto;
import com.polus.fibicomp.manpowerintegration.pojo.ManpowerLog;

public class ManpowerIntegrationVO {

	private Integer pageCount;

	private Integer currentPage;

	private Integer itemsPerPage;

	private Boolean isDownload;

	private String startDate;

	private String endDate;

	private Timestamp interfaceStartDate;

	private Timestamp interfaceEndDate;

	private String awardNumber;

	private String positionId;

	private String resourceUniqueId;

	private Integer workdayManpowerInterfaceId;

	private Integer manpowerLogId;

	private String comments;

	private String manpowerUserActionCode;

	private String personId;

	private String resourceName;

	private String budgetReferenceNumber;

	private String advancedSearch = "L";

	private String tabName;

	private Integer pageNumber;

	private String parentInterfaceStatus;

	private Map<String, String> sort = new HashMap<>();

	private List<ManpowerLog> manpowerLogs;

	private List<WorkdayManpowerInterface> workdayManpowerInterfaces;

	private List<ManpowerUserAction> manpowerUserActions;

	private List<WorkdayInterfaceLogDto> workdayInterfaceLogDtos;
	
	private String reverse ;

	private String sortBy;

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

	public ManpowerIntegrationVO() {
		manpowerLogs = new ArrayList<>();
		workdayManpowerInterfaces = new ArrayList<>();
		workdayInterfaceLogDtos = new ArrayList<>();
	}

	public Integer getPageCount() {
		return pageCount;
	}

	public void setPageCount(Integer pageCount) {
		this.pageCount = pageCount;
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

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public String getStartDate() {
		return startDate;
	}

	public void setStartDate(String startDate) {
		this.startDate = startDate;
	}

	public String getEndDate() {
		return endDate;
	}

	public void setEndDate(String endDate) {
		this.endDate = endDate;
	}

	public Timestamp getInterfaceStartDate() {
		return interfaceStartDate;
	}

	public void setInterfaceStartDate(Timestamp interfaceStartDate) {
		this.interfaceStartDate = interfaceStartDate;
	}

	public Timestamp getInterfaceEndDate() {
		return interfaceEndDate;
	}

	public void setInterfaceEndDate(Timestamp interfaceEndDate) {
		this.interfaceEndDate = interfaceEndDate;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public String getResourceUniqueId() {
		return resourceUniqueId;
	}

	public void setResourceUniqueId(String resourceUniqueId) {
		this.resourceUniqueId = resourceUniqueId;
	}

	public Integer getWorkdayManpowerInterfaceId() {
		return workdayManpowerInterfaceId;
	}

	public void setWorkdayManpowerInterfaceId(Integer workdayManpowerInterfaceId) {
		this.workdayManpowerInterfaceId = workdayManpowerInterfaceId;
	}

	public Integer getManpowerLogId() {
		return manpowerLogId;
	}

	public void setManpowerLogId(Integer manpowerLogId) {
		this.manpowerLogId = manpowerLogId;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getManpowerUserActionCode() {
		return manpowerUserActionCode;
	}

	public void setManpowerUserActionCode(String manpowerUserActionCode) {
		this.manpowerUserActionCode = manpowerUserActionCode;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getResourceName() {
		return resourceName;
	}

	public void setResourceName(String resourceName) {
		this.resourceName = resourceName;
	}

	public String getBudgetReferenceNumber() {
		return budgetReferenceNumber;
	}

	public void setBudgetReferenceNumber(String budgetReferenceNumber) {
		this.budgetReferenceNumber = budgetReferenceNumber;
	}

	public String getAdvancedSearch() {
		return advancedSearch;
	}

	public void setAdvancedSearch(String advancedSearch) {
		this.advancedSearch = advancedSearch;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public String getParentInterfaceStatus() {
		return parentInterfaceStatus;
	}

	public void setParentInterfaceStatus(String parentInterfaceStatus) {
		this.parentInterfaceStatus = parentInterfaceStatus;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public List<ManpowerLog> getManpowerLogs() {
		return manpowerLogs;
	}

	public void setManpowerLogs(List<ManpowerLog> manpowerLogs) {
		this.manpowerLogs = manpowerLogs;
	}

	public List<WorkdayManpowerInterface> getWorkdayManpowerInterfaces() {
		return workdayManpowerInterfaces;
	}

	public void setWorkdayManpowerInterfaces(List<WorkdayManpowerInterface> workdayManpowerInterfaces) {
		this.workdayManpowerInterfaces = workdayManpowerInterfaces;
	}

	public List<ManpowerUserAction> getManpowerUserActions() {
		return manpowerUserActions;
	}

	public void setManpowerUserActions(List<ManpowerUserAction> manpowerUserActions) {
		this.manpowerUserActions = manpowerUserActions;
	}

	public List<WorkdayInterfaceLogDto> getWorkdayInterfaceLogDtos() {
		return workdayInterfaceLogDtos;
	}

	public void setWorkdayInterfaceLogDtos(List<WorkdayInterfaceLogDto> workdayInterfaceLogDtos) {
		this.workdayInterfaceLogDtos = workdayInterfaceLogDtos;
	}

}
