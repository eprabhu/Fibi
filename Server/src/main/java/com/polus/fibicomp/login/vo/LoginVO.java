package com.polus.fibicomp.login.vo;

import java.sql.Timestamp;
import java.util.List;
import com.polus.fibicomp.login.pojo.PersonLoginDetail;
import com.polus.fibicomp.login.pojo.SystemNotification;

public class LoginVO {

	private Timestamp startDate;

	private Timestamp endDate;

	private String loginStatus;

	private String personId;

	private String userName;

	private String sortBy;

	private Integer currentPage;

	private String reverse;

	private Boolean isDownload;

	private Integer itemsPerPage;

	private String documentHeading;

	private Integer userActivityCount;
	
	private String unitNumber;
	
	private boolean isUnitAdmin = false;

	private boolean isSuperUser = false;

	private List<PersonLoginDetail> personLoginDetails;

	private String userFullName;
	
	private String parentUnitNumber;
	
	private List<String> unitList;

	private List<SystemNotification> systemNotifications;

	private Integer systemNotificationId;

	private Boolean systemNotificationRead = Boolean.FALSE;

	public String getParentUnitNumber() {
		return parentUnitNumber;
	}

	public void setParentUnitNumber(String parentUnitNumber) {
		this.parentUnitNumber = parentUnitNumber;
	}

	public boolean isUnitAdmin() {
		return isUnitAdmin;
	}

	public void setUnitAdmin(boolean isUnitAdmin) {
		this.isUnitAdmin = isUnitAdmin;
	}

	public boolean isSuperUser() {
		return isSuperUser;
	}

	public void setSuperUser(boolean isSuperUser) {
		this.isSuperUser = isSuperUser;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public String getLoginStatus() {
		return loginStatus;
	}

	public void setLoginStatus(String loginStatus) {
		this.loginStatus = loginStatus;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<PersonLoginDetail> getPersonLoginDetails() {
		return personLoginDetails;
	}

	public void setPersonLoginDetails(List<PersonLoginDetail> personLoginDetails) {
		this.personLoginDetails = personLoginDetails;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public Integer getItemsPerPage() {
		return itemsPerPage;
	}

	public void setItemsPerPage(Integer itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public Integer getUserActivityCount() {
		return userActivityCount;
	}

	public void setUserActivityCount(Integer userActivityCount) {
		this.userActivityCount = userActivityCount;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}
	
	public List<String> getUnitList() {
		return unitList;
	}

	public void setUnitList(List<String> unitList) {
		this.unitList = unitList;
	}

	public List<SystemNotification> getSystemNotifications() {
		return systemNotifications;
	}

	public void setSystemNotifications(List<SystemNotification> systemNotifications) {
		this.systemNotifications = systemNotifications;
	}

	public Integer getSystemNotificationId() {
		return systemNotificationId;
	}

	public void setSystemNotificationId(Integer systemNotificationId) {
		this.systemNotificationId = systemNotificationId;
	}

	public Boolean getSystemNotificationRead() {
		return systemNotificationRead;
	}

	public void setSystemNotificationRead(Boolean systemNotificationRead) {
		this.systemNotificationRead = systemNotificationRead;
	}

}
