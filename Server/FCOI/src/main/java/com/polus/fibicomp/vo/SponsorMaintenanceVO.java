package com.polus.fibicomp.vo;

import java.util.List;

import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.pojo.Unit;

import java.sql.Timestamp;

public class SponsorMaintenanceVO {
	private String sponsorCode;
	private String sponsorName;
	private String sponsorTypeCode;
	private boolean active;	
	private String acronym;
	private String unitNumber;
	private String addressLine1;
	private String addressLine2;
	private String addressLine3;
	private String sponsorLocation;
	private String emailAddress;
	private String phoneNumber;
	private List<SponsorType> sponsorTypes;
	private String acType;
	private Timestamp updateTimestamp;
	private String updateUser;
	private Integer rolodexId;
	private String rolodexName;
	private String createUser;
	private String responseMessage;
	private SponsorType sponsorType;
	private Unit unit;
	private String countryCode;
	private Country country;
	private String sponsorGroup;
	
	public String getSponsorCode() {
		return sponsorCode;
	}
	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}
	public String getSponsorName() {
		return sponsorName;
	}
	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}
	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}
	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}
	public boolean isActive() {
		return active;
	}
	public void setActive(boolean active) {
		this.active = active;
	}
	public String getAcronym() {
		return acronym;
	}
	public void setAcronym(String acronym) {
		this.acronym = acronym;
	}
	public String getUnitNumber() {
		return unitNumber;
	}
	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}
	public String getAddressLine1() {
		return addressLine1;
	}
	public void setAddressLine1(String addressLine1) {
		this.addressLine1 = addressLine1;
	}
	public String getAddressLine2() {
		return addressLine2;
	}
	public void setAddressLine2(String addressLine2) {
		this.addressLine2 = addressLine2;
	}
	public String getAddressLine3() {
		return addressLine3;
	}
	public void setAddressLine3(String addressLine3) {
		this.addressLine3 = addressLine3;
	}
	public String getSponsorLocation() {
		return sponsorLocation;
	}
	public void setSponsorLocation(String sponsorLocation) {
		this.sponsorLocation = sponsorLocation;
	}
	public String getEmailAddress() {
		return emailAddress;
	}
	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}
	public String getPhoneNumber() {
		return phoneNumber;
	}
	public void setPhoneNumber(String phoneNumber) {
		this.phoneNumber = phoneNumber;
	}
	public List<SponsorType> getSponsorTypes() {
		return sponsorTypes;
	}
	public void setSponsorTypes(List<SponsorType> sponsorTypes) {
		this.sponsorTypes = sponsorTypes;
	}
	public String getAcType() {
		return acType;
	}
	public void setAcType(String acType) {
		this.acType = acType;
	}
	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}
	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}
	public String getUpdateUser() {
		return updateUser;
	}
	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}
	public Integer getRolodexId() {
		return rolodexId;
	}
	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}
	public String getCreateUser() {
		return createUser;
	}
	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}
	public String getRolodexName() {
		return rolodexName;
	}
	public void setRolodexName(String rolodexName) {
		this.rolodexName = rolodexName;
	}
	public String getResponseMessage() {
		return responseMessage;
	}
	public void setResponseMessage(String responseMessage) {
		this.responseMessage = responseMessage;
	}
	public SponsorType getSponsorType() {
		return sponsorType;
	}
	public void setSponsorType(SponsorType sponsorType) {
		this.sponsorType = sponsorType;
	}
	public Unit getUnit() {
		return unit;
	}
	public void setUnit(Unit unit) {
		this.unit = unit;
	}
	public String getCountryCode() {
		return countryCode;
	}
	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}
	public Country getCountry() {
		return country;
	}
	public void setCountry(Country country) {
		this.country = country;
	}
	public String getSponsorGroup() {
		return sponsorGroup;
	}
	public void setSponsorGroup(String sponsorGroup) {
		this.sponsorGroup = sponsorGroup;
	}

}
