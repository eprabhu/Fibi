package com.polus.fibicomp.vo;

import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.SponsorType;

public class SponsorSearchResult {

	private String sponsorCode;
	private String sponsorName;
	private Country country;
	private String acronym;
	private String sponsorTypeCode;
	private SponsorType sponsorType;
	private String addressLine1;
	private String addressLine2;
	private String addressLine3;
	private String state;
	private String postalCode;
	private String sponsorLocation;

	public SponsorSearchResult() {
	}

	public SponsorSearchResult(String sponsorCode, String sponsorName) {
		this.sponsorCode = sponsorCode;
		this.sponsorName = sponsorName;
	}

	public SponsorSearchResult(String sponsorCode, String sponsorName, String countryCode, String countryName, String acronym, String sponsorTypeCode, String sponsorType, String addressLine1, String addressLine2, String addressLine3, String state, String postalCode, String sponsorLocation) {
		this.sponsorCode = sponsorCode;
		this.sponsorName = sponsorName;
		if (countryCode != null && countryName != null) {
			this.country = new Country(countryCode, countryName);
		}
		this.acronym = acronym;
		this.sponsorTypeCode = sponsorTypeCode;
		if (sponsorTypeCode != null && sponsorType != null) {
			this.sponsorType = new SponsorType(sponsorTypeCode, sponsorType);
		}
		this.addressLine1 = addressLine1;
		this.addressLine2 = addressLine2;
		this.addressLine3 = addressLine3;
		this.state = state;
		this.postalCode = postalCode;
		this.sponsorLocation = sponsorLocation;
	}

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

	public Country getCountry() {
		return country;
	}

	public void setCountry(Country country) {
		this.country = country;
	}

	public String getAcronym() {
		return acronym;
	}

	public void setAcronym(String acronym) {
		this.acronym = acronym;
	}

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public SponsorType getSponsorType() {
		return sponsorType;
	}

	public void setSponsorType(SponsorType sponsorType) {
		this.sponsorType = sponsorType;
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

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getSponsorLocation() {
		return sponsorLocation;
	}

	public void setSponsorLocation(String sponsorLocation) {
		this.sponsorLocation = sponsorLocation;
	}

}
