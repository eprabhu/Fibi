package com.polus.fibicomp.fastintegration.pojo;

import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({ "PROFIT_CENTER", "VALID_TO_DATE", "CONTROLLING_AREA", "VALID_FROM_DATE", "ENTERED_ON",
		"ENTERED_BY", "FIELD_OF_CO_PA", "DEPARTMENT", "PERSON_RESPONSIBLE", "PROFIT_CENTER_AREA", "COMPANY_CODE",
		"SEGMENT", "NAME", "LONG_TEXT", "SEARCH_TERM" })

public class SapProfitCenterDetails {

	@JsonProperty("PROFIT_CENTER")
	private String profitCenter;
	@JsonProperty("VALID_TO_DATE")
	private String validToDate;
	@JsonProperty("CONTROLLING_AREA")
	private String controllingArea;
	@JsonProperty("VALID_FROM_DATE")
	private String validFromDate;
	@JsonProperty("ENTERED_ON")
	private String enteredOn;
	@JsonProperty("ENTERED_BY")
	private String enteredBy;
	@JsonProperty("FIELD_OF_CO_PA")
	private String fieldOfCoPa;
	@JsonProperty("DEPARTMENT")
	private String department;
	@JsonProperty("PERSON_RESPONSIBLE")
	private String personResponsible;
	@JsonProperty("PROFIT_CENTER_AREA")
	private String profitCenterArea;
	@JsonProperty("COMPANY_CODE")
	private String companyCode;
	@JsonProperty("SEGMENT")
	private String segment;
	@JsonProperty("NAME")
	private String name;
	@JsonProperty("LONG_TEXT")
	private String longText;
	@JsonProperty("SEARCH_TERM")
	private String searchTerm;
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("PROFIT_CENTER")
	public String getProfitCenter() {
		return profitCenter;
	}

	@JsonProperty("PROFIT_CENTER")
	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	@JsonProperty("VALID_TO_DATE")
	public String getValidToDate() {
		return validToDate;
	}

	@JsonProperty("VALID_TO_DATE")
	public void setValidToDate(String validToDate) {
		this.validToDate = validToDate;
	}

	@JsonProperty("CONTROLLING_AREA")
	public String getControllingArea() {
		return controllingArea;
	}

	@JsonProperty("CONTROLLING_AREA")
	public void setControllingArea(String controllingArea) {
		this.controllingArea = controllingArea;
	}

	@JsonProperty("VALID_FROM_DATE")
	public String getValidFromDate() {
		return validFromDate;
	}

	@JsonProperty("VALID_FROM_DATE")
	public void setValidFromDate(String validFromDate) {
		this.validFromDate = validFromDate;
	}

	@JsonProperty("ENTERED_ON")
	public String getEnteredOn() {
		return enteredOn;
	}

	@JsonProperty("ENTERED_ON")
	public void setEnteredOn(String enteredOn) {
		this.enteredOn = enteredOn;
	}

	@JsonProperty("ENTERED_BY")
	public String getEnteredBy() {
		return enteredBy;
	}

	@JsonProperty("ENTERED_BY")
	public void setEnteredBy(String enteredBy) {
		this.enteredBy = enteredBy;
	}

	@JsonProperty("FIELD_OF_CO_PA")
	public String getFieldOfCoPa() {
		return fieldOfCoPa;
	}

	@JsonProperty("FIELD_OF_CO_PA")
	public void setFieldOfCoPa(String fieldOfCoPa) {
		this.fieldOfCoPa = fieldOfCoPa;
	}

	@JsonProperty("DEPARTMENT")
	public String getDepartment() {
		return department;
	}

	@JsonProperty("DEPARTMENT")
	public void setDepartment(String department) {
		this.department = department;
	}

	@JsonProperty("PERSON_RESPONSIBLE")
	public String getPersonResponsible() {
		return personResponsible;
	}

	@JsonProperty("PERSON_RESPONSIBLE")
	public void setPersonResponsible(String personResponsible) {
		this.personResponsible = personResponsible;
	}

	@JsonProperty("PROFIT_CENTER_AREA")
	public String getProfitCenterArea() {
		return profitCenterArea;
	}

	@JsonProperty("PROFIT_CENTER_AREA")
	public void setProfitCenterArea(String profitCenterArea) {
		this.profitCenterArea = profitCenterArea;
	}

	@JsonProperty("COMPANY_CODE")
	public String getCompanyCode() {
		return companyCode;
	}

	@JsonProperty("COMPANY_CODE")
	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	@JsonProperty("SEGMENT")
	public String getSegment() {
		return segment;
	}

	@JsonProperty("SEGMENT")
	public void setSegment(String segment) {
		this.segment = segment;
	}

	@JsonProperty("NAME")
	public String getName() {
		return name;
	}

	@JsonProperty("NAME")
	public void setName(String name) {
		this.name = name;
	}

	@JsonProperty("LONG_TEXT")
	public String getLongText() {
		return longText;
	}

	@JsonProperty("LONG_TEXT")
	public void setLongText(String longText) {
		this.longText = longText;
	}

	@JsonProperty("SEARCH_TERM")
	public String getSearchTerm() {
		return searchTerm;
	}

	@JsonProperty("SEARCH_TERM")
	public void setSearchTerm(String searchTerm) {
		this.searchTerm = searchTerm;
	}

	@JsonAnyGetter
	public Map<String, Object> getAdditionalProperties() {
		return this.additionalProperties;
	}

	@JsonAnySetter
	public void setAdditionalProperty(String name, Object value) {
		this.additionalProperties.put(name, value);
	}

}
