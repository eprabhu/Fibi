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
@JsonPropertyOrder({ "CONTROLLING_AREA", "COST_CENTER", "VALID_TO_DATE", "VALID_FROM_DATE", "PERSON_IN_CHARGE",
		"DEPARTMENT", "NAME", "DESCRIPTION", "FUNCTIONAL_AREA", "COSTCENTER_TYPE", "COSTCTR_HIER_GRP", "COMPANY_CODE",
		"BUSINESS_AREA", "CURRENCY", "PROFIT_CENTER", "RECORD_QUANTITY_IND", "LOCK_IND_ACTUAL_PRIMARY_COSTS",
		"LOCK_IND_PLAN_PRIMARY_COSTS", "LOCK_IND_ACT_SECONDARY_COSTS", "LOCK_IND_PLAN_SECONDARY_COSTS",
		"LOCK_IND_ACTUAL_REVENUES", "LOCK_IND_PLAN_REVENUES", "LOCK_IND_COMMITMENT_UPDATE", "CONDITION_TABLE_USAGE",
		"APPLICATION", "COSTING_SHEET", "ACTY_INDEP_TEMPLATE", "ACTY_DEP_TEMPLATE", "ADDR_TITLE", "ADDR_NAME1",
		"ADDR_NAME2", "ADDR_NAME3", "ADDR_NAME4", "ADDR_STREET", "ADDR_CITY", "ADDR_DISTRICT", "ADDR_COUNTRY",
		"ADDR_TAXJURCODE", "ADDR_PO_BOX", "ADDR_POSTL_CODE", "ADDR_POBX_PCD", "ADDR_REGION", "TELCO_LANGU",
		"TELCO_TELEPHONE", "TELCO_TELEPHONE2", "TELCO_TELEBOX", "TELCO_TELEX", "TELCO_FAX_NUMBER", "TELCO_TELETEX",
		"TELCO_PRINTER", "TELCO_DATALINE", "JV_VENTURE", "JV_REC_IND", "JV_EQUITY_TYP", "FUND", "GRANT_ID" })
public class CostCenterDetails {

	@JsonProperty("CONTROLLING_AREA")
	private String controllingArea;
	
	@JsonProperty("COST_CENTER")
	private String costCenter;
	
	@JsonProperty("VALID_TO_DATE")
	private String validToDate;
	
	@JsonProperty("VALID_FROM_DATE")
	private String validFromDate;
	
	@JsonProperty("PERSON_IN_CHARGE")
	private String personInCharge;
	
	@JsonProperty("DEPARTMENT")
	private String department;
	
	@JsonProperty("NAME")
	private String name;
	
	@JsonProperty("DESCRIPTION")
	private String description;
	
	@JsonProperty("FUNCTIONAL_AREA")
	private String functionalArea;
	
	@JsonProperty("COSTCENTER_TYPE")
	private String costcenterType;
	
	@JsonProperty("COSTCTR_HIER_GRP")
	private String costctrHierGrp;
	
	@JsonProperty("COMPANY_CODE")
	private String companyCode;
	
	@JsonProperty("BUSINESS_AREA")
	private String businessArea;
	
	@JsonProperty("CURRENCY")
	private String currency;
	
	@JsonProperty("PROFIT_CENTER")
	private String profitCenter;
	
	@JsonProperty("RECORD_QUANTITY_IND")
	private String recordQuantityInd;
	
	@JsonProperty("LOCK_IND_ACTUAL_PRIMARY_COSTS")
	private String lockIndActualPrimaryCosts;
	
	@JsonProperty("LOCK_IND_PLAN_PRIMARY_COSTS")
	private String lockIndPlanPrimaryCosts;
	
	@JsonProperty("LOCK_IND_ACT_SECONDARY_COSTS")
	private String lockIndActSecondaryCosts;
	
	@JsonProperty("LOCK_IND_PLAN_SECONDARY_COSTS")
	private String lockIndPlanSecondaryCosts;
	
	@JsonProperty("LOCK_IND_ACTUAL_REVENUES")
	private String lockIndActualRevenues;
	
	@JsonProperty("LOCK_IND_PLAN_REVENUES")
	private String lockIndPlanRevenues;
	
	@JsonProperty("LOCK_IND_COMMITMENT_UPDATE")
	private String lockIndCommitmentUpdate;
	
	@JsonProperty("CONDITION_TABLE_USAGE")
	private String conditionTableUsage;
	
	@JsonProperty("APPLICATION")
	private String application;
	
	@JsonProperty("COSTING_SHEET")
	private String costingSheet;
	
	@JsonProperty("ACTY_INDEP_TEMPLATE")
	private String actyIndepTemplate;
	
	@JsonProperty("ACTY_DEP_TEMPLATE")
	private String actyDepTemplate;
	
	@JsonProperty("ADDR_TITLE")
	private String addrTitle;
	
	@JsonProperty("ADDR_NAME1")
	private String addrName1;
	
	@JsonProperty("ADDR_NAME2")
	private String addrName2;
	
	@JsonProperty("ADDR_NAME3")
	private String addrName3;
	
	@JsonProperty("ADDR_NAME4")
	private String addrName4;
	
	@JsonProperty("ADDR_STREET")
	private String addrStreet;
	
	@JsonProperty("ADDR_CITY")
	private String addrCity;
	
	@JsonProperty("ADDR_DISTRICT")
	private String addrDistrict;
	
	@JsonProperty("ADDR_COUNTRY")
	private String addrCountry;
	
	@JsonProperty("ADDR_TAXJURCODE")
	private String addrTaxjurcode;
	
	@JsonProperty("ADDR_PO_BOX")
	private String addrPoBox;
	
	@JsonProperty("ADDR_POSTL_CODE")
	private String addrPostlCode;
	
	@JsonProperty("ADDR_POBX_PCD")
	private String addrPobxPcd;
	
	@JsonProperty("ADDR_REGION")
	private String addrRegion;
	
	@JsonProperty("TELCO_LANGU")
	private String telcoLangu;
	
	@JsonProperty("TELCO_TELEPHONE")
	private String telcoTelephone;
	
	@JsonProperty("TELCO_TELEPHONE2")
	private String telcoTelephone2;
	
	@JsonProperty("TELCO_TELEBOX")
	private String telcoTelebox;
	
	@JsonProperty("TELCO_TELEX")
	private String telcoTelex;
	
	@JsonProperty("TELCO_FAX_NUMBER")
	private String telcoFaxNumber;
	
	@JsonProperty("TELCO_TELETEX")
	private String telcoTeletex;
	
	@JsonProperty("TELCO_PRINTER")
	private String telcoPrinter;
	
	@JsonProperty("TELCO_DATALINE")
	private String telcoDataline;
	
	@JsonProperty("JV_VENTURE")
	private String jvVenture;
	
	@JsonProperty("JV_REC_IND")
	private String jvRecInd;
	
	@JsonProperty("JV_EQUITY_TYP")
	private String jvEquityTyp;
	
	@JsonProperty("FUND")
	private String fund;
	
	@JsonProperty("GRANT_ID")
	private String grantId;
	
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("CONTROLLING_AREA")
	public String getControllingArea() {
		return controllingArea;
	}

	@JsonProperty("CONTROLLING_AREA")
	public void setControllingArea(String controllingArea) {
		this.controllingArea = controllingArea;
	}

	@JsonProperty("COST_CENTER")
	public String getCostCenter() {
		return costCenter;
	}

	@JsonProperty("COST_CENTER")
	public void setCostCenter(String costCenter) {
		this.costCenter = costCenter;
	}

	@JsonProperty("VALID_TO_DATE")
	public String getValidToDate() {
		return validToDate;
	}

	@JsonProperty("VALID_TO_DATE")
	public void setValidToDate(String validToDate) {
		this.validToDate = validToDate;
	}

	@JsonProperty("VALID_FROM_DATE")
	public String getValidFromDate() {
		return validFromDate;
	}

	@JsonProperty("VALID_FROM_DATE")
	public void setValidFromDate(String validFromDate) {
		this.validFromDate = validFromDate;
	}

	@JsonProperty("PERSON_IN_CHARGE")
	public String getPersonInCharge() {
		return personInCharge;
	}

	@JsonProperty("PERSON_IN_CHARGE")
	public void setPersonInCharge(String personInCharge) {
		this.personInCharge = personInCharge;
	}

	@JsonProperty("DEPARTMENT")
	public String getDepartment() {
		return department;
	}

	@JsonProperty("DEPARTMENT")
	public void setDepartment(String department) {
		this.department = department;
	}

	@JsonProperty("NAME")
	public String getName() {
		return name;
	}

	@JsonProperty("NAME")
	public void setName(String name) {
		this.name = name;
	}

	@JsonProperty("DESCRIPTION")
	public String getDescription() {
		return description;
	}

	@JsonProperty("DESCRIPTION")
	public void setDescription(String description) {
		this.description = description;
	}

	@JsonProperty("FUNCTIONAL_AREA")
	public String getFunctionalArea() {
		return functionalArea;
	}

	@JsonProperty("FUNCTIONAL_AREA")
	public void setFunctionalArea(String functionalArea) {
		this.functionalArea = functionalArea;
	}

	@JsonProperty("COSTCENTER_TYPE")
	public String getCostcenterType() {
		return costcenterType;
	}

	@JsonProperty("COSTCENTER_TYPE")
	public void setCostcenterType(String costcenterType) {
		this.costcenterType = costcenterType;
	}

	@JsonProperty("COSTCTR_HIER_GRP")
	public String getCostctrHierGrp() {
		return costctrHierGrp;
	}

	@JsonProperty("COSTCTR_HIER_GRP")
	public void setCostctrHierGrp(String costctrHierGrp) {
		this.costctrHierGrp = costctrHierGrp;
	}

	@JsonProperty("COMPANY_CODE")
	public String getCompanyCode() {
		return companyCode;
	}

	@JsonProperty("COMPANY_CODE")
	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	@JsonProperty("BUSINESS_AREA")
	public String getBusinessArea() {
		return businessArea;
	}

	@JsonProperty("BUSINESS_AREA")
	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

	@JsonProperty("CURRENCY")
	public String getCurrency() {
		return currency;
	}

	@JsonProperty("CURRENCY")
	public void setCurrency(String currency) {
		this.currency = currency;
	}

	@JsonProperty("PROFIT_CENTER")
	public String getProfitCenter() {
		return profitCenter;
	}

	@JsonProperty("PROFIT_CENTER")
	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	@JsonProperty("RECORD_QUANTITY_IND")
	public String getRecordQuantityInd() {
		return recordQuantityInd;
	}

	@JsonProperty("RECORD_QUANTITY_IND")
	public void setRecordQuantityInd(String recordQuantityInd) {
		this.recordQuantityInd = recordQuantityInd;
	}

	@JsonProperty("LOCK_IND_ACTUAL_PRIMARY_COSTS")
	public String getLockIndActualPrimaryCosts() {
		return lockIndActualPrimaryCosts;
	}

	@JsonProperty("LOCK_IND_ACTUAL_PRIMARY_COSTS")
	public void setLockIndActualPrimaryCosts(String lockIndActualPrimaryCosts) {
		this.lockIndActualPrimaryCosts = lockIndActualPrimaryCosts;
	}

	@JsonProperty("LOCK_IND_PLAN_PRIMARY_COSTS")
	public String getLockIndPlanPrimaryCosts() {
		return lockIndPlanPrimaryCosts;
	}

	@JsonProperty("LOCK_IND_PLAN_PRIMARY_COSTS")
	public void setLockIndPlanPrimaryCosts(String lockIndPlanPrimaryCosts) {
		this.lockIndPlanPrimaryCosts = lockIndPlanPrimaryCosts;
	}

	@JsonProperty("LOCK_IND_ACT_SECONDARY_COSTS")
	public String getLockIndActSecondaryCosts() {
		return lockIndActSecondaryCosts;
	}

	@JsonProperty("LOCK_IND_ACT_SECONDARY_COSTS")
	public void setLockIndActSecondaryCosts(String lockIndActSecondaryCosts) {
		this.lockIndActSecondaryCosts = lockIndActSecondaryCosts;
	}

	@JsonProperty("LOCK_IND_PLAN_SECONDARY_COSTS")
	public String getLockIndPlanSecondaryCosts() {
		return lockIndPlanSecondaryCosts;
	}

	@JsonProperty("LOCK_IND_PLAN_SECONDARY_COSTS")
	public void setLockIndPlanSecondaryCosts(String lockIndPlanSecondaryCosts) {
		this.lockIndPlanSecondaryCosts = lockIndPlanSecondaryCosts;
	}

	@JsonProperty("LOCK_IND_ACTUAL_REVENUES")
	public String getLockIndActualRevenues() {
		return lockIndActualRevenues;
	}

	@JsonProperty("LOCK_IND_ACTUAL_REVENUES")
	public void setLockIndActualRevenues(String lockIndActualRevenues) {
		this.lockIndActualRevenues = lockIndActualRevenues;
	}

	@JsonProperty("LOCK_IND_PLAN_REVENUES")
	public String getLockIndPlanRevenues() {
		return lockIndPlanRevenues;
	}

	@JsonProperty("LOCK_IND_PLAN_REVENUES")
	public void setLockIndPlanRevenues(String lockIndPlanRevenues) {
		this.lockIndPlanRevenues = lockIndPlanRevenues;
	}

	@JsonProperty("LOCK_IND_COMMITMENT_UPDATE")
	public String getLockIndCommitmentUpdate() {
		return lockIndCommitmentUpdate;
	}

	@JsonProperty("LOCK_IND_COMMITMENT_UPDATE")
	public void setLockIndCommitmentUpdate(String lockIndCommitmentUpdate) {
		this.lockIndCommitmentUpdate = lockIndCommitmentUpdate;
	}

	@JsonProperty("CONDITION_TABLE_USAGE")
	public String getConditionTableUsage() {
		return conditionTableUsage;
	}

	@JsonProperty("CONDITION_TABLE_USAGE")
	public void setConditionTableUsage(String conditionTableUsage) {
		this.conditionTableUsage = conditionTableUsage;
	}

	@JsonProperty("APPLICATION")
	public String getApplication() {
		return application;
	}

	@JsonProperty("APPLICATION")
	public void setApplication(String application) {
		this.application = application;
	}

	@JsonProperty("COSTING_SHEET")
	public String getCostingSheet() {
		return costingSheet;
	}

	@JsonProperty("COSTING_SHEET")
	public void setCostingSheet(String costingSheet) {
		this.costingSheet = costingSheet;
	}

	@JsonProperty("ACTY_INDEP_TEMPLATE")
	public String getActyIndepTemplate() {
		return actyIndepTemplate;
	}

	@JsonProperty("ACTY_INDEP_TEMPLATE")
	public void setActyIndepTemplate(String actyIndepTemplate) {
		this.actyIndepTemplate = actyIndepTemplate;
	}

	@JsonProperty("ACTY_DEP_TEMPLATE")
	public String getActyDepTemplate() {
		return actyDepTemplate;
	}

	@JsonProperty("ACTY_DEP_TEMPLATE")
	public void setActyDepTemplate(String actyDepTemplate) {
		this.actyDepTemplate = actyDepTemplate;
	}

	@JsonProperty("ADDR_TITLE")
	public String getAddrTitle() {
		return addrTitle;
	}

	@JsonProperty("ADDR_TITLE")
	public void setAddrTitle(String addrTitle) {
		this.addrTitle = addrTitle;
	}

	@JsonProperty("ADDR_NAME1")
	public String getAddrName1() {
		return addrName1;
	}

	@JsonProperty("ADDR_NAME1")
	public void setAddrName1(String addrName1) {
		this.addrName1 = addrName1;
	}

	@JsonProperty("ADDR_NAME2")
	public String getAddrName2() {
		return addrName2;
	}

	@JsonProperty("ADDR_NAME2")
	public void setAddrName2(String addrName2) {
		this.addrName2 = addrName2;
	}

	@JsonProperty("ADDR_NAME3")
	public String getAddrName3() {
		return addrName3;
	}

	@JsonProperty("ADDR_NAME3")
	public void setAddrName3(String addrName3) {
		this.addrName3 = addrName3;
	}

	@JsonProperty("ADDR_NAME4")
	public String getAddrName4() {
		return addrName4;
	}

	@JsonProperty("ADDR_NAME4")
	public void setAddrName4(String addrName4) {
		this.addrName4 = addrName4;
	}

	@JsonProperty("ADDR_STREET")
	public String getAddrStreet() {
		return addrStreet;
	}

	@JsonProperty("ADDR_STREET")
	public void setAddrStreet(String addrStreet) {
		this.addrStreet = addrStreet;
	}

	@JsonProperty("ADDR_CITY")
	public String getAddrCity() {
		return addrCity;
	}

	@JsonProperty("ADDR_CITY")
	public void setAddrCity(String addrCity) {
		this.addrCity = addrCity;
	}

	@JsonProperty("ADDR_DISTRICT")
	public String getAddrDistrict() {
		return addrDistrict;
	}

	@JsonProperty("ADDR_DISTRICT")
	public void setAddrDistrict(String addrDistrict) {
		this.addrDistrict = addrDistrict;
	}

	@JsonProperty("ADDR_COUNTRY")
	public String getAddrCountry() {
		return addrCountry;
	}

	@JsonProperty("ADDR_COUNTRY")
	public void setAddrCountry(String addrCountry) {
		this.addrCountry = addrCountry;
	}

	@JsonProperty("ADDR_TAXJURCODE")
	public String getAddrTaxjurcode() {
		return addrTaxjurcode;
	}

	@JsonProperty("ADDR_TAXJURCODE")
	public void setAddrTaxjurcode(String addrTaxjurcode) {
		this.addrTaxjurcode = addrTaxjurcode;
	}

	@JsonProperty("ADDR_PO_BOX")
	public String getAddrPoBox() {
		return addrPoBox;
	}

	@JsonProperty("ADDR_PO_BOX")
	public void setAddrPoBox(String addrPoBox) {
		this.addrPoBox = addrPoBox;
	}

	@JsonProperty("ADDR_POSTL_CODE")
	public String getAddrPostlCode() {
		return addrPostlCode;
	}

	@JsonProperty("ADDR_POSTL_CODE")
	public void setAddrPostlCode(String addrPostlCode) {
		this.addrPostlCode = addrPostlCode;
	}

	@JsonProperty("ADDR_POBX_PCD")
	public String getAddrPobxPcd() {
		return addrPobxPcd;
	}

	@JsonProperty("ADDR_POBX_PCD")
	public void setAddrPobxPcd(String addrPobxPcd) {
		this.addrPobxPcd = addrPobxPcd;
	}

	@JsonProperty("ADDR_REGION")
	public String getAddrRegion() {
		return addrRegion;
	}

	@JsonProperty("ADDR_REGION")
	public void setAddrRegion(String addrRegion) {
		this.addrRegion = addrRegion;
	}

	@JsonProperty("TELCO_LANGU")
	public String getTelcoLangu() {
		return telcoLangu;
	}

	@JsonProperty("TELCO_LANGU")
	public void setTelcoLangu(String telcoLangu) {
		this.telcoLangu = telcoLangu;
	}

	@JsonProperty("TELCO_TELEPHONE")
	public String getTelcoTelephone() {
		return telcoTelephone;
	}

	@JsonProperty("TELCO_TELEPHONE")
	public void setTelcoTelephone(String telcoTelephone) {
		this.telcoTelephone = telcoTelephone;
	}

	@JsonProperty("TELCO_TELEPHONE2")
	public String getTelcoTelephone2() {
		return telcoTelephone2;
	}

	@JsonProperty("TELCO_TELEPHONE2")
	public void setTelcoTelephone2(String telcoTelephone2) {
		this.telcoTelephone2 = telcoTelephone2;
	}

	@JsonProperty("TELCO_TELEBOX")
	public String getTelcoTelebox() {
		return telcoTelebox;
	}

	@JsonProperty("TELCO_TELEBOX")
	public void setTelcoTelebox(String telcoTelebox) {
		this.telcoTelebox = telcoTelebox;
	}

	@JsonProperty("TELCO_TELEX")
	public String getTelcoTelex() {
		return telcoTelex;
	}

	@JsonProperty("TELCO_TELEX")
	public void setTelcoTelex(String telcoTelex) {
		this.telcoTelex = telcoTelex;
	}

	@JsonProperty("TELCO_FAX_NUMBER")
	public String getTelcoFaxNumber() {
		return telcoFaxNumber;
	}

	@JsonProperty("TELCO_FAX_NUMBER")
	public void setTelcoFaxNumber(String telcoFaxNumber) {
		this.telcoFaxNumber = telcoFaxNumber;
	}

	@JsonProperty("TELCO_TELETEX")
	public String getTelcoTeletex() {
		return telcoTeletex;
	}

	@JsonProperty("TELCO_TELETEX")
	public void setTelcoTeletex(String telcoTeletex) {
		this.telcoTeletex = telcoTeletex;
	}

	@JsonProperty("TELCO_PRINTER")
	public String getTelcoPrinter() {
		return telcoPrinter;
	}

	@JsonProperty("TELCO_PRINTER")
	public void setTelcoPrinter(String telcoPrinter) {
		this.telcoPrinter = telcoPrinter;
	}

	@JsonProperty("TELCO_DATALINE")
	public String getTelcoDataline() {
		return telcoDataline;
	}

	@JsonProperty("TELCO_DATALINE")
	public void setTelcoDataline(String telcoDataline) {
		this.telcoDataline = telcoDataline;
	}

	@JsonProperty("JV_VENTURE")
	public String getJvVenture() {
		return jvVenture;
	}

	@JsonProperty("JV_VENTURE")
	public void setJvVenture(String jvVenture) {
		this.jvVenture = jvVenture;
	}

	@JsonProperty("JV_REC_IND")
	public String getJvRecInd() {
		return jvRecInd;
	}

	@JsonProperty("JV_REC_IND")
	public void setJvRecInd(String jvRecInd) {
		this.jvRecInd = jvRecInd;
	}

	@JsonProperty("JV_EQUITY_TYP")
	public String getJvEquityTyp() {
		return jvEquityTyp;
	}

	@JsonProperty("JV_EQUITY_TYP")
	public void setJvEquityTyp(String jvEquityTyp) {
		this.jvEquityTyp = jvEquityTyp;
	}

	@JsonProperty("FUND")
	public String getFund() {
		return fund;
	}

	@JsonProperty("FUND")
	public void setFund(String fund) {
		this.fund = fund;
	}

	@JsonProperty("GRANT_ID")
	public String getGrantId() {
		return grantId;
	}

	@JsonProperty("GRANT_ID")
	public void setGrantId(String grantId) {
		this.grantId = grantId;
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
