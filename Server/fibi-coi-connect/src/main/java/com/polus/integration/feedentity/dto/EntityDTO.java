package com.polus.integration.feedentity.dto;

import java.util.Date;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class EntityDTO {

	private String primaryName;
	private String certifiedEmail;
	private String telephoneNumber;
	private String sponsorAddressLine1;
	private String sponsorAddressLine2;
	private String sponsorCity;
	private String sponsorState;
	private String sponsorPostCode;
	private String sponsorCountryCode;
	private String orgAddressLine1;
	private String orgAddressLine2;
	private String orgCity;
	private String orgState;
	private String orgPostCode;
	private String orgCountryCode;
	private String updatedBy;
	private String createdBy;
	private String sponsorCode;
	private String sponsorTypeCode;
	private String customerNumber;
	private String auditReportSentForFy;
	private String ueiNumber;
	private String dodacNumber;
	private String dunsNumber;
	private String acronym;
	private String cageNumber;
	private String dunningCampaignId;
	private String organizationId;
	private Integer numberOfEmployees;
	private String irsTaxExemption;
	private String federalEmployerId;
	private String massTaxExemptNum;
	private String agencySymbol;
	private String vendorCode;
	private String comGovEntityCode;
	private String massEmployeeClaim;
	private String humanSubAssurance;
	private String animalWelfareAssurance;
	private Date scienceMisconductComplDate;
	private String phsAcount;
	private String nsfInstitutionalCode;
	private String indirectCostRateAgreement;
	private Integer cognizantAuditor;
	private Integer onrResidentRep;
	private String lobbyingRegistrant;
	private String lobbyingIndividual;
	private Date samExpirationDate;
	private Date incorporationDate;
	private String incorporatedIn;
	private Boolean isCreateSponsor;
	private Boolean isCreateOrganization;
	private Integer entityId;
	private Integer rolodexId;
	private String riskLevel;

}
