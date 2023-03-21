package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.*;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@javax.persistence.Table(name="ORGANIZATION")
@EntityListeners(AuditingEntityListener.class)
public class Organization implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name="ORGANIZATION_ID")
	private String organizationId;
	
	@Column(name="ORGANIZATION_NAME")
	private String organizationName;
	
	@Column(name="CONTACT_ADDRESS_ID")
	private Integer contactAddressId;

	@Column(name="ADDRESS")
	private String address;

	@Column(name="CABLE_ADDRESS")
	private String cableAddress;

	@Column(name="TELEX_NUMBER")
	private String telexNumber;

	@Column(name="CONGRESSIONAL_DISTRICT")
	private String congressionalDistrict;

	@Column(name="INCORPORATED_IN")
	private String incorporatedIn;

	@Column(name="INCORPORATED_DATE")
	private Timestamp incorporatedDate;

	@JsonIgnore
	@Column(name="NUMBER_OF_EMPLOYEES")
	private Integer numberOfEmployees;
	
	@JsonIgnore
	@Column(name="IRS_TAX_EXCEMPTION")
	private String irsTaxExcemption;
	
	@JsonIgnore
	@Column(name="FEDRAL_EMPLOYER_ID")
	private String fedralEmployerId;
	
	@JsonIgnore
	@Column(name="MASS_TAX_EXCEMPT_NUM")
	private String massTaxExcemptNum;
	
	@JsonIgnore
	@Column(name="AGENCY_SYMBOL")
	private String agencySymbol;

	@Column(name="VENDOR_CODE")
	private String vendorCode;
	
	@JsonIgnore
	@Column(name="COM_GOV_ENTITY_CODE")
	private String comGovEntityCode;
	
	@JsonIgnore
	@Column(name="MASS_EMPLOYEE_CLAIM")
	private String massEmployeeClaim;

	@Column(name="DUNS_NUMBER")
	private String dunsNumber;
	
	@JsonIgnore
	@Column(name="DUNS_PLUS_FOUR_NUMBER")
	private String dunsPlusFourNumber;

	@Column(name="DODAC_NUMBER")
	private String dodacNumber;

	@Column(name="CAGE_NUMBER")
	private String cageNumber;

	@Column(name="HUMAN_SUB_ASSURANCE")
	private String humanSubAssurance;

	@Column(name="SCIENCE_MISCONDUCT_COMPL_DATE")
	private Timestamp scienceMisconductComplDate;

	@Column(name="ANIMAL_WELFARE_ASSURANCE")
	private String animalWelfareAssurance;

	@Column(name="PHS_ACOUNT")
	private String phsAcount;

	@Column(name="NSF_INSTITUTIONAL_CODE")
	private String nsfInstitutionalCode;

	@Column(name="INDIRECT_COST_RATE_AGREEMENT")
	private String indirectCostRateAgreement;

	@Column(name="COGNIZANT_AUDITOR")
	private Integer cognizantAuditor;
	
	@JsonIgnore
	@Column(name="ONR_RESIDENT_REP")
	private Integer onrResidentRep;

	@LastModifiedDate
	@Column(name="UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@LastModifiedBy
	@Column(name="UPDATE_USER")
	private String updateUser;

	@Column(name = "COUNTRY_CODE")
	private String countryCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORGANIZATION_FK1"), name = "COUNTRY_CODE", referencedColumnName = "COUNTRY_CODE", insertable = false, updatable = false)
	private Country country;

	@Transient
	private String contactPersonName;
	
	@Column(name = "IS_PARTNERING_ORGANIZATION")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPartneringOrganization;

	public Boolean getIsPartneringOrganization() {
		return isPartneringOrganization;
	}

	public void setIsPartneringOrganization(Boolean isPartneringOrganization) {
		this.isPartneringOrganization = isPartneringOrganization;
	}

	public String getContactPersonName() {
		return contactPersonName;
	}

	public void setContactPersonName(String contactPersonName) {
		this.contactPersonName = contactPersonName;
	}

	public Organization() {
		super();
	}

	public Organization(String organizationId, String organizationName) {
		this.organizationId = organizationId;
		this.organizationName = organizationName;
	}

	public Integer getContactAddressId() {
		return contactAddressId;
	}

	public void setContactAddressId(Integer contactAddressId) {
		this.contactAddressId = contactAddressId;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String getCableAddress() {
		return cableAddress;
	}

	public void setCableAddress(String cableAddress) {
		this.cableAddress = cableAddress;
	}

	public String getTelexNumber() {
		return telexNumber;
	}

	public void setTelexNumber(String telexNumber) {
		this.telexNumber = telexNumber;
	}

	public String getCongressionalDistrict() {
		return congressionalDistrict;
	}

	public void setCongressionalDistrict(String congressionalDistrict) {
		this.congressionalDistrict = congressionalDistrict;
	}

	public String getIncorporatedIn() {
		return incorporatedIn;
	}

	public void setIncorporatedIn(String incorporatedIn) {
		this.incorporatedIn = incorporatedIn;
	}

	public Timestamp getIncorporatedDate() {
		return incorporatedDate;
	}

	public void setIncorporatedDate(Timestamp incorporatedDate) {
		this.incorporatedDate = incorporatedDate;
	}

	public Integer getNumberOfEmployees() {
		return numberOfEmployees;
	}

	public void setNumberOfEmployees(Integer numberOfEmployees) {
		this.numberOfEmployees = numberOfEmployees;
	}

	public String getIrsTaxExcemption() {
		return irsTaxExcemption;
	}

	public void setIrsTaxExcemption(String irsTaxExcemption) {
		this.irsTaxExcemption = irsTaxExcemption;
	}

	public String getFedralEmployerId() {
		return fedralEmployerId;
	}

	public void setFedralEmployerId(String fedralEmployerId) {
		this.fedralEmployerId = fedralEmployerId;
	}

	public String getMassTaxExcemptNum() {
		return massTaxExcemptNum;
	}

	public void setMassTaxExcemptNum(String massTaxExcemptNum) {
		this.massTaxExcemptNum = massTaxExcemptNum;
	}

	public String getVendorCode() {
		return vendorCode;
	}

	public void setVendorCode(String vendorCode) {
		this.vendorCode = vendorCode;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public String getComGovEntityCode() {
		return comGovEntityCode;
	}

	public void setComGovEntityCode(String comGovEntityCode) {
		this.comGovEntityCode = comGovEntityCode;
	}

	public String getMassEmployeeClaim() {
		return massEmployeeClaim;
	}

	public void setMassEmployeeClaim(String massEmployeeClaim) {
		this.massEmployeeClaim = massEmployeeClaim;
	}

	public String getDunsNumber() {
		return dunsNumber;
	}

	public void setDunsNumber(String dunsNumber) {
		this.dunsNumber = dunsNumber;
	}

	public String getDunsPlusFourNumber() {
		return dunsPlusFourNumber;
	}

	public void setDunsPlusFourNumber(String dunsPlusFourNumber) {
		this.dunsPlusFourNumber = dunsPlusFourNumber;
	}

	public String getDodacNumber() {
		return dodacNumber;
	}

	public void setDodacNumber(String dodacNumber) {
		this.dodacNumber = dodacNumber;
	}

	public String getCageNumber() {
		return cageNumber;
	}

	public void setCageNumber(String cageNumber) {
		this.cageNumber = cageNumber;
	}

	public String getHumanSubAssurance() {
		return humanSubAssurance;
	}

	public void setHumanSubAssurance(String humanSubAssurance) {
		this.humanSubAssurance = humanSubAssurance;
	}

	public String getAnimalWelfareAssurance() {
		return animalWelfareAssurance;
	}

	public void setAnimalWelfareAssurance(String animalWelfareAssurance) {
		this.animalWelfareAssurance = animalWelfareAssurance;
	}

	public String getPhsAcount() {
		return phsAcount;
	}

	public void setPhsAcount(String phsAcount) {
		this.phsAcount = phsAcount;
	}

	public String getNsfInstitutionalCode() {
		return nsfInstitutionalCode;
	}

	public void setNsfInstitutionalCode(String nsfInstitutionalCode) {
		this.nsfInstitutionalCode = nsfInstitutionalCode;
	}

	public String getIndirectCostRateAgreement() {
		return indirectCostRateAgreement;
	}

	public void setIndirectCostRateAgreement(String indirectCostRateAgreement) {
		this.indirectCostRateAgreement = indirectCostRateAgreement;
	}

	public Integer getCognizantAuditor() {
		return cognizantAuditor;
	}

	public void setCognizantAuditor(Integer cognizantAuditor) {
		this.cognizantAuditor = cognizantAuditor;
	}

	public Integer getOnrResidentRep() {
		return onrResidentRep;
	}

	public void setOnrResidentRep(Integer onrResidentRep) {
		this.onrResidentRep = onrResidentRep;
	}

	public String getAgencySymbol() {
		return agencySymbol;
	}

	public void setAgencySymbol(String agencySymbol) {
		this.agencySymbol = agencySymbol;
	}

	public String getOrganizationId() {
		return organizationId;
	}

	public void setOrganizationId(String organizationId) {
		this.organizationId = organizationId;
	}

	public String getOrganizationName() {
		return organizationName;
	}

	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
	}

	public Timestamp getScienceMisconductComplDate() {
		return scienceMisconductComplDate;
	}

	public void setScienceMisconductComplDate(Timestamp scienceMisconductComplDate) {
		this.scienceMisconductComplDate = scienceMisconductComplDate;
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

}
