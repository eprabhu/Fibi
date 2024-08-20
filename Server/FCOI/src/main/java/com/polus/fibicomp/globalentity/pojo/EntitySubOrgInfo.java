package com.polus.fibicomp.globalentity.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Table(name = "ENTITY_SUB_ORG_INFO")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntitySubOrgInfo implements Serializable {

	private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "ID")
    private int id;

    @Column(name = "ENTITY_ID")
	private int entityId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_SUB_ORG_INFO_FK1"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private GlobalEntity entity;

    @Column(name = "ORGANIZATION_ID")
    private Integer organizationId;
   
    @Column(name = "ORGANIZATION_TYPE_CODE")
    private String organizationTypeCode;

    @Column(name = "FEED_STATUS_CODE")
    private String feedStatusCode;

    @Column(name = "NUMBER_OF_EMPLOYEES")
    private Integer numberOfEmployees;

    @Column(name = "IRS_TAX_EXEMPTION")
    private String irsTaxExemption;

    @Column(name = "FEDERAL_EMPLOYER_ID")
    private String federalEmployerId;

    @Column(name = "MASS_TAX_EXEMPT_NUM")
    private String massTaxExemptNum;

    @Column(name = "AGENCY_SYMBOL")
    private String agencySymbol;

    @Column(name = "VENDOR_CODE")
    private String vendorCode;

    @Column(name = "COM_GOV_ENTITY_CODE")
    private String comGovEntityCode;

    @Column(name = "MASS_EMPLOYEE_CLAIM")
    private String massEmployeeClaim;

    @Column(name = "HUMAN_SUB_ASSURANCE")
    private String humanSubAssurance;

    @Column(name = "ANIMAL_WELFARE_ASSURANCE")
    private String animalWelfareAssurance;

    @Column(name = "SCIENCE_MISCONDUCT_COMPL_DATE")
    private Date scienceMisconductComplDate;

    @Column(name = "PHS_ACOUNT")
    private String phsAcount;

    @Column(name = "NSF_INSTITUTIONAL_CODE")
    private String nsfInstitutionalCode;

    @Column(name = "INDIRECT_COST_RATE_AGREEMENT")
    private String indirectCostRateAgreement;

    @Column(name = "COGNIZANT_AUDITOR")
    private String cognizantAuditor;

    @Column(name = "ONR_RESIDENT_REP")
    private String onrResidentRep;

    @Column(name = "LOBBYING_REGISTRANT")
    private String lobbyingRegistrant;

    @Column(name = "LOBBYING_INDIVIDUAL")
    private String lobbyingIndividual;

    @Column(name = "SAM_EXPIRATION_DATE")
    private Date samExpirationDate;

    @Column(name = "SUB_AWD_RISK_ASSMT_DATE")
    private Date subAwdRiskAssmtDate;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATED_BY")
    private String updatedBy;

}
