package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

public class AgreementAssociationDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	private Integer moduleCode;

	private String moduleItemKey;

	private String piName;

	private String accountNumber;

	private String leadUnitNumber;

	private String leadUnitName;

	private String title;

	private Timestamp loadTimestamp;

	private Integer statusCode;

	private String status;

	private Timestamp startDate;

	private Timestamp endDate;

	private String associatedDocument;

	private String sponsorCode;

	private String sponsorName;

	private Integer protocolId;

	private String protocolType;

	private Integer coiDisclosureId;

	private String disclosureDisposition;

	private String coiDisclosureNumber;

	private String subAwardPo;

	private String subrecipient;

	private String subPrime;

	private String submissionStatus;

	private String approvalDate;

	private String expirationDate;

	private String sponsorAwardId;

	private Integer moduleItemId;

	private String personId;

	private String documentNumber;

	private String contractAdminPersonId;

	private String primeSponsorCode;

	private String rolodexId;

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public String getLeadUnitName() {
		return leadUnitName;
	}

	public void setLeadUnitName(String leadUnitName) {
		this.leadUnitName = leadUnitName;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Timestamp getLoadTimestamp() {
		return loadTimestamp;
	}

	public void setLoadTimestamp(Timestamp loadTimestamp) {
		this.loadTimestamp = loadTimestamp;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
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

	public String getAssociatedDocument() {
		return associatedDocument;
	}

	public void setAssociatedDocument(String associatedDocument) {
		this.associatedDocument = associatedDocument;
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

	public Integer getProtocolId() {
		return protocolId;
	}

	public void setProtocolId(Integer protocolId) {
		this.protocolId = protocolId;
	}

	public String getProtocolType() {
		return protocolType;
	}

	public void setProtocolType(String protocolType) {
		this.protocolType = protocolType;
	}

	public Integer getCoiDisclosureId() {
		return coiDisclosureId;
	}

	public void setCoiDisclosureId(Integer coiDisclosureId) {
		this.coiDisclosureId = coiDisclosureId;
	}

	public String getDisclosureDisposition() {
		return disclosureDisposition;
	}

	public void setDisclosureDisposition(String disclosureDisposition) {
		this.disclosureDisposition = disclosureDisposition;
	}

	public String getCoiDisclosureNumber() {
		return coiDisclosureNumber;
	}

	public void setCoiDisclosureNumber(String coiDisclosureNumber) {
		this.coiDisclosureNumber = coiDisclosureNumber;
	}

	public String getSubAwardPo() {
		return subAwardPo;
	}

	public void setSubAwardPo(String subAwardPo) {
		this.subAwardPo = subAwardPo;
	}

	public String getSubrecipient() {
		return subrecipient;
	}

	public void setSubrecipient(String subrecipient) {
		this.subrecipient = subrecipient;
	}

	public String getSubPrime() {
		return subPrime;
	}

	public void setSubPrime(String subPrime) {
		this.subPrime = subPrime;
	}

	public String getSubmissionStatus() {
		return submissionStatus;
	}

	public void setSubmissionStatus(String submissionStatus) {
		this.submissionStatus = submissionStatus;
	}

	public String getApprovalDate() {
		return approvalDate;
	}

	public void setApprovalDate(String approvalDate) {
		this.approvalDate = approvalDate;
	}

	public String getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(String expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getSponsorAwardId() {
		return sponsorAwardId;
	}

	public void setSponsorAwardId(String sponsorAwardId) {
		this.sponsorAwardId = sponsorAwardId;
	}

	public Integer getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(Integer moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public String getContractAdminPersonId() {
		return contractAdminPersonId;
	}

	public void setContractAdminPersonId(String contractAdminPersonId) {
		this.contractAdminPersonId = contractAdminPersonId;
	}

	public String getPrimeSponsorCode() {
		return primeSponsorCode;
	}

	public void setPrimeSponsorCode(String primeSponsorCode) {
		this.primeSponsorCode = primeSponsorCode;
	}

	public String getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(String rolodexId) {
		this.rolodexId = rolodexId;
	}
}
